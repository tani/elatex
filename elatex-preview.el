;;; elatex-preview.el --- Realtime math-at-point previews  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tex, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; `elatex-preview-mode' displays an eLaTeX rendering below the source line
;; whenever point is inside mathematical content, never on its delimiters.
;; It supports `markdown-mode', `markdown-ts-mode', `org-mode', `latex-mode',
;; and `latex-ts-mode'.  Markdown also recognizes GitHub dollar-backtick
;; inline math and `math' fenced code blocks.
;;
;; Enable it in one buffer with:
;;
;;   M-x elatex-preview-mode
;;
;; Or enable it automatically in every supported buffer with:
;;
;;   (elatex-preview-global-mode 1)
;;
;; The preview is a terminal-safe text overlay: it encloses eLaTeX output in
;; a Unicode box and works in graphical frames and in `emacs -nw'.  Source
;; text is never modified.  Updates are coalesced with a short idle timer
;; while typing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'elatex)

(declare-function markdown-code-block-at-point-p "markdown-mode" (&optional pos))
(declare-function markdown-inline-code-at-point-p "markdown-mode" (&optional pos))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-property "org-element" (property node &optional dflt force-undefer))
(declare-function org-element-type "org-element" (element))

(defgroup elatex-preview nil
  "Realtime eLaTeX previews for source buffers."
  :group 'elatex)

(defcustom elatex-preview-idle-delay 0.05
  "Idle seconds before refreshing a preview after point or text changes.
Set this to zero for synchronous updates."
  :type 'number
  :group 'elatex-preview)

(defcustom elatex-preview-style 'unicode
  "Drawing style used for previews."
  :type '(choice (const unicode) (const ascii))
  :group 'elatex-preview)

(defcustom elatex-preview-font "text"
  "Root mathematical FONT passed to `elatex-render'."
  :type 'string
  :group 'elatex-preview)

(defcustom elatex-preview-line-width 0
  "Preferred preview width in cells.
Zero disables eLaTeX line wrapping."
  :type 'integer
  :group 'elatex-preview)

(defcustom elatex-preview-max-scan-distance 20000
  "Maximum characters searched on either side of point for math markup."
  :type 'integer
  :group 'elatex-preview)

(defcustom elatex-preview-supported-modes
  '(markdown-mode markdown-ts-mode org-mode latex-mode latex-ts-mode)
  "Major modes in which `elatex-preview-mode' may be enabled."
  :type '(repeat symbol)
  :group 'elatex-preview)

(defcustom elatex-preview-math-environments
  '("math" "displaymath" "equation" "equation*" "align" "align*"
    "aligned" "gather" "gather*" "multline" "multline*" "eqnarray"
    "eqnarray*")
  "LaTeX environments recognized as mathematical source."
  :type '(repeat string)
  :group 'elatex-preview)

(defface elatex-preview-output-face
  '((t :inherit default))
  "Face used for rendered preview text."
  :group 'elatex-preview)

(cl-defstruct (elatex-preview--context
               (:constructor elatex-preview--make-context))
  begin end content-begin content-end content)

(defconst elatex-preview--single-dollar-regexp
  (concat "\\(" (regexp-quote "$") "\\)"
          "\\(\\(?:\\\\.\\|[^$]\\)*?\\)"
          "\\(" (regexp-quote "$") "\\)"))

(defconst elatex-preview--double-dollar-regexp
  (concat "\\(" (regexp-quote "$$") "\\)"
          "\\(\\(?:\\\\.\\|[^$]\\)*?\\)"
          "\\(" (regexp-quote "$$") "\\)"))

(defconst elatex-preview--dollar-backtick-regexp
  (concat "\\(" (regexp-quote "$`") "\\)"
          "\\([^`\n]*?\\)"
          "\\(" (regexp-quote "`$") "\\)"))

(defun elatex-preview--paired-regexp (open close)
  "Return a regexp matching OPEN, arbitrary content, and CLOSE.
The three components are captured in groups one through three."
  (concat "\\(" (regexp-quote open) "\\)"
          "\\(\\(?:.\\|\n\\)*?\\)"
          "\\(" (regexp-quote close) "\\)"))

(defconst elatex-preview--parenthesized-regexp
  (elatex-preview--paired-regexp "\\(" "\\)"))

(defconst elatex-preview--bracketed-regexp
  (elatex-preview--paired-regexp "\\[" "\\]"))

(defvar elatex-preview-mode)

(defvar-local elatex-preview--overlay nil)
(defvar-local elatex-preview--timer nil)
(defvar-local elatex-preview--last-signature nil)

(defun elatex-preview--supported-mode-p ()
  "Return non-nil when the current major mode supports previews."
  (apply #'derived-mode-p elatex-preview-supported-modes))

(defun elatex-preview--escaped-p (position)
  "Return non-nil when the character at POSITION is backslash-escaped."
  (let ((index (1- position))
        (slashes 0))
    (while (and (>= index (point-min))
                (= (char-after index) ?\\))
      (setq slashes (1+ slashes)
            index (1- index)))
    (= (% slashes 2) 1)))

(defun elatex-preview--markdown-literal-p (position)
  "Return non-nil when POSITION is Markdown code rather than prose."
  (and (derived-mode-p 'markdown-mode 'markdown-ts-mode)
       (or (and (fboundp 'markdown-inline-code-at-point-p)
                (markdown-inline-code-at-point-p position))
           (and (fboundp 'markdown-code-block-at-point-p)
                (markdown-code-block-at-point-p position)))))

(defun elatex-preview--ignored-position-p
    (position &optional allow-markdown-literal)
  "Return non-nil when math markup at POSITION must be ignored.
ALLOW-MARKDOWN-LITERAL permits GitHub's backtick-delimited math syntax."
  (save-excursion
    (or (nth 4 (syntax-ppss position))
        (and (not allow-markdown-literal)
             (elatex-preview--markdown-literal-p position)))))

(defun elatex-preview--valid-dollar-match-p (kind begin end)
  "Return non-nil when a KIND dollar match from BEGIN to END is isolated."
  (let ((single-p (eq kind 'single)))
    (and (not (elatex-preview--escaped-p begin))
         (not (eq (char-before begin) ?$))
         (not (eq (char-after end) ?$))
         (or (not single-p)
             (and (not (eq (char-after (1+ begin)) ?$))
                  (not (eq (char-before (1- end)) ?$)))))))

(defun elatex-preview--content-at-position-p (begin end position)
  "Return non-nil when POSITION is within nonempty content BEGIN through END."
  (and (< begin end) (<= begin position) (< position end)))

(defun elatex-preview--regexp-context
    (regexp position &optional dollar-kind allow-markdown-literal)
  "Return math context matching REGEXP around POSITION.
DOLLAR-KIND is nil, `single', or `double' and enables dollar validation.
ALLOW-MARKDOWN-LITERAL permits GitHub's backtick-delimited math syntax."
  (save-excursion
    (goto-char (max (point-min)
                    (- position (max 0 elatex-preview-max-scan-distance))))
    (let ((limit (min (point-max)
                      (+ position (max 0 elatex-preview-max-scan-distance))))
          best)
      (while (re-search-forward regexp limit t)
        (let ((begin (match-beginning 1))
              (end (match-end 3))
              (content-begin (match-beginning 2))
              (content-end (match-end 2)))
          (when (and (elatex-preview--content-at-position-p
                      content-begin content-end position)
                     (not (elatex-preview--escaped-p begin))
                     (not (elatex-preview--ignored-position-p
                           begin allow-markdown-literal))
                     (or (null dollar-kind)
                         (elatex-preview--valid-dollar-match-p
                          dollar-kind begin end)))
            (let ((candidate
                   (elatex-preview--make-context
                    :begin begin :end end
                    :content-begin content-begin :content-end content-end
                    :content (match-string-no-properties 2))))
              (when (or (null best)
                        (< (- end begin)
                           (- (elatex-preview--context-end best)
                              (elatex-preview--context-begin best))))
                (setq best candidate))))))
      best)))

(defun elatex-preview--markdown-fenced-math-context (position)
  "Return the fenced Markdown math block containing POSITION."
  (save-excursion
    (goto-char (max (point-min)
                    (- position (max 0 elatex-preview-max-scan-distance))))
    (let ((case-fold-search nil)
          (limit (min (point-max)
                      (+ position (max 0 elatex-preview-max-scan-distance))))
          context)
      (while (and (not context)
                  (re-search-forward
                   "^ \\{0,3\\}\\(`\\{3,\\}\\)[ \t]*math[ \t]*\r?$"
                   position t))
        (let* ((begin (match-beginning 0))
               (fence (match-string-no-properties 1))
               (content-begin
                (min (point-max) (1+ (line-end-position))))
               (close-regexp
                (concat "^ \\{0,3\\}" (regexp-quote fence)
                        "`*[ \t]*\r?$")))
          (save-excursion
            (goto-char content-begin)
            (when (re-search-forward close-regexp limit t)
              (let* ((close-begin (match-beginning 0))
                     (end (line-end-position))
                     (content-end
                      (if (and (> close-begin content-begin)
                               (= (char-before close-begin) ?\n))
                          (1- close-begin)
                        close-begin)))
                (when (and (elatex-preview--content-at-position-p
                            content-begin content-end position)
                           (<= begin position) (<= position end))
                  (setq context
                        (elatex-preview--make-context
                         :begin begin :end end
                         :content-begin content-begin :content-end content-end
                         :content (buffer-substring-no-properties
                                   content-begin content-end)))))))))
      context)))

(defun elatex-preview--environment-context (position)
  "Return the innermost supported LaTeX environment around POSITION."
  (save-excursion
    (goto-char position)
    (let ((limit (max (point-min)
                      (- position (max 0 elatex-preview-max-scan-distance))))
          best)
      (while (re-search-backward "\\\\begin{\\([^}\n]+\\)}" limit t)
        (let ((begin (match-beginning 0))
              (content-begin (match-end 0))
              (name (match-string-no-properties 1)))
          (when (and (member name elatex-preview-math-environments)
                     (not (elatex-preview--escaped-p begin))
                     (not (elatex-preview--ignored-position-p begin)))
            (save-excursion
              (goto-char content-begin)
              (when (search-forward (format "\\end{%s}" name)
                                    (min (point-max)
                                         (+ position
                                            (max 0 elatex-preview-max-scan-distance)))
                                    t)
                (let ((end (point))
                      (content-end (match-beginning 0)))
                  (when (and (elatex-preview--content-at-position-p
                              content-begin content-end position)
                             (<= begin position) (<= position end))
                    (let ((candidate
                           (elatex-preview--make-context
                            :begin begin :end end
                            :content-begin content-begin
                            :content-end content-end
                            :content (buffer-substring-no-properties
                                      content-begin content-end))))
                      (when (or (null best)
                                (< (- end begin)
                                   (- (elatex-preview--context-end best)
                                      (elatex-preview--context-begin best))))
                        (setq best candidate))))))))))
      best)))

(defun elatex-preview--org-content-bounds (source)
  "Return SOURCE offsets delimiting the mathematical content."
  (cond
   ((and (string-prefix-p "$$" source) (string-suffix-p "$$" source))
    (cons 2 (- (length source) 2)))
   ((and (string-prefix-p "$" source) (string-suffix-p "$" source))
    (cons 1 (1- (length source))))
   ((and (string-prefix-p "\\(" source) (string-suffix-p "\\)" source))
    (cons 2 (- (length source) 2)))
   ((and (string-prefix-p "\\[" source) (string-suffix-p "\\]" source))
    (cons 2 (- (length source) 2)))
   ((string-match "\\`\\\\begin{\\([^}\n]+\\)}[ \t]*\n?" source)
    (let* ((name (match-string 1 source))
           (begin (match-end 0))
           (close (concat "\\end{" name "}")))
      (if (string-match
           (concat (regexp-quote close) "[ \t]*\n?\\'") source begin)
          (cons begin (match-beginning 0))
        (cons 0 (length source)))))
   (t (cons 0 (length source)))))


(defun elatex-preview--org-context (position)
  "Return Org math context around POSITION."
  (require 'org-element)
  (save-excursion
    (let (context)
      (dolist (probe (delete-dups
                      (mapcar (lambda (value)
                                (min (point-max) (max (point-min) value)))
                              (list position (1- position) (1+ position)))))
        (unless context
          (goto-char probe)
          (let* ((element (org-element-context))
                 (type (org-element-type element)))
            (when (memq type '(latex-fragment latex-environment))
              (let* ((begin (org-element-property :begin element))
                     (source (org-element-property :value element))
                     (end (+ begin (length source)))
                     (bounds (elatex-preview--org-content-bounds source))
                     (content-begin (+ begin (car bounds)))
                     (content-end (+ begin (cdr bounds))))
                (when (elatex-preview--content-at-position-p
                       content-begin content-end position)
                  (setq context
                        (elatex-preview--make-context
                         :begin begin :end end
                         :content-begin content-begin :content-end content-end
                         :content (substring source
                                             (car bounds) (cdr bounds))))))))))
      context)))

(defun elatex-preview--prefer-context (&rest contexts)
  "Return the smallest non-nil context in CONTEXTS."
  (let (best)
    (dolist (context contexts best)
      (when (and context
                 (or (null best)
                     (< (- (elatex-preview--context-end context)
                           (elatex-preview--context-begin context))
                        (- (elatex-preview--context-end best)
                           (elatex-preview--context-begin best)))))
        (setq best context)))))

(defun elatex-preview--delimited-context (position)
  "Return a delimited or environment math context around POSITION."
  (elatex-preview--prefer-context
   (elatex-preview--regexp-context
    elatex-preview--double-dollar-regexp position 'double)
   (elatex-preview--regexp-context
    elatex-preview--single-dollar-regexp position 'single)
   (elatex-preview--regexp-context elatex-preview--parenthesized-regexp position)
   (elatex-preview--regexp-context elatex-preview--bracketed-regexp position)
   (elatex-preview--environment-context position)))

(defun elatex-preview--context-at-point ()
  "Return a supported mathematical context containing point."
  (cond
   ((derived-mode-p 'org-mode) (elatex-preview--org-context (point)))
   ((derived-mode-p 'markdown-mode 'markdown-ts-mode)
    (or (elatex-preview--markdown-fenced-math-context (point))
        (elatex-preview--regexp-context
         elatex-preview--dollar-backtick-regexp (point) nil t)
        (elatex-preview--delimited-context (point))))
   ((derived-mode-p 'latex-mode 'latex-ts-mode)
    (elatex-preview--delimited-context (point)))))

(defun elatex-preview--box-output (output)
  "Return OUTPUT enclosed in a padded Unicode box.
An empty OUTPUT has no rendered formula and is returned unchanged.  Widths use
eLaTeX's pinned cell-width tables so every row reaches the same terminal
column before its right border."
  (if (string-empty-p output)
      output
    (let* ((lines (split-string output "\n" nil))
           (width (apply #'max (mapcar #'elatex--strspaces lines)))
           (horizontal (make-string (+ width 2) ?─)))
      (concat "╭" horizontal "╮\n"
              (mapconcat
               (lambda (line)
                 (concat "│ " line
                         (make-string (- width (elatex--strspaces line)) ?\s)
                         " │"))
               lines "\n")
              "\n╰" horizontal "╯"))))

(defun elatex-preview--format-after-string (output errors)
  "Format boxed rendered OUTPUT and ordered ERRORS for an overlay after-string."
  (let* ((boxed-output (elatex-preview--box-output output))
         (error-text (and errors (mapconcat #'identity errors "; "))))
    (concat
     (unless (string-empty-p boxed-output)
       (concat "\n"
               (propertize boxed-output 'face 'elatex-preview-output-face)))
     (when error-text
       (concat "\n" (propertize error-text 'face 'error))))))

(defun elatex-preview--clear ()
  "Remove the current preview overlay and cached signature."
  (when (overlayp elatex-preview--overlay)
    (delete-overlay elatex-preview--overlay))
  (setq elatex-preview--overlay nil
        elatex-preview--last-signature nil))
(defun elatex-preview--show (context output errors)
  "Display OUTPUT and ERRORS for CONTEXT below its final source line."
  (let* ((position
          (save-excursion
            (goto-char (elatex-preview--context-end context))
            (line-end-position)))
         ;; A zero-width overlay at end of buffer has no terminal cell to
         ;; attach its `after-string' to.  Anchor to the final source cell so
         ;; the preview remains visible in `emacs -nw' at end of buffer.
         (start (if (= position (point-min)) position (1- position))))
    (unless (overlayp elatex-preview--overlay)
      (setq elatex-preview--overlay (make-overlay start position nil t t))
      (overlay-put elatex-preview--overlay 'evaporate t)
      (overlay-put elatex-preview--overlay 'priority 100))
    (move-overlay elatex-preview--overlay start position)
    (overlay-put elatex-preview--overlay 'after-string
                 (elatex-preview--format-after-string output errors))))

(defun elatex-preview-refresh ()
  "Synchronously refresh the math preview at point."
  (interactive)
  (let ((context
         (and elatex-preview-mode
              (elatex-preview--supported-mode-p)
              (elatex-preview--context-at-point))))
    (if (null context)
        (elatex-preview--clear)
      (let ((signature
             (list (elatex-preview--context-begin context)
                   (elatex-preview--context-end context)
                   (elatex-preview--context-content context)
                   elatex-preview-style elatex-preview-font
                   elatex-preview-line-width)))
        (unless (equal signature elatex-preview--last-signature)
          (setq elatex-preview--last-signature signature)
          (condition-case error-data
              (let* ((result
                      (elatex-render
                       (elatex-preview--context-content context)
                       :style elatex-preview-style
                       :font elatex-preview-font
                       :line-width elatex-preview-line-width))
                     (output (elatex-result-output result))
                     (errors (elatex-result-errors result)))
                (elatex-preview--show context output errors))
            (error
             (elatex-preview--show
              context "" (list (error-message-string error-data))))))))))

(defun elatex-preview--timer-fire (buffer)
  "Refresh the preview in BUFFER after an idle delay."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq elatex-preview--timer nil)
      (when elatex-preview-mode
        (elatex-preview-refresh)))))

(defun elatex-preview--schedule-update (&rest _ignored)
  "Schedule a preview refresh, coalescing repeated edits and commands."
  (when (timerp elatex-preview--timer)
    (cancel-timer elatex-preview--timer))
  (setq elatex-preview--timer nil)
  (if (<= elatex-preview-idle-delay 0)
      (elatex-preview-refresh)
    (setq elatex-preview--timer
          (run-with-idle-timer elatex-preview-idle-delay nil
                               #'elatex-preview--timer-fire
                               (current-buffer)))))

(defun elatex-preview--disable ()
  "Remove hooks, timers, and overlays owned by the preview mode."
  (remove-hook 'post-command-hook #'elatex-preview--schedule-update t)
  (remove-hook 'after-change-functions #'elatex-preview--schedule-update t)
  (when (timerp elatex-preview--timer)
    (cancel-timer elatex-preview--timer))
  (setq elatex-preview--timer nil)
  (elatex-preview--clear))

;;;###autoload
(define-minor-mode elatex-preview-mode
  "Preview the TeX-like mathematical expression containing point.

The preview appears below the expression's final source line and updates after
point movement or buffer edits.  Supported major modes are controlled by
`elatex-preview-supported-modes'."
  :lighter " eL"
  :group 'elatex-preview
  (if elatex-preview-mode
      (if (not (elatex-preview--supported-mode-p))
          (progn
            (setq elatex-preview-mode nil)
            (user-error "eLaTeX preview does not support %s" major-mode))
        (add-hook 'post-command-hook #'elatex-preview--schedule-update nil t)
        (add-hook 'after-change-functions #'elatex-preview--schedule-update nil t)
        (elatex-preview--schedule-update))
    (elatex-preview--disable)))

(defun elatex-preview--turn-on ()
  "Enable `elatex-preview-mode' in supported buffers."
  (when (elatex-preview--supported-mode-p)
    (elatex-preview-mode 1)))

;;;###autoload
(define-globalized-minor-mode elatex-preview-global-mode
  elatex-preview-mode elatex-preview--turn-on
  :group 'elatex-preview)

(provide 'elatex-preview)
;;; elatex-preview.el ends here
