;;; elatex.el --- Unicode TeX-like math rendering  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tex, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Native Emacs Lisp port of libtexprintf 1.31 by Bart Pieters, pinned at
;; revision 18977837b20649d56a651eb6bf846f1c914db77a.

;;; Commentary:

;; Public facade for Unicode TeX-like mathematical rendering.
;;
;; The translated entry points are `elatex-string` (texstring),
;; `elatex-sprintf` (stexprintf), `elatex-printf` (texprintf),
;; `elatex-fprintf` (ftexprintf), `elatex-box-tree` (texboxtree),
;; `elatex-list-symbols` (ListSymbols), `elatex-symbols-string`
;; (Symbols_Str), `elatex-errors` (E_Messages), `elatex-errors-string`
;; (E_Messages_str), `elatex-error-state` (texerror_state), and the style and
;; font setters corresponding to SetStyleASCII, SetStyleUNICODE,
;; ToggleMapSuperSub, ToggleAvoidCombining, and SetRootFont.
;;
;; Formatted wrappers intentionally use native Emacs `format`.  Invalid
;; configuration, bytes, code points, and source undefined behavior become
;; deterministic Lisp conditions rather than process exit or unsafe memory.
;; Lisp GC replaces texfree.  `elatex-render` adds an idiomatic per-call result
;; API without persistent configuration side effects.

;;; Code:

(require 'cl-lib)
(require 'elatex-draw)
(require 'elatex-error)
(defvar external-debugging-output #'external-debugging-output
  "Output stream used by `elatex-errors'; defaults to Emacs' stderr function.")
(require 'elatex-parser)

(defgroup elatex nil
  "Render TeX-like mathematics as Unicode monospace text."
  :group 'tex)

(defcustom elatex-line-width 0
  "Preferred output line width in cells.
Zero disables wrapping.  Negative values are accepted and behave as zero."
  :type 'integer
  :group 'elatex)

(defcustom elatex-default-font "text"
  "Persistent root font name used by translated rendering entry points."
  :type 'string
  :group 'elatex)

(defcustom elatex-wide-character-width 2
  "Cell width assigned to characters in the pinned wide ranges.
The value must be either 1 or 2."
  :type '(choice (const 1) (const 2))
  :group 'elatex)

(defcustom elatex-full-width-character-width 2
  "Cell width assigned to characters in the pinned full-width ranges.
The value must be either 1 or 2."
  :type '(choice (const 1) (const 2))
  :group 'elatex)

(define-error 'elatex-error "eLaTeX error")
(define-error 'elatex-invalid-input "Invalid eLaTeX input" 'elatex-error)
(define-error 'elatex-invalid-option "Invalid eLaTeX option" 'elatex-error)
(define-error 'elatex-render-error "eLaTeX render error" 'elatex-error)

(defconst elatex-symbols
  (vconcat
   (mapcar (lambda (record) (cons (aref record 0) (aref record 1)))
           (append elatex--symbols nil)))
  "Ordered vector of known symbol commands and their Unicode code points.")

(defvar elatex--unicode-style
  (copy-elatex--style elatex--style-unicode-template)
  "Persistent mutable Unicode style record.")
(defvar elatex--ascii-style
  (copy-elatex--style elatex--style-ascii-template)
  "Persistent mutable ASCII style record.")
(defvar elatex--selected-style 'unicode
  "Persistent selected style name, either `unicode' or `ascii'.")

(cl-defstruct (elatex-result
               (:constructor elatex--make-result (output errors)))
  "Immutable public result of `elatex-render'."
  (output nil :read-only t)
  (errors nil :read-only t))

(defun elatex--invalid-input (value)
  "Signal `elatex-invalid-input' carrying VALUE."
  (signal 'elatex-invalid-input (list value)))

(defun elatex--invalid-option (name value)
  "Signal `elatex-invalid-option' carrying option NAME and VALUE."
  (signal 'elatex-invalid-option (list name value)))

(defun elatex--valid-scalar-p (character)
  "Return non-nil when CHARACTER is a valid Unicode scalar value."
  (and (integerp character) (<= 0 character) (<= character #x10FFFF)
       (not (<= #xD800 character #xDFFF))
       (not (eq (char-charset character) 'eight-bit))))

(defun elatex--normalize-input (value)
  "Validate VALUE as input, decode strict UTF-8 bytes, and stop at NUL."
  (unless (stringp value) (elatex--invalid-input value))
  (let ((string
         (if (multibyte-string-p value)
             value
           (let ((decoded (decode-coding-string value 'utf-8)))
             (unless (and
                      (equal value (encode-coding-string decoded 'utf-8))
                      (cl-every #'elatex--valid-scalar-p
                                (string-to-list decoded)))
               (elatex--invalid-input value))
             decoded))))
    (unless (cl-every #'elatex--valid-scalar-p (string-to-list string))
      (elatex--invalid-input value))
    (let ((nul (cl-position 0 string)))
      (if nul (substring string 0 nul) string))))

(defun elatex--validate-render-options
    (font line-width style wide-width full-width on-error)
  "Validate common render options and signal deterministic conditions."
  (unless (stringp font) (elatex--invalid-option 'font font))
  (unless (integerp line-width) (elatex--invalid-option 'line-width line-width))
  (unless (memq style '(nil unicode ascii))
    (elatex--invalid-option 'style style))
  (unless (memq wide-width '(1 2))
    (elatex--invalid-option 'wide-character-width wide-width))
  (unless (memq full-width '(1 2))
    (elatex--invalid-option 'full-width-character-width full-width))
  (unless (or (null on-error) (functionp on-error))
    (elatex--invalid-option 'on-error on-error)))

(defun elatex--persistent-style (kind)
  "Return persistent style record for KIND or the current selection."
  (pcase (or kind elatex--selected-style)
    ('ascii elatex--ascii-style)
    (_ elatex--unicode-style)))

(defun elatex--render-values
    (input line-width font style-kind wide-width full-width
           &optional map-super-sub map-supplied avoid-combining avoid-supplied)
  "Render INPUT and return (OUTPUT COUNTS STATE) with dynamic configuration."
  (let ((elatex--error-counts (elatex--fresh-error-counts))
        (elatex--error-state 0)
        (elatex--wide-character-width wide-width)
        (elatex--full-width-character-width full-width)
        (elatex--style-kind (or style-kind elatex--selected-style))
        (elatex--style (copy-elatex--style
                        (elatex--persistent-style style-kind))))
    (when map-supplied
      (setf (elatex--style-map-super-sub elatex--style)
            (if map-super-sub 1 0)))
    (when avoid-supplied
      (setf (elatex--style-avoid-combining elatex--style)
            (if avoid-combining 1 0)))
    (let ((root (elatex--parse-string input (max 0 line-width) font)))
      (elatex--box-pos root)
      (list (elatex--draw-box root)
            (copy-sequence elatex--error-counts)
            elatex--error-state))))

(defun elatex--translated-render (value)
  "Render VALUE under persistent translated-API settings and publish errors."
  (let ((input (elatex--normalize-input value)))
    (elatex--validate-render-options
     elatex-default-font elatex-line-width nil elatex-wide-character-width
     elatex-full-width-character-width nil)
    (pcase-let ((`(,output ,counts ,state)
                 (elatex--render-values
                  input elatex-line-width elatex-default-font nil
                  elatex-wide-character-width elatex-full-width-character-width)))
      (elatex--publish-errors counts state)
      output)))

(defun elatex-string (string)
  "Render STRING without formatting or an added newline.
This is the translated texstring parity entry point.  Recoverable errors leave
partial output and are published for `elatex-errors-string'."
  (elatex--translated-render string))

(defun elatex--formatted-input (format-string arguments)
  "Apply Emacs `format' to FORMAT-STRING and ARGUMENTS after validation."
  (unless (stringp format-string) (elatex--invalid-input format-string))
  (apply #'format format-string arguments))

(defun elatex-sprintf (format-string &rest arguments)
  "Format ARGUMENTS with Emacs `format' and return the rendered string.
This is the translated stexprintf entry point."
  (elatex-string (elatex--formatted-input format-string arguments)))

(defun elatex-printf (format-string &rest arguments)
  "Format and render ARGUMENTS, write a trailing newline to `standard-output'.
Return the emitted UTF-8 byte count including that newline.  This is the
translated texprintf entry point."
  (let* ((output (elatex-string (elatex--formatted-input format-string arguments)))
         (text (concat output "\n")))
    (princ text)
    (string-bytes (encode-coding-string text 'utf-8))))

(defun elatex-fprintf (stream format-string &rest arguments)
  "Format and render ARGUMENTS to Emacs output STREAM without a newline.
Return the emitted UTF-8 byte count.  This is the translated ftexprintf entry
point; ordinary Emacs stream errors propagate normally."
  (let ((output (elatex-string
                 (elatex--formatted-input format-string arguments))))
    (princ output stream)
    (string-bytes (encode-coding-string output 'utf-8))))

(defun elatex-box-tree (format-string &rest arguments)
  "Format ARGUMENTS and write the exact retained box tree to `standard-output'.
Errors are reset and published as for rendering.  Return nil.  This is the
translated texboxtree debugger entry point."
  (let ((input (elatex--normalize-input
                (elatex--formatted-input format-string arguments))))
    (elatex--validate-render-options
     elatex-default-font elatex-line-width nil elatex-wide-character-width
     elatex-full-width-character-width nil)
    (let ((elatex--error-counts (elatex--fresh-error-counts))
          (elatex--error-state 0)
          (elatex--wide-character-width elatex-wide-character-width)
          (elatex--full-width-character-width elatex-full-width-character-width)
          (elatex--style-kind elatex--selected-style)
          (elatex--style (copy-elatex--style (elatex--persistent-style nil))))
      (let ((root (elatex--parse-string input (max 0 elatex-line-width)
                                        elatex-default-font)))
        (elatex--box-pos root)
        (princ (elatex--draw-box-tree-string root))
        (elatex--publish-errors elatex--error-counts elatex--error-state))))
  nil)

(defun elatex--combining-symbol-entries ()
  "Return source-ordered (COMMAND . COMBINING-CODEPOINT) entries."
  (let (entries)
    (seq-doseq (combining elatex--combining-commands)
      (let ((identity (aref combining 0)) (code-point (aref combining 1)))
        (seq-doseq (key elatex--keys)
          (when (= (aref key 1) identity)
            (push (cons (aref key 0) code-point) entries)))))
    (nreverse entries)))

(defun elatex-symbols-string ()
  "Return Symbols_Str's exact source-ordered machine serialization.
Each item ends in a semicolon; combining entries use a dotted circle.  This
function does not change the published error state."
  (let ((dotted "◌") pieces)
    (seq-doseq (record elatex--symbols)
      (push (format "%s:%c;" (aref record 0) (aref record 1)) pieces))
    (dolist (entry (elatex--combining-symbol-entries))
      (push (format "%s %s:%s%c;" (car entry) dotted dotted (cdr entry)) pieces))
    (apply #'concat (nreverse pieces))))

(defun elatex-list-symbols ()
  "Write ListSymbols' exact human-readable symbol listing to `standard-output'.
The source order and dotted-circle combining entries are retained.  Return nil
without changing the published error state."
  (let ((maximum 0) (dotted "◌"))
    (seq-doseq (record elatex--symbols)
      (setq maximum (max maximum (length (aref record 0)))))
    (seq-doseq (record elatex--symbols)
      (let ((name (aref record 0)))
        (princ (format "Symbol: %s%s %c\n" name
                       (make-string (- (+ maximum 2) (length name)) ?\s)
                       (aref record 1)))))
    (dolist (entry (elatex--combining-symbol-entries))
      (let ((name (car entry)))
        (princ (format "Symbol: %s %s%s%s%c\n" name dotted
                       (make-string (- (+ maximum 1) (length name)) ?\s)
                       dotted (cdr entry))))))
  nil)

(defun elatex-errors ()
  "Write E_Messages' exact ERROR lines to `external-debugging-output'.
Consume detailed counts while leaving `elatex-last-error-state' unchanged.
Return nil."
  (let ((text (elatex--errors-human-string elatex--published-error-counts)))
    (setq elatex--published-error-counts (elatex--fresh-error-counts))
    (princ text external-debugging-output))
  nil)

(defun elatex-errors-string ()
  "Return E_Messages_str's semicolon-delimited errors and consume details.
`elatex-last-error-state' remains unchanged."
  (prog1 (elatex--errors-combined-string elatex--published-error-counts)
    (setq elatex--published-error-counts (elatex--fresh-error-counts))))

(defun elatex-error-state ()
  "Return texerror_state's integer state from the latest completed call."
  elatex-last-error-state)

(defun elatex-set-style-ascii ()
  "Select the persistent ASCII drawing style, as SetStyleASCII does.
Return nil."
  (setq elatex--selected-style 'ascii)
  nil)

(defun elatex-set-style-unicode ()
  "Select the persistent Unicode drawing style, as SetStyleUNICODE does.
Return nil."
  (setq elatex--selected-style 'unicode)
  nil)

(defun elatex-toggle-map-super-sub ()
  "Toggle script-character mapping in the selected persistent style.
This implements ToggleMapSuperSub and returns nil."
  (let ((style (elatex--persistent-style nil)))
    (setf (elatex--style-map-super-sub style)
          (if (= (elatex--style-map-super-sub style) 0) 1 0)))
  nil)

(defun elatex-toggle-avoid-combining ()
  "Toggle combining-mark avoidance in the selected persistent style.
This implements ToggleAvoidCombining and returns nil."
  (let ((style (elatex--persistent-style nil)))
    (setf (elatex--style-avoid-combining style)
          (if (= (elatex--style-avoid-combining style) 0) 1 0)))
  nil)

(defun elatex-set-root-font (font)
  "Set persistent root FONT as SetRootFont does and return nil.
FONT must be a string.  Unknown names are stored as `unknown' and cause the
source recoverable fallback on the next render."
  (unless (stringp font) (elatex--invalid-option 'font font))
  (setq elatex-default-font
        (if (memq (elatex--lookup-font-without-error font)
                  (list elatex--pd-text elatex--pd-mathbf elatex--pd-mathbfit
                        elatex--pd-mathcal elatex--pd-mathscr elatex--pd-mathfrak
                        elatex--pd-mathbb elatex--pd-mathsf elatex--pd-mathsfbf
                        elatex--pd-mathsfit elatex--pd-mathsfbfit
                        elatex--pd-mathtt elatex--pd-mathnormal))
            font
          "unknown"))
  nil)

(defun elatex--lookup-font-without-error (font)
  "Return FONT's key identity without touching recoverable error state."
  (let ((record (gethash (concat "\\" font) elatex--key-index)))
    (and record (aref record 1))))

(cl-defun elatex-render
    (string &key
            (line-width elatex-line-width)
            (font elatex-default-font)
            style
            (wide-character-width elatex-wide-character-width)
            (full-width-character-width elatex-full-width-character-width)
            (map-super-sub nil map-super-sub-supplied-p)
            (avoid-combining nil avoid-combining-supplied-p)
            on-error signal-on-error)
  "Render STRING and return an `elatex-result'.

LINE-WIDTH, FONT, STYLE, WIDE-CHARACTER-WIDTH, and
FULL-WIDTH-CHARACTER-WIDTH override configuration for this call only.  STYLE is
nil, `unicode', or `ascii'.  Explicit MAP-SUPER-SUB and AVOID-COMBINING values
override flags on the copied call-local style.  ON-ERROR is called once with
the ordered error string list unless SIGNAL-ON-ERROR is non-nil; in that case
`elatex-render-error' is signaled with the complete result as sole data.
Nested callbacks and all per-call overrides leave persistent settings intact."
  (let ((input (elatex--normalize-input string)))
    (elatex--validate-render-options
     font line-width style wide-character-width full-width-character-width
     on-error)
    (pcase-let* ((`(,output ,counts ,state)
                  (elatex--render-values
                   input line-width font style wide-character-width
                   full-width-character-width map-super-sub
                   map-super-sub-supplied-p avoid-combining
                   avoid-combining-supplied-p))
                 (errors (elatex--error-list counts))
                 (result (elatex--make-result output errors)))
      (setq elatex-last-error-state state
            elatex--published-error-counts (elatex--fresh-error-counts))
      (if (null errors)
          result
        (unwind-protect
            (cond
             (signal-on-error
              (signal 'elatex-render-error (list result)))
             (on-error (funcall on-error errors) result)
             (t result))
          (setq elatex-last-error-state state
                elatex--published-error-counts
                (elatex--fresh-error-counts)))))))

(provide 'elatex)
;;; elatex.el ends here
