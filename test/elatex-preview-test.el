;;; elatex-preview-test.el --- Realtime preview tests  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'elatex-preview)

(defun elatex-preview-test--exercise-mode (mode source)
  "Exercise preview behavior for MODE using SOURCE around a+b."
  (with-temp-buffer
    (if (eq mode 'org-mode)
        (org-mode)
      (setq major-mode mode))
    (insert source "\nTail")
    (goto-char (point-min))
    (search-forward "a+b")
    (goto-char (match-beginning 0))
    (let ((elatex-preview-idle-delay 0)
          (elatex-preview-backend 'after-string))
      (unwind-protect
          (progn
            (elatex-preview-mode 1)
            (elatex-preview-refresh)
            (should (overlayp elatex-preview--overlay))
            (should
             (equal (substring-no-properties
                     (overlay-get elatex-preview--overlay 'after-string))
                    "\n╭─────╮\n│ a+b │\n╰─────╯"))
            (goto-char (point-max))
            (run-hooks 'post-command-hook)
            (should-not (overlayp elatex-preview--overlay)))
        (elatex-preview-mode -1)))))

(ert-deftest elatex-preview/box-output ()
  (should (equal (elatex-preview--box-output "a\nbc")
                 "╭────╮\n│ a  │\n│ bc │\n╰────╯"))
  (should (equal (elatex-preview--box-output "") "")))

(ert-deftest elatex-preview/child-frame-payload-is-unboxed ()
  (let ((payload (elatex-preview--format-payload
                  "a+b" '("First error" "Second error"))))
    (should (equal (substring-no-properties payload)
                   "a+b\nFirst error; Second error"))
    (should (eq (get-text-property 0 'face payload)
                'elatex-preview-output-face))
    (should (eq (get-text-property 4 'face payload) 'error)))
  (should (equal
           (substring-no-properties
            (elatex-preview--format-after-string "a+b" nil))
           "\n╭─────╮\n│ a+b │\n╰─────╯")))

(ert-deftest elatex-preview/child-frame-fit-uses-rendered-pixel-size ()
  (let (calls)
    (cl-letf (((symbol-function 'frame-root-window)
               (lambda (_frame) 'window))
              ((symbol-function 'window-text-pixel-size)
               (lambda (&rest _) '(37 . 24)))
              ((symbol-function 'window-pixel-width)
               (lambda (_window) 37))
              ((symbol-function 'window-pixel-height)
               (lambda (_window) 24))
              ((symbol-function 'set-frame-size)
               (lambda (&rest values) (push values calls))))
      (elatex-preview--child-frame-fit 'child)
      (should (equal (nreverse calls) '((child 37 24 t)))))))

(ert-deftest elatex-preview/child-frame-fit-corrects-visible-deficits ()
  (let (calls)
    (cl-letf (((symbol-function 'frame-root-window)
               (lambda (_frame) 'window))
              ((symbol-function 'window-text-pixel-size)
               (lambda (&rest _) '(37 . 24)))
              ((symbol-function 'window-pixel-width)
               (lambda (_window) 36))
              ((symbol-function 'window-pixel-height)
               (lambda (_window) 23))
              ((symbol-function 'frame-pixel-width)
               (lambda (_frame) 38))
              ((symbol-function 'frame-pixel-height)
               (lambda (_frame) 25))
              ((symbol-function 'set-frame-size)
               (lambda (&rest values) (push values calls))))
      (elatex-preview--child-frame-fit 'child)
      (should (equal (nreverse calls)
                     '((child 37 24 t) (child 39 26 t)))))))

(ert-deftest elatex-preview/delimiters-and-fences-trigger-rendering ()
  (dolist (scenario
           '((markdown-mode "$a+b$")
             (markdown-mode "$$a+b$$")
             (markdown-mode "$`a+b`$")
             (markdown-mode "```math\na+b\n```")
             (latex-mode "\\(a+b\\)")
             (latex-mode "\\[a+b\\]")
             (latex-mode "\\begin{equation}\na+b\n\\end{equation}")
             (org-mode "\\(a+b\\)")
             (org-mode "\\begin{equation}\na+b\n\\end{equation}")))
    (pcase-let ((`(,mode ,source) scenario))
      (with-temp-buffer
        (if (eq mode 'org-mode)
            (org-mode)
          (setq major-mode mode))
        (insert source)
        (let ((elatex-preview-idle-delay 0)
              (elatex-preview-backend 'after-string))
          (unwind-protect
              (progn
                (goto-char (point-min))
                (let ((expected (elatex-preview--context-at-point)))
                  (should expected)
                  (should (equal (elatex-preview--context-content expected) "a+b"))
                  (let ((begin (elatex-preview--context-begin expected))
                        (end (elatex-preview--context-end expected)))
                    (dotimes (offset (- end begin))
                      (goto-char (+ begin offset))
                      (let ((context (elatex-preview--context-at-point)))
                        (should context)
                        (should (equal (elatex-preview--context-content context)
                                       "a+b")))
                      (elatex-preview-mode 1)
                      (elatex-preview-refresh)
                      (should (overlayp elatex-preview--overlay))
                      (elatex-preview-mode -1))
                    (goto-char end)
                    (should-not (elatex-preview--context-at-point))
                    (elatex-preview-mode 1)
                    (elatex-preview-refresh)
                    (should-not (overlayp elatex-preview--overlay))
                    (elatex-preview-mode -1))))
            (when elatex-preview-mode
              (elatex-preview-mode -1)))))))
  (with-temp-buffer
    (setq major-mode 'markdown-mode)
    (insert "$$")
    (goto-char (point-min))
    (should-not (elatex-preview--context-at-point))))

(ert-deftest elatex-preview/markdown-mode ()
  (elatex-preview-test--exercise-mode 'markdown-mode "Text $a+b$"))

(ert-deftest elatex-preview/markdown-ts-mode ()
  (elatex-preview-test--exercise-mode 'markdown-ts-mode "Text $$a+b$$"))

(ert-deftest elatex-preview/markdown-dollar-backtick ()
  (elatex-preview-test--exercise-mode 'markdown-mode "Text $`a+b`$"))

(ert-deftest elatex-preview/markdown-ts-dollar-backtick ()
  (elatex-preview-test--exercise-mode 'markdown-ts-mode "Text $`a+b`$"))

(ert-deftest elatex-preview/markdown-fenced-math ()
  (elatex-preview-test--exercise-mode
   'markdown-mode
   "```math\na+b\n```"))

(ert-deftest elatex-preview/markdown-ts-fenced-math ()
  (elatex-preview-test--exercise-mode
   'markdown-ts-mode
   "```math\na+b\n```"))

(ert-deftest elatex-preview/org-mode ()
  (elatex-preview-test--exercise-mode 'org-mode "Text \\(a+b\\)"))

(ert-deftest elatex-preview/latex-mode ()
  (elatex-preview-test--exercise-mode 'latex-mode "Text \\[a+b\\]"))

(ert-deftest elatex-preview/latex-ts-mode ()
  (elatex-preview-test--exercise-mode
   'latex-ts-mode
   "\\begin{equation}\na+b\n\\end{equation}"))

(ert-deftest elatex-preview/terminal-frame-overlay ()
  (skip-unless (not (display-graphic-p)))
  (with-temp-buffer
    (setq major-mode 'markdown-mode)
    (insert "Text $\\frac{a}{b}$")
    (search-backward "frac")
    (let ((elatex-preview-idle-delay 0)
          (elatex-preview-backend 'after-string))
      (unwind-protect
          (progn
            (elatex-preview-mode 1)
            (elatex-preview-refresh)
            (should (< (overlay-start elatex-preview--overlay)
                       (overlay-end elatex-preview--overlay)))
            (let ((preview
                   (overlay-get elatex-preview--overlay 'after-string)))
              (should (equal (substring-no-properties preview)
                             "\n╭───╮\n│ a │\n│ ─ │\n│ b │\n╰───╯"))
              (should-not (get-text-property 1 'display preview))))
        (elatex-preview-mode -1)))))

(ert-deftest elatex-preview/realtime-edit-refresh ()
  (with-temp-buffer
    (setq major-mode 'markdown-mode)
    (insert "Text $a+b$")
    (search-backward "b")
    (let ((elatex-preview-idle-delay 0)
          (elatex-preview-backend 'after-string))
      (unwind-protect
          (progn
            (elatex-preview-mode 1)
            (delete-char 1)
            (insert "c")
            (backward-char 1)
            (elatex-preview-refresh)
            (should
             (equal (substring-no-properties
                     (overlay-get elatex-preview--overlay 'after-string))
                    "\n╭─────╮\n│ a+c │\n╰─────╯")))
        (elatex-preview-mode -1)))))

(ert-deftest elatex-preview/recoverable-error-output ()
  (with-temp-buffer
    (setq major-mode 'latex-mode)
    (insert "$\\alp$")
    (search-backward "alp")
    (let ((elatex-preview-idle-delay 0)
          (elatex-preview-backend 'after-string))
      (unwind-protect
          (progn
            (elatex-preview-mode 1)
            (elatex-preview-refresh)
            (should (string-match-p
                     (regexp-quote "Unknown command (1x)")
                     (overlay-get elatex-preview--overlay 'after-string))))
        (elatex-preview-mode -1)))))


(ert-deftest elatex-preview/escaped-dollar-remains-ignored ()
  (with-temp-buffer
    (setq major-mode 'markdown-mode)
    (insert "\\$a+b$")
    (goto-char (1+ (point-min)))
    (should-not (elatex-preview--context-at-point))))

(ert-deftest elatex-preview/child-frame-position ()
  (should (equal (elatex-preview--child-frame-position 20 40 15 100 30 300 200)
                 '(20 . 55)))
  (should (equal (elatex-preview--child-frame-position 20 180 15 100 30 300 200)
                 '(20 . 150)))
  (should (equal (elatex-preview--child-frame-position 250 5 15 100 50 300 40)
                 '(200 . 0))))

(ert-deftest elatex-preview/child-frame-terminal-fallback ()
  (with-temp-buffer
    (setq major-mode 'markdown-mode)
    (insert "$a+b$")
    (goto-char (point-min))
    (let ((elatex-preview-idle-delay 0)
          (elatex-preview-backend 'child-frame))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) nil))
                ((symbol-function 'make-frame)
                 (lambda (&rest _) (error "child frame must not be created"))))
        (unwind-protect
            (progn
              (elatex-preview-mode 1)
              (elatex-preview-refresh)
              (should (eq elatex-preview--active-backend 'after-string))
              (should (overlayp elatex-preview--overlay)))
          (elatex-preview-mode -1))))))

(ert-deftest elatex-preview/backend-transition-reuses-render ()
  (with-temp-buffer
    (setq major-mode 'markdown-mode)
    (insert "$a+b$")
    (goto-char (point-min))
    (let ((elatex-preview-idle-delay 0)
          (backend 'after-string)
          (renders 0)
          shown hidden)
      (let ((original-render (symbol-function 'elatex-render)))
        (cl-letf (((symbol-function 'elatex-preview--effective-backend)
                   (lambda () backend))
                  ((symbol-function 'elatex-render)
                   (lambda (&rest arguments)
                     (setq renders (1+ renders))
                     (apply original-render arguments)))
                  ((symbol-function 'elatex-preview--after-string-show)
                   (lambda (&rest _) (push 'after-string shown) t))
                  ((symbol-function 'elatex-preview--child-frame-show)
                   (lambda (&rest _) (push 'child-frame shown) t))
                  ((symbol-function 'elatex-preview--after-string-hide)
                   (lambda () (push 'after-string hidden)))
                  ((symbol-function 'elatex-preview--child-frame-hide)
                   (lambda () (push 'child-frame hidden))))
          (unwind-protect
              (progn
                (elatex-preview-mode 1)
                (elatex-preview-refresh)
                (goto-char (+ (point-min) 2))
                (elatex-preview-refresh)
                (setq backend 'child-frame)
                (elatex-preview-refresh)
                (should (= renders 1))
                (should (memq 'after-string hidden))
                (should (memq 'child-frame shown))
                (goto-char (point-max))
                (elatex-preview-refresh)
                (should (memq 'child-frame hidden)))
            (elatex-preview-mode -1)))))))

(ert-deftest elatex-preview/child-frame-failure-falls-back-without-render-error ()
  (with-temp-buffer
    (setq major-mode 'markdown-mode)
    (insert "$a+b$")
    (goto-char (point-min))
    (let ((elatex-preview-idle-delay 0)
          after-output after-errors)
      (cl-letf (((symbol-function 'elatex-preview--effective-backend)
                 (lambda () 'child-frame))
                ((symbol-function 'elatex-preview--child-frame-show)
                 (lambda (&rest _) (error "native child failure")))
                ((symbol-function 'elatex-preview--child-frame-destroy)
                 (lambda ()))
                ((symbol-function 'elatex-preview--after-string-show)
                 (lambda (_context output errors)
                   (setq after-output output
                         after-errors errors)
                   t)))
        (unwind-protect
            (progn
              (elatex-preview-mode 1)
              (should elatex-preview--child-frame-failed)
              (should (equal after-output "a+b"))
              (should-not after-errors))
          (elatex-preview-mode -1))))))

(ert-deftest elatex-preview/disable-destroys-retained-child-resources-once ()
  (with-temp-buffer
    (setq major-mode 'markdown-mode)
    (insert "$a+b$")
    (let ((elatex-preview-idle-delay 0)
          (child-destroys 0))
      (cl-letf (((symbol-function 'elatex-preview--backend-destroy)
                 (lambda (backend)
                   (when (eq backend 'child-frame)
                     (setq child-destroys (1+ child-destroys))))))
        (elatex-preview-mode 1)
        (elatex-preview-mode -1)
        (should (= child-destroys 1))))))

(ert-deftest elatex-preview/kill-buffer-destroys-retained-child-resources-once ()
  (let ((source (generate-new-buffer " *elatex-preview-test-source*"))
        (child-destroys 0))
    (unwind-protect
        (cl-letf (((symbol-function 'elatex-preview--backend-destroy)
                   (lambda (backend)
                     (when (eq backend 'child-frame)
                       (setq child-destroys (1+ child-destroys))))))
          (with-current-buffer source
            (setq major-mode 'markdown-mode)
            (insert "$a+b$")
            (goto-char (point-min))
            (let ((elatex-preview-idle-delay 0))
              (elatex-preview-mode 1))
            (kill-buffer source))
          (should (= child-destroys 1)))
      (when (buffer-live-p source)
        (kill-buffer source)))))

(ert-deftest elatex-preview/child-frame-graphical-lifecycle ()
  (skip-unless (display-graphic-p))
  (let ((source (generate-new-buffer " *elatex-preview-test-source*")))
    (unwind-protect
        (with-current-buffer source
          (setq major-mode 'markdown-mode)
          (insert "$a+b$")
          (let ((window (display-buffer source))
                (elatex-preview-idle-delay 0)
                (elatex-preview-backend 'child-frame))
            (select-window window)
            (goto-char (point-min))
            (elatex-preview-mode 1)
            (elatex-preview-refresh)
            (let ((child elatex-preview--child-frame)
                  (payload elatex-preview--child-frame-buffer))
              (should (eq (frame-parent child) (window-frame window)))
              (should (equal (substring-no-properties
                              (with-current-buffer payload (buffer-string)))
                             "a+b"))
              (should (frame-visible-p child))
              (let* ((child-window (frame-root-window child))
                     (required-width
                      (car (window-text-pixel-size child-window t t t))))
                (should (>= (window-pixel-width child-window) required-width)))
              (let ((initial-width (frame-pixel-width child)))
                (goto-char (1- (point-max)))
                (insert "+c")
                (elatex-preview-refresh)
                (should (equal (substring-no-properties
                                (with-current-buffer payload (buffer-string)))
                               "a+b+c"))
                (should (> (frame-pixel-width child) initial-width)))
                (let* ((child-window (frame-root-window child))
                       (required-width
                        (car (window-text-pixel-size child-window t t t))))
                  (should (>= (window-pixel-width child-window)
                              required-width)))
              (goto-char (point-max))
              (elatex-preview-refresh)
              (should-not (frame-visible-p child))
              (elatex-preview-mode -1)
              (should-not (frame-live-p child))
              (should-not (buffer-live-p payload)))))
      (when (buffer-live-p source)
        (kill-buffer source)))))
(provide 'elatex-preview-test)
;;; elatex-preview-test.el ends here
