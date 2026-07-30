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
    (let ((elatex-preview-idle-delay 0))
      (unwind-protect
          (progn
            (elatex-preview-mode 1)
            (elatex-preview-refresh)
            (should (overlayp elatex-preview--overlay))
            (should
             (equal (substring-no-properties
                     (overlay-get elatex-preview--overlay 'after-string))
                    "\na+b"))
            (goto-char (point-max))
            (run-hooks 'post-command-hook)
            (should-not (overlayp elatex-preview--overlay)))
        (elatex-preview-mode -1)))))

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

(ert-deftest elatex-preview/realtime-edit-refresh ()
  (with-temp-buffer
    (setq major-mode 'markdown-mode)
    (insert "Text $a+b$")
    (search-backward "b")
    (let ((elatex-preview-idle-delay 0))
      (unwind-protect
          (progn
            (elatex-preview-mode 1)
            (delete-char 1)
            (insert "c")
            (should
             (equal (substring-no-properties
                     (overlay-get elatex-preview--overlay 'after-string))
                    "\na+c")))
        (elatex-preview-mode -1)))))

(ert-deftest elatex-preview/recoverable-error-output ()
  (with-temp-buffer
    (setq major-mode 'latex-mode)
    (insert "$\\alp$")
    (search-backward "alp")
    (let ((elatex-preview-idle-delay 0))
      (unwind-protect
          (progn
            (elatex-preview-mode 1)
            (elatex-preview-refresh)
            (should (string-match-p
                     "Unknown command (1x)"
                     (overlay-get elatex-preview--overlay 'after-string))))
        (elatex-preview-mode -1)))))

(provide 'elatex-preview-test)
;;; elatex-preview-test.el ends here
