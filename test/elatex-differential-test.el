;;; elatex-differential-test.el --- Pinned C oracle comparison  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Opt-in three-way comparison.  Set ELATEX_ORACLE to the built utftex path.

;;; Code:

(require 'ert)
(require 'elatex-fixtures)

(defun elatex-differential--arguments (option)
  "Translate exact fixture OPTION to an argv list."
  (cond
   ((string-empty-p option) nil)
   ((string= option "-S") '("-S"))
   ((string= option "-A") '("-A"))
   ((string= option "-m") '("-m"))
   ((string= option "-a") '("-a"))
   ((string-match "\\`-F \\([^ \t\n\r]+\\)\\'" option)
    (list "-F" (match-string 1 option)))
   (t (error "Unknown differential option: %S" option))))

(defun elatex-differential--oracle-render (oracle execution)
  "Render EXECUTION with ORACLE and return decoded stdout without its final LF."
  (let ((stdout (generate-new-buffer " *elatex-oracle-out*"))
        (stderr (make-temp-file "elatex-oracle-err-"))
        (coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'binary)
        (process-coding-system-alist nil)
        (process-environment
         (cons "LC_ALL=C.UTF-8"
               (cons "LANG=C.UTF-8"
                     (cl-remove-if
                      (lambda (entry)
                        (or (string-prefix-p "LC_ALL=" entry)
                            (string-prefix-p "LANG=" entry)))
                      process-environment)))))
    (with-current-buffer stdout (set-buffer-multibyte nil))
    (unwind-protect
        (with-temp-buffer
          (insert (elatex-fixture-execution-input execution))
          (let ((status
                 (apply #'call-process-region
                        (point-min) (point-max) oracle nil
                        (list stdout stderr) nil
                        (elatex-differential--arguments
                         (elatex-fixture-execution-option execution)))))
            (unless (and (integerp status) (= status 0))
              (error "Oracle failed (%S): %s" status
                     (let ((bytes
                            (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally stderr)
                              (buffer-string))))
                       (decode-coding-string bytes 'utf-8-unix)))))
          (with-current-buffer stdout
            (let ((output (decode-coding-string
                           (buffer-substring-no-properties (point-min) (point-max))
                           'utf-8-unix)))
              (unless (string-suffix-p "\n" output)
                (error "Oracle output lacks final LF"))
              (substring output 0 -1))))
      (when (buffer-live-p stdout) (kill-buffer stdout))
      (delete-file stderr))))

(ert-deftest elatex-differential/pinned-c-golden-and-lisp ()
  (let ((oracle (getenv "ELATEX_ORACLE")))
    (unless (and oracle (file-executable-p oracle))
      (ert-skip "ELATEX_ORACLE does not name an executable"))
    (dolist (execution (elatex-fixture-load-all))
      (let ((expected (elatex-fixture-execution-expected execution)))
        (should (equal (elatex-differential--oracle-render oracle execution)
                       expected))
        (should (equal (elatex-fixture-render execution) expected))))))

(provide 'elatex-differential-test)
;;; elatex-differential-test.el ends here
