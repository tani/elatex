;;; elatex-fixtures.el --- Strict upstream fixture loader  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Parse the pinned test.awk fixture grammar without invoking a shell.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'elatex)

(defconst elatex-fixtures--directory
  (expand-file-name "../reference/test/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Pinned upstream fixture directory located relative to this module.")

(cl-defstruct (elatex-fixture-execution
               (:constructor elatex-fixture--make-execution))
  file block-name block-index reference-index alternative-index
  input expected option)

(defun elatex-fixture--first-field (line)
  "Return LINE's first whitespace-delimited field."
  (car (split-string line "[ \t]+" t)))

(defun elatex-fixture--second-field (line)
  "Return LINE's second whitespace-delimited field, or nil."
  (nth 1 (split-string line "[ \t]+" t)))

(defun elatex-fixture--trim-shell-whitespace (string)
  "Trim only surrounding ASCII shell whitespace from STRING."
  (replace-regexp-in-string
   "[ \t\n\r]+\\'" ""
   (replace-regexp-in-string "\\`[ \t\n\r]+" "" string)))

(defun elatex-fixture--validate-option (option)
  "Validate and return exact fixture OPTION."
  (unless (or (member option '("" "-S" "-A" "-m" "-a"))
              (string-match-p "\\`-F [^ \t\n\r]+\\'" option))
    (error "Unknown fixture option: %S" option))
  option)

(defun elatex-fixture--header-options (line)
  "Return validated alternatives from a <ref> header LINE."
  (let* ((start (string-match "<ref>" line))
         (rest (substring line (+ start 5))))
    (mapcar (lambda (option)
              (elatex-fixture--validate-option
               (elatex-fixture--trim-shell-whitespace option)))
            (split-string rest "|" nil))))

(defun elatex-fixture-load (file)
  "Load FILE and return (EXECUTIONS BLOCK-COUNT REFERENCE-COUNT).
Every input and reference character is retained.  Exactly the CLI-added final
LF is removed from expected output."
  (let ((path (expand-file-name file elatex-fixtures--directory))
        (state 'outside) block-name block-index reference-index input
        input-lines reference-lines options executions
        (blocks 0) (references 0))
    (cl-labels
        ((finish-reference
          ()
          (unless (eq state 'reference)
            (error "Fixture reference state mismatch in %s" file))
          (let ((expected (apply #'concat (nreverse reference-lines))))
            (unless (string-suffix-p "\n" expected)
              (error "Fixture reference lacks final LF: %s block %d ref %d"
                     file block-index reference-index))
            (setq expected (substring expected 0 -1))
            (cl-loop for option in options
                     for alternative-index from 0 do
                     (push (elatex-fixture--make-execution
                            :file file :block-name block-name
                            :block-index block-index
                            :reference-index reference-index
                            :alternative-index alternative-index
                            :input input :expected expected :option option)
                           executions)))
          (setq references (1+ references)
                reference-lines nil options nil))
         (start-reference
          (line)
          (when (eq state 'reference) (finish-reference))
          (when (eq state 'input)
            (setq input (apply #'concat (nreverse input-lines))))
          (setq state 'reference
                reference-index (if reference-index (1+ reference-index) 0)
                options (elatex-fixture--header-options line)
                reference-lines nil)))
      (let ((coding-system-for-read 'utf-8-unix))
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (while (< (point) (point-max))
            (let* ((begin (point))
                   (end (line-end-position))
                   (line (buffer-substring-no-properties begin end))
                   (with-lf (buffer-substring-no-properties
                             begin (min (point-max) (1+ end))))
                   (field (elatex-fixture--first-field line)))
              (cond
               ((equal field "<input>")
                (unless (eq state 'outside)
                  (error "Nested fixture input in %s" file))
                (setq block-name (elatex-fixture--second-field line)
                      block-index blocks reference-index nil input nil
                      input-lines nil state 'input)
                (unless block-name
                  (error "Missing fixture case name in %s" file)))
               ((equal field "<ref>")
                (unless (memq state '(input reference))
                  (error "Reference outside input block in %s" file))
                (start-reference line))
               ((equal field "<end>")
                (unless (eq state 'reference)
                  (error "End outside reference block in %s" file))
                (finish-reference)
                (setq blocks (1+ blocks) state 'outside))
               ((eq state 'input) (push with-lf input-lines))
               ((eq state 'reference) (push with-lf reference-lines)))
              (forward-line 1)))))
      (unless (eq state 'outside)
        (error "Unterminated fixture block in %s" file))
      (list (nreverse executions) blocks references))))

(defconst elatex-fixture--specifications
  '(("testsuite.txt" 94 161 377)
    ("testeqs.txt" 29 61 94)
    ("testfonts.txt" 6 20 33))
  "Expected pinned fixture structure: file, blocks, references, executions.")

(defun elatex-fixture-load-all ()
  "Load all fixtures after asserting their exact pinned structure."
  (let (all)
    (dolist (spec elatex-fixture--specifications)
      (pcase-let* ((`(,file ,blocks ,references ,executions) spec)
                   (`(,loaded ,actual-blocks ,actual-references)
                    (elatex-fixture-load file)))
        (unless (and (= blocks actual-blocks)
                     (= references actual-references)
                     (= executions (length loaded)))
          (error "Fixture structure changed for %s: expected %S, got %S"
                 file (cdr spec)
                 (list actual-blocks actual-references (length loaded))))
        (setq all (nconc all loaded))))
    all))

(defun elatex-fixture-render (execution)
  "Render one fixture EXECUTION without persistent configuration changes."
  (let ((option (elatex-fixture-execution-option execution))
        (input (elatex-fixture-execution-input execution)))
    (cond
     ((member option '("" "-S"))
      (elatex-result-output (elatex-render input :style 'unicode :font "text")))
     ((string= option "-A")
      (elatex-result-output
       (elatex-render input :style 'ascii :font "text"
                      :map-super-sub nil :avoid-combining nil)))
     ((string= option "-m")
      (elatex-result-output
       (elatex-render input :style 'unicode :font "text" :map-super-sub nil)))
     ((string= option "-a")
      (elatex-result-output
       (elatex-render input :style 'unicode :font "text" :avoid-combining t)))
     ((string-match "\\`-F \\([^ \t\n\r]+\\)\\'" option)
      (elatex-result-output
       (elatex-render input :style 'unicode :font (match-string 1 option))))
     (t (error "Unreachable fixture option: %S" option)))))

(provide 'elatex-fixtures)
;;; elatex-fixtures.el ends here
