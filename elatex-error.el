;;; elatex-error.el --- Counted recoverable errors  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tex, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Native Emacs Lisp port of libtexprintf 1.31 by Bart Pieters, pinned at
;; revision 18977837b20649d56a651eb6bf846f1c914db77a.

;;; Commentary:

;; Deterministic translation of error.c and generated ERRORFLAG records.
;; Counts are dynamically bound per render and copied when published.

;;; Code:

(require 'elatex-data)

(defconst elatex--errboxinbox 0)
(defconst elatex--errboxatpos 1)
(defconst elatex--erruboxsize 2)
(defconst elatex--erraboxsize 3)
(defconst elatex--errpboxsize 4)
(defconst elatex--errnegrelpos 5)
(defconst elatex--errdboxsize 6)
(defconst elatex--errelboxsize 7)
(defconst elatex--errlboxsize 8)
(defconst elatex--errunknownbox 9)
(defconst elatex--errdrawboxnoroot 10)
(defconst elatex--errabsposunknown 11)
(defconst elatex--lexprematureend 12)
(defconst elatex--errunknownfont 13)
(defconst elatex--errmultisub 14)
(defconst elatex--errmultisup 15)
(defconst elatex--invaliddelimiter 16)
(defconst elatex--norightbrac 17)
(defconst elatex--errdoublehline 18)
(defconst elatex--errunexphline 19)
(defconst elatex--errnumcolmatch 20)
(defconst elatex--errvalighn 21)
(defconst elatex--errnomatchinend 22)
(defconst elatex--errnovalidalignc 23)
(defconst elatex--alrowsmatch 24)
(defconst elatex--errhlinesinmatrix 25)
(defconst elatex--errunknownenv 26)
(defconst elatex--errlinetoolong 27)
(defconst elatex--errtoofewmandarg 28)
(defconst elatex--errtoomanyoptarg 29)
(defconst elatex--errunknowncomm 30)
(defconst elatex--errunmatchdollar 31)
(defconst elatex--errunmatchbrac 32)
(defconst elatex--errtoomanyprimes 33)
(defconst elatex--errscaledelposbox 34)
(defconst elatex--errnobodyinlr 35)
(defconst elatex--errscalevposbox 36)
(defconst elatex--errscalehposbox 37)

(defconst elatex--error-records
  '[["ERRBOXINBOX" "BoxInBox cannot take the root box as an agument" "boxes.c"]
 ["ERRBOXATPOS" "Box positions unknown in FindBoxAtPos" "boxes.c"]
 ["ERRUBOXSIZE" "Call of UnitBoxSize on something not a unit box" "boxes.c"]
 ["ERRABOXSIZE" "Call of ArrayBoxSize on something not an array box" "boxes.c"]
 ["ERRPBOXSIZE" "Call of PosBoxSize on something not a pos box" "boxes.c"]
 ["ERRNEGRELPOS" "Relative positions may not be negative in PosBoxSize" "boxes.c"]
 ["ERRDBOXSIZE" "Call of DummyBoxSize on something not a dummy box" "boxes.c"]
 ["ERRELBOXSIZE" "Call of EndlineBoxSize on something not a endline box" "boxes.c"]
 ["ERRLBOXSIZE" "LineBoxSize can only be used on line boxes" "boxes.c"]
 ["ERRUNKNOWNBOX" "Unknown box type in BoxSize" "boxes.c"]
 ["ERRDRAWBOXNOROOT" "Drawbox needs a rootbox as input" "drawbox.c"]
 ["ERRABSPOSUNKNOWN" "DrawBox cannot draw box, box positions not absolute" "drawbox.c"]
 ["LEXPREMATUREEND" "Premature end of string" "lexer.c"]
 ["ERRUNKNOWNFONT" "Unknown font type, using text instead" "lexer.c"]
 ["ERRMULTISUB" "Multiple Subscripts" "lexer.c"]
 ["ERRMULTISUP" "Multiple Superscripts" "lexer.c"]
 ["INVALIDDELIMITER" "Invalid Delimiter" "lexer.c"]
 ["NORIGHTBRAC" "Premature end, no \\right found" "lexer.c"]
 ["ERRDOUBLEHLINE" "Double \\hline" "lexer.c"]
 ["ERRUNEXPHLINE" "unexpected \\hline in the middle of a row" "lexer.c"]
 ["ERRNUMCOLMATCH" "Unequal number of columns in different rows" "lexer.c"]
 ["ERRVALIGHN" "\\begin{array} requires column-wise alignment info" "lexer.c"]
 ["ERRNOMATCHINEND" "\\begin does not match closed with \\end" "lexer.c"]
 ["ERRNOVALIDALIGNC" "Illegal character in alignment info" "lexer.c"]
 ["ALROWSMATCH" "\\number of rows does not match the alignment inf" "lexer.c"]
 ["ERRHLINESINMATRIX" "no \\hline's allowed in the matrix environment" "lexer.c"]
 ["ERRUNKNOWNENV" "Unknown environment" "lexer.c"]
 ["ERRLINETOOLONG" "Input string is too long, truncated input" "lexer.c"]
 ["ERRTOOFEWMANDARG" "Too few mandatory arguments to command" "lexer.c"]
 ["ERRTOOMANYOPTARG" "Too many optional arguments to command, excess ignored" "lexer.c"]
 ["ERRUNKNOWNCOMM" "Unknown command" "lexer.c"]
 ["ERRUNMATCHDOLLAR" "Missing $ inserted" "lexer.c"]
 ["ERRUNMATCHBRAC" "Missing } inserted" "lexer.c"]
 ["ERRTOOMANYPRIMES" "Too many primes" "lexer.c"]
 ["ERRSCALEDELPOSBOX" "Variable size delimiters need a posbox" "parser.c"]
 ["ERRNOBODYINLR" "Missing body argument in \\left ... \\right construct" "parser.c"]
 ["ERRSCALEVPOSBOX" "RescaleVsep should only be used on a posbox" "parser.c"]
 ["ERRSCALEHPOSBOX" "RescaleHsep should only be used on a posbox" "parser.c"]]
  "ERRORFLAG records in generator order.")

(defvar elatex--error-counts (make-vector (length elatex--error-records) 0)
  "Dynamically bound counts for the active render.")

(defvar elatex--error-state 0
  "Dynamically bound boolean-as-integer state for the active render.")

(defvar elatex--published-error-counts
  (make-vector (length elatex--error-records) 0)
  "Detailed error counts left by the latest published render.")

(defvar elatex-last-error-state 0
  "Integer error state from the most recently completed rendering call.
Zero means no recoverable error occurred; one means at least one occurred.
Reading this variable does not consume detailed error counts.")

(defun elatex--fresh-error-counts ()
  "Return a zeroed error-count vector."
  (make-vector (length elatex--error-records) 0))

(defun elatex--reset-errors ()
  "Reset the dynamically active error state."
  (setq elatex--error-counts (elatex--fresh-error-counts)
        elatex--error-state 0))

(defun elatex--add-error (flag)
  "Increment recoverable error FLAG deterministically."
  (unless (and (integerp flag) (<= 0 flag) (< flag (length elatex--error-counts)))
    (error "eLaTeX internal error flag out of range: %S" flag))
  (aset elatex--error-counts flag (1+ (aref elatex--error-counts flag)))
  (setq elatex--error-state 1))

(defun elatex--query-error (flag)
  "Return non-nil when recoverable error FLAG has occurred."
  (and (integerp flag)
       (<= 0 flag)
       (< flag (length elatex--error-counts))
       (> (aref elatex--error-counts flag) 0)))

(defun elatex--publish-errors (&optional counts state)
  "Publish COUNTS and STATE, defaulting to the dynamically active values."
  (setq elatex--published-error-counts
        (copy-sequence (or counts elatex--error-counts))
        elatex-last-error-state (or state elatex--error-state)))

(defun elatex--error-list (&optional counts)
  "Return ordered combined messages represented by COUNTS."
  (let ((source (or counts elatex--published-error-counts))
        (index 0) result)
    (while (< index (length elatex--error-records))
      (let ((count (aref source index)))
        (when (> count 0)
          (push (format "%s (%dx)"
                        (aref (aref elatex--error-records index) 1)
                        count)
                result)))
      (setq index (1+ index)))
    (nreverse result)))

(defun elatex--errors-human-string (&optional counts)
  "Serialize COUNTS as ordered human-readable ERROR lines."
  (mapconcat (lambda (message) (concat "ERROR: " message "
"))
             (elatex--error-list counts) ""))

(defun elatex--errors-combined-string (&optional counts)
  "Serialize COUNTS as an ordered semicolon-delimited string."
  (mapconcat #'identity (elatex--error-list counts) "; "))

(defun elatex--consume-published-errors ()
  "Return published combined messages and clear only detailed counts."
  (prog1 (elatex--error-list elatex--published-error-counts)
    (setq elatex--published-error-counts (elatex--fresh-error-counts))))

(provide 'elatex-error)
;;; elatex-error.el ends here
