;;; elatex-lexer.el --- TeX-like preprocessing lexer  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tex, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Native Emacs Lisp port of libtexprintf 1.31 by Bart Pieters, pinned at
;; revision 18977837b20649d56a651eb6bf846f1c914db77a.

;;; Commentary:

;; Preprocessing-first lexer translated from lexer.c.  Its command-specific
;; terminators, whitespace rules, greedy rewrites, and recovery behavior are
;; intentionally not replaced by a generic TeX grammar.

;;; Code:

(require 'cl-lib)
(require 'elatex-string)
(require 'elatex-error)

(defconst elatex--max-string-bytes 100000)

(cl-defstruct (elatex--token
               (:constructor elatex--make-token)
               (:predicate elatex--token-struct-p))
  (args [] :type vector)
  (nargs 0 :type integer)
  (opt [] :type vector)
  (nopt 0 :type integer)
  sub
  super
  next
  self
  (limits 0 :type integer)
  (p elatex--pd-none :type integer)
  (f elatex--f-nofont :type integer))

(defun elatex--ascii-letter-p (character)
  "Return non-nil for an ASCII command letter CHARACTER."
  (and character
       (or (and (<= ?A character) (<= character ?Z))
           (and (<= ?a character) (<= character ?z)))))

(defun elatex--command-char-kind (character)
  "Return source command-character kind for CHARACTER."
  (cond ((elatex--ascii-letter-p character) 1)
        ((memq character '(?, ?\; ?: ?\\ ?\")) 2)
        (t 0)))

(defun elatex--command-end (string begin)
  "Return the CommandEnd index in STRING from BEGIN, or nil."
  (when (and begin (< begin (length string)))
    (let ((end (1+ begin)))
      (if (= (elatex--command-char-kind
              (and (< end (length string)) (aref string end)))
             2)
          (setq end (1+ end))
        (while (and (< end (length string))
                    (= (elatex--command-char-kind (aref string end)) 1))
          (setq end (1+ end))))
      end)))

(defun elatex--lookup-record (string begin index)
  "Look up STRING's command at BEGIN in first-entry INDEX."
  (let ((end (elatex--command-end string begin)))
    (and end (gethash (substring string begin end) index))))

(defun elatex--lookup-key (string begin)
  "Look up a source key beginning at STRING index BEGIN."
  (elatex--lookup-record string begin elatex--key-index))

(defun elatex--lookup-environment (name)
  "Look up exact environment NAME."
  (gethash name elatex--environment-index))

(defun elatex--symbol-command-end (string begin)
  "Return IsSymbol's candidate end in STRING from BEGIN."
  (let* ((length (length string))
         (position (1+ begin)))
    (if (and (< position length) (memq (aref string position) '(?, ?\;)))
        (1+ position)
      (when (< position length) (setq position (1+ position)))
      (while (and (< position length)
                  (elatex--ascii-letter-p (aref string position)))
        (setq position (1+ position)))
      position)))

(defun elatex--lookup-symbol (string begin)
  "Look up an exact symbol command in STRING at BEGIN."
  (let ((end (elatex--symbol-command-end string begin)))
    (or (gethash (substring string begin end) elatex--symbol-index)
        (gethash (substring string begin end) elatex--mathjax-symbol-index))))

(defun elatex--lookup-font (name)
  "Translate root font NAME to its parser identity with recovery."
  (let* ((record (gethash (concat "\\" name) elatex--key-index))
         (identity (and record (aref record 1))))
    (if (memq identity
              (list elatex--pd-text elatex--pd-mathbf elatex--pd-mathbfit
                    elatex--pd-mathcal elatex--pd-mathscr elatex--pd-mathfrak
                    elatex--pd-mathbb elatex--pd-mathsf elatex--pd-mathsfbf
                    elatex--pd-mathsfit elatex--pd-mathsfbfit
                    elatex--pd-mathtt elatex--pd-mathnormal))
        identity
      (elatex--add-error elatex--errunknownfont)
      elatex--pd-text)))

(defun elatex--lookup-delimiter (string begin)
  "Return (IDENTITY . SOURCE-NAME) for STRING at BEGIN.
The scan is ordered and prefix-sensitive.  Invalid input returns DEL_NONE and
source sentinel name dot."
  (let ((index 0) result)
    (while (and (null result) (< index (length elatex--delimiters)))
      (let* ((record (aref elatex--delimiters index))
             (name (aref record 0)))
        (when (and (<= (+ begin (length name)) (length string))
                   (string= name (substring string begin (+ begin (length name)))))
          (setq result (cons (aref record 1) name))))
      (setq index (1+ index)))
    (or result (cons elatex--del-none "."))))

(defun elatex--lookup-combining (identity)
  "Return combining record for parser IDENTITY, or a zero record."
  (or (gethash identity elatex--combining-command-index)
      (vector identity 0 0 0)))

(defun elatex--option-argument (string begin open close)
  "Read nested OPEN/CLOSE argument in STRING at BEGIN.
Return (VALUE . NEXT), or (nil . BEGIN) when no immediate opener exists."
  (let ((length (length string)))
    (if (or (>= begin length) (/= (aref string begin) open))
        (cons nil begin)
      (let ((depth 1)
            (end begin))
        (while (and (< end length) (> depth 0))
          (setq end (1+ end))
          (when (< end length)
            (cond ((= (aref string end) open) (setq depth (1+ depth)))
                  ((= (aref string end) close) (setq depth (1- depth))))))
        (when (> depth 0) (elatex--add-error elatex--lexprematureend))
        (cons (substring string (1+ begin) end)
              (if (< end length) (1+ end) end))))))

(defun elatex--option (string begin)
  "Read an immediate square-bracket option in STRING at BEGIN."
  (elatex--option-argument string begin ?\[ ?\]))

(defun elatex--argument (string begin)
  "Read a mandatory argument in STRING at BEGIN with source whitespace rules."
  (let ((length (length string)))
    (while (and (< begin length)
                (memq (aref string begin) '(?\s ?\t ?\n ?\r ?\f ?\v)))
      (setq begin (1+ begin)))
    (let ((braced (elatex--option-argument string begin ?{ ?})))
      (if (car braced)
          braced
        (if (or (>= begin length)
                (cl-position (aref string begin) "\\ ^_+-*/()@#$%&{},;\n"))
            (cons nil begin)
          (cons (char-to-string (aref string begin)) (1+ begin)))))))

(defun elatex--script (string begin)
  "Read a source Script in STRING at BEGIN and return (VALUE . NEXT)."
  (let ((length (length string)))
    (cond
     ((>= begin length) (cons "" begin))
     ((and (/= (aref string begin) ?\\) (/= (aref string begin) ?{))
      (cons (char-to-string (aref string begin)) (1+ begin)))
     ((= (aref string begin) ?\\)
      (let ((end (1+ begin)))
        (while (and (< end length)
                    (not (cl-position (aref string end) " \t+-*/&\\_^}")))
          (setq end (1+ end)))
        (when (and (< end length) (memq (aref string end) '(?\s ?})))
          (setq end (1+ end)))
        (cons (substring string begin end) end)))
     (t
      (let ((depth 1) (end begin))
        (while (and (< end length) (> depth 0))
          (setq end (1+ end))
          (when (< end length)
            (cond ((= (aref string end) ?{) (setq depth (1+ depth)))
                  ((= (aref string end) ?}) (setq depth (1- depth))))))
        (when (> depth 0) (elatex--add-error elatex--lexprematureend))
        (cons (substring string (1+ begin) end)
              (if (< end length) (1+ end) end)))))))

(defun elatex--token-set-args (token args)
  "Set TOKEN argument vector to ARGS."
  (setf (elatex--token-args token) (vconcat args)
        (elatex--token-nargs token) (length args)))

(defun elatex--token-set-options (token options)
  "Set TOKEN option vector to OPTIONS."
  (setf (elatex--token-opt token) (vconcat options)
        (elatex--token-nopt token) (length options)))

(defun elatex--peek-ahead (token string begin)
  "Apply source postfix lookahead to TOKEN in STRING at BEGIN."
  (let ((backup begin)
        (length (length string))
        (reset t))
    (while (and (< begin length) (= (aref string begin) ?\s))
      (setq begin (1+ begin)))
    (let* ((record (elatex--lookup-key string begin))
           (identity (and record (aref record 1))))
      (cond
       ((eq identity elatex--pd-limits)
        (setq begin (+ begin (length (aref record 0))) reset nil)
        (setf (elatex--token-limits token) 1))
       ((eq identity elatex--pd-nolimits)
        (setq begin (+ begin (length (aref record 0))) reset nil)
        (setf (elatex--token-limits token) 0))
       ((memq identity (list elatex--pd-over elatex--pd-choose))
        (setq reset nil)
        (setf (elatex--token-p token)
              (if (= identity elatex--pd-over) elatex--pd-frac elatex--pd-binom))
        (let ((left (substring string (elatex--token-self token) begin))
              (operator-length (if (= identity elatex--pd-over) 5 7)))
          (setq begin (+ begin operator-length))
          (while (and (< begin length) (memq (aref string begin) '(?\s ?\t)))
            (setq begin (1+ begin)))
          (let ((right
                 (if (and (< begin length) (= (aref string begin) ?{))
                     (elatex--argument string begin)
                   (if (or (>= begin length) (= (aref string begin) ?\\))
                       (cons nil begin)
                     (let ((end begin))
                       (while (and (< end length)
                                   (not (cl-position (aref string end) "\\ \t{")))
                         (setq end (1+ end)))
                       (cons (and (> end begin) (substring string begin end)) end))))))
            (if (null (car right))
                (progn (elatex--add-error elatex--errtoofewmandarg)
                       (setf (elatex--token-p token) elatex--pd-none
                             (elatex--token-next token) begin))
              (elatex--token-set-args token (list left (car right)))
              (elatex--peek-ahead token string (cdr right))))
          (setq begin nil))))
      )
    (when begin
      (while (and (< begin length) (memq (aref string begin) '(?_ ?^)))
        (setq reset nil)
        (let ((kind (aref string begin))
              (script (elatex--script string (1+ begin))))
          (if (= kind ?_)
              (progn
                (when (elatex--token-sub token)
                  (elatex--add-error elatex--errmultisub))
                (setf (elatex--token-sub token) (car script)))
            (when (elatex--token-super token)
              (elatex--add-error elatex--errmultisup))
            (setf (elatex--token-super token) (car script)))
          (setq begin (cdr script))))
      (setf (elatex--token-next token) (if reset backup begin))))
  token)

(defun elatex--left-middle-right (string begin)
  "Scan a source left/middle/right construct in STRING at BEGIN.
Return vector [NEXT ARG1 ARG2 OPEN MIDDLE CLOSE]."
  (let ((length (length string))
        (depth 1) arg1 arg2 middle)
    (while (and (< begin length) (= (aref string begin) ?\s))
      (setq begin (1+ begin)))
    (let* ((open-record (elatex--lookup-delimiter string begin))
           (open-id (car open-record))
           (open (cdr open-record)))
      (if (= open-id elatex--del-none)
          (elatex--add-error elatex--invaliddelimiter)
        (setq begin (+ begin (length open))))
      (let ((end begin))
        (while (and (< end length) (> depth 0))
          (when (= (aref string end) ?\\)
            (cond
             ((and (<= (+ end 6) length)
                   (string= "\\right" (substring string end (+ end 6))))
              (setq depth (1- depth)))
             ((and (<= (+ end 5) length)
                   (string= "\\left" (substring string end (+ end 5))))
              (setq depth (1+ depth)))
             ((and (= depth 1) (<= (+ end 7) length)
                   (string= "\\middle" (substring string end (+ end 7))))
              (setq arg1 (substring string begin end)
                    end (+ end 7))
              (while (and (< end length) (= (aref string end) ?\s))
                (setq end (1+ end)))
              (let ((record (elatex--lookup-delimiter string end)))
                (setq middle (cdr record))
                (if (= (car record) elatex--del-none)
                    (elatex--add-error elatex--invaliddelimiter)
                  (setq begin (+ end (length middle))))))))
          (setq end (1+ end)))
        (setq end (1- end))
        (if (and (>= end 0) (< end length))
            (progn
              (setq arg2 (substring string begin end)
                    end (+ end 6))
              (while (and (< end length) (= (aref string end) ?\s))
                (setq end (1+ end)))
              (let* ((record (elatex--lookup-delimiter string end))
                     (close (cdr record)))
                (if (= (car record) elatex--del-none)
                    (elatex--add-error elatex--invaliddelimiter)
                  (setq end (+ end (length close))))
                (vector end (or arg1 "") (or arg2 "") open
                        (or middle ".") close)))
          (elatex--add-error elatex--norightbrac)
          (vector length (or arg1 "") "" open (or middle ".") "."))))))

(defun elatex--left-right-block-end (string begin)
  "Return index after the balanced left/right block at BEGIN."
  (let ((length (length string)) (depth 1))
    (while (and (< begin length) (= (aref string begin) ?\s))
      (setq begin (1+ begin)))
    (let ((open (elatex--lookup-delimiter string begin)))
      (unless (= (car open) elatex--del-none)
        (setq begin (+ begin (length (cdr open))))))
    (let ((end begin))
      (while (and (< end length) (> depth 0))
        (setq end (1+ end))
        (when (and (< end length) (= (aref string end) ?\\))
          (cond ((and (<= (+ end 6) length)
                      (string= "\\right" (substring string end (+ end 6))))
                 (setq depth (1- depth)))
                ((and (<= (+ end 5) length)
                      (string= "\\left" (substring string end (+ end 5))))
                 (setq depth (1+ depth))))))
      (when (< end length)
        (setq end (+ end 6))
        (while (and (< end length) (= (aref string end) ?\s))
          (setq end (1+ end)))
        (let ((close (elatex--lookup-delimiter string end)))
          (unless (= (car close) elatex--del-none)
            (setq end (+ end (length (cdr close)))))))
      end)))

(defun elatex--matching-environment-end (string begin)
  "Return the end of a nested environment starting at BEGIN."
  (let ((position begin)
        (length (length string))
        (depth 0)
        done)
    (while (and (< position length) (not done))
      (cond
       ((and (<= (+ position 6) length)
             (string= "\\begin" (substring string position (+ position 6))))
        (setq depth (1+ depth)
              position (+ position 6)))
       ((and (<= (+ position 4) length)
             (string= "\\end" (substring string position (+ position 4))))
        (setq depth (1- depth)
              position (+ position 4))
        (setq position (cdr (elatex--argument string position)))
        (when (= depth 0)
          (setq done t)))
       (t
        (setq position (1+ position)))))
    position))

(defun elatex--table-read (string begin)
  "Read an environment table from STRING at BEGIN.
Return vector [CELLS END NC HSEP]."
  (let ((length (length string))
        (cells (list ""))
        (current "")
        (position begin)
        (column 0)
        expected-columns
        (row 0)
        (hsep (list ?c))
        (line nil)
        done)
    (cl-labels ((set-current () (setcar (last cells) current))
                (set-sep (index value)
                  (while (<= (length hsep) index) (setq hsep (append hsep (list ?c))))
                  (setf (nth index hsep) value)))
      (while (and (< position length) (not done))
        (cond
         ((and (<= (+ position 6) length)
               (string= "\\begin" (substring string position (+ position 6))))
          (let ((end (elatex--matching-environment-end string position)))
            (setq current (concat current (substring string position end))
                  position end line t)))
         ((and (<= (+ position 4) length)
               (string= "\\end" (substring string position (+ position 4))))
          (setq position (+ position 4) done t))
         ((and (<= (+ position 6) length)
               (string= "\\hline" (substring string position (+ position 6))))
          (cond
           ((= column 0)
            (when (and (> row 0) (= (nth (1- row) hsep) ?-))
              (elatex--add-error elatex--errdoublehline)
              (setq row (1- row)))
            (set-sep row ?-)
            (setq row (1+ row))
            (set-sep row ?c))
           ((or (null expected-columns) (= column expected-columns))
            (unless expected-columns (setq expected-columns column))
            (setq row (1+ row))
            (set-sep row ?-))
           (t (elatex--add-error elatex--errunexphline)))
          (setq position (+ position 6)))
         ((and (<= (+ position 5) length)
               (string= "\\left" (substring string position (+ position 5))))
          (let ((end (elatex--left-right-block-end string position)))
            (setq current (concat current (substring string position end))
                  position end line t)))
         ((= (aref string position) ?&)
          (set-current)
          (setq cells (append cells (list "")) current ""
                column (1+ column) position (1+ position) line t))
         ((and (= (aref string position) ?\\)
               (< (1+ position) length)
               (= (aref string (1+ position)) ?\\))
          (set-current)
          (setq position (+ position 2) row (1+ row) line nil)
          (set-sep row ?c)
          (unless expected-columns (setq expected-columns column))
          (let ((skip 0))
            (when (/= column expected-columns)
              (if (< expected-columns column)
                  (setq skip (- column expected-columns))
                (elatex--add-error elatex--errnumcolmatch)))
            (dotimes (_ skip) (setq cells (append cells (list nil))))
            (setq cells (append cells (list "")) current "" column 0)))
         ((= (aref string position) ?{)
          (let ((argument (elatex--option-argument string position ?{ ?})))
            (setq current (concat current "{" (car argument)
                                  (if (< (cdr argument) length) "}" ""))
                  position (cdr argument) line t)))
         (t
          (let ((character (aref string position)))
            (setq current (concat current (char-to-string character))
                  position (1+ position))
            (unless (memq character '(?\s ?\t ?\n ?\r ?\f ?\v))
              (setq line t))))))
      (if line
          (set-current)
        (setq cells (butlast cells)))
      (unless expected-columns (setq expected-columns column))
      (when (and line (/= column expected-columns))
        (if (< expected-columns column)
            (dotimes (_ (- column expected-columns))
              (setq cells (append cells (list nil))))
          (elatex--add-error elatex--errnumcolmatch)))
      (vector (vconcat cells) position (1+ expected-columns)
              (apply #'string (cl-subseq hsep 0 (min (length hsep)
                                                     (+ row (if line 1 0)))))))))

(defun elatex--environment-closes-p (string end name)
  "Return non-nil when STRING at END closes environment NAME."
  (and (< end (length string))
       (< (1+ end) (length string))
       (string-prefix-p name (substring string (1+ end)))))

(defun elatex--repair-column-alignment (alignment columns)
  "Validate and cyclically repair ALIGNMENT for COLUMNS."
  (let* ((validated
          (apply #'string
                 (mapcar
                  (lambda (character)
                    (cond
                     ((memq character '(?c ?l ?r ?|)) character)
                     (t
                      (elatex--add-error elatex--errnovalidalignc)
                      ?c)))
                  (string-to-list alignment))))
         (count (cl-count-if (lambda (character)
                               (memq character '(?c ?l ?r)))
                             validated)))
    (if (= count columns)
        validated
      (let ((fixed "")
            (aligned 0)
            (index 0)
            (source-length (length validated)))
        (while (< aligned columns)
          (let ((character (aref validated (% index source-length))))
            (setq fixed (concat fixed (char-to-string character)))
            (when (memq character '(?c ?l ?r))
              (setq aligned (1+ aligned)))
            (setq index (1+ index))))
        (when (= (aref validated (% index source-length)) ?|)
          (setq fixed (concat fixed "|")))
        fixed))))

(defun elatex--apply-row-alignment (hsep alignment)
  "Apply cyclic row ALIGNMENT to non-rule entries in HSEP."
  (let ((characters (string-to-list hsep))
        (row 0)
        (alignment-index 0))
    (while (< row (length characters))
      (unless (= (nth row characters) ?-)
        (setf (nth row characters)
              (aref alignment (% alignment-index (length alignment))))
        (setq alignment-index (1+ alignment-index)))
      (setq row (1+ row)))
    (when (/= alignment-index (length alignment))
      (elatex--add-error elatex--alrowsmatch))
    (apply #'string characters)))

(defun elatex--begin-array-env (token string identity result)
  "Expand array or align TOKEN with IDENTITY into RESULT."
  (let (row-alignment column-alignment begin)
    (if (= identity elatex--pd-array)
        (let ((option (elatex--option string (elatex--token-next token))))
          (setq row-alignment (car option))
          (let ((argument (elatex--argument string (cdr option))))
            (setq column-alignment (car argument)
                  begin (cdr argument)))
          (when (or (null column-alignment)
                    (string-empty-p column-alignment))
            (elatex--add-error elatex--errvalighn)
            (setq begin nil)))
      (setq column-alignment "rl"
            begin (elatex--token-next token)))
    (when begin
      (let* ((table (elatex--table-read string begin))
             (cells (aref table 0))
             (end (aref table 1))
             (columns (aref table 2))
             (hsep (aref table 3))
             (name (aref (elatex--token-args token) 0)))
        (cond
         ((elatex--query-error elatex--errnumcolmatch)
          (elatex--token-set-args result (append cells nil)))
         ((not (elatex--environment-closes-p string end name))
          (elatex--add-error elatex--errnomatchinend))
         (t
          (setq end (+ end (length name) 2)
                column-alignment
                (elatex--repair-column-alignment column-alignment columns))
          (when row-alignment
            (setq hsep (elatex--apply-row-alignment hsep row-alignment)))
          (setf (elatex--token-p result) elatex--pd-array
                (elatex--token-next result) end)
          (elatex--token-set-args result (append cells nil))
          (elatex--token-set-options
           result
           (list (number-to-string columns) column-alignment hsep))))))))

(defun elatex--begin-matrix-env (token string identity result)
  "Expand matrix-like TOKEN with IDENTITY into RESULT."
  (let* ((option (elatex--option string (elatex--token-next token)))
         (vertical (or (and (car option)
                            (> (length (car option)) 0)
                            (aref (car option) 0))
                       ?c))
         (table (elatex--table-read string (cdr option)))
         (cells (aref table 0))
         (end (aref table 1))
         (columns (aref table 2))
         (hsep (aref table 3))
         (name (aref (elatex--token-args token) 0)))
    (unless (memq vertical '(?l ?r ?c))
      (elatex--add-error elatex--errnovalidalignc)
      (setq vertical ?c))
    (if (not (elatex--environment-closes-p string end name))
        (elatex--add-error elatex--errnomatchinend)
      (setq end (+ end (length name) 2))
      (let ((clean ""))
        (mapc (lambda (character)
                (if (= character ?-)
                    (elatex--add-error elatex--errhlinesinmatrix)
                  (setq clean (concat clean (char-to-string character)))))
              (string-to-list hsep))
        (setf (elatex--token-p result) identity
              (elatex--token-next result) end)
        (elatex--token-set-args result (append cells nil))
        (elatex--token-set-options
         result
         (list (number-to-string columns)
               (make-string columns vertical)
               clean))))))

(defun elatex--begin-env (token string)
  "Expand a begin-environment TOKEN in STRING."
  (let* ((name (aref (elatex--token-args token) 0))
         (record (elatex--lookup-environment name))
         (identity (and record (aref record 1)))
         (result (elatex--make-token :self (elatex--token-self token)
                                     :f (elatex--token-f token))))
    (cond
     ((memq identity (list elatex--pd-align elatex--pd-array))
      (elatex--begin-array-env token string identity result))
     ((memq identity
            (list elatex--pd-cases elatex--pd-matrix
                  elatex--pd-vmatrix elatex--pd-vvmatrix
                  elatex--pd-bbmatrix elatex--pd-bmatrix
                  elatex--pd-pmatrix))
      (elatex--begin-matrix-env token string identity result))
     (t
      (elatex--add-error elatex--errunknownenv)))
    (when (and (elatex--token-next result)
               (/= (elatex--token-p result) elatex--pd-none))
      (elatex--peek-ahead result string (elatex--token-next result)))
    result))

(defun elatex--truncate-utf8-bytes (string limit)
  "Return STRING truncated before its UTF-8 encoding would exceed LIMIT."
  (let ((index 0) (bytes 0) (length (length string)))
    (while (and (< index length)
                (let ((next (+ bytes (string-bytes (char-to-string (aref string index))))))
                  (when (<= next limit) (setq bytes next) t)))
      (setq index (1+ index)))
    (substring string 0 index)))

(defun elatex--tex-construct-p (string)
  "Return non-nil when STRING contains a source construct marker."
  (cl-some (lambda (character) (memq character '(?\\ ?_ ?^)))
           (string-to-list string)))

(defun elatex--sublexer (string begin font)
  "Lex one TOKEN from preprocessed STRING at BEGIN in FONT state."
  (let* ((length (length string))
         (token (elatex--make-token :self begin :f font :next begin)))
    (cond
     ((>= begin length) token)
     ((= (aref string begin) ?\\)
      (let* ((key (elatex--lookup-key string begin))
             (identity (and key (aref key 1))))
        (cond
         ((eq identity elatex--pd-leftright)
          (let ((parts (elatex--left-middle-right
                        string (+ begin (length (aref key 0))))))
            (setf (elatex--token-p token) identity)
            (elatex--token-set-args token (append (cl-subseq parts 1 6) nil))
            (elatex--peek-ahead token string (aref parts 0))))
         ((and identity (<= elatex--pd-big1 identity) (<= identity elatex--pd-big4))
          (setq begin (+ begin (length (aref key 0))))
          (while (and (< begin length) (= (aref string begin) ?\s))
            (setq begin (1+ begin)))
          (let ((delimiter (elatex--lookup-delimiter string begin)))
            (when (= (car delimiter) elatex--del-none)
              (elatex--add-error elatex--invaliddelimiter))
            (setf (elatex--token-p token) identity)
            (elatex--token-set-args token (list (cdr delimiter)))
            (elatex--peek-ahead token string (+ begin (length (cdr delimiter))))))
         ((memq identity (list elatex--pd-function elatex--pd-lim))
          (let ((end (1+ begin)))
            (while (and (< end length)
                        (not (cl-position (aref string end) "\\_^/*{ ,;(")))
              (setq end (1+ end)))
            (setf (elatex--token-p token) elatex--pd-text
                  (elatex--token-limits token) (if (= identity elatex--pd-lim) 1 0))
            (elatex--token-set-args token (list (substring string (1+ begin) end)))
            (when (and (< end length) (= (aref string end) ?\s)) (setq end (1+ end)))
            (elatex--peek-ahead token string end)))
         ((memq identity (list elatex--pd-setitalic elatex--pd-setbold elatex--pd-setroman))
          (setq begin (+ begin 3))
          (pcase identity
            ((pred (lambda (value) (= value elatex--pd-setitalic)))
             (setf (elatex--token-f token) elatex--f-italic
                   (elatex--token-p token) elatex--pd-mathsfit))
            ((pred (lambda (value) (= value elatex--pd-setbold)))
             (setf (elatex--token-f token) elatex--f-bold
                   (elatex--token-p token) elatex--pd-mathbf))
            (_ (setf (elatex--token-f token) elatex--f-roman
                     (elatex--token-p token) elatex--pd-text)))
          (when (and (< begin length) (= (aref string begin) ?\s)) (setq begin (1+ begin)))
          (let ((remainder (substring string begin)))
            (when (> (string-bytes remainder) elatex--max-string-bytes)
              (elatex--add-error elatex--errlinetoolong)
              (setq remainder (elatex--truncate-utf8-bytes remainder elatex--max-string-bytes)))
            (elatex--token-set-args token (list remainder))
            (setf (elatex--token-next token) (+ begin (length remainder)))))
         ((eq identity elatex--pd-endline)
          (setf (elatex--token-p token) identity
                (elatex--token-next token) (+ begin 2)))
         ((eq identity elatex--pd-kern)
          (setq begin (+ begin 5))
          (pcase-let* ((`(,number . ,numeric-end)
                        (elatex--get-num-part (substring string begin)))
                       (number-end (+ begin numeric-end))
                       (unit-end (or (elatex--command-end string number-end) number-end))
                       (unit (substring string number-end unit-end)))
            (ignore number)
            (setf (elatex--token-p token) identity)
            (cond
             ((>= (elatex--lookup-unit unit) 0)
              (elatex--token-set-args token (list (substring string begin unit-end)))
              (setf (elatex--token-next token)
                    (min length (if (< unit-end length) (1+ unit-end) unit-end))))
             ((> number-end begin)
              (elatex--token-set-args token (list (substring string begin number-end)))
              (setf (elatex--token-next token) number-end))
             (t (elatex--add-error elatex--errtoofewmandarg)
                (setf (elatex--token-p token) elatex--pd-none)))))
         (key
          (setf (elatex--token-p token) identity)
          (setq begin (+ begin (length (aref key 0))))
          (let ((remaining-options (aref key 3)) options option)
            (while (and (> remaining-options 0)
                        (car (setq option (elatex--option string begin))))
              (setq options (append options (list (car option)))
                    begin (cdr option)
                    remaining-options (1- remaining-options)))
            (setq option (elatex--option string begin))
            (when (car option)
              (elatex--add-error elatex--errtoomanyoptarg)
              (while (car option)
                (setq begin (cdr option)
                      option (elatex--option string begin))))
            (elatex--token-set-options token options))
          (let ((remaining (aref key 2)) arguments argument failed)
            (while (and (> remaining 0) (not failed))
              (setq argument (elatex--argument string begin))
              (if (null (car argument))
                  (setq failed t)
                (setq arguments (append arguments (list (car argument)))
                      begin (cdr argument)
                      remaining (1- remaining))))
            (elatex--token-set-args token arguments)
            (if failed
                (progn
                  (if (and arguments
                           (elatex--query-error elatex--lexprematureend)
                           (string-prefix-p "\\" (car (last arguments))))
                      (elatex--add-error elatex--errunknowncomm)
                    (elatex--add-error elatex--errtoofewmandarg))
                  (setf (elatex--token-p token) elatex--pd-none))
              (elatex--peek-ahead token string begin))))
         (t
          (setq begin (1+ begin))
          (if (and (< begin length)
                   (= (elatex--command-char-kind (aref string begin)) 0))
              (progn
                (setf (elatex--token-p token) elatex--pd-symbol)
                (elatex--token-set-args token
                                        (list (char-to-string (aref string begin))))
                (elatex--peek-ahead token string (1+ begin)))
            (elatex--add-error elatex--errunknowncomm)
            (setf (elatex--token-p token) elatex--pd-none))))))
     ((= (aref string begin) ?$)
      (let ((end (1+ begin)))
        (while (and (< end length) (/= (aref string end) ?$))
          (setq end (1+ end)))
        (if (< end length)
            (progn
              (setf (elatex--token-p token) elatex--pd-rootfont)
              (elatex--token-set-args token (list (substring string (1+ begin) end)))
              (elatex--peek-ahead token string (1+ end)))
          (elatex--add-error elatex--errunmatchdollar)
          (setf (elatex--token-p token) elatex--pd-rootfont)
          (elatex--token-set-args token (list (substring string (1+ begin))))
          (elatex--peek-ahead token string length))))
     ((= (aref string begin) ?{)
      (let ((argument (elatex--option-argument string begin ?{ ?})))
        (when (>= (cdr argument) length)
          (elatex--add-error elatex--errunmatchbrac))
        (setf (elatex--token-p token) elatex--pd-block)
        (elatex--token-set-args token (list (car argument)))
        (elatex--peek-ahead token string (cdr argument))))
     ((= (aref string begin) ?')
      (let ((end (1+ begin)) (count 1))
        (while (and (< count 255) (< end length) (= (aref string end) ?'))
          (setq count (1+ count) end (1+ end)))
        (when (= count 255) (elatex--add-error elatex--errtoomanyprimes))
        (setf (elatex--token-p token) elatex--pd-prime)
        (elatex--token-set-args token (list (string count)))
        (elatex--peek-ahead token string end)))
     ((memq (aref string begin) '(?^ ?_))
      (setf (elatex--token-p token) elatex--pd-box)
      (elatex--token-set-args token '("0" "1"))
      (elatex--peek-ahead token string begin))
     (t
      (let ((end (1+ begin)))
        (while (and (< end length)
                    (not (cl-position (aref string end) "\\_^/*{ +-'")))
          (setq end (1+ end)))
        (while (and (< end length) (= (aref string end) ?\s))
          (setq end (1+ end)))
        (let ((raw (substring string begin end))
              (result "")
              previous-space)
          (mapc (lambda (character)
                  (if (memq character '(?\s ?\t))
                      (unless previous-space
                        (setq result (concat result (char-to-string character))
                              previous-space t))
                    (setq result (concat result (char-to-string character))
                          previous-space nil)))
                (string-to-list raw))
          (setf (elatex--token-p token) elatex--pd-symbol)
          (elatex--token-set-args token (list result))
          (elatex--peek-ahead token string end)))))
    token))

(defun elatex--lexer (string begin font)
  "Lex one token from STRING at BEGIN, expanding environments."
  (let ((token (elatex--sublexer string begin font)))
    (if (= (elatex--token-p token) elatex--pd-begin)
        (elatex--begin-env token string)
      token)))

(defun elatex--preprocess-symbols (string)
  "Expand symbols and reorder pending combining marks in STRING."
  (let ((position 0) (length (length string)) (pieces nil) (pending nil))
    (cl-labels ((emit (piece) (push piece pieces))
                (flush ()
                  (dolist (code pending) (emit (char-to-string code)))
                  (setq pending nil)))
      (while (< position length)
        (let ((character (aref string position)))
          (cond
           ((= character ?\\)
            (let ((key (elatex--lookup-key string position)))
              (if key
                  (let ((name (aref key 0)))
                    (emit name)
                    (setq position (+ position (length name))))
                (let ((symbol (elatex--lookup-symbol string position)))
                  (if symbol
                      (let ((code (aref symbol 1))
                            (name (aref symbol 0))
                            literal-command)
                        (setq literal-command
                              (and (= (length name) 2)
                                   (= (elatex--command-char-kind
                                       (aref name 1))
                                      0)))
                        (cond
                         (literal-command
                          (emit name)
                          (flush))
                         ((elatex--combining-mark-p code)
                          (push code pending))
                         (t
                          (emit (char-to-string code))
                          (flush)))
                        (setq position (+ position (length name)))
                        (when (and (not literal-command)
                                   (< position length)
                                   (= (aref string position) ?\s))
                          (setq position (1+ position))))
                    (emit "\\")
                    (setq position (1+ position))
                    (flush))))))
           ((= character ?\n) (setq position (1+ position)))
           (t (emit (char-to-string character))
              (setq position (1+ position))
              (flush)))))
      (apply #'concat (nreverse pieces)))))

(defun elatex--preprocess-greedy (string operator)
  "Insert greedy argument braces around OPERATOR occurrences in STRING."
  (let ((position 0) (operator-length (length operator)))
    (while (< position (length string))
      (if (and (<= (+ position operator-length) (length string))
               (string= operator (substring string position
                                            (+ position operator-length))))
          (if (elatex--ascii-letter-p
               (and (< (+ position operator-length) (length string))
                    (aref string (+ position operator-length))))
              (setq position (length string))
            (progn
              (unless (and (> position 0) (= (aref string (1- position)) ?}))
                (let ((start (max 0 (1- position))) (depth 1))
                  (while (and (> start 0) (> depth 0))
                    (setq start (1- start))
                    (cond ((= (aref string start) ?{) (setq depth (1- depth)))
                          ((= (aref string start) ?}) (setq depth (1+ depth)))))
                  (setq string (concat (substring string 0 start) "{"
                                       (substring string start position) "}"
                                       (substring string position))
                        position (+ position 2))))
              (let ((start (+ position operator-length)))
                (when (and (< start (length string)) (= (aref string start) ?\s))
                  (setq start (1+ start)))
                (unless (and (< start (length string)) (= (aref string start) ?{))
                  (let ((end start) (depth 1))
                    (while (and (< end (length string)) (> depth 0))
                      (setq end (1+ end))
                      (when (< end (length string))
                        (cond ((= (aref string end) ?}) (setq depth (1- depth)))
                              ((= (aref string end) ?{) (setq depth (1+ depth))))))
                    (setq string (concat (substring string 0 start) "{"
                                         (substring string start end) "}"
                                         (substring string end))))))
              (setq position (1+ position))))
        (setq position (1+ position))))
    string))

(defun elatex--preprocessor (string)
  "Run pinned symbol expansion, greedy over, then greedy choose."
  (let ((result (elatex--preprocess-symbols string)))
    (setq result (elatex--preprocess-greedy result "\\over"))
    (elatex--preprocess-greedy result "\\choose")))

(provide 'elatex-lexer)
;;; elatex-lexer.el ends here
