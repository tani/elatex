;;; elatex-string.el --- Pinned Unicode string utilities  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tex, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Native Emacs Lisp port of libtexprintf 1.31 by Bart Pieters, pinned at
;; revision 18977837b20649d56a651eb6bf846f1c914db77a.

;;; Commentary:

;; Internal translation of stringutils.c and the length readers in lexer.c.
;; Width decisions use only the pinned source tables, never Emacs display
;; metrics or Unicode properties.

;;; Code:

(require 'cl-lib)
(require 'elatex-data)

(defvar elatex--wide-character-width 2
  "Dynamically bound width assigned to pinned wide characters.")

(defvar elatex--full-width-character-width 2
  "Dynamically bound width assigned to pinned full-width characters.")

(defun elatex--merge-ranges (ranges)
  "Return sorted, overlap-merged membership index for RANGES."
  (let* ((sorted (sort (append ranges nil)
                       (lambda (left right)
                         (or (< (aref left 0) (aref right 0))
                             (and (= (aref left 0) (aref right 0))
                                  (< (aref left 1) (aref right 1)))))))
         merged)
    (dolist (range sorted)
      (let ((start (aref range 0))
            (end (aref range 1)))
        (if (and merged (<= start (1+ (cdr (car merged)))))
            (setcdr (car merged) (max end (cdr (car merged))))
          (push (cons start end) merged))))
    (vconcat (mapcar (lambda (range) (vector (car range) (cdr range)))
                     (nreverse merged)))))

(defconst elatex--combining-membership-index
  (elatex--merge-ranges elatex--combining-ranges)
  "Sorted and merged combining-mark membership index.")

(defconst elatex--wide-membership-index
  (elatex--merge-ranges elatex--wide-ranges)
  "Sorted and merged wide-character membership index.")

(defconst elatex--full-width-membership-index
  (elatex--merge-ranges elatex--full-width-ranges)
  "Sorted and merged full-width-character membership index.")

(defun elatex--in-range-index-p (character index)
  "Return non-nil when CHARACTER is contained in sorted range INDEX."
  (let ((low 0)
        (high (1- (length index)))
        found)
    (while (and (not found) (<= low high))
      (let* ((middle (/ (+ low high) 2))
             (range (aref index middle))
             (start (aref range 0))
             (end (aref range 1)))
        (cond
         ((< character start) (setq high (1- middle)))
         ((> character end) (setq low (1+ middle)))
         (t (setq found t)))))
    found))

(defun elatex--combining-mark-p (character)
  "Return non-nil when CHARACTER is in the pinned combining table."
  (elatex--in-range-index-p character elatex--combining-membership-index))

(defun elatex--wide-character-p (character)
  "Return non-nil when CHARACTER is in the pinned wide table."
  (elatex--in-range-index-p character elatex--wide-membership-index))

(defun elatex--full-width-character-p (character)
  "Return non-nil when CHARACTER is in the pinned full-width table."
  (elatex--in-range-index-p character elatex--full-width-membership-index))

(defun elatex--strspaces (string)
  "Count pinned monospace cells occupied by STRING.
Combining, wide, and full-width tests are independent as in strspaces."
  (let ((width 0))
    (mapc (lambda (character)
            (unless (elatex--combining-mark-p character)
              (setq width (1+ width)))
            (when (elatex--wide-character-p character)
              (setq width (+ width (1- elatex--wide-character-width))))
            (when (elatex--full-width-character-p character)
              (setq width (+ width (1- elatex--full-width-character-width)))))
          (string-to-list string))
    width))

(defun elatex--map-code-point (code-point mappings)
  "Map CODE-POINT through sorted MAPPINGS, or return it unchanged."
  (let ((low 0)
        (high (1- (length mappings)))
        result)
    (while (and (null result) (<= low high))
      (let* ((middle (/ (+ low high) 2))
             (mapping (aref mappings middle))
             (source (aref mapping 0)))
        (cond
         ((< code-point source) (setq high (1- middle)))
         ((> code-point source) (setq low (1+ middle)))
         (t (setq result (aref mapping 1))))))
    (or result code-point)))

(defun elatex--unicode-mapper (string)
  "Repair mathematical-alphabet holes in STRING at emission time."
  (apply #'string
         (mapcar (lambda (character)
                   (elatex--map-code-point character
                                           elatex--font-hole-mappings))
                 (string-to-list string))))

(defconst elatex--superscript-quick-set
  "231hjrwylsxABDEGHIJKLMNOPRTUWabdegkmoptuvcfz0i456789+-=()nV! "
  "ASCII bytes accepted by the source superscript quick check.")

(defconst elatex--subscript-quick-set
  "iruv0123456789+-=()aeoxhklmnpstj "
  "ASCII bytes accepted by the source subscript quick check.")

(defun elatex--mappable-script-p (script quick-set)
  "Return non-nil when every character in SCRIPT occurs in QUICK-SET."
  (cl-every (lambda (character) (not (null (cl-position character quick-set))))
            (string-to-list script)))

(defun elatex--mappable-super-p (script)
  "Return non-nil when SCRIPT can be mapped to superscript characters."
  (elatex--mappable-script-p script elatex--superscript-quick-set))

(defun elatex--mappable-sub-p (script)
  "Return non-nil when SCRIPT can be mapped to subscript characters."
  (elatex--mappable-script-p script elatex--subscript-quick-set))

(defun elatex--map-script (script mappings)
  "Map every code point in SCRIPT through MAPPINGS."
  (apply #'string
         (mapcar (lambda (character)
                   (elatex--map-code-point character mappings))
                 (string-to-list script))))

(defun elatex--map-super-script (script)
  "Map SCRIPT to pinned superscript code points."
  (elatex--map-script script elatex--superscript-mappings))

(defun elatex--map-sub-script (script)
  "Map SCRIPT to pinned subscript code points."
  (elatex--map-script script elatex--subscript-mappings))

(defun elatex--ascii-digit-p (character)
  "Return non-nil when CHARACTER is an ASCII decimal digit."
  (and character (<= ?0 character) (<= character ?9)))

(defun elatex--get-num-part (string)
  "Return (VALUE . END) for STRING's source numeric prefix.
The grammar is an optional sign, digits, an optional dot, and digits.  No
prefix yields the source default 1.0; a bare sign or dot parses as zero."
  (let* ((length (length string))
         (position 0))
    (when (and (< position length)
               (memq (aref string position) '(?+ ?-)))
      (setq position (1+ position)))
    (while (and (< position length)
                (elatex--ascii-digit-p (aref string position)))
      (setq position (1+ position)))
    (when (and (< position length) (= (aref string position) ?.))
      (setq position (1+ position)))
    (while (and (< position length)
                (elatex--ascii-digit-p (aref string position)))
      (setq position (1+ position)))
    (if (= position 0)
        (cons 1.0 0)
      (let ((prefix (substring string 0 position)))
        (cons (if (string-match-p "[0-9]" prefix)
                  (string-to-number prefix)
                0.0)
              position)))))

(defun elatex--lookup-unit (string)
  "Return the exact pinned scale for unit STRING, or -1."
  (let ((index 0)
        result)
    (while (and (null result) (< index (length elatex--length-units)))
      (let ((record (aref elatex--length-units index)))
        (when (string= string (aref record 0))
          (setq result (aref record 1))))
      (setq index (1+ index)))
    (if (null result) -1.0 result)))

(defun elatex--round-away-from-zero (number)
  "Round NUMBER to an integer, with half ties away from zero."
  (if (>= number 0)
      (floor (+ number 0.5))
    (ceiling (- number 0.5))))

(defun elatex--read-length-width (string)
  "Read STRING as a pinned length measured in character widths."
  (pcase-let* ((`(,number . ,end) (elatex--get-num-part string))
               (unit (elatex--lookup-unit (substring string end))))
    (elatex--round-away-from-zero
     (if (>= unit 0) (* number unit) number))))

(defun elatex--read-length-height (string)
  "Read STRING as a pinned length measured in character heights."
  (pcase-let* ((`(,number . ,end) (elatex--get-num-part string))
               (unit (elatex--lookup-unit (substring string end))))
    (elatex--round-away-from-zero
     (if (>= unit 0) (/ (* number unit) 2.0) number))))

(provide 'elatex-string)
;;; elatex-string.el ends here
