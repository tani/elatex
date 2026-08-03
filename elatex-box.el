;;; elatex-box.el --- Retained rendering boxes  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tex, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Native Emacs Lisp port of libtexprintf 1.31 by Bart Pieters, pinned at
;; revision 18977837b20649d56a651eb6bf846f1c914db77a.

;;; Commentary:

;; Formula-for-formula retained box layout translated from boxes.c.  Child
;; storage grows geometrically while child order and first-overlap priority
;; remain observable.  Empty aggregate geometry is defined as all zero.

;;; Code:

(require 'cl-lib)
(require 'elatex-string)
(require 'elatex-error)

(cl-defstruct (elatex--box (:constructor elatex--make-box))
  parent
  (children [] :type vector)
  (child-count 0 :type integer)
  (type elatex--b-unit :type integer)
  (state elatex--init :type integer)
  content
  (x-align elatex--center :type integer)
  (y-align elatex--center :type integer)
  (rx 0 :type integer)
  (ry 0 :type integer)
  (ax 0 :type integer)
  (ay 0 :type integer)
  (width 0 :type integer)
  (height 0 :type integer)
  (x-center 0 :type integer)
  (y-center 0 :type integer))

(cl-defstruct (elatex--proof-layout
               (:constructor elatex--make-proof-layout))
  (premise-count 0 :type integer)
  (conclusion-index 0 :type integer)
  (rule-index 0 :type integer)
  left-label-index
  right-label-index
  (rule-pattern "" :type string)
  root-at-top)

(defun elatex--c-truncate (number)
  "Truncate NUMBER toward zero as a C integer conversion does."
  (if (< number 0) (ceiling number) (floor number)))

(defun elatex--c-div (numerator denominator)
  "Divide NUMERATOR by DENOMINATOR with C integer truncation."
  (elatex--c-truncate (/ (float numerator) denominator)))

(defun elatex--init-box (parent type content)
  "Initialize and return a box with PARENT, TYPE, and typed CONTENT."
  (let ((box (elatex--make-box :parent parent :type type :content content)))
    (cond
     ((= type elatex--b-dummy)
      (setf (elatex--box-width box) (aref content 0)
            (elatex--box-height box) (aref content 1)
            (elatex--box-state box) elatex--sizeknown))
     ((= type elatex--b-endline)
      (setf (elatex--box-state box) elatex--sizeknown)))
    box))

(defun elatex--box-child (box index)
  "Return child INDEX of BOX."
  (aref (elatex--box-children box) index))

(defun elatex--append-child-box (parent child)
  "Append existing CHILD to PARENT and return CHILD."
  (let* ((count (elatex--box-child-count parent))
         (children (elatex--box-children parent))
         (capacity (length children)))
    (when (= count capacity)
      (let* ((new-capacity (if (= capacity 0) 1 (* capacity 2)))
             (new-children (make-vector new-capacity nil)))
        (dotimes (index count)
          (aset new-children index (aref children index)))
        (setq children new-children)
        (setf (elatex--box-children parent) children)))
    (setf (elatex--box-parent child) parent)
    (aset children count child)
    (setf (elatex--box-child-count parent) (1+ count))
    child))

(defun elatex--add-child (parent type content)
  "Append a child of TYPE and CONTENT to PARENT and return it."
  (elatex--append-child-box parent (elatex--init-box parent type content)))

(defun elatex--box-in-box (box type content)
  "Wrap non-root BOX in a new box of TYPE carrying CONTENT.
Return zero on success and one for the recoverable root-box error."
  (if (null (elatex--box-parent box))
      (progn (elatex--add-error elatex--errboxinbox) 1)
    (let ((old (copy-elatex--box box)))
      (setf (elatex--box-parent old) box)
      (dotimes (index (elatex--box-child-count old))
        (setf (elatex--box-parent (elatex--box-child old index)) old))
      (setf (elatex--box-children box) (vector old)
            (elatex--box-child-count box) 1
            (elatex--box-state box) elatex--init
            (elatex--box-x-align box) elatex--center
            (elatex--box-y-align box) elatex--center
            (elatex--box-type box) type
            (elatex--box-content box) content
            (elatex--box-rx box) 0
            (elatex--box-ry box) 0
            (elatex--box-ax box) 0
            (elatex--box-ay box) 0
            (elatex--box-width box) 0
            (elatex--box-height box) 0
            (elatex--box-x-center box) 0
            (elatex--box-y-center box) 0)
      0)))

(defun elatex--state-boxtree (box)
  "Return (MINIMUM-STATE . BOX-WITH-STATE) for BOX's tree.
Ties select the later child, matching StateBoxtree."
  (let ((state (elatex--box-state box))
        (minimum box))
    (dotimes (index (elatex--box-child-count box))
      (pcase-let ((`(,child-state . ,child-minimum)
                   (elatex--state-boxtree (elatex--box-child box index))))
        (when (<= child-state state)
          (setq state child-state
                minimum child-minimum))))
    (cons state minimum)))

(defun elatex--box-contains-p (box x y)
  "Return non-nil when half-open BOX geometry contains X,Y."
  (and (<= (elatex--box-ax box) x)
       (< x (+ (elatex--box-ax box) (elatex--box-width box)))
       (<= (elatex--box-ay box) y)
       (< y (+ (elatex--box-ay box) (elatex--box-height box)))))

(defun elatex--find-box-at-pos (box x y)
  "Find the first non-dummy leaf containing X,Y, starting from BOX."
  (if (/= (elatex--box-state box) elatex--absposknown)
      (progn (elatex--add-error elatex--errboxatpos) nil)
    (while (and (elatex--box-parent box)
                (not (elatex--box-contains-p box x y)))
      (setq box (elatex--box-parent box)))
    (if (not (elatex--box-contains-p box x y))
        nil
      (let ((searching t)
            result)
        (while (and searching (> (elatex--box-child-count box) 0))
          (let ((index 0)
                found)
            (while (and (< index (elatex--box-child-count box)) (not found))
              (let ((child (elatex--box-child box index)))
                (when (elatex--box-contains-p child x y)
                  (setq box child found t)))
              (setq index (1+ index)))
            (unless (and found (/= (elatex--box-type box) elatex--b-dummy))
              (setq searching nil box nil))))
        (when box (setq result box))
        result))))

(defun elatex--set-alignment-centers (box preserve-single-line-baseline)
  "Set BOX centers from its alignments.
When PRESERVE-SINGLE-LINE-BASELINE is non-nil, the caller sets Y center."
  (setf (elatex--box-x-center box)
        (pcase (elatex--box-x-align box)
          ((pred (lambda (value) (= value elatex--max)))
           (elatex--box-width box))
          ((pred (lambda (value) (= value elatex--min))) 0)
          ((pred (lambda (value) (= value elatex--center)))
           (elatex--c-div (1- (elatex--box-width box)) 2))
          (_ (elatex--box-x-center box))))
  (unless preserve-single-line-baseline
    (setf (elatex--box-y-center box)
          (pcase (elatex--box-y-align box)
            ((pred (lambda (value) (= value elatex--max)))
             (elatex--box-height box))
            ((pred (lambda (value) (= value elatex--min))) 0)
            ((pred (lambda (value) (= value elatex--center)))
             (elatex--c-div (1- (elatex--box-height box)) 2))
            (_ (elatex--box-y-center box))))))

(defun elatex--unit-box-size (box)
  "Compute BOX as a unit box; return zero on success."
  (if (/= (elatex--box-type box) elatex--b-unit)
      (progn (elatex--add-error elatex--erruboxsize) 1)
    (when (< (elatex--box-state box) elatex--sizeknown)
      (setf (elatex--box-width box) (elatex--strspaces (elatex--box-content box))
            (elatex--box-height box) 1
            (elatex--box-y-center box) 0)
      (elatex--set-alignment-centers box t)
      (setf (elatex--box-state box) elatex--sizeknown))
    0))

(defun elatex--box-size-children (box)
  "Compute all sizes below BOX bottom-up; return zero on success."
  (let ((error-count 0))
    (dotimes (index (elatex--box-child-count box))
      (let ((child (elatex--box-child box index)) state-minimum)
        (while (= (car (setq state-minimum (elatex--state-boxtree child)))
                  elatex--init)
          (setq error-count
                (+ error-count (elatex--box-size (cdr state-minimum)))))))
    (if (> error-count 0) 1 0)))

(defun elatex--array-box-size (box)
  "Compute array BOX geometry; return zero on success."
  (cond
   ((/= (elatex--box-type box) elatex--b-array)
    (elatex--add-error elatex--erraboxsize) 1)
   ((/= (elatex--box-size-children box) 0) 1)
   ((= (elatex--box-child-count box) 0)
    (setf (elatex--box-width box) 0 (elatex--box-height box) 0
          (elatex--box-x-center box) 0 (elatex--box-y-center box) 0
          (elatex--box-state box) elatex--sizeknown)
    0)
   (t
    (let* ((count (elatex--box-child-count box))
           (declared-columns (aref (elatex--box-content box) 0))
           (columns (if (<= declared-columns 0) count declared-columns))
           (rows (if (<= declared-columns 0) 1
                   (+ (/ count columns) (if (> (% count columns) 0) 1 0))))
           (heights (make-vector rows 0))
           (y-centers (make-vector rows 0))
           (widths (make-vector columns 0))
           (x-centers (make-vector columns 0))
           (row-y (make-vector rows 0))
           (column-x (make-vector columns 0)))
      (dotimes (index count)
        (let* ((column (% index columns))
               (row (/ index columns))
               (child (elatex--box-child box index))
               (upper (- (elatex--box-height child)
                         (elatex--box-y-center child))))
          (when (> upper (- (aref heights row) (aref y-centers row)))
            (aset heights row (+ (aref heights row)
                                 (- upper (aref heights row))
                                 (aref y-centers row))))
          (let ((lower (elatex--box-y-center child)))
            (when (> lower (aref y-centers row))
              (aset heights row (+ (aref heights row)
                                   (- lower (aref y-centers row))))
              (aset y-centers row lower)))
          (let ((right (- (elatex--box-width child)
                          (elatex--box-x-center child))))
            (when (> right (- (aref widths column)
                              (aref x-centers column)))
              (aset widths column (+ (aref widths column)
                                     (- right (aref widths column))
                                     (aref x-centers column)))))
          (let ((left (elatex--box-x-center child)))
            (when (> left (aref x-centers column))
              (aset widths column (+ (aref widths column)
                                     (- left (aref x-centers column))))
              (aset x-centers column left)))))
      (cl-loop for index from 1 below columns do
               (aset column-x index (+ (aref column-x (1- index))
                                       (aref widths (1- index)))))
      (cl-loop for index downfrom (- rows 2) to 0 do
               (aset row-y index (+ (aref row-y (1+ index))
                                    (aref heights (1+ index)))))
      (setf (elatex--box-width box)
            (+ (aref column-x (1- columns)) (aref widths (1- columns)))
            (elatex--box-height box)
            (+ (aref row-y 0) (aref heights 0))
            (elatex--box-state box) elatex--sizeknown)
      (elatex--set-alignment-centers box nil)
      (dotimes (index count)
        (let* ((column (% index columns))
               (row (/ index columns))
               (child (elatex--box-child box index)))
          (setf (elatex--box-ry child)
                (+ (aref row-y row)
                   (- (aref y-centers row) (elatex--box-y-center child)))
                (elatex--box-rx child)
                (+ (aref column-x column)
                   (- (aref x-centers column) (elatex--box-x-center child)))
                (elatex--box-state child) elatex--relposknown)))
      0))))

(defun elatex--pos-box-size (box)
  "Compute positioned BOX geometry; return zero on success."
  (cond
   ((/= (elatex--box-type box) elatex--b-pos)
    (elatex--add-error elatex--errpboxsize) 1)
   ((/= (elatex--box-size-children box) 0) 1)
   (t
    (setf (elatex--box-width box) 0 (elatex--box-height box) 0)
    (let ((coordinates (elatex--box-content box))
          (index 0)
          invalid)
      (while (and (< index (elatex--box-child-count box)) (not invalid))
        (let ((x (aref coordinates (* 2 index)))
              (y (aref coordinates (1+ (* 2 index))))
              (child (elatex--box-child box index)))
          (if (or (< x 0) (< y 0))
              (setq invalid t)
            (setf (elatex--box-rx child) x
                  (elatex--box-ry child) y
                  (elatex--box-state child) elatex--relposknown
                  (elatex--box-width box)
                  (max (elatex--box-width box) (+ x (elatex--box-width child)))
                  (elatex--box-height box)
                  (max (elatex--box-height box) (+ y (elatex--box-height child))))))
        (setq index (1+ index)))
      (if invalid
          (progn (elatex--add-error elatex--errnegrelpos) 1)
        (setf (elatex--box-state box) elatex--sizeknown)
        (if (= (elatex--box-child-count box) 0)
            (setf (elatex--box-x-center box) 0
                  (elatex--box-y-center box) 0)
          (elatex--set-alignment-centers box nil))
        0)))))

(defun elatex--dummy-box-size (box)
  "Validate dummy BOX and return zero on success."
  (if (/= (elatex--box-type box) elatex--b-dummy)
      (progn (elatex--add-error elatex--errdboxsize) 1)
    (when (< (elatex--box-state box) elatex--sizeknown)
      (setf (elatex--box-state box) elatex--sizeknown))
    0))

(defun elatex--endline-box-size (box)
  "Compute explicit end-line BOX and return zero on success."
  (if (/= (elatex--box-type box) elatex--b-endline)
      (progn (elatex--add-error elatex--errelboxsize) 1)
    (setf (elatex--box-width box) 0 (elatex--box-height box) 0
          (elatex--box-x-center box) 0 (elatex--box-y-center box) 0)
    (when (< (elatex--box-state box) elatex--sizeknown)
      (setf (elatex--box-state box) elatex--sizeknown))
    0))

(defun elatex--line-box-size (box)
  "Compute wrapping line BOX geometry; return zero on success."
  (cond
   ((/= (elatex--box-type box) elatex--b-line)
    (elatex--add-error elatex--errlboxsize) 1)
   ((/= (elatex--box-size-children box) 0) 1)
   ((= (elatex--box-child-count box) 0)
    (setf (elatex--box-width box) 0 (elatex--box-height box) 0
          (elatex--box-x-center box) 0 (elatex--box-y-center box) 0
          (elatex--box-state box) elatex--sizeknown)
    0)
   (t
    (let* ((count (elatex--box-child-count box))
           (line-width (max 0 (aref (elatex--box-content box) 0)))
           (lines (make-vector count 0))
           (y (make-vector (1+ count) 0))
           (y-centers (make-vector (1+ count) 0))
           (line-number 0) (height 0) (baseline 0) (width 0) (x 0))
      (dotimes (index count)
        (let ((child (elatex--box-child box index)))
          (when (or (and (> line-width 0)
                         (> (+ x (elatex--box-width child)) line-width)
                         (> x 0))
                    (= (elatex--box-type child) elatex--b-endline))
            (dotimes (prior line-number)
              (aset y prior (+ (aref y prior) height)))
            (aset y line-number height)
            (aset y-centers line-number baseline)
            (setq height 0 baseline 0 line-number (1+ line-number) x 0))
          (setf (elatex--box-rx child) x)
          (setq x (+ x (elatex--box-width child))
                width (max width x))
          (aset lines index line-number)
          (let ((upper (- (elatex--box-height child)
                          (elatex--box-y-center child))))
            (when (> upper (- height baseline))
              (setq height (+ height (- upper (- height baseline))))))
          (when (> (elatex--box-y-center child) baseline)
            (setq height (+ height (- (elatex--box-y-center child) baseline))
                  baseline (elatex--box-y-center child)))))
      (dotimes (prior line-number)
        (aset y prior (+ (aref y prior) height)))
      (aset y line-number height)
      (aset y-centers line-number baseline)
      (setq height (aref y 0))
      (dotimes (line line-number)
        (aset y line (aref y (1+ line))))
      (aset y line-number 0)
      (cl-loop for index downfrom (1- count) to 0 do
               (let* ((child (elatex--box-child box index))
                      (line (aref lines index)))
                 (setf (elatex--box-ry child)
                       (+ (aref y line)
                          (- (aref y-centers line)
                             (elatex--box-y-center child)))
                       (elatex--box-state child) elatex--relposknown)))
      (setf (elatex--box-height box) height
            (elatex--box-width box) width
            (elatex--box-state box) elatex--sizeknown)
      (elatex--set-alignment-centers box (= line-number 0))
      (if (= line-number 0)
          (setf (elatex--box-y-center box) (aref y-centers 0))
        (elatex--set-alignment-centers box nil))
      0))))

(defun elatex--proof-rule-string (pattern width)
  "Repeat proof rule PATTERN to exactly WIDTH cells."
  (if (string-empty-p pattern)
      (make-string width ?\s)
    (let ((result ""))
      (while (< (length result) width)
        (setq result (concat result pattern)))
      (substring result 0 width))))

(defun elatex--proof-box-size (box)
  "Compute retained proof BOX geometry; return zero on success."
  (cond
   ((/= (elatex--box-type box) elatex--b-proof)
    (elatex--add-error elatex--errunknownbox) 1)
   ((/= (elatex--box-size-children box) 0) 1)
   (t
    (let* ((layout (elatex--box-content box))
           (premise-count (elatex--proof-layout-premise-count layout))
           (conclusion
            (elatex--box-child box
                               (elatex--proof-layout-conclusion-index layout)))
           (rule
            (elatex--box-child box (elatex--proof-layout-rule-index layout)))
           (premise-height 0)
           (premise-width 0)
           first-axis last-axis premise-axis
           premise-left premise-right)
      (dotimes (index premise-count)
        (let ((premise (elatex--box-child box index)))
          (setf (elatex--box-rx premise) premise-width)
          (let ((axis (+ premise-width (elatex--box-x-center premise))))
            (unless first-axis (setq first-axis axis))
            (setq last-axis axis))
          (setq premise-width (+ premise-width (elatex--box-width premise))
                premise-height (max premise-height
                                    (elatex--box-height premise)))
          (when (< index (1- premise-count))
            (setq premise-width (1+ premise-width)))))
      (if (> premise-count 0)
          (setq premise-axis (elatex--c-div (+ first-axis last-axis) 2)
                premise-left premise-axis
                premise-right (max 0 (1- (- premise-width premise-axis))))
        (setq premise-axis (elatex--box-x-center conclusion)
              premise-left 0
              premise-right 0))
      (let* ((conclusion-left (elatex--box-x-center conclusion))
             (conclusion-right
              (max 0 (1- (- (elatex--box-width conclusion)
                            conclusion-left))))
             (rule-left (max premise-left conclusion-left))
             (rule-right (max premise-right conclusion-right))
             (rule-width (max 1 (+ 1 rule-left rule-right)))
             (premise-shift (- rule-left premise-axis))
             (conclusion-x (- rule-left conclusion-left))
             (rule-y (if (elatex--proof-layout-root-at-top layout)
                         premise-height
                       (elatex--box-height conclusion)))
             (premise-y (if (elatex--proof-layout-root-at-top layout)
                            0
                          (1+ rule-y)))
             (conclusion-y (if (elatex--proof-layout-root-at-top layout)
                               (1+ rule-y)
                             0))
             (minimum-x 0)
             (maximum-x rule-width)
             (minimum-y 0)
             (maximum-y (+ conclusion-y (elatex--box-height conclusion))))
        (setf (elatex--box-content rule)
              (elatex--proof-rule-string
               (elatex--proof-layout-rule-pattern layout) rule-width)
              (elatex--box-width rule) rule-width
              (elatex--box-height rule) 1
              (elatex--box-x-center rule) (elatex--c-div (1- rule-width) 2)
              (elatex--box-y-center rule) 0
              (elatex--box-rx rule) 0
              (elatex--box-ry rule) rule-y
              (elatex--box-state rule) elatex--relposknown)
        (setq maximum-y (max maximum-y (1+ rule-y)))
        (dotimes (index premise-count)
          (let ((premise (elatex--box-child box index)))
            (cl-incf (elatex--box-rx premise) premise-shift)
            (setf (elatex--box-ry premise)
                  (if (elatex--proof-layout-root-at-top layout)
                      (- premise-height (elatex--box-height premise))
                    premise-y)
                  (elatex--box-state premise) elatex--relposknown)
            (setq minimum-x (min minimum-x (elatex--box-rx premise))
                  maximum-x (max maximum-x
                                 (+ (elatex--box-rx premise)
                                    (elatex--box-width premise)))
                  maximum-y (max maximum-y
                                 (+ (elatex--box-ry premise)
                                    (elatex--box-height premise))))))
        (setf (elatex--box-rx conclusion) conclusion-x
              (elatex--box-ry conclusion) conclusion-y
              (elatex--box-state conclusion) elatex--relposknown)
        (dolist (entry
                 (list
                  (cons 'left (elatex--proof-layout-left-label-index layout))
                  (cons 'right (elatex--proof-layout-right-label-index layout))))
          (when (cdr entry)
            (let* ((label (elatex--box-child box (cdr entry)))
                   (x (if (eq (car entry) 'left)
                          (- -1 (elatex--box-width label))
                        (1+ rule-width)))
                   (y (- rule-y (elatex--box-y-center label))))
              (setf (elatex--box-rx label) x
                    (elatex--box-ry label) y
                    (elatex--box-state label) elatex--relposknown)
              (setq minimum-x (min minimum-x x)
                    maximum-x (max maximum-x (+ x (elatex--box-width label)))
                    minimum-y (min minimum-y y)
                    maximum-y (max maximum-y (+ y (elatex--box-height label)))))))
        (let ((x-shift (- minimum-x))
              (y-shift (- minimum-y)))
          (dotimes (index (elatex--box-child-count box))
            (let ((child (elatex--box-child box index)))
              (cl-incf (elatex--box-rx child) x-shift)
              (cl-incf (elatex--box-ry child) y-shift)))
          (setf (elatex--box-width box) (- maximum-x minimum-x)
                (elatex--box-height box) (- maximum-y minimum-y)
                (elatex--box-x-center box) (+ rule-left x-shift)
                (elatex--box-y-center box)
                (+ conclusion-y (elatex--box-y-center conclusion) y-shift)
                (elatex--box-x-align box) elatex--fix
                (elatex--box-y-align box) elatex--fix
                (elatex--box-state box) elatex--sizeknown))
        0)))))

(defun elatex--box-size (box)
  "Compute BOX size if initialized; return zero on success."
  (if (/= (elatex--box-state box) elatex--init)
      0
    (pcase (elatex--box-type box)
      ((pred (lambda (type) (= type elatex--b-unit)))
       (elatex--unit-box-size box))
      ((pred (lambda (type) (= type elatex--b-array)))
       (elatex--array-box-size box))
      ((pred (lambda (type) (= type elatex--b-pos)))
       (elatex--pos-box-size box))
      ((pred (lambda (type) (= type elatex--b-dummy))) 0)
      ((pred (lambda (type) (= type elatex--b-line)))
       (elatex--line-box-size box))
      ((pred (lambda (type) (= type elatex--b-endline)))
       (elatex--endline-box-size box))
      ((pred (lambda (type) (= type elatex--b-proof)))
       (elatex--proof-box-size box))
      (_ (elatex--add-error elatex--errunknownbox) 1))))

(defun elatex--box-pos-recursive (box)
  "Propagate absolute positions below BOX top-down."
  (dotimes (index (elatex--box-child-count box))
    (let ((child (elatex--box-child box index)))
      (setf (elatex--box-ax child) (+ (elatex--box-ax box) (elatex--box-rx child))
            (elatex--box-ay child) (+ (elatex--box-ay box) (elatex--box-ry child))
            (elatex--box-state child) elatex--absposknown)
      (elatex--box-pos-recursive child))))

(defun elatex--box-pos (box)
  "Size BOX bottom-up, then absolutely position it at zero, zero."
  (when (< (elatex--box-state box) elatex--sizeknown)
    (elatex--box-size box))
  (setf (elatex--box-ax box) 0 (elatex--box-ay box) 0
        (elatex--box-state box) elatex--absposknown)
  (elatex--box-pos-recursive box))

(defun elatex--box-set-state (box state)
  "Lower BOX and descendant states to STATE where necessary."
  (when (> (elatex--box-state box) state)
    (setf (elatex--box-state box) state))
  (dotimes (index (elatex--box-child-count box))
    (elatex--box-set-state (elatex--box-child box index) state)))

(provide 'elatex-box)
;;; elatex-box.el ends here
