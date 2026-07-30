;;; elatex-parser.el --- TeX construct to box-tree parser  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tex, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Native translation of reference/src/parser.c from libtexprintf 1.31.

;;; Code:

(require 'cl-lib)
(require 'elatex-box)
(require 'elatex-lexer)
(require 'elatex-string)

(defvar elatex--style (copy-elatex--style elatex--style-unicode-template)
  "Mutable drawing style for the current compatibility configuration.")
(defvar elatex--style-kind 'unicode
  "Dynamically selected style kind for ASCII-specific parser behavior.")
(defvar elatex--root-font elatex--pd-text
  "Resolved root font identity for the current parse.")

(defun elatex--last-child (box)
  "Return the last child of BOX."
  (elatex--box-child box (1- (elatex--box-child-count box))))

(defun elatex--new-line-child (parent &optional line-width)
  "Append and return a line child of PARENT with LINE-WIDTH."
  (elatex--add-child parent elatex--b-line (vector (or line-width 0))))

(defun elatex--add-unit (parent string)
  "Append a unit containing STRING to PARENT."
  (elatex--add-child parent elatex--b-unit string))

(defun elatex--ensure-position-capacity (box count)
  "Ensure BOX's coordinate vector can hold COUNT children."
  (let* ((need (* 2 count))
         (old (elatex--box-content box)))
    (when (< (length old) need)
      (setf (elatex--box-content box)
            (vconcat old (make-vector (- need (length old)) 0))))))

(defun elatex--set-child-position (box index x y)
  "Set BOX child INDEX relative position to X, Y."
  (elatex--ensure-position-capacity box (1+ index))
  (let ((coordinates (elatex--box-content box)))
    (aset coordinates (* 2 index) x)
    (aset coordinates (1+ (* 2 index)) y)))

(defun elatex--positioned-unit (box character x y)
  "Append CHARACTER as a unit in BOX at X, Y."
  (let ((index (elatex--box-child-count box)))
    (elatex--add-unit box (char-to-string character))
    (elatex--set-child-position box index x y)))

(defun elatex--mappable-tree-p (box predicate)
  "Implement IsMappableLineBoxtree for BOX using PREDICATE."
  (pcase (elatex--box-type box)
    ((pred (lambda (type) (= type elatex--b-unit)))
     (funcall predicate (elatex--box-content box)))
    ((pred (lambda (type) (= type elatex--b-line)))
     (let ((index 0) (result t))
       (while (and result (< index (elatex--box-child-count box)))
         (setq result (elatex--mappable-tree-p
                       (elatex--box-child box index) predicate)
               index (1+ index)))
       result))
    ((pred (lambda (type) (= type elatex--b-pos)))
     (and (= (elatex--box-child-count box) 1)
          (elatex--mappable-tree-p (elatex--box-child box 0) predicate)))
    (_ nil)))

(defun elatex--map-box-tree (box mapper)
  "Implement MapBoxtree by applying MAPPER to all unit contents in BOX."
  (if (= (elatex--box-type box) elatex--b-unit)
      (setf (elatex--box-content box)
            (funcall mapper (elatex--box-content box)))
    (dotimes (index (elatex--box-child-count box))
      (elatex--map-box-tree (elatex--box-child box index) mapper))))

(defun elatex--add-scripts (subscript superscript box limits font)
  "Implement AddScripts around BOX in FONT."
  (when (or subscript superscript)
    (elatex--box-pos box)
    (let ((height (elatex--box-height box))
          (width (elatex--box-width box))
          (y-offset 0)
          (box-index 1)
          (mapped-sub nil))
      (when (= 0 (elatex--box-in-box box elatex--b-pos (make-vector 6 0)))
        (setf (elatex--box-y-center box)
              (elatex--box-y-center (elatex--box-child box 0))
              (elatex--box-y-align box) elatex--fix)
        (when subscript
          (let ((script (elatex--new-line-child box)))
            (elatex--parse-string-recursive subscript script font)
            (setf (elatex--box-state box) elatex--init)
            (elatex--box-pos box)
            (cond
             ((and (/= 0 (elatex--style-map-super-sub elatex--style))
                   (not limits)
                   (elatex--mappable-tree-p script #'elatex--mappable-sub-p))
              (elatex--map-box-tree script #'elatex--map-sub-script)
              (elatex--set-child-position box box-index width 0)
              (setq mapped-sub t))
             ((and (/= 0 (elatex--style-map-super-sub elatex--style))
                   limits
                   (elatex--mappable-tree-p script #'elatex--mappable-super-p))
              (setq y-offset (+ y-offset (elatex--box-height script)))
              (elatex--set-child-position box 0 0 y-offset)
              (elatex--map-box-tree script #'elatex--map-super-script)
              (let ((x (/ (- width (elatex--box-width script)) 2)))
                (when (< x 0)
                  (dotimes (index box-index)
                    (elatex--set-child-position
                     box index (- x)
                     (aref (elatex--box-content box) (1+ (* 2 index)))))
                  (setq width (elatex--box-width script) x 0))
                (elatex--set-child-position box box-index x 0))
              (cl-incf (elatex--box-y-center box)
                       (elatex--box-height script)))
             (t
              (setq y-offset (+ y-offset (elatex--box-height script)))
              (elatex--set-child-position box 0 0 y-offset)
              (let ((x (if limits
                           (/ (- width (elatex--box-width script)) 2)
                         width)))
                (when (< x 0)
                  (dotimes (index box-index)
                    (elatex--set-child-position
                     box index (- x)
                     (aref (elatex--box-content box) (1+ (* 2 index)))))
                  (setq width (elatex--box-width script) x 0))
                (elatex--set-child-position box box-index x 0))
              (cl-incf (elatex--box-y-center box)
                       (elatex--box-height script))))
            (setq box-index (1+ box-index))))
        (when superscript
          (let ((script (elatex--new-line-child box)))
            (elatex--parse-string-recursive superscript script font)
            (setf (elatex--box-state box) elatex--init)
            (elatex--box-pos box)
            (cond
             ((and (/= 0 (elatex--style-map-super-sub elatex--style))
                   (or (not mapped-sub) (> height 1))
                   (not limits)
                   (elatex--mappable-tree-p script #'elatex--mappable-super-p))
              (elatex--map-box-tree script #'elatex--map-super-script)
              (elatex--set-child-position
               box box-index width (+ y-offset height (if (> height 0) -1 0))))
             ((and (/= 0 (elatex--style-map-super-sub elatex--style))
                   limits
                   (elatex--mappable-tree-p script #'elatex--mappable-sub-p))
              (elatex--map-box-tree script #'elatex--map-sub-script)
              (let ((x (/ (- width (elatex--box-width script)) 2)))
                (when (< x 0)
                  (dotimes (index box-index)
                    (elatex--set-child-position
                     box index (- x)
                     (aref (elatex--box-content box) (1+ (* 2 index)))))
                  (setq width (elatex--box-width script) x 0))
                (elatex--set-child-position box box-index x (+ y-offset height))))
             (t
              (let ((x (if limits
                           (/ (- width (elatex--box-width script)) 2)
                         width)))
                (when (< x 0)
                  (dotimes (index box-index)
                    (elatex--set-child-position
                     box index (- x)
                     (aref (elatex--box-content box) (1+ (* 2 index)))))
                  (setq width (elatex--box-width script) x 0))
                (elatex--set-child-position box box-index x (+ y-offset height)))))))
        (setf (elatex--box-state box) elatex--init)
        (elatex--box-pos box)
        (elatex--box-set-state box elatex--sizeknown)))))

(defun elatex--bracket-chars (delimiter)
  "Return ELATEX--STYLE character vector for DELIMITER."
  (cond
   ((= delimiter elatex--del-lcurl) (elatex--style-lcurly elatex--style))
   ((= delimiter elatex--del-rcurl) (elatex--style-rcurly elatex--style))
   ((= delimiter elatex--del-lsq) (elatex--style-lsquare elatex--style))
   ((= delimiter elatex--del-rsq) (elatex--style-rsquare elatex--style))
   ((= delimiter elatex--del-l) (elatex--style-lbrack elatex--style))
   ((= delimiter elatex--del-r) (elatex--style-rbrack elatex--style))
   ((= delimiter elatex--del-vbar) (elatex--style-vbar elatex--style))
   ((= delimiter elatex--del-dvbar) (elatex--style-dvbar elatex--style))
   ((= delimiter elatex--del-lfloor) (elatex--style-lfloor elatex--style))
   ((= delimiter elatex--del-rfloor) (elatex--style-rfloor elatex--style))
   ((= delimiter elatex--del-lceil) (elatex--style-lceil elatex--style))
   ((= delimiter elatex--del-rceil) (elatex--style-rceil elatex--style))
   ((= delimiter elatex--del-uparrow) (elatex--style-uparrow elatex--style))
   ((= delimiter elatex--del-downarrow) (elatex--style-downarrow elatex--style))
   ((= delimiter elatex--del-updownarrow) (elatex--style-updownarrow elatex--style))
   ((= delimiter elatex--del-duparrow) (elatex--style-duparrow elatex--style))
   ((= delimiter elatex--del-ddownarrow) (elatex--style-ddownarrow elatex--style))
   ((= delimiter elatex--del-dupdownarrow) (elatex--style-dupdownarrow elatex--style))))

(defun elatex--draw-regular-bracket (box height characters)
  "Implement Brac in BOX at HEIGHT from CHARACTERS."
  (setq height (max 1 height))
  (if (= height 1)
      (elatex--positioned-unit box (aref characters 0) 0 0)
    (elatex--positioned-unit box (aref characters 1) 0 0)
    (dotimes (offset (- height 2))
      (elatex--positioned-unit box (aref characters 2) 0 (1+ offset)))
    (elatex--positioned-unit box (aref characters 3) 0 (1- height))))

(defun elatex--draw-symmetric-bracket (box height characters)
  "Implement SymBrac in BOX at HEIGHT from CHARACTERS."
  (when (= (% height 2) 0) (setq height (1+ height)))
  (if (= height 1)
      (elatex--positioned-unit box (aref characters 0) 0 0)
    (dotimes (row height)
      (elatex--positioned-unit
       box (cond ((= row 0) (aref characters 1))
                 ((= row (/ height 2)) (aref characters 2))
                 ((= row (1- height)) (aref characters 4))
                 (t (aref characters 3)))
       0 row))))

(defun elatex--draw-angle-bracket (box height left-p)
  "Draw an angle bracket of HEIGHT in BOX; LEFT-P selects orientation."
  (let ((characters (elatex--style-angle elatex--style)))
    (when (and (/= height 1) (= (% height 2) 1)) (setq height (1+ height)))
    (if (= height 1)
        (elatex--positioned-unit box (aref characters (if left-p 0 1)) 0 0)
      (dotimes (row height)
        (let ((upper (< row (/ height 2))))
          (elatex--positioned-unit
           box (aref characters (if (eq upper left-p) 2 3))
           (if left-p
               (if upper (- (/ height 2) row 1) (- row (/ height 2)))
             (if upper row (- height row 1)))
           row))))))

(defun elatex--draw-slash (box height forward-p)
  "Draw a slash of HEIGHT in BOX.  FORWARD-P selects slash direction."
  (setq height (max 1 height))
  (dotimes (row height)
    (elatex--positioned-unit
     box (if forward-p (elatex--style-fslash elatex--style)
           (elatex--style-bslash elatex--style))
     (if forward-p row (- height row 1)) row)))

(defun elatex--draw-scalable-delimiter (delimiter box height)
  "Implement DrawScalableDelim for DELIMITER in BOX at HEIGHT."
  (cond
   ((memq delimiter (list elatex--del-lcurl elatex--del-rcurl))
    (elatex--draw-symmetric-bracket box height (elatex--bracket-chars delimiter)))
   ((memq delimiter (list elatex--del-langle elatex--del-rangle))
    (elatex--draw-angle-bracket box height (= delimiter elatex--del-langle)))
   ((= delimiter elatex--del-slash) (elatex--draw-slash box height t))
   ((= delimiter elatex--del-backslash) (elatex--draw-slash box height nil))
   ((elatex--bracket-chars delimiter)
    (elatex--draw-regular-bracket box height (elatex--bracket-chars delimiter)))))

(defun elatex--delimiter-id (name)
  "Return delimiter identity for exact source NAME."
  (car (elatex--lookup-delimiter name 0)))

(defun elatex--make-left-right (token parent font)
  "Implement MakeLeftRight for TOKEN in PARENT using FONT."
  (let* ((line (elatex--new-line-child parent))
         (left (elatex--delimiter-id (aref (elatex--token-args token) 2)))
         (middle (elatex--delimiter-id (aref (elatex--token-args token) 3)))
         (right (elatex--delimiter-id (aref (elatex--token-args token) 4)))
         left-box middle-box right-box body1 body2)
    (unless (= left elatex--del-dot)
      (setq left-box (elatex--add-child line elatex--b-pos (vector 0 0))))
    (unless (string-empty-p (aref (elatex--token-args token) 0))
      (setq body1 (elatex--new-line-child line))
      (elatex--parse-string-recursive (aref (elatex--token-args token) 0) body1 font))
    (unless (= middle elatex--del-dot)
      (setq middle-box (elatex--add-child line elatex--b-pos (vector 0 0))))
    (unless (string-empty-p (aref (elatex--token-args token) 1))
      (setq body2 (elatex--new-line-child line))
      (elatex--parse-string-recursive (aref (elatex--token-args token) 1) body2 font))
    (unless (= right elatex--del-dot)
      (setq right-box (elatex--add-child line elatex--b-pos (vector 0 0))))
    (if (and (null body1) (null body2))
        (elatex--add-error elatex--errnobodyinlr)
      (let ((center 0) (upper1 0) (upper2 0))
        (when body1
          (elatex--box-pos body1)
          (elatex--box-set-state body1 elatex--sizeknown)
          (setq center (elatex--box-y-center body1)
                upper1 (- (elatex--box-height body1) center)))
        (when body2
          (elatex--box-pos body2)
          (elatex--box-set-state body2 elatex--sizeknown)
          (setq center (max center (elatex--box-y-center body2))
                upper2 (- (elatex--box-height body2)
                          (elatex--box-y-center body2))))
        (let ((height (+ center (max upper1 upper2))))
          (when (and (cl-some (lambda (d) (memq d (list elatex--del-lcurl elatex--del-rcurl)))
                              (list left middle right))
                     (= (% height 2) 0))
            (setq height (1+ height) center (1+ center)))
          (when (and (cl-some (lambda (d) (memq d (list elatex--del-langle elatex--del-rangle)))
                              (list left middle right))
                     (/= height 1) (= (% height 2) 1))
            (setq height (1+ height)))
          (dolist (entry (list (cons left left-box) (cons middle middle-box)
                               (cons right right-box)))
            (when (cdr entry)
              (elatex--draw-scalable-delimiter (car entry) (cdr entry) height)
              (setf (elatex--box-y-center (cdr entry)) center
                    (elatex--box-y-align (cdr entry)) elatex--fix)))))
      (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                           line (/= 0 (elatex--token-limits token)) font))))

(defun elatex--make-big-brace (token parent font)
  "Implement MakeBigBrace for TOKEN."
  (let* ((line (elatex--new-line-child parent))
         (delimiter (elatex--delimiter-id (aref (elatex--token-args token) 0)))
         (height (pcase (elatex--token-p token)
                   ((pred (lambda (p) (= p elatex--pd-big1))) 2)
                   ((pred (lambda (p) (= p elatex--pd-big2))) 3)
                   ((pred (lambda (p) (= p elatex--pd-big3))) 4)
                   (_ 5)))
         bracket)
    (unless (= delimiter elatex--del-dot)
      (setq bracket (elatex--add-child line elatex--b-pos (vector 0 0))))
    (elatex--add-child line elatex--b-dummy (vector 0 height))
    (when bracket
      (elatex--draw-scalable-delimiter delimiter bracket height)
      (setf (elatex--box-y-center bracket) (/ (1- height) 2)
            (elatex--box-y-align bracket) elatex--fix))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         line (/= 0 (elatex--token-limits token)) font)))

(defun elatex--add-braces (left-name right-name box)
  "Implement AddBraces around BOX using LEFT-NAME and RIGHT-NAME."
  (let ((left (elatex--delimiter-id left-name))
        (right (elatex--delimiter-id right-name)))
    (unless (and (= left elatex--del-dot) (= right elatex--del-dot))
      (elatex--box-pos box)
      (let ((height (elatex--box-height box)))
        (elatex--box-set-state box elatex--sizeknown)
        (when (= 0 (elatex--box-in-box box elatex--b-line (vector 0)))
          (let ((body (elatex--box-child box 0)) left-box right-box)
            (unless (= left elatex--del-dot)
              (setq left-box (elatex--add-child box elatex--b-pos (vector 0 0)))
              (aset (elatex--box-children box) 0 left-box)
              (aset (elatex--box-children box) 1 body))
            (unless (= right elatex--del-dot)
              (setq right-box (elatex--add-child box elatex--b-pos (vector 0 0))))
            (dolist (entry (list (cons left left-box) (cons right right-box)))
              (when (cdr entry)
                (elatex--draw-scalable-delimiter (car entry) (cdr entry) height)
                (setf (elatex--box-y-center (cdr entry))
                      (elatex--box-y-center body)
                      (elatex--box-y-align (cdr entry)) elatex--fix)))))))))

(defun elatex--make-dummy-box (token parent font)
  "Implement MakeBox for TOKEN."
  (let ((box (elatex--add-child
              parent elatex--b-dummy
              (vector (elatex--read-length-width (aref (elatex--token-args token) 0))
                      (elatex--read-length-height (aref (elatex--token-args token) 1))))))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         box (/= 0 (elatex--token-limits token)) font)))

(defun elatex--make-kern (token parent font)
  "Implement MakeKern for TOKEN."
  (let ((box (elatex--add-child parent elatex--b-dummy
                                (vector (elatex--read-length-width
                                         (aref (elatex--token-args token) 0)) 1))))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         box (/= 0 (elatex--token-limits token)) font)))

(defun elatex--root-line-width (box)
  "Return line width stored in BOX's root."
  (while (elatex--box-parent box) (setq box (elatex--box-parent box)))
  (aref (elatex--box-content box) 0))

(defun elatex--make-phantom (token parent font vertical horizontal)
  "Implement MakeAPhantom with VERTICAL and HORIZONTAL dimensions retained."
  (let* ((source (elatex--preprocessor (aref (elatex--token-args token) 0)))
         (dummy (elatex--init-box nil elatex--b-line
                                  (vector (elatex--root-line-width parent)))))
    (elatex--parse-string-recursive source dummy font)
    (elatex--box-pos dummy)
    (let ((box (elatex--add-child
                parent elatex--b-dummy
                (vector (if horizontal (elatex--box-width dummy) 0)
                        (if vertical (elatex--box-height dummy) 0)))))
      (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                           box (/= 0 (elatex--token-limits token)) font))))

(defun elatex--parse-string-in-box (source parent font)
  "Implement ParseStringInBox for SOURCE in PARENT using FONT."
  (let ((line (elatex--new-line-child parent)))
    (elatex--parse-string-recursive source line font)
    line))

(defun elatex--make-frac (token parent font)
  "Implement MakeFrac for TOKEN."
  (let ((frac (elatex--add-child parent elatex--b-array (vector 1))))
    (elatex--parse-string-in-box (aref (elatex--token-args token) 0) frac font)
    (let ((bar (elatex--add-unit frac "")))
      (elatex--parse-string-in-box (aref (elatex--token-args token) 1) frac font)
      (elatex--box-pos frac)
      (let ((baseline (elatex--box-ry bar)))
        (setf (elatex--box-content bar)
              (make-string (elatex--box-width frac)
                           (elatex--style-fracline elatex--style))
              (elatex--box-width bar) (elatex--box-width frac)
              (elatex--box-x-center bar) (elatex--box-x-center frac)
              (elatex--box-state frac) elatex--init)
        (elatex--box-pos frac)
        (elatex--box-set-state frac elatex--sizeknown)
        (setf (elatex--box-y-center frac) baseline
              (elatex--box-y-align frac) elatex--fix
              (elatex--box-state frac) elatex--sizeknown)))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         frac (/= 0 (elatex--token-limits token)) font)))

(defun elatex--make-stack (token parent font distance)
  "Implement MakeStack with DISTANCE rows between arguments."
  (let ((stack (elatex--add-child parent elatex--b-array (vector 1))))
    (dotimes (index (1- (elatex--token-nargs token)))
      (elatex--parse-string-in-box (aref (elatex--token-args token) index) stack font)
      (elatex--add-child stack elatex--b-dummy (vector 0 distance)))
    (elatex--parse-string-in-box
     (aref (elatex--token-args token) (1- (elatex--token-nargs token))) stack font)
    (dotimes (index (elatex--token-nopt token))
      (elatex--add-child stack elatex--b-dummy (vector 0 distance))
      (elatex--parse-string-in-box (aref (elatex--token-opt token) index) stack font))
    (setf (elatex--box-state stack) elatex--init)
    (elatex--box-pos stack)
    (elatex--box-set-state stack elatex--sizeknown)
    (when (> (elatex--box-child-count stack) 1)
      (setf (elatex--box-y-center stack) (elatex--box-ry (elatex--box-child stack 1))
            (elatex--box-y-align stack) elatex--fix
            (elatex--box-state stack) elatex--sizeknown))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         stack (/= 0 (elatex--token-limits token)) font)
    stack))

(defun elatex--make-binom (token parent font)
  "Implement MakeBinom for TOKEN."
  (let ((sub (elatex--token-sub token)) (super (elatex--token-super token)))
    (setf (elatex--token-sub token) nil (elatex--token-super token) nil)
    (let ((binom (elatex--make-stack token parent font 1)))
      (setf (elatex--token-sub token) sub (elatex--token-super token) super)
      (elatex--add-braces "(" ")" parent)
      (elatex--add-scripts sub super binom (/= 0 (elatex--token-limits token)) font))))

(defun elatex--unit-box-count (box)
  "Implement UnitBoxCount for BOX."
  (if (= (elatex--box-type box) elatex--b-unit) 1
    (let ((sum 0) (index 0))
      (while (< index (elatex--box-child-count box))
        (setq sum (+ sum (elatex--unit-box-count
                          (elatex--box-child box index)))
              index (1+ index)))
      sum)))

(defun elatex--first-unit-box (box)
  "Implement FirstUnitBox for BOX."
  (if (= (elatex--box-type box) elatex--b-unit) box
    (let ((index 0) result)
      (while (and (null result) (< index (elatex--box-child-count box)))
        (setq result (elatex--first-unit-box (elatex--box-child box index))
              index (1+ index)))
      result)))

(defun elatex--add-box-below-above (box character align above repeat)
  "Implement AddBoxBelowAbove around BOX."
  (elatex--box-pos box)
  (let ((width (elatex--box-width box)))
    (when (= 0 (elatex--box-in-box box elatex--b-array (vector 1)))
      (setf (elatex--box-x-align (elatex--box-child box 0)) align
            (elatex--box-state (elatex--box-child box 0)) elatex--init)
      (let ((mark (elatex--add-unit
                   box (if repeat (make-string width character)
                         (char-to-string character)))))
        (setf (elatex--box-x-align mark) align)
        (when above
          (let ((first (elatex--box-child box 0)))
            (aset (elatex--box-children box) 0 mark)
            (aset (elatex--box-children box) 1 first)))
        (setf (elatex--box-state box) elatex--init)
        (elatex--box-pos box)
        (setf (elatex--box-y-center box)
              (elatex--box-ry (elatex--box-child box (if above 1 0)))
              (elatex--box-y-align box) elatex--fix)))))

(defun elatex--make-combining (token parent font)
  "Implement MakeCombining for TOKEN."
  (let* ((record (elatex--lookup-combining (elatex--token-p token)))
         (combining (aref record 1))
         (alternative (aref record 2))
         (ascii (aref record 3))
         (stuff (elatex--parse-string-in-box
                 (aref (elatex--token-args token) 0) parent font))
         combined)
    (if (and (eq elatex--style-kind 'ascii) (/= ascii 0))
        (setq alternative ascii)
      (when (or (= 0 (elatex--style-avoid-combining elatex--style))
                (= alternative 0))
        (when (= (elatex--unit-box-count stuff) 1)
          (let ((unit (elatex--first-unit-box stuff)))
            (when (= (elatex--strspaces (elatex--box-content unit)) 1)
              (setf (elatex--box-content unit)
                    (concat (elatex--box-content unit) (char-to-string combining)))
              (setq combined t))))))
    (unless combined
      (when (/= alternative 0)
        (cond
         ((= (elatex--token-p token) elatex--pd-comb-overline)
          (elatex--add-box-below-above stuff alternative elatex--center t t))
         ((= (elatex--token-p token) elatex--pd-comb-underline)
          (elatex--add-box-below-above stuff alternative elatex--center nil t))
         ((memq (elatex--token-p token)
                (list elatex--pd-comb-utilde elatex--pd-comb-wideutilde
                      elatex--pd-comb-threeunderdot elatex--pd-comb-underleftarrow
                      elatex--pd-comb-underrightarrow elatex--pd-comb-underbar
                      elatex--pd-comb-underleftrightarrow
                      elatex--pd-comb-underrightharpoondown
                      elatex--pd-comb-underleftharpoondown elatex--pd-comb-palh
                      elatex--pd-comb-rh elatex--pd-comb-sbbrg))
          (elatex--add-box-below-above stuff alternative elatex--center nil nil))
         ((memq (elatex--token-p token)
                (list elatex--pd-comb-ocommatopright elatex--pd-comb-droang))
          (elatex--add-box-below-above stuff alternative elatex--max t nil))
         (t (elatex--add-box-below-above stuff alternative elatex--center t nil)))))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         stuff (/= 0 (elatex--token-limits token)) font)))

(defun elatex--make-block (token parent font)
  "Implement MakeBlock for TOKEN."
  (let ((block (elatex--new-line-child parent)))
    (if (string-empty-p (aref (elatex--token-args token) 0))
        (elatex--add-child block elatex--b-dummy (vector 0 0))
      (elatex--parse-string-in-box (aref (elatex--token-args token) 0) block font))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         block (/= 0 (elatex--token-limits token)) font)))

(defun elatex--make-sqrt (token parent font)
  "Implement MakeSqrt for TOKEN."
  (let* ((sqrt (elatex--add-child parent elatex--b-pos (vector 0 0)))
         (has-index (> (elatex--token-nopt token) 0))
         (offset (if has-index 1 0)) (x-offset 0))
    (when has-index
      (let ((index-box (elatex--parse-string-in-box
                        (aref (elatex--token-opt token) 0) sqrt font)))
        (elatex--box-pos index-box)
        (setq x-offset (1- (elatex--box-width index-box)))))
    (let ((body (elatex--parse-string-in-box
                 (aref (elatex--token-args token) 0) sqrt font)))
      (elatex--box-pos body)
      (let ((width (elatex--box-width body)) (height (elatex--box-height body))
            (characters (elatex--style-sqrt elatex--style)))
        (when has-index (elatex--set-child-position sqrt 0 0 (1+ (/ height 2))))
        (elatex--set-child-position sqrt offset (+ (/ height 2) 2 x-offset) 0)
        (dotimes (row height)
          (elatex--positioned-unit sqrt (aref characters 1)
                                   (+ (/ height 2) x-offset 1) row))
        (dotimes (index (1+ (/ height 2)))
          (elatex--positioned-unit sqrt (aref characters 0)
                                   (+ index x-offset) (- (/ height 2) index)))
        (elatex--positioned-unit sqrt (aref characters 2)
                                 (+ (/ height 2) 1 x-offset) height)
        (dotimes (index width)
          (elatex--positioned-unit sqrt (aref characters 3)
                                   (+ (/ height 2) 2 x-offset index) height))
        (elatex--positioned-unit sqrt (aref characters 4)
                                 (+ (/ height 2) 2 x-offset width) height)
        (elatex--box-set-state sqrt elatex--relposknown)
        (setf (elatex--box-state sqrt) elatex--init
              (elatex--box-y-center sqrt) (elatex--box-y-center body)
              (elatex--box-y-align sqrt) elatex--fix)))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         sqrt (/= 0 (elatex--token-limits token)) font)))

(defun elatex--draw-integral (parent count size contour-p)
  "Implement DrawInt and DrawOInt in PARENT."
  (let* ((integral (elatex--add-child parent elatex--b-pos (vector 0 0)))
         (small (if contour-p (elatex--style-oiint elatex--style)
                  (elatex--style-iint elatex--style))))
    (if (<= size 1)
        (cond
         ((= count 2) (elatex--positioned-unit integral (aref small 1) 0 0))
         ((= count 3) (elatex--positioned-unit integral (aref small 2) 0 0))
         ((= count 4)
          (dotimes (index 4)
            (elatex--positioned-unit integral (aref small 0) index 0)))
         ((= count 5)
          (elatex--positioned-unit integral (aref small 0) 0 0)
          (elatex--positioned-unit integral (aref small 3) 1 0)
          (elatex--positioned-unit integral (aref small 0) 2 0))
         (t (elatex--positioned-unit integral (aref small 0) 0 0)))
      (let ((draw-count (if (> count 4) 2 count))
            (intchars (elatex--style-int elatex--style)))
        (dotimes (column draw-count)
          (dotimes (row size)
            (elatex--positioned-unit
             integral (aref intchars (cond ((= row 0) 0)
                                            ((= row (1- size)) 2) (t 1)))
             (+ column (if contour-p 1 0) (if (> count 4) column 0)) row)))
        (when (> count 4)
          (elatex--positioned-unit integral (aref small 3)
                                           (if contour-p 2 1) (/ size 2)))
        (when contour-p
          (let ((circle (elatex--style-oint elatex--style)))
            (elatex--positioned-unit integral (aref circle 0) 0 (/ size 2))
            (elatex--positioned-unit integral (aref circle 1)
                                     (if (> count 4) 4 (1+ count)) (/ size 2))))))
    (setf (elatex--box-y-center integral) (/ (1- size) 2)
          (elatex--box-y-align integral) elatex--fix)
    integral))

(defun elatex--make-integral (token parent count contour-p font source)
  "Implement MakeInt, possibly consuming a scaled RHS from SOURCE."
  (if (and (> (elatex--token-nopt token) 0)
           (= (aref (aref (elatex--token-opt token) 0) 0) ?S))
      (let* ((start (elatex--token-next token))
             (equals (cl-position ?= source :start start))
             (end (or equals (length source)))
             (integral-holder (elatex--add-child parent elatex--b-pos (vector 0 0)))
             (next (elatex--add-child parent elatex--b-array (vector 0))))
        (elatex--parse-string-in-box (substring source start end) next font)
        (setf (elatex--token-next token) end)
        (elatex--box-pos next)
        (elatex--box-set-state next elatex--relposknown)
        (let ((integral (elatex--draw-integral integral-holder count
                                               (elatex--box-height next) contour-p)))
          (setf (elatex--box-y-center integral) (elatex--box-y-center next)
                (elatex--box-y-align integral) elatex--fix)
          (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                               integral (or (/= 0 (elatex--token-limits token))
                                            (> count 1)) font)))
    (let ((integral (elatex--draw-integral parent count 3 contour-p)))
      (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                           integral (or (/= 0 (elatex--token-limits token))
                                        (> count 1)) font))))
(defun elatex--draw-large-symbol (parent characters)
  "Implement DrawSymbol from row-major CHARACTERS."
  (let* ((width (aref characters 0)) (height (aref characters 1))
         (symbol (elatex--add-child parent elatex--b-pos (vector))))
    (dotimes (row height)
      (dotimes (column width)
        (elatex--positioned-unit symbol
                                 (aref characters (+ 2 (* row width) column))
                                 column row)))
    (setf (elatex--box-y-center parent) (/ height 2))
    symbol))

(defun elatex--make-large-symbol (token parent font product-p)
  "Make sum or product TOKEN according to PRODUCT-P."
  (let ((symbol (elatex--draw-large-symbol
                 parent (if product-p (elatex--style-prod elatex--style)
                          (elatex--style-sum elatex--style)))))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         symbol t font)))

(defun elatex--make-prime (token parent font)
  "Implement MakePrime for TOKEN."
  (let* ((count (aref (aref (elatex--token-args token) 0) 0))
         (table (cond ((= count 1) (elatex--style-prime elatex--style))
                      ((= count 2) (elatex--style-dprime elatex--style))
                      ((= count 3) (elatex--style-tprime elatex--style))
                      ((= count 4) (elatex--style-qprime elatex--style))))
         symbol)
    (if table (setq symbol (elatex--draw-large-symbol parent table))
      (dotimes (_ count)
        (setq symbol (elatex--draw-large-symbol
                      parent (elatex--style-prime elatex--style)))))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         symbol t font)))

(defun elatex--font-map-record (character table)
  "Map CHARACTER through sentinel-terminated TABLE."
  (let ((index 0) result)
    (while (and (null result) (< index (length table)))
      (let ((record (aref table index)))
        (when (= character (aref record 0)) (setq result (aref record 1))))
      (setq index (1+ index)))
    result))

(defun elatex--font-map-character (character font)
  "Implement MakeSymbol's mathematical FONT mapping for CHARACTER."
  (let ((upper (and (<= ?A character) (<= character ?Z)))
        (lower (and (<= ?a character) (<= character ?z)))
        (digit (and (<= ?0 character) (<= character ?9))))
    (cl-labels ((base (upper-base lower-base &optional digit-base)
                  (cond (upper (+ upper-base (- character ?A)))
                        (lower (+ lower-base (- character ?a)))
                        ((and digit digit-base) (+ digit-base (- character ?0))))))
      (cond
       ((memq font (list elatex--pd-bold elatex--pd-mathbf))
        (or (base #x1D400 #x1D41A #x1D7CE)
            (elatex--font-map-record character elatex--greek-bftable)))
       ((= font elatex--pd-mathbfit)
        (or (base #x1D468 #x1D482)
            (elatex--font-map-record character elatex--greek-bfittable)))
       ((= font elatex--pd-mathcal) (base #x1D49C #x1D4B6))
       ((= font elatex--pd-mathscr) (base #x1D4D0 #x1D4EA))
       ((= font elatex--pd-mathfrak) (base #x1D504 #x1D51E))
       ((= font elatex--pd-mathbb) (base #x1D538 #x1D552 #x1D7D8))
       ((= font elatex--pd-mathsf) (base #x1D5A0 #x1D5BA #x1D7E2))
       ((= font elatex--pd-mathsfbf)
        (or (base #x1D5D4 #x1D5EE #x1D7EC)
            (elatex--font-map-record character elatex--greek-sfbftable)))
       ((= font elatex--pd-mathsfit)
        (or (base #x1D608 #x1D622)
            (elatex--font-map-record character elatex--greek-sfittable)))
       ((= font elatex--pd-mathsfbfit)
        (or (base #x1D63C #x1D656)
            (elatex--font-map-record character elatex--greek-sfbfittable)))
       ((= font elatex--pd-mathtt) (base #x1D670 #x1D68A #x1D7F6))
       ((= font elatex--pd-text) nil)
       (t (base #x1D434 #x1D44E))))))

(defun elatex--make-symbol (token parent font &optional replacement)
  "Implement MakeSymbol for TOKEN, optionally using REPLACEMENT text."
  (let ((text (or replacement (aref (elatex--token-args token) 0))))
    (when (/= (elatex--token-f token) elatex--f-nofont)
      (setq font (pcase (elatex--token-f token)
                   ((pred (lambda (f) (= f elatex--f-italic))) elatex--pd-mathsfit)
                   ((pred (lambda (f) (= f elatex--f-roman))) elatex--pd-text)
                   ((pred (lambda (f) (= f elatex--f-bold))) elatex--pd-mathbf)
                   (_ elatex--root-font))))
    (when (= font elatex--pd-rootfont) (setq font elatex--root-font))
    (let ((mapped (apply #'string
                         (mapcar (lambda (character)
                                   (or (elatex--font-map-character character font)
                                       character))
                                 (string-to-list text)))))
      (let ((box (elatex--add-unit parent mapped)))
        (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                             box (/= 0 (elatex--token-limits token)) font)))))

(defun elatex--make-math-font (token parent font)
  "Render TOKEN argument in its requested math font."
  (let ((box (elatex--parse-string-in-box
              (aref (elatex--token-args token) 0) parent (elatex--token-p token))))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         box (/= 0 (elatex--token-limits token)) font)))

(defun elatex--make-mod (token parent font)
  "Implement MakeMOD for TOKEN."
  (pcase (elatex--token-p token)
    ((pred (lambda (p) (= p elatex--pd-pmod)))
     (elatex--add-unit parent "(mod ")
     (elatex--parse-string-in-box (aref (elatex--token-args token) 0) parent font)
     (elatex--add-unit parent ")"))
    ((or (pred (lambda (p) (= p elatex--pd-mod)))
         (pred (lambda (p) (= p elatex--pd-bmod))))
     (elatex--add-unit parent "mod ")
     (elatex--parse-string-in-box (aref (elatex--token-args token) 0) parent font)
     (elatex--add-unit parent " "))
    (_
     (elatex--add-unit parent "(")
     (elatex--parse-string-in-box (aref (elatex--token-args token) 0) parent font)
     (elatex--add-unit parent ")")))
  (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                       parent (/= 0 (elatex--token-limits token)) font))

(defun elatex--raise-box (token parent font)
  "Implement RaiseBox for TOKEN."
  (elatex--parse-string-recursive (aref (elatex--token-args token) 1) parent font)
  (elatex--box-pos parent)
  (cl-decf (elatex--box-y-center parent)
           (elatex--read-length-height (aref (elatex--token-args token) 0)))
  (setf (elatex--box-y-align parent) elatex--fix)
  (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                       parent (/= 0 (elatex--token-limits token)) font))

(defun elatex--init-vsep (parent height)
  "Append a vertical array separator of HEIGHT."
  (when (> height 0)
    (let ((separator (elatex--add-child parent elatex--b-pos (vector))))
      (dotimes (row height)
        (elatex--positioned-unit separator
                                 (aref (elatex--style-array elatex--style) 0)
                                 0 row))
      separator)))

(defun elatex--init-hsep (parent width)
  "Append a horizontal array separator of WIDTH."
  (when (> width 0)
    (let ((separator (elatex--add-child parent elatex--b-pos (vector))))
      (dotimes (column width)
        (elatex--positioned-unit separator
                                 (aref (elatex--style-array elatex--style) 1)
                                 column 0))
      separator)))

(defun elatex--rescale-separator (separator length vertical-p)
  "Resize SEPARATOR to LENGTH along VERTICAL-P axis."
  (when (> length 0)
    (let ((old-count (elatex--box-child-count separator)))
      (when (< old-count length)
        (dotimes (_ (- length old-count))
          (elatex--add-unit separator
                            (char-to-string
                             (aref (elatex--style-array elatex--style)
                                   (if vertical-p 0 1))))))
      (setf (elatex--box-child-count separator) length
            (elatex--box-content separator) (make-vector (* 2 length) 0))
      (dotimes (index length)
        (elatex--set-child-position separator index
                                    (if vertical-p 0 index)
                                    (if vertical-p index 0)))
      (setf (elatex--box-state separator) elatex--init))))

(defun elatex--make-array-body (token parent font)
  "Implement MakeArrayBody for TOKEN."
  (let* ((columns (aref (elatex--token-opt token) 1))
         (rows (aref (elatex--token-opt token) 2))
         (column-count (length columns))
         (array (elatex--add-child parent elatex--b-array (vector column-count)))
         (argument 0) (cell-index 0))
    (cl-labels
        ((set-column-align (box column)
           (setf (elatex--box-x-align box)
                 (pcase (aref columns column) (?r elatex--max) (?l elatex--min)
                   (_ elatex--center))))
         (set-row-align (box row)
           (setf (elatex--box-y-align box)
                 (pcase (aref rows row) (?t elatex--max) (?b elatex--min)
                   (_ elatex--center))))
         (add-horizontal-row (bottom-p)
           (dotimes (column column-count)
             (let ((box
                    (if (= (aref columns column) ?|)
                        (elatex--add-unit
                         array (char-to-string
                                (aref (elatex--style-array elatex--style)
                                      (if bottom-p
                                          (cond ((= column 0) 8)
                                                ((= column (1- column-count)) 10)
                                                (t 9))
                                        (cond ((= (/ cell-index column-count) 0)
                                               (cond ((= column 0) 2)
                                                     ((= column (1- column-count)) 4)
                                                     (t 3)))
                                              ((= column 0) 5)
                                              ((= column (1- column-count)) 7)
                                              (t 6))))))
                      (elatex--init-hsep array 1))))
               (set-column-align box column)
               (setq cell-index (1+ cell-index))))))
      (while (< argument (elatex--token-nargs token))
        (let ((row (/ cell-index column-count)))
          (when (= (aref rows row) ?-) (add-horizontal-row nil))
          (setq row (/ cell-index column-count))
          (dotimes (column column-count)
            (let ((box (if (= (aref columns column) ?|)
                           (elatex--init-vsep array 1)
                         (prog1
                             (elatex--parse-string-in-box
                              (aref (elatex--token-args token) argument) array font)
                           (setq argument (1+ argument))))))
              (set-column-align box column)
              (set-row-align box row)
              (setq cell-index (1+ cell-index))))))
      (when (and (< (/ cell-index column-count) (length rows))
                 (= (aref rows (/ cell-index column-count)) ?-))
        (add-horizontal-row t)))
    (elatex--box-pos array)
    (let* ((row-count (/ (elatex--box-child-count array) column-count))
           (widths (make-vector column-count 0))
           (heights (make-vector row-count 0))
           (x-centers (make-vector column-count 0))
           (y-centers (make-vector row-count 0)))
      (dotimes (index (elatex--box-child-count array))
        (let* ((column (% index column-count)) (row (/ index column-count))
               (child (elatex--box-child array index))
               (right (- (elatex--box-width child) (elatex--box-x-center child)))
               (upper (- (elatex--box-height child) (elatex--box-y-center child))))
          (when (> upper (- (aref heights row) (aref y-centers row)))
            (aset heights row (+ (aref y-centers row) upper)))
          (when (> (elatex--box-y-center child) (aref y-centers row))
            (cl-incf (aref heights row)
                     (- (elatex--box-y-center child) (aref y-centers row)))
            (aset y-centers row (elatex--box-y-center child)))
          (when (> right (- (aref widths column) (aref x-centers column)))
            (aset widths column (+ (aref x-centers column) right)))
          (when (> (elatex--box-x-center child) (aref x-centers column))
            (cl-incf (aref widths column)
                     (- (elatex--box-x-center child) (aref x-centers column)))
            (aset x-centers column (elatex--box-x-center child)))))
      (dotimes (index (elatex--box-child-count array))
        (let ((column (% index column-count)) (row (/ index column-count))
              (child (elatex--box-child array index)))
          (cond ((= (aref rows row) ?-)
                 (unless (= (aref columns column) ?|)
                   (elatex--rescale-separator child (aref widths column) nil)))
                ((= (aref columns column) ?|)
                 (elatex--rescale-separator child (aref heights row) t))))))
    (setf (elatex--box-state array) elatex--init)
    array))

(defun elatex--make-array (token parent font left right &optional double)
  "Render TOKEN array in PARENT and optional LEFT, RIGHT braces."
  (let ((array (elatex--make-array-body token parent font)))
    (unless (and (string= left ".") (string= right "."))
      (elatex--add-braces left right array)
      (when double (elatex--add-braces left right array)))
    (elatex--add-scripts (elatex--token-sub token) (elatex--token-super token)
                         array (/= 0 (elatex--token-limits token)) font)))

(defun elatex--combining-identity-p (identity)
  "Return non-nil when IDENTITY is a combining command."
  (and (>= identity elatex--pd-comb-grave)
       (<= identity elatex--pd-comb-vertoverlay)))

(defun elatex--font-identity-p (identity)
  "Return non-nil when IDENTITY selects a scoped font."
  (memq identity
        (list elatex--pd-mathbf elatex--pd-mathbfit elatex--pd-mathcal
              elatex--pd-mathscr elatex--pd-mathfrak elatex--pd-mathbb
              elatex--pd-mathsf elatex--pd-mathsfbf elatex--pd-mathsfit
              elatex--pd-mathsfbfit elatex--pd-mathtt elatex--pd-mathnormal
              elatex--pd-text elatex--pd-bold elatex--pd-rootfont)))

(defun elatex--parse-string-recursive (source parent font)
  "Implement ParseStringRecursive for preprocessed SOURCE."
  (let ((position 0) (legacy-font elatex--f-nofont) (length (length source)))
    (while (< position length)
      (let* ((line (elatex--new-line-child parent))
             (token (elatex--lexer source position legacy-font))
             (identity (elatex--token-p token)))
        (setq legacy-font (elatex--token-f token))
        (cond
         ((= identity elatex--pd-raisebox) (elatex--raise-box token line font))
         ((= identity elatex--pd-frac) (elatex--make-frac token line font))
         ((memq identity (list elatex--pd-pmod elatex--pd-bmod
                               elatex--pd-mod elatex--pd-pod))
          (elatex--make-mod token line font))
         ((= identity elatex--pd-binom) (elatex--make-binom token line font))
         ((= identity elatex--pd-stack) (elatex--make-stack token line font 0))
         ((= identity elatex--pd-sqrt) (elatex--make-sqrt token line font))
         ((memq identity (list elatex--pd-int elatex--pd-iint elatex--pd-iiint
                               elatex--pd-iiiint elatex--pd-idotsint))
          (elatex--make-integral token line
                                 (1+ (- identity elatex--pd-int)) nil font source))
         ((memq identity (list elatex--pd-oint elatex--pd-oiint elatex--pd-oiiint
                               elatex--pd-oiiiint elatex--pd-oidotsint))
          (elatex--make-integral token line
                                 (1+ (- identity elatex--pd-oint)) t font source))
         ((= identity elatex--pd-sum) (elatex--make-large-symbol token line font nil))
         ((= identity elatex--pd-prod) (elatex--make-large-symbol token line font t))
         ((= identity elatex--pd-backslash) (elatex--make-symbol token line font "\\"))
         ((= identity elatex--pd-symbol) (elatex--make-symbol token line font))
         ((= identity elatex--pd-leftright) (elatex--make-left-right token line font))
         ((memq identity (list elatex--pd-big1 elatex--pd-big2
                               elatex--pd-big3 elatex--pd-big4))
          (elatex--make-big-brace token line font))
         ((= identity elatex--pd-array) (elatex--make-array token line font "." "."))
         ((= identity elatex--pd-cases) (elatex--make-array token line font "{" "."))
         ((= identity elatex--pd-pmatrix) (elatex--make-array token line font "(" ")"))
         ((= identity elatex--pd-bmatrix) (elatex--make-array token line font "[" "]"))
         ((= identity elatex--pd-bbmatrix) (elatex--make-array token line font "{" "}"))
         ((= identity elatex--pd-vmatrix) (elatex--make-array token line font "|" "|"))
         ((= identity elatex--pd-vvmatrix) (elatex--make-array token line font "‖" "‖"))
         ((= identity elatex--pd-matrix) (elatex--make-array token line font "." "."))
         ((= identity elatex--pd-box) (elatex--make-dummy-box token line font))
         ((= identity elatex--pd-kern) (elatex--make-kern token line font))
         ((= identity elatex--pd-phantom) (elatex--make-phantom token line font t t))
         ((= identity elatex--pd-vphantom) (elatex--make-phantom token line font t nil))
         ((= identity elatex--pd-hphantom) (elatex--make-phantom token line font nil t))
         ((= identity elatex--pd-block) (elatex--make-block token line font))
         ((elatex--font-identity-p identity) (elatex--make-math-font token line font))
         ((= identity elatex--pd-endline)
          (setf (elatex--box-type line) elatex--b-endline
                (elatex--box-width line) 0 (elatex--box-height line) 0
                (elatex--box-state line) elatex--sizeknown))
         ((= identity elatex--pd-none) (setq position length))
         ((= identity elatex--pd-tspace) (elatex--make-symbol token line font "   "))
         ((= identity elatex--pd-dspace) (elatex--make-symbol token line font "  "))
         ((= identity elatex--pd-space) (elatex--make-symbol token line font " "))
         ((= identity elatex--pd-prime) (elatex--make-prime token line font))
         ((elatex--combining-identity-p identity)
          (elatex--make-combining token line font)))
        (unless (>= position length)
          (let ((next (elatex--token-next token)))
            (if (or (null next) (<= next position))
                (setq position length)
              (setq position next)))))))
  parent)

(defun elatex--parse-string (source line-width font-name)
  "Implement ParseString for SOURCE, LINE-WIDTH, and FONT-NAME."
  (let* ((preprocessed (elatex--preprocessor source))
         (root (elatex--init-box nil elatex--b-line (vector line-width))))
    (setq elatex--root-font (elatex--lookup-font font-name))
    (elatex--parse-string-recursive preprocessed root elatex--root-font)
    root))

(provide 'elatex-parser)
;;; elatex-parser.el ends here
