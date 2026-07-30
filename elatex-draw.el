;;; elatex-draw.el --- Draw retained elatex box trees  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tex, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Translation of reference/src/drawbox.c from libtexprintf 1.31.

;;; Code:

(require 'cl-lib)
(require 'elatex-box)
(require 'elatex-string)

(defun elatex--remove-line-trailing-whitespace (string)
  "Implement RemoveLineTrailingWhitespace on STRING."
  (mapconcat (lambda (line) (replace-regexp-in-string " +\\'" "" line))
             (split-string string "\n" nil) "\n"))

(defun elatex--draw-box (box)
  "Implement DrawBox and return BOX's rendered string."
  (if (/= (elatex--box-state box) elatex--absposknown)
      (progn (elatex--add-error elatex--errabsposunknown) "")
    (if (or (/= (elatex--box-ax box) 0) (/= (elatex--box-ay box) 0))
        (progn (elatex--add-error elatex--errdrawboxnoroot) "")
      (let (rows)
        (cl-loop for y downfrom (1- (elatex--box-height box)) to 0 do
                 (let ((x 0) pieces)
                   (while (< x (elatex--box-width box))
                     (let ((unit (elatex--find-box-at-pos box x y)))
                       (cond
                        ((and unit (= (elatex--box-ax unit) x))
                         (push (elatex--unicode-mapper
                                (elatex--box-content unit)) pieces))
                        ((null unit) (push " " pieces))))
                     (setq x (1+ x)))
                   (push (replace-regexp-in-string
                          " +\\'" "" (apply #'concat (nreverse pieces))) rows)))
        (mapconcat #'identity (nreverse rows) "\n")))))

(defun elatex--print-box (box)
  "Implement PrintBox for BOX and return emitted character count."
  (let ((rendered (elatex--draw-box box)))
    (princ rendered)
    (princ "\n")
    (1+ (length rendered))))

(defun elatex--box-tree-lines (box indent)
  "Return DrawBoxTreeRec lines for BOX at INDENT."
  (let* ((padding (make-string indent ?\s))
         (detail (make-string (+ indent 2) ?\s))
         (state (elatex--box-state box))
         (lines (list (concat padding "Box:")
                      (format "%sState: %d" padding state)
                      (concat padding "Pos:")
                      (if (= state elatex--absposknown)
                          (format "%s(x,y)=(%d,%d)" detail
                                  (elatex--box-ax box) (elatex--box-ay box))
                        (concat detail "(x,y)=(?,?)"))
                      (if (>= state elatex--relposknown)
                          (format "%s(rx,ry)=(%d,%d)" detail
                                  (elatex--box-rx box) (elatex--box-ry box))
                        (concat detail "(rx,ry)=(?,?)")))))
    (setq lines
          (append lines
                  (if (>= state elatex--sizeknown)
                      (list (format "%s(xc,yc)=(%d,%d)" detail
                                    (elatex--box-x-center box)
                                    (elatex--box-y-center box))
                            (format "%s(X,Y)=(%d,%d)" detail
                                    (elatex--box-x-align box)
                                    (elatex--box-y-align box))
                            (format "%s(w,h)=(%d,%d)" detail
                                    (elatex--box-width box)
                                    (elatex--box-height box)))
                    (list (concat detail "(xc,yc)=(?,?)")
                          (concat detail "(X,Y)=(?,?)")
                          (concat detail "(w,h)=(?,?)")))))
    (let ((type (elatex--box-type box)))
      (setq lines
            (append lines
                    (cond
                     ((= type elatex--b-unit)
                      (list (concat padding "Type: UNIT")
                            (concat detail "Content: "
                                    (elatex--unicode-mapper
                                     (elatex--box-content box)))))
                     ((= type elatex--b-array)
                      (list (concat padding "Type: ARRAY")
                            (format "%sNc=%d" detail
                                    (elatex--box-child-count box))))
                     ((= type elatex--b-pos)
                      (list (concat padding "Type: POS")
                            (format "%sNc=%d" detail
                                    (elatex--box-child-count box))))
                     ((= type elatex--b-dummy)
                      (list (concat padding "Type: DUMMY")))
                     ((= type elatex--b-line)
                      (list (concat padding "Type: LINE")
                            (format "%sNc=%d" detail
                                    (elatex--box-child-count box))))
                     ((= type elatex--b-endline)
                      (list (concat padding "Type: ENDLINE")))
                     (t nil))))
      (when (memq type (list elatex--b-array elatex--b-pos elatex--b-line))
        (dotimes (index (elatex--box-child-count box))
          (setq lines
                (append lines
                        (elatex--box-tree-lines
                         (elatex--box-child box index) (+ indent 2)))))))
    lines))

(defun elatex--draw-box-tree-string (box)
  "Return DrawBoxTree output for BOX as a string."
  (concat (mapconcat #'identity (elatex--box-tree-lines box 0) "\n") "\n"))

(defun elatex--draw-box-tree (box)
  "Implement DrawBoxTree for BOX and return emitted text."
  (let ((text (elatex--draw-box-tree-string box)))
    (princ text)
    text))

(provide 'elatex-draw)
;;; elatex-draw.el ends here
