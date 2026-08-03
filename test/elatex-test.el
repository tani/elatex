;;; elatex-test.el --- Golden and API tests for elatex  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elatex-fixtures)
(defvar external-debugging-output)

(defconst elatex-test--executions (elatex-fixture-load-all))

(defun elatex-test--fixture-name (execution)
  "Return a unique ERT symbol for fixture EXECUTION."
  (intern
   (format "elatex-golden/%s/b%03d/r%03d/a%03d"
           (file-name-base (elatex-fixture-execution-file execution))
           (elatex-fixture-execution-block-index execution)
           (elatex-fixture-execution-reference-index execution)
           (elatex-fixture-execution-alternative-index execution))))

(dolist (execution elatex-test--executions)
  (let ((case execution))
    (eval
     `(ert-deftest ,(elatex-test--fixture-name execution) ()
        (should (equal (elatex-fixture-render ',case)
                       (elatex-fixture-execution-expected ',case)))))))

(ert-deftest elatex-tables/pinned-counts ()
  (should (= (length elatex--keys) 173))
  (should (= (length elatex--environments) 23))
  (should (= (length elatex--delimiters) 27))
  (should (= (length elatex--combining-commands) 50))
  (should (= (length elatex--symbols) 2916))
  (should (= (length elatex--mathjax-symbols) 54))
  (should (= (length elatex--length-units) 9))
  (should (= (length elatex--combining-ranges) 269))
  (should (= (length elatex--wide-ranges) 17))
  (should (= (length elatex--full-width-ranges) 5))
  (should (= (length elatex--error-records) 38))
  (should (= (length elatex-symbols) 2916)))

(ert-deftest elatex-errors/exact-testerrors-payload ()
  (should (equal (elatex-string "\\frac{\\alp") ""))
  (should (= (elatex-error-state) 1))
  (should (equal (elatex-errors-string)
                 "Premature end of string (1x); Unknown command (1x)"))
  (should (equal (elatex-errors-string) ""))
  (should (= (elatex-error-state) 1))
  (should (equal (elatex-result-errors (elatex-render "\\frac{\\alp"))
                 '("Premature end of string (1x)" "Unknown command (1x)"))))

(ert-deftest elatex-errors/human-output-and-consumption ()
  (elatex-string "\\alp")
  (let ((external-debugging-output (generate-new-buffer " *elatex-errors*")))
    (unwind-protect
        (progn
          (elatex-errors)
          (with-current-buffer external-debugging-output
            (should (equal (buffer-string) "ERROR: Unknown command (1x)\n")))
          (should (= (elatex-error-state) 1))
          (should (equal (elatex-errors-string) "")))
      (kill-buffer external-debugging-output))))

(defun elatex-test--symbols-oracle ()
  "Read the full pinned machine-readable symbol payload."
  (let ((coding-system-for-read 'utf-8-unix)
        (path (expand-file-name "symbols.txt" elatex-fixtures--directory)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(ert-deftest elatex-symbols/full-machine-payload ()
  (should (equal (elatex-symbols-string) (elatex-test--symbols-oracle)))
  (should (string-suffix-p ";" (elatex-symbols-string))))

(ert-deftest elatex-symbols/human-list-preserves-error-state ()
  (elatex-string "\\alp")
  (let ((state (elatex-error-state)) output)
    (setq output (with-output-to-string (elatex-list-symbols)))
    (should (= state (elatex-error-state)))
    (should (string-prefix-p "Symbol: \\_" output))
    (should (string-suffix-p "◌⃒\n" output))))

(ert-deftest elatex-symbols/mathjax-base-and-ams-compatibility ()
  (seq-doseq (record elatex--mathjax-symbols)
    (let ((result (elatex-render (aref record 0))))
      (should (equal (elatex-result-output result)
                     (char-to-string (aref record 1))))
      (should-not (elatex-result-errors result))))
  (should (equal (elatex-string "p \\land q \\lor r") "p ∧q ∨r"))
  (should (equal (elatex-string "\\# \\$ \\% \\& \\_") "# $ % & _"))
  (dolist (input '("\\landscape" "\\lord"))
    (should (equal (elatex-result-errors (elatex-render input))
                   '("Unknown command (1x)"))))
  ;; Existing pinned mappings win over conflicting MathJax mappings.
  (should (equal (elatex-string "\\Box \\varTheta") "□ϴ"))
  (should (= (length elatex-symbols) 2916)))

(defun elatex-test--proof (&rest commands)
  "Wrap concatenated COMMANDS in a prooftree environment."
  (concat "\\begin{prooftree}" (apply #'concat commands)
          "\\end{prooftree}"))

(ert-deftest elatex-bussproofs/centered-inference-arities-and-aliases ()
  (should
   (equal
    (elatex-string
     (elatex-test--proof "\\AxiomC{A}" "\\AxiomC{B}" "\\BinaryInfC{C}"))
    " A   B\n───────\n   C"))
  (should
   (equal
    (elatex-string
     (elatex-test--proof "\\AXC{A}" "\\AXC{B}" "\\AXC{C}" "\\TIC{D}"))
    " A   B   C\n───────────\n     D"))
  (should
   (equal
    (elatex-string
     (elatex-test--proof
      "\\AxiomC{A}" "\\AxiomC{B}" "\\AxiomC{C}" "\\AxiomC{D}"
      "\\QuaternaryInfC{E}"))
    " A   B   C   D\n───────────────\n       E"))
  (should
   (equal
    (elatex-string
     (elatex-test--proof
      "\\AxiomC{A}" "\\AxiomC{B}" "\\AxiomC{C}" "\\AxiomC{D}"
      "\\AxiomC{E}" "\\QuinaryInfC{F}"))
    " A   B   C   D   E\n───────────────────\n         F")))

(ert-deftest elatex-bussproofs/nesting-labels-and-loose-content ()
  (should
   (equal
    (elatex-string
     (elatex-test--proof
      "\\AxiomC{A}" "\\UnaryInfC{LONG}" "\\AxiomC{B}" "\\BinaryInfC{C}"))
    "  A\n──────\n LONG   B\n──────────\n     C"))
  ;; Pending labels survive intervening axioms and apply to the next inference.
  (should
   (equal
    (elatex-string
     (elatex-test--proof
      "\\LL{L}" "\\RL{R}" "\\AxiomC{A}" "\\UnaryInfC{B}"))
    "   A\nL ─── R\n   B"))
  (should
   (equal
    (elatex-string
     (elatex-test--proof "\\AXC{}" "\\RightLabel{H}" "\\UIC{P}"))
    "─── H\n P"))
  ;; Math content and uncombined roots are retained in source order.
  (should
   (equal
    (elatex-string
     (elatex-test--proof "$\\alpha$" "\\AxiomC{A}" "\\AxiomC{B}"))
    "α A  B")))

(ert-deftest elatex-bussproofs/line-styles-and-root-direction ()
  (should
   (equal
    (elatex-string
     (elatex-test--proof "\\AXC{A}" "\\noLine" "\\UIC{B}" "\\UIC{C}"))
    " A\n\n B\n───\n C"))
  (should
   (equal
    (elatex-string
     (elatex-test--proof
      "\\alwaysDashedLine" "\\AXC{A}" "\\UIC{B}" "\\UIC{C}"))
    " A\n┄┄┄\n B\n┄┄┄\n C"))
  ;; A one-shot solid rule resets to the persistent dashed default.
  (should
   (equal
    (elatex-string
     (elatex-test--proof
      "\\alwaysDashedLine" "\\AXC{A}" "\\singleLine" "\\UIC{B}"
      "\\UIC{C}"))
    " A\n───\n B\n┄┄┄\n C"))
  (should
   (equal
    (elatex-result-output
     (elatex-render
      (elatex-test--proof "\\AXC{A}" "\\dashedLine" "\\UIC{B}")
      :style 'ascii))
    " A\n- -\n B"))
  (should
   (equal
    (elatex-string
     (elatex-test--proof
      "\\rootAtTop" "\\AXC{A}" "\\UIC{B}" "\\rootAtBottom" "\\UIC{C}"))
    " B\n───\n A\n───\n C")))

(ert-deftest elatex-bussproofs/original-style-record-remains-compatible ()
  (let ((elatex--unicode-style
         (apply #'record
                (cl-loop for index below 39
                         collect (aref elatex--style-unicode-template index)))))
    (should
     (equal
      (elatex-string (elatex-test--proof "\\AXC{A}" "\\UIC{B}"))
      " A\n───\n B"))))

(ert-deftest elatex-bussproofs/recoverable-errors-and-environment-scope ()
  (let ((outside (elatex-render "\\AxiomC{A}")))
    (should (equal (elatex-result-output outside) ""))
    (should (equal (elatex-result-errors outside) '("Unknown command (1x)"))))
  (let ((underflow
         (elatex-render
          (elatex-test--proof "\\AxiomC{A}" "\\BinaryInfC{B}"))))
    (should (equal (elatex-result-output underflow) " A"))
    (should
     (equal (elatex-result-errors underflow)
            '("Too few mandatory arguments to command (1x)"))))
  (let ((missing (elatex-render (elatex-test--proof "\\AxiomC"))))
    (should (equal (elatex-result-output missing) ""))
    (should
     (equal (elatex-result-errors missing)
            '("Too few mandatory arguments to command (1x)"))))
  (let ((unsupported
         (elatex-render
          "\\begin{prooftree}\\Axiom{A}\\end{prooftree}")))
    (should (equal (elatex-result-output unsupported) ""))
    (should (equal (elatex-result-errors unsupported)
                   '("Unknown command (1x)"))))
  (let ((mismatch
         (elatex-render "\\begin{prooftree}\\AxiomC{A}\\end{matrix}")))
    (should (equal (elatex-result-output mismatch) ""))
    (should
     (equal (elatex-result-errors mismatch)
            '("\\begin does not match closed with \\end (1x)")))))

(ert-deftest elatex-bussproofs/debugger-retains-proof-layout ()
  (let ((tree
         (with-output-to-string
           (elatex-box-tree
            (elatex-test--proof "\\AXC{A}" "\\UIC{B}")))))
    (should (string-match-p "Type: PROOF\n      Nc=3" tree))
    (should (string-match-p "Content: ───" tree))))

(ert-deftest elatex-format/native-emacs-format ()
  (should (equal (elatex-sprintf "\\frac{%s}{%d}" "a" 2) "a\n─\n2")))

(ert-deftest elatex-streams/utf8-byte-counts ()
  (let (printed)
    (let ((output (with-output-to-string
                    (setq printed (elatex-printf "\\alpha")))))
      (should (equal output "α\n"))
      (should (= printed 3))))
  (with-temp-buffer
    (should (= (elatex-fprintf (current-buffer) "\\alpha") 2))
    (should (equal (buffer-string) "α"))))
(ert-deftest elatex-input/empty-nul-and-utf8-bytes ()
  (should (equal (elatex-string "") ""))
  (should (equal (elatex-string "a\0ignored") "a"))
  (should (equal (elatex-string (encode-coding-string "α" 'utf-8)) "α"))
  (should-error (elatex-string (unibyte-string #xff))
                :type 'elatex-invalid-input))

(ert-deftest elatex-input/invalid-options ()
  (dolist (form '((elatex-render 3)
                  (elatex-render "x" :font 3)
                  (elatex-render "x" :line-width 1.5)
                  (elatex-render "x" :style 'terminal)
                  (elatex-render "x" :wide-character-width 3)
                  (elatex-render "x" :full-width-character-width 0)
                  (elatex-render "x" :on-error 4)))
    (should-error (eval form)
                  :type (if (equal form '(elatex-render 3))
                            'elatex-invalid-input
                          'elatex-invalid-option)))
  (should-error (elatex-set-root-font 3) :type 'elatex-invalid-option)
  (let ((elatex-line-width -8))
    (should (equal (elatex-string "abc") "abc"))))

(ert-deftest elatex-configuration/persistent-style-switching ()
  (let ((elatex--unicode-style (copy-elatex--style elatex--style-unicode-template))
        (elatex--ascii-style (copy-elatex--style elatex--style-ascii-template))
        (elatex--selected-style 'unicode)
        (elatex-default-font "text"))
    (should (equal (elatex-string "x_1") "x₁"))
    (elatex-toggle-map-super-sub)
    (should (equal (elatex-string "x_1") "x\n 1"))
    (elatex-set-style-ascii)
    (should (equal (elatex-string "x_1") "x\n 1"))
    (elatex-toggle-map-super-sub)
    (elatex-set-style-unicode)
    (should (equal (elatex-string "x_1") "x\n 1"))
    (elatex-toggle-map-super-sub)
    (should (equal (elatex-string "x_1") "x₁"))))

(ert-deftest elatex-configuration/root-font-and-call-isolation ()
  (let ((elatex-default-font "text")
        (elatex--selected-style 'unicode))
    (should (equal (elatex-result-output
                    (elatex-render "A" :font "mathnormal")) "𝐴"))
    (should (equal elatex-default-font "text"))
    (elatex-set-root-font "unknown-font")
    (should (equal elatex-default-font "unknown"))
    (should (equal (elatex-string "A") "A"))
    (should (equal (elatex-errors-string)
                   "Unknown font type, using text instead (1x)"))))

(ert-deftest elatex-render/callback-nesting-and-restoration ()
  (let (nested callback-errors)
    (let ((outer
           (elatex-render
            "a\\alp" :on-error
            (lambda (errors)
              (setq callback-errors errors
                    nested (elatex-render "\\frac{x}{y}" :style 'ascii))))))
      (should (equal (elatex-result-output outer) "a"))
      (should (equal (elatex-result-errors outer) '("Unknown command (1x)")))
      (should (equal callback-errors '("Unknown command (1x)")))
      (should (equal (elatex-result-output nested) "x\n-\ny"))
      (should (= (elatex-error-state) 1))
      (should (equal (elatex-errors-string) "")))))

(ert-deftest elatex-render/signal-precedes-callback ()
  (let ((called nil) result)
    (condition-case data
        (elatex-render "\\alp" :signal-on-error t
                       :on-error (lambda (_) (setq called t)))
      (elatex-render-error (setq result (cadr data))))
    (should-not called)
    (should (elatex-result-p result))
    (should (equal (elatex-result-errors result) '("Unknown command (1x)")))
    (should (= (elatex-error-state) 1))
    (should (equal (elatex-errors-string) ""))))

(ert-deftest elatex-render/callback-error-restores-outer-state ()
  (should-error
   (elatex-render "\\alp" :on-error (lambda (_) (error "callback failed")))
   :type 'error)
  (should (= (elatex-error-state) 1))
  (should (equal (elatex-errors-string) "")))

(ert-deftest elatex-debugger/exact-small-tree ()
  (let ((expected
         "Box:\nState: 3\nPos:\n  (x,y)=(0,0)\n  (rx,ry)=(0,0)\n  (xc,yc)=(0,0)\n  (X,Y)=(1,1)\n  (w,h)=(1,1)\nType: LINE\n  Nc=1\n  Box:\n  State: 3\n  Pos:\n    (x,y)=(0,0)\n    (rx,ry)=(0,0)\n    (xc,yc)=(0,0)\n    (X,Y)=(1,1)\n    (w,h)=(1,1)\n  Type: LINE\n    Nc=1\n    Box:\n    State: 3\n    Pos:\n      (x,y)=(0,0)\n      (rx,ry)=(0,0)\n      (xc,yc)=(0,0)\n      (X,Y)=(1,1)\n      (w,h)=(1,1)\n    Type: UNIT\n      Content: x\n"))
    (should (equal (with-output-to-string (elatex-box-tree "x")) expected))))

(provide 'elatex-test)
;;; elatex-test.el ends here
