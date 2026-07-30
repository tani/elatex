;;; init.el --- Minimal eLaTeX preview example  -*- lexical-binding: t; -*-

;; Start from the repository root with either:
;;
;;   emacs --init-directory "$PWD/example" "$PWD/example/preview.org"
;;   emacs --init-directory "$PWD/example" "$PWD/example/preview.md"
;; Add `-nw' to either command to run the same inline previews in a terminal.
;;
;; The Markdown example requires markdown-mode to be available on `load-path'.

;;; Code:

(add-to-list 'load-path (expand-file-name ".." user-emacs-directory))

(require 'elatex-preview)

(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(elatex-preview-global-mode 1)

;;; init.el ends here
