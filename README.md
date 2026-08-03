# eLaTeX

A self-contained native Emacs Lisp port of [Bart Pieters's libtexprintf](https://github.com/bartp5/libtexprintf), pinned to version 1.31 at revision `18977837b20649d56a651eb6bf846f1c914db77a`.

It renders TeX-like mathematics as Unicode monospace text.  The runtime has no C, Node, WASM, subprocess, FFI, or third-party Elisp dependency.

## Provenance

eLaTeX is a derivative work of `libtexprintf`.  Its Emacs Lisp implementation was produced through an AI-assisted translation of the pinned upstream C source, then checked against its pinned fixtures and compiled oracle.

Upstream URL: <https://github.com/bartp5/libtexprintf>

The project acknowledges and respects Bart Pieters and the `libtexprintf` contributors for the original implementation.

## Requirements

GNU Emacs 29.1 or later.

## Install

Place this repository on `load-path`, then load the public facade:

```elisp
(add-to-list 'load-path "/path/to/elatex")
(require 'elatex)
```

## Render mathematics

`elatex-string` is the direct rendering API:

```elisp
(elatex-string "\\frac{a}{b}")
```

```text
a
─
b
```

Use `elatex-render` for a per-call configuration and structured recoverable errors.  It returns an `elatex-result`; access its fields with `elatex-result-output` and `elatex-result-errors`.

```elisp
(let ((result (elatex-render "\\frac{a}{b}" :style 'unicode)))
  (message "%s" (elatex-result-output result))
  (when-let ((errors (elatex-result-errors result)))
    (message "eLaTeX: %s" (string-join errors "; "))))
```

Per-call options do not mutate persistent configuration.  `:line-width`, `:font`, `:style`, `:wide-character-width`, `:full-width-character-width`, `:map-super-sub`, `:avoid-combining`, `:on-error`, and `:signal-on-error` are supported.  Invalid input or option values signal `elatex-invalid-input` or `elatex-invalid-option`; recoverable parse and layout faults are reported in the result.

Translated compatibility entry points are also available:

- `elatex-sprintf`, `elatex-printf`, and `elatex-fprintf`
- `elatex-list-symbols` and `elatex-symbols-string`
- `elatex-errors`, `elatex-errors-string`, and `elatex-error-state`
- `elatex-box-tree` for inspecting the retained layout tree

Command recognition additionally covers the single-glyph and delimiter names
from the MathJax 4.1.3 TeX `base` and `ams` registries.  This compatibility
layer supplies semantic Unicode equivalents only: MathJax layout macros,
operator sizing, variant glyph forms, and delimiter stretching are outside its
scope.  The translated 2,916-entry symbol-list APIs remain pinned exactly to
`libtexprintf`.

## Live preview mode

`elatex-preview` provides realtime output for `markdown-mode`,
`markdown-ts-mode`, `org-mode`, `latex-mode`, and `latex-ts-mode`.  On
graphical frames, its default `child-frame` backend shows the rendered
mathematics without a box beside the cursor row.  Text terminals, or a failed
child-frame creation, automatically use the terminal-safe rounded Unicode-box
after-string backend below the source line.

```elisp
(require 'elatex-preview)
(elatex-preview-global-mode 1)
```

The preview recognizes `$…$`, `$$…$$`, `\(...\)`, `\[...\]`, configured LaTeX
math environments, Org math fragments, GitHub dollar-backtick inline math, and
Markdown `math` fences.  Every character of a recognized nonempty construct,
including its delimiters and fences, keeps the preview visible.

Use the prior always-in-buffer presentation explicitly:

```elisp
(setq elatex-preview-backend 'after-string)
```

Try the isolated example configuration from the repository root:

```sh
emacs --init-directory "$PWD/example" "$PWD/example/preview.org"
```

Use `preview.md` when `markdown-mode` is available on `load-path`; add `-nw`
to exercise the automatic terminal-safe fallback in a terminal frame.

## Verify

Source smoke test:

```sh
emacs -Q --batch -L . \
  --eval '(progn (require (quote elatex)) (princ (elatex-string "\\frac{a}{b}")))'
```

Byte-compile the package and tests:

```sh
emacs -Q --batch -L . -L test \
  -f batch-byte-compile \
  elatex-data.el elatex-string.el elatex-error.el elatex-box.el \
  elatex-lexer.el elatex-parser.el elatex-draw.el elatex.el \
  elatex-preview.el test/elatex-fixtures.el test/elatex-test.el \
  test/elatex-preview-test.el test/elatex-differential-test.el
```

Run the normal ERT suite:

```sh
emacs -Q --batch -L . -L test \
  -l test/elatex-test.el \
  -l test/elatex-preview-test.el \
  -f ert-run-tests-batch-and-exit
```

The optional differential suite compares the Emacs Lisp renderer against a separately built pinned `libtexprintf` oracle.  Set `ELATEX_ORACLE` to the compiled `utftex` executable, then run:

```sh
LC_ALL=C.UTF-8 LANG=C.UTF-8 ELATEX_ORACLE=/path/to/utftex \
  emacs -Q --batch -L . -L test \
  -l test/elatex-differential-test.el \
  -f ert-run-tests-batch-and-exit
```

## License

`libtexprintf` is licensed under GPL-3.0-or-later.  As its derivative work, eLaTeX is also distributed under GPL-3.0-or-later; see [COPYING](COPYING).
