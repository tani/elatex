# Repository Guidelines

## Project Overview

`elatex` is a self-contained native Emacs Lisp port of Bart Pieters's `libtexprintf` 1.31, pinned to revision `18977837b20649d56a651eb6bf846f1c914db77a`. It renders TeX-like mathematics as Unicode monospace text and exposes recoverable errors, configuration, symbol listings, stream adapters, and a box-tree debugger.

The root package has no runtime C, Node, WASM, subprocess, FFI, or third-party Elisp dependency. Treat `reference/` as an immutable behavioral oracle, not as active root source or packaging configuration.

## Architecture & Data Flow

Load-time dependency order:

1. `elatex-data.el` — stable integer identities, ordered tables, indexes, style templates.
2. `elatex-string.el`, `elatex-error.el` — Unicode measurement/mapping and counted errors.
3. `elatex-box.el`, `elatex-lexer.el` — retained box geometry and TeX preprocessing/tokenization.
4. `elatex-parser.el` — token-to-box composition and layout constructs.
5. `elatex-draw.el` — rasterization and exact box-tree serialization.
6. `elatex.el` — the only public facade.

Render flow:

```text
public input
  -> normalize/validate
  -> dynamically bind fresh errors, widths, font, and copied style
  -> preprocess (symbols, combining marks, greedy over/choose)
  -> lex and recursively parse into a retained box tree
  -> size and absolutely position boxes
  -> rasterize Unicode/ASCII cells
  -> publish output plus ordered counted errors
```

`elatex-string` and translated setters use persistent package configuration. `elatex-render` applies per-call keyword options to copied state and returns an `elatex-result`; nested calls are isolated with dynamic bindings and `unwind-protect`.

The code is synchronous. There is no async framework or dependency-injection container. Configuration is injected through per-call options and dynamically bound variables. Recoverable parser/layout faults use `elatex--add-error` and may retain partial output; Lisp conditions are reserved for invalid API input/options, invariant failures, or explicit `signal-on-error` behavior.

## Key Directories

- `/` — eight runtime Emacs Lisp modules, `COPYING`, and package-level `.gitignore`.
- `test/` — ERT suites, fixture parser, and opt-in C-oracle differential adapter.
- `reference/` — vendored pinned C source, fixtures, scripts, man pages, and upstream metadata.
- `reference/src/` — definitive C behavior and ordered data tables used for source-to-port tracing.
- `reference/test/` — checked-in golden fixture grammar and expected output.
- `.github/workflows/` — active root CI. Ignore nested `reference/.github/workflows/` for root development.

## Development Commands

Run a source smoke test from the repository root:

```sh
emacs -Q --batch -L . \
  --eval '(progn (require (quote elatex)) (princ (elatex-string "\\frac{a}{b}")))'
```

Byte-compile all package and test files:

```sh
emacs -Q --batch -L . -L test \
  -f batch-byte-compile \
  elatex-data.el elatex-string.el elatex-error.el elatex-box.el \
  elatex-lexer.el elatex-parser.el elatex-draw.el elatex.el \
  test/elatex-fixtures.el test/elatex-test.el \
  test/elatex-differential-test.el
```

Run the normal ERT suite:

```sh
emacs -Q --batch -L . -L test \
  -l test/elatex-test.el \
  -f ert-run-tests-batch-and-exit
```

Build and check the pinned C oracle:

```sh
cd reference
LC_ALL=C.UTF-8 LANG=C.UTF-8 sh autogen.sh
LC_ALL=C.UTF-8 LANG=C.UTF-8 ./configure
LC_ALL=C.UTF-8 LANG=C.UTF-8 make -j2
LC_ALL=C.UTF-8 LANG=C.UTF-8 make check
cd ..
```

Run differential parity after a successful oracle build:

```sh
LC_ALL=C.UTF-8 LANG=C.UTF-8 \
ELATEX_ORACLE="$PWD/reference/src/utftex" \
emacs -Q --batch -L . -L test \
  -l test/elatex-differential-test.el \
  -f ert-run-tests-batch-and-exit
```

Use the CI dependency set for the oracle: `build-essential autoconf automake libtool gawk`. Do not patch vendored `configure.ac` to accommodate a local Autotools mismatch; use a compatible toolchain or a temporary out-of-tree workaround.

## Code Conventions & Common Patterns

- Keep `lexical-binding: t`, package metadata, and a matching `(provide 'elatex-...)` in every module.
- Public API names use `elatex-*`; implementation-only symbols use `elatex--*`.
- Follow standard two-space Emacs Lisp indentation. No formatter or linter is configured; byte-compilation is the static check.
- Use existing `cl-defstruct` records and `setf` mutation. Do not introduce parallel plist/alist representations.
- Preserve stable integer identities, vector ordering, exact strings, and first-entry-wins indexes. Ordering affects lookup precedence, error numbering, and symbol serialization.
- Preserve C-compatible arithmetic helpers and pinned Unicode width tables; do not replace them with host display-width behavior.
- Copy mutable style/error vectors for per-call isolation. Restore dynamically published state with `unwind-protect` around callbacks or signaling paths.
- Public formatted wrappers intentionally use native Emacs `format`; `elatex-string` is unformatted.
- Output whitespace, final-newline behavior, UTF-8 byte counts, diagnostic capitalization/order, and debugger indentation are observable contracts.
- Never format or normalize golden fixture files or the large symbol literal in `reference/testtexsymbols.sh`.
- Keep `reference/` unchanged during normal Elisp work. An upstream refresh must be a deliberate pin update with regenerated tables and full differential review.

## Important Files

- `elatex.el` — public entry point, options, conditions, render lifecycle, and `elatex-result`.
- `elatex-data.el` — generated/pinned tables and stable parser, font, delimiter, box, and style identities.
- `elatex-parser.el` — recursive construct dispatch and token-to-box composition.
- `elatex-lexer.el` — preprocessing, command lookup, arguments, scripts, and environments.
- `elatex-box.el` / `elatex-draw.el` — geometry, positioning, raster output, and debugger tree.
- `elatex-error.el` — ordered 38-record counted error model and serializers.
- `test/elatex-test.el` — primary golden and API contract suite.
- `test/elatex-fixtures.el` — strict parser for pinned upstream fixture grammar.
- `test/elatex-differential-test.el` — C/Lisp/golden three-way comparison.
- `.github/workflows/ci.yml` — authoritative supported commands and Emacs matrix.
- `reference/src/{parsedef.h,lexer.c,parser.c,boxes.c,drawbox.c,stringutils.c}` — primary source-mapping oracle.
- `reference/src/{unicodeblocks.h,mapunicode.h,drawchars.h}` — pinned Unicode/style data.
- `reference/{Makefile.am,src/Makefile.am}` — oracle targets, generators, and upstream checks.

## Runtime/Tooling Preferences

- Required runtime: GNU Emacs 29.1 or newer. CI exercises exact 29.1 and 30.2.
- Root dependencies: built-in `cl-lib`, ERT, and standard Emacs libraries only.
- There is no root Makefile, Cask, Eask, package manager, Bun, or Node workflow. Use the explicit batch Emacs commands above.
- Node/Emscripten/WASM files under `reference/` are upstream packaging references, not root dependencies.
- Oracle work requires a C toolchain, Autoconf, Automake, Libtool, GNU awk, Make, and `C.UTF-8`.
- Root `*.elc` files and `reference/` build products are generated and ignored; do not commit them.
- `reference/src/errorflags.h`, `errormessages.h`, and `texprintfsymbols` are generated outputs. Do not hand-edit them.

## Testing & QA

Tests use built-in ERT. `test/elatex-test.el` creates 504 exact golden tests from the three pinned fixture files and adds 15 API/contract tests, for 519 normal tests.

- Golden names: `elatex-golden/<fixture>/bNNN/rNNN/aNNN`.
- Handwritten names: `elatex-<area>/<behavior>`.
- Golden comparisons are byte-sensitive after UTF-8 decoding: preserve code points, whitespace, blank lines, and absence of the CLI-only final LF.
- Stateful tests must dynamically bind or copy persistent style/configuration variables.
- Add tests for observable behavior, boundaries, precedence, state restoration, and real error output—not implementation plumbing.
- The normal ERT suite does not require C. The differential suite skips unless `ELATEX_ORACLE` names an executable, so a skipped run is not parity proof.
- Run the compiled oracle and differential suite for renderer, parser, lexer, layout, table, fixture, symbol, or output-format changes.
- `make check` in `reference/` runs five upstream drivers: equations, main suite, fonts, malformed-error formatting, and full symbol serialization.
- No coverage tool or numeric coverage threshold is configured. Exact golden parity, API regressions, warning-free byte compilation, and both supported Emacs versions are the QA bar.
