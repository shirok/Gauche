#!/bin/sh
# Run tests/control-flow.scm against the SRFI-226 reference impl on
# ChezScheme.
#
# Runs each top-level form of control-flow.scm against the reference
# (srfi-226/lib), in a fresh Chez process per form (the reference's
# `run` is single-shot; composable continuations corrupt the next
# `run` in the same process).  Reports tests where the reference
# diverges from Gauche native (the `expect` values in
# control-flow.scm).
#
# Requires:
#   chezscheme (installed)
#   a built gosh ($top_builddir/src/gosh)
#   srfi-226 reference at $top_builddir/../srfi-226
#   tests/control-flow.scm
#
# Usage (from the tests/ directory):
#   ./control-flow-srfi-226.sh
# Env overrides: GOSH, CHEZ, SRFI226_LIB
set -e

HERE=$(cd "$(dirname "$0")" && pwd)
GOSH=${GOSH:-"$HERE/../src/gosh -ftest"}
CHEZ=${CHEZ:-chezscheme}
SRFI226_LIB=${SRFI226_LIB:-"$HERE/../../srfi-226/lib"}

command -v "$CHEZ" >/dev/null 2>&1 || { echo "ERROR: chezscheme not found (set CHEZ=...)"; exit 1; }
[ -x "$GOSH" ] || command -v $GOSH >/dev/null 2>&1 || { echo "ERROR: gosh not found (set GOSH=...)"; exit 1; }

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# Copy the reference lib to a scratch dir and precompile (.so) once.
LIB="$WORK/lib"
cp -r "$SRFI226_LIB" "$LIB"
cat > "$WORK/precomp.scm" <<'EOF'
(import (chezscheme))
(compile-imported-libraries #t)
(eval '(import (control-features)) (environment '(chezscheme)))
(exit)
EOF
echo "Precompiling reference library (first run only, ~minutes)..."
"$CHEZ" --libdirs "$LIB:." --compile-imported-libraries "$WORK/precomp.scm" >/dev/null 2>&1 || true

# Split control-flow.scm into top-level forms.
mkdir -p "$WORK/forms"
$GOSH -e "
(let ((forms (call-with-input-file \"$HERE/control-flow.scm\"
               (lambda (p) (let loop ((a '())) (let ((f (read p)))
                 (if (eof-object? f) (reverse a) (loop (cons f a)))))))))
  (let loop ((fs forms) (i 0))
    (unless (null? fs)
      (call-with-output-file (format \"$WORK/forms/~3,'0d.scm\" i)
        (lambda (o) (write (car fs) o) (newline o)))
      (loop (cdr fs) (+ i 1)))))
(exit)"

# Run each form in a fresh Chez process; collect RESULTROW lines.
RAW="$WORK/raw.txt"
: > "$RAW"
for f in "$WORK"/forms/*.scm; do
  cat "$HERE/control-flow-srfi-226-prelude.scm" "$f" > "$WORK/run.scm"
  echo "(exit)" >> "$WORK/run.scm"
  "$CHEZ" --libdirs "$LIB:." "$WORK/run.scm" 2>&1 | grep "RESULTROW" >> "$RAW" || true
done

# Aggregate: first emission per test name wins; report divergences.
$GOSH -e "
(define seen (make-hash-table 'string=?))
(define divs '()) (define total 0)
(call-with-input-file \"$RAW\"
  (lambda (p) (let loop () (let ((f (read p)))
    (unless (eof-object? f)
      (let ((name (list-ref f 1)))   ; f = (RESULTROW name expect pass? actual)
        (unless (hash-table-exists? seen name)
          (hash-table-put! seen name #t) (set! total (+ total 1))
          (unless (list-ref f 3)
            (set! divs (cons (list name (list-ref f 2) (list-ref f 4)) divs)))))
      (loop))))))
(print \"TOTAL \" total \" tests, \" (length divs)
       \" divergence(s) [real SRFI-226 reference != gauche native]:\")
(for-each (lambda (d) (print \"DIVERGE: \" (car d))
            (print \"  native: \" (cadr d))
            (print \"  ref   : \" (caddr d)))
          (reverse divs))
(exit)"
