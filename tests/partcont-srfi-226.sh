#!/bin/sh
# Run tests/partcont.scm with SRFI-226 reference impl w/ ChezScheme.
#
# Runs each top-level form of partcont.scm against the SRFI-226 reference
# implementation (srfi-226/lib) on ChezScheme, in a fresh process per form
# (the reference's `run` is single-shot; composable continuations corrupt the
# next `run` in the same process).  Reports tests where the real reference
# diverges from Gauche native (the `expect` values in partcont.scm).
#
# Requires:
#   chezscheme (installed)
#   a built gosh ($top_builddir/src/gosh)
#   srfi-226 ($top_builddir/../srfi-226)
#   $top_srcdir/tests/partcont-srfi-226.scm
#
# Usage (from the tests/ directory):
#   ./partcont-oracle.sh
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

# Copy the reference lib to a scratch dir and precompile (.so) once, so the
# per-form runs are fast and we don't pollute the source tree.
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

# Split partcont.scm into top-level forms (keeps dependent test pairs together,
# since they live in a single let-block = one form).
mkdir -p "$WORK/forms"
$GOSH -e "
(let ((forms (call-with-input-file \"$HERE/partcont.scm\"
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
  cat "$HERE/partcont-srfi-226-prelude.scm" "$f" > "$WORK/run.scm"
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
      (let ((name (~ f 1)))   ; f = (RESULTROW name expect pass? actual)
        (format #t \"~a: ~a~%\" (if (~ f 3) 'ok 'NG) name)
        (unless (hash-table-exists? seen name)
          (hash-table-put! seen name #t) (set! total (+ total 1))
          (unless (list-ref f 3)
            (set! divs (cons (list name (list-ref f 2) (list-ref f 4)) divs)))))
      (loop))))))
(print \"TOTAL \" total \" tests, \" (length divs)
       \" divergence(s) [real SRFI-226 reference != gauche native]:\")
(for-each (lambda (d) (print \"DIVERGE: \" (car d))
            (print \"  native: \" (write-to-string (cadr d)))
            (print \"  ref   : \" (write-to-string (caddr d))))
          (reverse divs))
(exit)"
