;;;
;;; interactive/ed.scm - invoke external editor
;;;
;;;   Copyright (c) 2015-2019  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; This module is autoloaded from gauche.interactive.

(define-module gauche.interactive.ed
  (use gauche.process)
  (use file.util)
  (export ed ed-pick-file))
(select-module gauche.interactive.ed)

;; To remember editor name user typed in
(define *user-entered-editor* #f)

;; API
;; (ed path-or-proc :key editor load-after)
;;   Starts an external editor.  The editor program path is determined
;; in the following order:
;;
;;   - The :editor keyword argument, if it's not #f.
;;   - The value of *editor* in the user module, if defined.
;;   - The value of the environment variable GAUCHE_EDITOR
;;   - The value of the environment variable EDITOR
;;   - Ask user
;;
;; The editor may be called in one of the following way:
;;
;;   EDITOR filename
;;   EDITOR +lineno filename
;;
;; The latter expects to locate the cursor on the specified line in the
;; filename.
;;
;; The file to open is determined by a generic function ed-pick-file.
;; It should return (<filename> <lineno>), or #f to indicate that it
;; couldn't determine the file to edit.
;;
;; NB: Common Lisp's ed can be invoked without argument.  For our usage,
;; though, that feature doesn't seem too useful---it's more likely that
;; repl IS inside an editor so we only need to open a specific file in
;; a buffer.  Emacsclient requires filename, so it further complicates things.
;; For now we just require one argument.

(define (ed path-or-proc :key (editor #f) (load-after 'ask))
  (if-let1 target (ed-pick-file path-or-proc)
    (let* ([filename (car target)]
           [lineno   (cadr target)]
           [sig0     (file-signature filename)])
      (if-let1 e (pick-editor editor)
        (begin (invoke-editor e (car target) (cadr target))
               (when (and (not (equal? (file-signature filename) sig0))
                          (file-exists? filename)
                          (load-after? load-after filename))
                 (load (car target))))
        (print "Can't find an editor.  Aborted.")))
    (format #t "Don't know how to edit ~s.\n" path-or-proc))
  (values))

;; Returns (<size> <mtime>) of a named file, to check if the file is updated
;; by the editor.  Returns #f if filename doesn't exit.
(define (file-signature filename)
  (and (file-exists? filename)
       (list (file-size filename :follow-link? #t)
             (file-mtime filename :follow-link? #t))))

(define-generic ed-pick-file)
(define-method ed-pick-file ((fn <string>)) (list fn 1))
(define-method ed-pick-file ((fn <procedure>))
  (let1 loc (source-location fn)
    ;; If the recorded file does not exist, it is either (1) removed
    ;; after loaded, (2) cwd has been changed, or (3) it's a pseudo filename
    ;; such as "(standard input)".  In either case, opening such file isn't
    ;; much useful since that won't contain the definition.  So we ignore it.
    (and (file-exists? (car loc)) loc)))
(define-method ed-pick-file ((fn <top>)) #f)

;; internal
;; NB: If specified editor isn't actually name an executable, we let
;; run-process to throw an error rather than doing some clever things.
(define (pick-editor editor)
  (or (and (string? editor) editor)
      (global-variable-ref (find-module 'user) '*editor* #f)
      (sys-getenv "GAUCHE_EDITOR")
      (sys-getenv "EDITOR")
      (begin
        (if *user-entered-editor*
          (format #t "Editor name [~a]: " *user-entered-editor*)
          (format #t "Editor name (or just return to abort): "))
        (flush)
        (let1 ans (read-line)
          (if (#/^\s*$/ ans)
            *user-entered-editor*
            (begin (set! *user-entered-editor* ans) ans))))))

;; internal
;; Determine whether we should reload the file if it's updated.
(define (load-after? load-after name)
  (ecase load-after
    [(#t) #t]
    [(#f) #f]
    [(ask)
     (let loop ()
       (format #t "Reload ~s? [y/N]: " name)
       (flush)
       (rxmatch-case (read-line)
         [#/^y/i () #t]
         [#/^n/i () #f]
         [else (loop)]))]))

;; internal
;; Invoke external editor; won't return until the editor exits.
(define (invoke-editor editor filename lineno)
  (run-process `(,editor ,#"+~lineno" ,filename) :wait #t))

