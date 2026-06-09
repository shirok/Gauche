;;
;; Package ffi-libuuid
;;

(define-gauche-package "ffi-libuuid"
  ;; Repository URL, e.g. github
  ;;  This URL uniquely identifies the package.
  :repository "http://github.com/shirok/Gauche"

  ;;
  :version "0.1"

  ;; Description of the package.  The first line is used as a short
  ;; summary.
  :description "Sample library using FFI"

  ;; List of dependencies.
  ;; Example:
  ;;     :require (("Gauche" (>= "0.9.5"))  ; requires Gauche 0.9.5 or later
  ;;               ("Gauche-gl" "0.6"))     ; and Gauche-gl 0.6
  :require (("Gauche" (>= "0.9.16_pre3")))

  ;; List of providing modules
  ;; NB: This will be recognized >= Gauche 0.9.7.
  ;; Example:
  ;;      :providing-modules (util.algorithm1 util.algorithm1.option)
  :providing-modules (ffilibuuid)

  ;; List name and contact info of authors.
  ;; e.g. ("Eva Lu Ator <eval@example.com>"
  ;;       "Alyssa P. Hacker <lisper@example.com>")
  :authors ("Shiro Kawai <shiro@acm.org>")

  ;; List name and contact info of package maintainers, if they differ
  ;; from authors.
  ;; e.g. ("Cy D. Fect <c@example.com>")
  :maintainers ()

  ;; List licenses
  ;; e.g. ("BSD")
  :licenses ("BSD")

  ;; Homepage URL, if any.
  ; :homepage "http://example.com/libuuid/"
  )
