;;
;; Package @@package@@
;;

(define-gauche-package "@@package@@"
  ;; Repository URL, e.g. github
  ;;  This URL uniquely identifies the package.
  :repository "http://example.com/@@package@@.git"

  ;; Current version number string.
  :version "0.1"

  ;; Stable version number, if it differs from 'version' above.
  ;; If the current version is being actively developed and not suitable
  ;; for the general users, you can specify the recommended stable version
  ;; string (e.g. "1.0") here.  If this is not #f, it is shown in the
  ;; Gauche extension package page.
  :stable-version #f

  ;; Description of the package.  The first line is used as a short
  ;; summary.
  :description "Sample package.scm\n\
                Write your package description here."

  ;; List of dependencies.
  ;; Example:
  ;;     :require (("Gauche" (>= "0.9.5"))  ; requires Gauche 0.9.5 or later
  ;;               ("Gauche-gl" "0.6"))     ; and Gauche-gl 0.6
  :require (("Gauche" (>= "@@gauche-version@@")))

  ;; List of providing modules
  ;; NB: This will be recognized >= Gauche 0.9.7.
  ;; Example:
  ;;      :providing-modules (util.algorithm1 util.algorithm1.option)
  :providing-modules (@@modname@@)

  ;; List name and contact info of authors.
  ;; e.g. ("Eva Lu Ator <eval@example.com>"
  ;;       "Alyssa P. Hacker <lisper@example.com>")
  :authors (@@author@@)

  ;; List name and contact info of package maintainers, if they differ
  ;; from authors.
  ;; e.g. ("Cy D. Fect <c@example.com>")
  :maintainers ()

  ;; List licenses
  ;; e.g. ("BSD")
  :licenses (@@license@@)

  ;; Homepage URL, if any.
  ; :homepage "http://example.com/@@package@@/"
  )
