#!/usr/bin/env gosh

(use gauche.generator)
(use gauche.process)
(use srfi-13)
(use file.util)

;; Global parameters

;; we build stuff under *builddir*/$ARCH
(define *builddir* "build-ios")

;; the final output destination
(define *outdir* (build-path *builddir* "Gauche-iOS-core.framework"))

;; gauche abi version
(define *abi-version* "0.9")

;; SDK versions
(define *ios-deploy-target-version* "7.1")
;(define *ios-build-sdk-version* "8.3")  ;unused for now

;;
;; Architecture-dependent stuff
;;
(define (devroot target)
  (ecase target
    [(armv7 armv7s arm64)
     "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer"]
    [(i386 x86_64)
     "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer"]))

(define (sdkroot target)
  (ecase target
    [(armv7 armv7s arm64)
     (build-path (devroot target)
                 #`"SDKs/iPhoneOS.sdk")]
    [(i386 x86_64)
     (build-path (devroot target)
                 #`"SDKs/iPhoneSimulator.sdk")]))

(define (environment-alist target)
  (let ([dev (devroot target)]
        [sdkdir (sdkroot target)]
        [cflags-xtra (case target
                       [(i386 x86_64) " -DNO_DYLD_BIND_FULLY_IMAGE"]
                       [else ""])]
        [sdk (case target
               [(i386 x86_64) "iphonesimulator"]
               [else "iphoneos"])])
    `(("CC"       . ,(process-output->string `("xcrun" "-find" "-sdk" ,sdk "clang")))
      ("LD"       . ,(build-path dev "usr/bin/ld"))
      ("AR"       . ,(process-output->string `("xcrun" "-find" "-sdk" ,sdk "ar")))
      ("NM"       . ,(build-path dev "usr/bin/nm"))
      ("RANLIB"   . ,(process-output->string `("xcrun" "-find" "-sdk" ,sdk "ranlib")))
      ("CFLAGS"   . ,#`"-arch ,target -pipe -no-cpp-precomp -isysroot ,sdkdir -miphoneos-version-min=,*ios-deploy-target-version* -I,|sdkdir|/usr/include/ ,cflags-xtra")
      ("LDFLAGS"  . ,#`"-L,|sdkdir|/usr/lib/"))))

(define (build-1 target)
  (let* ([envs (map (^p #`",(car p)=,(cdr p)") (environment-alist target))]
         [build (process-output->string "./config.guess")]
         [builddir (build-path *builddir* (x->string target))]
         [host (ecase target
                 [(armv7)  "arm-apple-darwin7"]
                 [(armv7s) "arm-apple-darwin7s"]
                 [(arm64) "arm-apple-darwin8"]
                 [(i386)   "i386-apple-darwin12.4"]
                 [(x86_64)   "x86_64-apple-darwin12.4"])]
         [configure-cmd `("/usr/bin/env" ,@envs
                          "../../configure"
                          ,#`"--build=,build"
                          ,#`"--host=,host"
                          "--with-dbm=no")]
         [make-cmd      `("/usr/bin/env" ,@envs "make" "-j")]
         [static-cmd    `("/usr/bin/env" ,@envs "make" "static")])
    (define (run1 cmd dir)
      (print cmd)
      (let1 p (run-process cmd :wait #t :directory dir)
        (unless (= (process-exit-status p) 0)
          (error "Process failed:" cmd))))

    (make-directory* builddir)
    (run1 configure-cmd builddir)
    (run1 make-cmd builddir)
    (run1 static-cmd (build-path builddir "src"))))

(define (run-lipo)
  (define (archive target)
    (build-path *builddir* (x->string target) "src"
                #`"libgauche-static-,|*abi-version*|.a"))
  (make-directory* *outdir*)
  (let* ([cmd `("xcrun" "-sdk" "iphoneos" "lipo"
                "-arch" "armv7s" ,(archive 'armv7s)
                "-arch" "armv7" ,(archive 'armv7)
                "-arch" "arm64" ,(archive 'arm64)
                "-arch" "i386" ,(archive 'i386)
                "-arch" "x86_64" ,(archive 'x86_64)
                "-create"
                "-output" ,(build-path *outdir*
                                       #`"libgauche-,|*abi-version*|.a"))]
         [p (run-process cmd :wait #t)])
    (unless (= (process-exit-status p) 0)
      (error "Process failed:" cmd))))

(define (run-all)
  (when (file-exists? *builddir*)
    (remove-directory* *builddir*))
  (build-1 'armv7)
  (build-1 'armv7s)
  (build-1 'arm64)
  (build-1 'i386)
  (build-1 'x86_64)
  (run-lipo))

(define (copy-includes)
  (define (copy file dst)
    (let* ([src1 (build-path "src" file)]
           [src2 (build-path *builddir* "i386/src" file)]
           [src3 (build-path *builddir* "x86_64/src" file)]
           [src (cond [(file-exists? src1) src1]
                      [(file-exists? src2) src2]
                      [(file-exists? src3) src3]
                      [else (error "Missing file:" file)])])
      (print "Copying " src " -> " dst)
      (with-output-to-file dst
        (^[]
          (do-generator [line (file->line-generator src)]
            (print (regexp-replace-all*
                    line
                    #/#include <gc\.h>/ "#include <Gauche-iOS-core/gc.h>"
                    #/#\s*include <(gauche.*)\.h>/ "#include <Gauche-iOS-core/\\1.h>"
                    #/#\s*include \"(gc.*)\.h\"/ "#include \"Gauche-iOS-core/\\1.h\"")))))))
  (make-directory* (build-path *outdir* "Headers/gauche"))
  (dolist [hdr (mfvar-ref "src/Makefile.in" "INSTALL_HEADERS")]
    (copy  hdr (build-path *outdir* "Headers" (sys-basename hdr))))
  (dolist [hdr (mfvar-ref "src/Makefile.in" "INSTALL_SUBHEADERS")]
    (copy hdr (build-path *outdir* "Headers" hdr))))

(define (copy-resources)
  (make-directory* (build-path *outdir* "Resources"))
  (copy-file "src/Info.plist" (build-path *outdir* "Resources/Info.plist")))

(define (wire-stuff)
  (sys-symlink #`"libgauche-,|*abi-version*|.a"
               (build-path *outdir* "Gauche-iOS-core")))

(define (preparation)
  (sys-system "./DIST gen")
  (sys-system "./configure")
  (sys-system "(cd src; make pre-package)")
  (sys-system "make distclean"))

(define (main args)
  (preparation)
  (run-all)
  (copy-includes)
  (copy-resources)
  (wire-stuff)
  0)

;;
;; Utilities
;;
(define read-line/continuation
  (gbuffer-filter (^[v s]
                    (if-let1 m (#/\\$/ v)
                      (values '() (cons (m 'before) s))
                      (values `(,(string-concatenate-reverse (cons v s))) '())))
                  '()
                  read-line
                  (^[s] `(,(string-concatenate-reverse s)))))

(define (mfvar-ref makefile var :optional default)
  (if-let1 line (with-input-from-file makefile
                  (cute generator-find (string->regexp #`"^,|var|\\b")
                        read-line/continuation))
    (remove string-null?
            (string-split
             (rxmatch->string (string->regexp #`"^,|var|\\s*=\\s*")
                              line 'after)
             #[\s+]))
    (if (undefined? default)
      (errorf "Cannot find ~a definition in ~a" var makefile)
      default)))

