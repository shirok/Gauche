;; -*- mode: scheme -*-
;; "axTLS/ssl/test/ssltest.c" modification script
;;
;; "axTLS/ssl/test/ssltest.c" heavily uses system(3) to run background process
;; to communicate, but it doesn't always work on some platforms (e.g.
;; OSX doesn't like calling system(3) from multiple threads).
;; So we provide an alternative.
;;
;; During the build, we preprocess ssltest.c to generate ssltest.mod.c,
;; and our alternative system(3) implementation is embedded in the latter.
;;
;; Usage:
;;  Read input data from stdin, write modified data to stdout.
;;  $ gosh ssltest-mod.scm SRCDIR BUILDDIR < ssltest.c > ssltest.mod.c

(use file.util)
(use file.filter)
(use util.match)

(define (p . args) (for-each print args))

(define (usage)
  (p #"Usage: ,*program-name* $srcdir $builddir < ssltest.c > ssltest.mod.c"
     "  Transforms axTLS's ssltest.c to the suitable form."
     "  Give absolute pathname of $srcdir and $builddir.")
  (exit 1))

(define (main args)
  (match (cdr args)
    [(srcdir builddir) (do-translate srcdir builddir)]
    [_ (usage)]))

(define (do-translate srcdir builddir)
  (cond-expand
    [gauche.os.windows
     (define srcpath (regexp-replace-all #/\\/ (build-path srcdir   "axTLS/ssl")       "/"))
     (define kicker  (regexp-replace-all #/\\/ (build-path builddir "kick_openssl.sh") "/"))
     (define srcpath-replace #"~|srcpath|/")
     (define kicker-replace  #"sh ~kicker ")
     ]
    [else
     (define srcpath (build-path srcdir   "axTLS/ssl"))
     (define kicker  (build-path builddir "kick_openssl.sh"))
     (define srcpath-replace #"~|srcpath|/")
     (define kicker-replace  #"~kicker ")
     ])

  (p "/* This is generated file. Don't edit! */"
     "static int safe_system(const char *);")

  (file-filter-for-each
   (^[line seed]
     ($ format #t "~a\n" $ regexp-replace-all* line
        #/\.\.\/ssl\// srcpath-replace
        #/openssl /    kicker-replace
        #/system/      "safe_system")))

  (p "#include <errno.h>"
     "int safe_system(const char *commands)"
     "{"
     "#if !defined(WIN32)"
     "  pid_t pid;"
     "  fprintf(stdout, \"system: executing {%s}\\n\", commands);"
     "  if ((pid = fork()) == 0) {"
     "    execlp(\"sh\", \"sh\", \"-c\", commands, NULL);"
     "    fprintf(stdout, \"system: couldn't invoke sh: %s\\n\", strerror(errno));"
     "    exit(1);"
     "  } else {"
     "    int status;"
     "    if (waitpid(pid, &status, 0) < 0) {"
     "      fprintf(stdout, \"waitpid failed on pid %d (%s)\\n\", pid,"
     "              strerror(errno));"
     "      return -1;"
     "    }"
     "    if (status != 0) {"
     "       fprintf(stdout, \"process exit with %d (command: %s)\\n\","
     "               status, commands);"
     "       return -1;"
     "    }"
     "    return 0;"
     "  }"
     "#else  /*WIN32*/"
     "  fprintf(stdout, \"system: executing (%s)\\n\", commands);"
     "  /* We know system() works on MinGW.  Just pretend that we honor the"
     "     return value of system() so that the compiler won't complain.  */"
     "  if (system(commands)) do {} while (0);"
     "  /* This is needed to give time for kick_openssl to invoke openssl. */"
     "  Sleep(200);"
     "  return 0;"
     "#endif /*WIN32*/"
     "}")
  0)
