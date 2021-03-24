;;
;; Parse Gauche source
;;

;; Run ./gosh -ftest ../test/lang-c-tester.scm under src

(use gauche.time)
(use lang.c.parser)
(cpp-include-paths '("." "../gc/include" "../gc/libatomic_ops/src"))

(define *sources*
  '(autoloads.c
    bignum.c
    bits.c
    boolean.c
    box.c
    builtin-syms.c
    char.c
    class.c
    code.c
    collection.c
    compare.c
    compaux.c
    compile.c
    connection.c
    core.c
    dispatch.c
    error.c
    execenv.c
    gauche-config.c
    gloc.c
    hash.c
    lazy.c
    libalpha.c
    libbool.c
    libchar.c
    libcmp.c
    libcode.c
    libdict.c
    libeval.c
    libexc.c
    libextra.c
    libfmt.c
    libio.c
    liblazy.c
    liblist.c
    libmacbase.c
    libmacro.c
    libmisc.c
    libmod.c
    libnative.c
    libnum.c
    libobj.c
    libomega.c
    libparam.c
    libproc.c
    librx.c
    libsrfis.c
    libstr.c
    libsym.c
    libsys.c
    libvec.c
    list.c
    load.c
    macro.c
    main.c
    module.c
    native.c
    number.c
    parameter.c
    paths.c
    port.c
    proc.c
    prof.c
    read.c
    regexp.c
    repl.c
    serial.c
    signal.c
    staticinit.c
    string.c
    symbol.c
    system.c
    test-arith.c
    test-extra.c
    test-vmstack.c
    treemap.c
    vector.c
    vm.c
    vmstat.c
    weak.c
    win-compat.c
    winmain.c
    write.c
    ))

(define (do-parse file)
  (print "parsing " file " ...")
  (print (time-this 1 (^[] (c-parse-file file)))))

(define (main args)
  (for-each do-parse *sources*)
  0)
