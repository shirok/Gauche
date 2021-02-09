
(use gauche.test)
(use gauche.config)
(use gauche.ffitest)

(cond-expand
 [gauche.windows (exit 0)]
 [else
  (unless (#/^x86_64-/ (gauche-config "--arch"))
    (exit 0))])

(test-start "ffitest")






(test-end)




