;;
;; a short test program to measure port-I/O primitive performance
;;

(use gauche.time)

(time (with-input-from-file "/usr/share/dict/words"
        (lambda ()
          (with-output-to-file "/dev/null"
            (lambda ()
              (generator-for-each write-byte read-byte))))))

      
