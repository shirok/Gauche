;; -*- coding: utf-8 -*-

;; This is a test of windows console code page support function.
;; This can't be automated easily, so it's not run by make check.

(print "日本語を入力してみてください。")
(flush)
(print "「" (read-line) "」が入力されました。")
(print "[Enter]キーで終了します。")
(flush)
(read-line)
