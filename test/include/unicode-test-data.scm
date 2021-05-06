;; -*- coding:utf-8 -*-
;; Generated from Unicode 13.0.0 test data files

;; auxiliary/GraphemeBreakTest.txt
(define *grapheme-break-tests* '(
(0 (#t 32 #t 32 #t) "÷ [0.2] SPACE (Other) ÷ [999.0] SPACE (Other) ÷ [0.3]")
(1 (#t 32 #f 776 #t 32 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(2 (#t 32 #t 13 #t)
 "÷ [0.2] SPACE (Other) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(3 (#t 32 #f 776 #t 13 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(4 (#t 32 #t 10 #t)
 "÷ [0.2] SPACE (Other) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(5 (#t 32 #f 776 #t 10 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(6 (#t 32 #t 1 #t)
 "÷ [0.2] SPACE (Other) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]")
(7 (#t 32 #f 776 #t 1 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(8 (#t 32 #f 847 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]")
(9 (#t 32 #f 776 #f 847 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(10 (#t 32 #t 127462 #t)
 "÷ [0.2] SPACE (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(11 (#t 32 #f 776 #t 127462 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(12 (#t 32 #t 1536 #t)
 "÷ [0.2] SPACE (Other) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]")
(13 (#t 32 #f 776 #t 1536 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(14 (#t 32 #f 2307 #t)
 "÷ [0.2] SPACE (Other) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]")
(15 (#t 32 #f 776 #f 2307 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(16 (#t 32 #t 4352 #t)
 "÷ [0.2] SPACE (Other) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]")
(17 (#t 32 #f 776 #t 4352 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(18 (#t 32 #t 4448 #t)
 "÷ [0.2] SPACE (Other) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]")
(19 (#t 32 #f 776 #t 4448 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(20 (#t 32 #t 4520 #t)
 "÷ [0.2] SPACE (Other) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]")
(21 (#t 32 #f 776 #t 4520 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(22 (#t 32 #t 44032 #t)
 "÷ [0.2] SPACE (Other) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]")
(23 (#t 32 #f 776 #t 44032 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(24 (#t 32 #t 44033 #t)
 "÷ [0.2] SPACE (Other) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]")
(25 (#t 32 #f 776 #t 44033 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(26 (#t 32 #t 8986 #t)
 "÷ [0.2] SPACE (Other) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(27 (#t 32 #f 776 #t 8986 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(28 (#t 32 #f 768 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(29 (#t 32 #f 776 #f 768 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(30 (#t 32 #f 8205 #t)
 "÷ [0.2] SPACE (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]")
(31 (#t 32 #f 776 #f 8205 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(32 (#t 32 #t 888 #t)
 "÷ [0.2] SPACE (Other) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]")
(33 (#t 32 #f 776 #t 888 #t)
 "÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(34 (#t 13 #t 32 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] SPACE (Other) ÷ [0.3]")
(35 (#t 13 #t 776 #t 32 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(36 (#t 13 #t 13 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(37 (#t 13 #t 776 #t 13 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(38 (#t 13 #f 10 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) × [3.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(39 (#t 13 #t 776 #t 10 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(40 (#t 13 #t 1 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(41 (#t 13 #t 776 #t 1 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(42 (#t 13 #t 847 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(43 (#t 13 #t 776 #f 847 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(44 (#t 13 #t 127462 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(45 (#t 13 #t 776 #t 127462 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(46 (#t 13 #t 1536 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(47 (#t 13 #t 776 #t 1536 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(48 (#t 13 #t 2307 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(49 (#t 13 #t 776 #f 2307 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(50 (#t 13 #t 4352 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(51 (#t 13 #t 776 #t 4352 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(52 (#t 13 #t 4448 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(53 (#t 13 #t 776 #t 4448 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(54 (#t 13 #t 4520 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(55 (#t 13 #t 776 #t 4520 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(56 (#t 13 #t 44032 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]")
(57 (#t 13 #t 776 #t 44032 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(58 (#t 13 #t 44033 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(59 (#t 13 #t 776 #t 44033 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(60 (#t 13 #t 8986 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] WATCH (ExtPict) ÷ [0.3]")
(61 (#t 13 #t 776 #t 8986 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(62 (#t 13 #t 768 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(63 (#t 13 #t 776 #f 768 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(64 (#t 13 #t 8205 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(65 (#t 13 #t 776 #f 8205 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(66 (#t 13 #t 888 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] <reserved-0378> (Other) ÷ [0.3]")
(67 (#t 13 #t 776 #t 888 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(68 (#t 10 #t 32 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] SPACE (Other) ÷ [0.3]")
(69 (#t 10 #t 776 #t 32 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(70 (#t 10 #t 13 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(71 (#t 10 #t 776 #t 13 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(72 (#t 10 #t 10 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(73 (#t 10 #t 776 #t 10 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(74 (#t 10 #t 1 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <START OF HEADING> (Control) ÷ [0.3]")
(75 (#t 10 #t 776 #t 1 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(76 (#t 10 #t 847 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(77 (#t 10 #t 776 #f 847 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(78 (#t 10 #t 127462 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(79 (#t 10 #t 776 #t 127462 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(80 (#t 10 #t 1536 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]")
(81 (#t 10 #t 776 #t 1536 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(82 (#t 10 #t 2307 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(83 (#t 10 #t 776 #f 2307 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(84 (#t 10 #t 4352 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]")
(85 (#t 10 #t 776 #t 4352 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(86 (#t 10 #t 4448 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]")
(87 (#t 10 #t 776 #t 4448 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(88 (#t 10 #t 4520 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]")
(89 (#t 10 #t 776 #t 4520 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(90 (#t 10 #t 44032 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]")
(91 (#t 10 #t 776 #t 44032 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(92 (#t 10 #t 44033 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]")
(93 (#t 10 #t 776 #t 44033 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(94 (#t 10 #t 8986 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] WATCH (ExtPict) ÷ [0.3]")
(95 (#t 10 #t 776 #t 8986 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(96 (#t 10 #t 768 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(97 (#t 10 #t 776 #f 768 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(98 (#t 10 #t 8205 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(99 (#t 10 #t 776 #f 8205 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(100 (#t 10 #t 888 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <reserved-0378> (Other) ÷ [0.3]")
(101 (#t 10 #t 776 #t 888 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(102 (#t 1 #t 32 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] SPACE (Other) ÷ [0.3]")
(103 (#t 1 #t 776 #t 32 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(104 (#t 1 #t 13 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(105 (#t 1 #t 776 #t 13 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(106 (#t 1 #t 10 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(107 (#t 1 #t 776 #t 10 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(108 (#t 1 #t 1 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(109 (#t 1 #t 776 #t 1 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(110 (#t 1 #t 847 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(111 (#t 1 #t 776 #f 847 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(112 (#t 1 #t 127462 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(113 (#t 1 #t 776 #t 127462 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(114 (#t 1 #t 1536 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(115 (#t 1 #t 776 #t 1536 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(116 (#t 1 #t 2307 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(117 (#t 1 #t 776 #f 2307 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(118 (#t 1 #t 4352 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(119 (#t 1 #t 776 #t 4352 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(120 (#t 1 #t 4448 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(121 (#t 1 #t 776 #t 4448 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(122 (#t 1 #t 4520 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(123 (#t 1 #t 776 #t 4520 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(124 (#t 1 #t 44032 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]")
(125 (#t 1 #t 776 #t 44032 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(126 (#t 1 #t 44033 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(127 (#t 1 #t 776 #t 44033 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(128 (#t 1 #t 8986 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] WATCH (ExtPict) ÷ [0.3]")
(129 (#t 1 #t 776 #t 8986 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(130 (#t 1 #t 768 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(131 (#t 1 #t 776 #f 768 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(132 (#t 1 #t 8205 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(133 (#t 1 #t 776 #f 8205 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(134 (#t 1 #t 888 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] <reserved-0378> (Other) ÷ [0.3]")
(135 (#t 1 #t 776 #t 888 #t)
 "÷ [0.2] <START OF HEADING> (Control) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(136 (#t 847 #t 32 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] SPACE (Other) ÷ [0.3]")
(137 (#t 847 #f 776 #t 32 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(138 (#t 847 #t 13 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(139 (#t 847 #f 776 #t 13 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(140 (#t 847 #t 10 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(141 (#t 847 #f 776 #t 10 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(142 (#t 847 #t 1 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(143 (#t 847 #f 776 #t 1 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(144 (#t 847 #f 847 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(145 (#t 847 #f 776 #f 847 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(146 (#t 847 #t 127462 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(147 (#t 847 #f 776 #t 127462 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(148 (#t 847 #t 1536 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(149 (#t 847 #f 776 #t 1536 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(150 (#t 847 #f 2307 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(151 (#t 847 #f 776 #f 2307 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(152 (#t 847 #t 4352 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(153 (#t 847 #f 776 #t 4352 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(154 (#t 847 #t 4448 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(155 (#t 847 #f 776 #t 4448 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(156 (#t 847 #t 4520 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(157 (#t 847 #f 776 #t 4520 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(158 (#t 847 #t 44032 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(159 (#t 847 #f 776 #t 44032 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(160 (#t 847 #t 44033 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(161 (#t 847 #f 776 #t 44033 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(162 (#t 847 #t 8986 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(163 (#t 847 #f 776 #t 8986 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(164 (#t 847 #f 768 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(165 (#t 847 #f 776 #f 768 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(166 (#t 847 #f 8205 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(167 (#t 847 #f 776 #f 8205 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(168 (#t 847 #t 888 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(169 (#t 847 #f 776 #t 888 #t)
 "÷ [0.2] COMBINING GRAPHEME JOINER (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(170 (#t 127462 #t 32 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(171 (#t 127462 #f 776 #t 32 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(172 (#t 127462 #t 13 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(173 (#t 127462 #f 776 #t 13 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(174 (#t 127462 #t 10 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(175 (#t 127462 #f 776 #t 10 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(176 (#t 127462 #t 1 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(177 (#t 127462 #f 776 #t 1 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(178 (#t 127462 #f 847 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(179 (#t 127462 #f 776 #f 847 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(180 (#t 127462 #f 127462 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [12.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(181 (#t 127462 #f 776 #t 127462 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(182 (#t 127462 #t 1536 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(183 (#t 127462 #f 776 #t 1536 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(184 (#t 127462 #f 2307 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(185 (#t 127462 #f 776 #f 2307 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(186 (#t 127462 #t 4352 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(187 (#t 127462 #f 776 #t 4352 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(188 (#t 127462 #t 4448 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(189 (#t 127462 #f 776 #t 4448 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(190 (#t 127462 #t 4520 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(191 (#t 127462 #f 776 #t 4520 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(192 (#t 127462 #t 44032 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(193 (#t 127462 #f 776 #t 44032 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(194 (#t 127462 #t 44033 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(195 (#t 127462 #f 776 #t 44033 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(196 (#t 127462 #t 8986 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(197 (#t 127462 #f 776 #t 8986 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(198 (#t 127462 #f 768 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(199 (#t 127462 #f 776 #f 768 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(200 (#t 127462 #f 8205 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(201 (#t 127462 #f 776 #f 8205 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(202 (#t 127462 #t 888 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(203 (#t 127462 #f 776 #t 888 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(204 (#t 1536 #f 32 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] SPACE (Other) ÷ [0.3]")
(205 (#t 1536 #f 776 #t 32 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(206 (#t 1536 #t 13 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(207 (#t 1536 #f 776 #t 13 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(208 (#t 1536 #t 10 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(209 (#t 1536 #f 776 #t 10 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(210 (#t 1536 #t 1 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(211 (#t 1536 #f 776 #t 1 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(212 (#t 1536 #f 847 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(213 (#t 1536 #f 776 #f 847 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(214 (#t 1536 #f 127462 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(215 (#t 1536 #f 776 #t 127462 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(216 (#t 1536 #f 1536 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(217 (#t 1536 #f 776 #t 1536 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(218 (#t 1536 #f 2307 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(219 (#t 1536 #f 776 #f 2307 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(220 (#t 1536 #f 4352 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(221 (#t 1536 #f 776 #t 4352 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(222 (#t 1536 #f 4448 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(223 (#t 1536 #f 776 #t 4448 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(224 (#t 1536 #f 4520 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(225 (#t 1536 #f 776 #t 4520 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(226 (#t 1536 #f 44032 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] HANGUL SYLLABLE GA (LV) ÷ [0.3]")
(227 (#t 1536 #f 776 #t 44032 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(228 (#t 1536 #f 44033 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(229 (#t 1536 #f 776 #t 44033 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(230 (#t 1536 #f 8986 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] WATCH (ExtPict) ÷ [0.3]")
(231 (#t 1536 #f 776 #t 8986 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(232 (#t 1536 #f 768 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(233 (#t 1536 #f 776 #f 768 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(234 (#t 1536 #f 8205 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(235 (#t 1536 #f 776 #f 8205 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(236 (#t 1536 #f 888 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.2] <reserved-0378> (Other) ÷ [0.3]")
(237 (#t 1536 #f 776 #t 888 #t)
 "÷ [0.2] ARABIC NUMBER SIGN (Prepend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(238 (#t 2307 #t 32 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(239 (#t 2307 #f 776 #t 32 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(240 (#t 2307 #t 13 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(241 (#t 2307 #f 776 #t 13 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(242 (#t 2307 #t 10 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(243 (#t 2307 #f 776 #t 10 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(244 (#t 2307 #t 1 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(245 (#t 2307 #f 776 #t 1 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(246 (#t 2307 #f 847 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(247 (#t 2307 #f 776 #f 847 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(248 (#t 2307 #t 127462 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(249 (#t 2307 #f 776 #t 127462 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(250 (#t 2307 #t 1536 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(251 (#t 2307 #f 776 #t 1536 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(252 (#t 2307 #f 2307 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(253 (#t 2307 #f 776 #f 2307 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(254 (#t 2307 #t 4352 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(255 (#t 2307 #f 776 #t 4352 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(256 (#t 2307 #t 4448 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(257 (#t 2307 #f 776 #t 4448 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(258 (#t 2307 #t 4520 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(259 (#t 2307 #f 776 #t 4520 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(260 (#t 2307 #t 44032 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(261 (#t 2307 #f 776 #t 44032 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(262 (#t 2307 #t 44033 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(263 (#t 2307 #f 776 #t 44033 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(264 (#t 2307 #t 8986 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(265 (#t 2307 #f 776 #t 8986 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(266 (#t 2307 #f 768 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(267 (#t 2307 #f 776 #f 768 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(268 (#t 2307 #f 8205 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(269 (#t 2307 #f 776 #f 8205 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(270 (#t 2307 #t 888 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(271 (#t 2307 #f 776 #t 888 #t)
 "÷ [0.2] DEVANAGARI SIGN VISARGA (SpacingMark) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(272 (#t 4352 #t 32 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] SPACE (Other) ÷ [0.3]")
(273 (#t 4352 #f 776 #t 32 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(274 (#t 4352 #t 13 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(275 (#t 4352 #f 776 #t 13 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(276 (#t 4352 #t 10 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(277 (#t 4352 #f 776 #t 10 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(278 (#t 4352 #t 1 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(279 (#t 4352 #f 776 #t 1 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(280 (#t 4352 #f 847 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(281 (#t 4352 #f 776 #f 847 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(282 (#t 4352 #t 127462 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(283 (#t 4352 #f 776 #t 127462 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(284 (#t 4352 #t 1536 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(285 (#t 4352 #f 776 #t 1536 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(286 (#t 4352 #f 2307 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(287 (#t 4352 #f 776 #f 2307 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(288 (#t 4352 #f 4352 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [6.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(289 (#t 4352 #f 776 #t 4352 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(290 (#t 4352 #f 4448 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [6.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(291 (#t 4352 #f 776 #t 4448 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(292 (#t 4352 #t 4520 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(293 (#t 4352 #f 776 #t 4520 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(294 (#t 4352 #f 44032 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [6.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]")
(295 (#t 4352 #f 776 #t 44032 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(296 (#t 4352 #f 44033 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [6.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]")
(297 (#t 4352 #f 776 #t 44033 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(298 (#t 4352 #t 8986 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(299 (#t 4352 #f 776 #t 8986 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(300 (#t 4352 #f 768 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(301 (#t 4352 #f 776 #f 768 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(302 (#t 4352 #f 8205 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(303 (#t 4352 #f 776 #f 8205 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(304 (#t 4352 #t 888 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]")
(305 (#t 4352 #f 776 #t 888 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(306 (#t 4448 #t 32 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] SPACE (Other) ÷ [0.3]")
(307 (#t 4448 #f 776 #t 32 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(308 (#t 4448 #t 13 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(309 (#t 4448 #f 776 #t 13 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(310 (#t 4448 #t 10 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(311 (#t 4448 #f 776 #t 10 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(312 (#t 4448 #t 1 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(313 (#t 4448 #f 776 #t 1 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(314 (#t 4448 #f 847 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(315 (#t 4448 #f 776 #f 847 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(316 (#t 4448 #t 127462 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(317 (#t 4448 #f 776 #t 127462 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(318 (#t 4448 #t 1536 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(319 (#t 4448 #f 776 #t 1536 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(320 (#t 4448 #f 2307 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(321 (#t 4448 #f 776 #f 2307 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(322 (#t 4448 #t 4352 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(323 (#t 4448 #f 776 #t 4352 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(324 (#t 4448 #f 4448 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [7.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(325 (#t 4448 #f 776 #t 4448 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(326 (#t 4448 #f 4520 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [7.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(327 (#t 4448 #f 776 #t 4520 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(328 (#t 4448 #t 44032 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(329 (#t 4448 #f 776 #t 44032 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(330 (#t 4448 #t 44033 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(331 (#t 4448 #f 776 #t 44033 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(332 (#t 4448 #t 8986 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(333 (#t 4448 #f 776 #t 8986 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(334 (#t 4448 #f 768 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(335 (#t 4448 #f 776 #f 768 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(336 (#t 4448 #f 8205 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(337 (#t 4448 #f 776 #f 8205 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(338 (#t 4448 #t 888 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(339 (#t 4448 #f 776 #t 888 #t)
 "÷ [0.2] HANGUL JUNGSEONG FILLER (V) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(340 (#t 4520 #t 32 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] SPACE (Other) ÷ [0.3]")
(341 (#t 4520 #f 776 #t 32 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(342 (#t 4520 #t 13 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(343 (#t 4520 #f 776 #t 13 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(344 (#t 4520 #t 10 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(345 (#t 4520 #f 776 #t 10 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(346 (#t 4520 #t 1 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(347 (#t 4520 #f 776 #t 1 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(348 (#t 4520 #f 847 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(349 (#t 4520 #f 776 #f 847 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(350 (#t 4520 #t 127462 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(351 (#t 4520 #f 776 #t 127462 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(352 (#t 4520 #t 1536 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(353 (#t 4520 #f 776 #t 1536 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(354 (#t 4520 #f 2307 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(355 (#t 4520 #f 776 #f 2307 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(356 (#t 4520 #t 4352 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(357 (#t 4520 #f 776 #t 4352 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(358 (#t 4520 #t 4448 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(359 (#t 4520 #f 776 #t 4448 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(360 (#t 4520 #f 4520 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [8.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(361 (#t 4520 #f 776 #t 4520 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(362 (#t 4520 #t 44032 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(363 (#t 4520 #f 776 #t 44032 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(364 (#t 4520 #t 44033 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(365 (#t 4520 #f 776 #t 44033 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(366 (#t 4520 #t 8986 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(367 (#t 4520 #f 776 #t 8986 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(368 (#t 4520 #f 768 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(369 (#t 4520 #f 776 #f 768 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(370 (#t 4520 #f 8205 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(371 (#t 4520 #f 776 #f 8205 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(372 (#t 4520 #t 888 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(373 (#t 4520 #f 776 #t 888 #t)
 "÷ [0.2] HANGUL JONGSEONG KIYEOK (T) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(374 (#t 44032 #t 32 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] SPACE (Other) ÷ [0.3]")
(375 (#t 44032 #f 776 #t 32 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(376 (#t 44032 #t 13 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(377 (#t 44032 #f 776 #t 13 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(378 (#t 44032 #t 10 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(379 (#t 44032 #f 776 #t 10 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(380 (#t 44032 #t 1 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]")
(381 (#t 44032 #f 776 #t 1 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(382 (#t 44032 #f 847 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(383 (#t 44032 #f 776 #f 847 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(384 (#t 44032 #t 127462 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(385 (#t 44032 #f 776 #t 127462 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(386 (#t 44032 #t 1536 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(387 (#t 44032 #f 776 #t 1536 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(388 (#t 44032 #f 2307 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(389 (#t 44032 #f 776 #f 2307 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(390 (#t 44032 #t 4352 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]")
(391 (#t 44032 #f 776 #t 4352 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(392 (#t 44032 #f 4448 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [7.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]")
(393 (#t 44032 #f 776 #t 4448 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(394 (#t 44032 #f 4520 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [7.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]")
(395 (#t 44032 #f 776 #t 4520 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(396 (#t 44032 #t 44032 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]")
(397 (#t 44032 #f 776 #t 44032 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(398 (#t 44032 #t 44033 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]")
(399 (#t 44032 #f 776 #t 44033 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(400 (#t 44032 #t 8986 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(401 (#t 44032 #f 776 #t 8986 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(402 (#t 44032 #f 768 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(403 (#t 44032 #f 776 #f 768 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(404 (#t 44032 #f 8205 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(405 (#t 44032 #f 776 #f 8205 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(406 (#t 44032 #t 888 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]")
(407 (#t 44032 #f 776 #t 888 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(408 (#t 44033 #t 32 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] SPACE (Other) ÷ [0.3]")
(409 (#t 44033 #f 776 #t 32 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(410 (#t 44033 #t 13 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(411 (#t 44033 #f 776 #t 13 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(412 (#t 44033 #t 10 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(413 (#t 44033 #f 776 #t 10 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(414 (#t 44033 #t 1 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(415 (#t 44033 #f 776 #t 1 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(416 (#t 44033 #f 847 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(417 (#t 44033 #f 776 #f 847 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(418 (#t 44033 #t 127462 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(419 (#t 44033 #f 776 #t 127462 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(420 (#t 44033 #t 1536 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(421 (#t 44033 #f 776 #t 1536 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(422 (#t 44033 #f 2307 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(423 (#t 44033 #f 776 #f 2307 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(424 (#t 44033 #t 4352 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(425 (#t 44033 #f 776 #t 4352 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(426 (#t 44033 #t 4448 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(427 (#t 44033 #f 776 #t 4448 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(428 (#t 44033 #f 4520 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [8.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(429 (#t 44033 #f 776 #t 4520 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(430 (#t 44033 #t 44032 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]")
(431 (#t 44033 #f 776 #t 44032 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(432 (#t 44033 #t 44033 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(433 (#t 44033 #f 776 #t 44033 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(434 (#t 44033 #t 8986 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(435 (#t 44033 #f 776 #t 8986 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(436 (#t 44033 #f 768 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(437 (#t 44033 #f 776 #f 768 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(438 (#t 44033 #f 8205 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(439 (#t 44033 #f 776 #f 8205 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(440 (#t 44033 #t 888 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]")
(441 (#t 44033 #f 776 #t 888 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(442 (#t 8986 #t 32 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] SPACE (Other) ÷ [0.3]")
(443 (#t 8986 #f 776 #t 32 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(444 (#t 8986 #t 13 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(445 (#t 8986 #f 776 #t 13 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(446 (#t 8986 #t 10 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(447 (#t 8986 #f 776 #t 10 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(448 (#t 8986 #t 1 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]")
(449 (#t 8986 #f 776 #t 1 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(450 (#t 8986 #f 847 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]")
(451 (#t 8986 #f 776 #f 847 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(452 (#t 8986 #t 127462 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(453 (#t 8986 #f 776 #t 127462 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(454 (#t 8986 #t 1536 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]")
(455 (#t 8986 #f 776 #t 1536 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(456 (#t 8986 #f 2307 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(457 (#t 8986 #f 776 #f 2307 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(458 (#t 8986 #t 4352 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]")
(459 (#t 8986 #f 776 #t 4352 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(460 (#t 8986 #t 4448 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]")
(461 (#t 8986 #f 776 #t 4448 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(462 (#t 8986 #t 4520 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]")
(463 (#t 8986 #f 776 #t 4520 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(464 (#t 8986 #t 44032 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]")
(465 (#t 8986 #f 776 #t 44032 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(466 (#t 8986 #t 44033 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]")
(467 (#t 8986 #f 776 #t 44033 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(468 (#t 8986 #t 8986 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(469 (#t 8986 #f 776 #t 8986 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(470 (#t 8986 #f 768 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(471 (#t 8986 #f 776 #f 768 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(472 (#t 8986 #f 8205 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]")
(473 (#t 8986 #f 776 #f 8205 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(474 (#t 8986 #t 888 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]")
(475 (#t 8986 #f 776 #t 888 #t)
 "÷ [0.2] WATCH (ExtPict) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(476 (#t 768 #t 32 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(477 (#t 768 #f 776 #t 32 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(478 (#t 768 #t 13 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(479 (#t 768 #f 776 #t 13 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(480 (#t 768 #t 10 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(481 (#t 768 #f 776 #t 10 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(482 (#t 768 #t 1 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(483 (#t 768 #f 776 #t 1 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(484 (#t 768 #f 847 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(485 (#t 768 #f 776 #f 847 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(486 (#t 768 #t 127462 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(487 (#t 768 #f 776 #t 127462 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(488 (#t 768 #t 1536 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(489 (#t 768 #f 776 #t 1536 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(490 (#t 768 #f 2307 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(491 (#t 768 #f 776 #f 2307 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(492 (#t 768 #t 4352 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(493 (#t 768 #f 776 #t 4352 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(494 (#t 768 #t 4448 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(495 (#t 768 #f 776 #t 4448 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(496 (#t 768 #t 4520 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(497 (#t 768 #f 776 #t 4520 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(498 (#t 768 #t 44032 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(499 (#t 768 #f 776 #t 44032 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(500 (#t 768 #t 44033 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(501 (#t 768 #f 776 #t 44033 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(502 (#t 768 #t 8986 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(503 (#t 768 #f 776 #t 8986 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(504 (#t 768 #f 768 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(505 (#t 768 #f 776 #f 768 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(506 (#t 768 #f 8205 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(507 (#t 768 #f 776 #f 8205 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(508 (#t 768 #t 888 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(509 (#t 768 #f 776 #t 888 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(510 (#t 8205 #t 32 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]")
(511 (#t 8205 #f 776 #t 32 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(512 (#t 8205 #t 13 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(513 (#t 8205 #f 776 #t 13 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(514 (#t 8205 #t 10 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(515 (#t 8205 #f 776 #t 10 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(516 (#t 8205 #t 1 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(517 (#t 8205 #f 776 #t 1 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(518 (#t 8205 #f 847 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(519 (#t 8205 #f 776 #f 847 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(520 (#t 8205 #t 127462 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(521 (#t 8205 #f 776 #t 127462 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(522 (#t 8205 #t 1536 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(523 (#t 8205 #f 776 #t 1536 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(524 (#t 8205 #f 2307 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(525 (#t 8205 #f 776 #f 2307 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(526 (#t 8205 #t 4352 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(527 (#t 8205 #f 776 #t 4352 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(528 (#t 8205 #t 4448 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(529 (#t 8205 #f 776 #t 4448 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(530 (#t 8205 #t 4520 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(531 (#t 8205 #f 776 #t 4520 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(532 (#t 8205 #t 44032 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(533 (#t 8205 #f 776 #t 44032 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(534 (#t 8205 #t 44033 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(535 (#t 8205 #f 776 #t 44033 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(536 (#t 8205 #t 8986 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(537 (#t 8205 #f 776 #t 8986 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(538 (#t 8205 #f 768 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(539 (#t 8205 #f 776 #f 768 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(540 (#t 8205 #f 8205 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(541 (#t 8205 #f 776 #f 8205 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(542 (#t 8205 #t 888 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(543 (#t 8205 #f 776 #t 888 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(544 (#t 888 #t 32 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [999.0] SPACE (Other) ÷ [0.3]")
(545 (#t 888 #f 776 #t 32 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(546 (#t 888 #t 13 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(547 (#t 888 #f 776 #t 13 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(548 (#t 888 #t 10 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(549 (#t 888 #f 776 #t 10 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(550 (#t 888 #t 1 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]")
(551 (#t 888 #f 776 #t 1 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [5.0] <START OF HEADING> (Control) ÷ [0.3]"
 )
(552 (#t 888 #f 847 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(553 (#t 888 #f 776 #f 847 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAPHEME JOINER (Extend) ÷ [0.3]"
 )
(554 (#t 888 #t 127462 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(555 (#t 888 #f 776 #t 127462 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(556 (#t 888 #t 1536 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(557 (#t 888 #f 776 #t 1536 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) ÷ [0.3]"
 )
(558 (#t 888 #f 2307 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(559 (#t 888 #f 776 #f 2307 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [0.3]"
 )
(560 (#t 888 #t 4352 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]")
(561 (#t 888 #f 776 #t 4352 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(562 (#t 888 #t 4448 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(563 (#t 888 #f 776 #t 4448 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JUNGSEONG FILLER (V) ÷ [0.3]"
 )
(564 (#t 888 #t 4520 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(565 (#t 888 #f 776 #t 4520 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL JONGSEONG KIYEOK (T) ÷ [0.3]"
 )
(566 (#t 888 #t 44032 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]")
(567 (#t 888 #f 776 #t 44032 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GA (LV) ÷ [0.3]"
 )
(568 (#t 888 #t 44033 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]")
(569 (#t 888 #f 776 #t 44033 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] HANGUL SYLLABLE GAG (LVT) ÷ [0.3]"
 )
(570 (#t 888 #t 8986 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(571 (#t 888 #f 776 #t 8986 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(572 (#t 888 #f 768 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(573 (#t 888 #f 776 #f 768 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] COMBINING GRAVE ACCENT (Extend_ExtCccZwj) ÷ [0.3]"
 )
(574 (#t 888 #f 8205 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(575 (#t 888 #f 776 #f 8205 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(576 (#t 888 #t 888 #t)
 "÷ [0.2] <reserved-0378> (Other) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]")
(577 (#t 888 #f 776 #t 888 #t)
 "÷ [0.2] <reserved-0378> (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] <reserved-0378> (Other) ÷ [0.3]"
 )
(578 (#t 13 #f 10 #t 97 #t 10 #t 776 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) × [3.0] <LINE FEED (LF)> (LF) ÷ [4.0] LATIN SMALL LETTER A (Other) ÷ [5.0] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [0.3]"
 )
(579 (#t 97 #f 776 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [0.3]"
 )
(580 (#t 32 #f 8205 #t 1606 #t)
 "÷ [0.2] SPACE (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] ARABIC LETTER NOON (Other) ÷ [0.3]"
 )
(581 (#t 1606 #f 8205 #t 32 #t)
 "÷ [0.2] ARABIC LETTER NOON (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]"
 )
(582 (#t 4352 #f 4352 #t)
 "÷ [0.2] HANGUL CHOSEONG KIYEOK (L) × [6.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(583 (#t 44032 #f 4520 #t 4352 #t)
 "÷ [0.2] HANGUL SYLLABLE GA (LV) × [7.0] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(584 (#t 44033 #f 4520 #t 4352 #t)
 "÷ [0.2] HANGUL SYLLABLE GAG (LVT) × [8.0] HANGUL JONGSEONG KIYEOK (T) ÷ [999.0] HANGUL CHOSEONG KIYEOK (L) ÷ [0.3]"
 )
(585 (#t 127462 #f 127463 #t 127464 #t 98 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [12.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
 )
(586 (#t 97 #t 127462 #f 127463 #t 127464 #t 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [13.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
 )
(587 (#t 97 #t 127462 #f 127463 #f 8205 #t 127464 #t 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [13.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
 )
(588 (#t 97 #t 127462 #f 8205 #t 127463 #f 127464 #t 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) × [13.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
 )
(589 (#t 97 #t 127462 #f 127463 #t 127464 #f 127465 #t 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [13.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) × [13.0] REGIONAL INDICATOR SYMBOL LETTER D (RI) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
 )
(590 (#t 97 #f 8205 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [0.3]"
 )
(591 (#t 97 #f 776 #t 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
 )
(592 (#t 97 #f 2307 #t 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) × [9.1] DEVANAGARI SIGN VISARGA (SpacingMark) ÷ [999.0] LATIN SMALL LETTER B (Other) ÷ [0.3]"
 )
(593 (#t 97 #t 1536 #f 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) ÷ [999.0] ARABIC NUMBER SIGN (Prepend) × [9.2] LATIN SMALL LETTER B (Other) ÷ [0.3]"
 )
(594 (#t 128118 #f 127999 #t 128118 #t)
 "÷ [0.2] BABY (ExtPict) × [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) ÷ [999.0] BABY (ExtPict) ÷ [0.3]"
 )
(595 (#t 97 #f 127999 #t 128118 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) ÷ [999.0] BABY (ExtPict) ÷ [0.3]"
 )
(596 (#t 97 #f 127999 #t 128118 #f 8205 #f 128721 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) ÷ [999.0] BABY (ExtPict) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [11.0] OCTAGONAL SIGN (ExtPict) ÷ [0.3]"
 )
(597 (#t 128118 #f 127999 #f 776 #f 8205 #f 128118 #f 127999 #t)
 "÷ [0.2] BABY (ExtPict) × [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [11.0] BABY (ExtPict) × [9.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend) ÷ [0.3]"
 )
(598 (#t 128721 #f 8205 #f 128721 #t)
 "÷ [0.2] OCTAGONAL SIGN (ExtPict) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [11.0] OCTAGONAL SIGN (ExtPict) ÷ [0.3]"
 )
(599 (#t 97 #f 8205 #t 128721 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] OCTAGONAL SIGN (ExtPict) ÷ [0.3]"
 )
(600 (#t 9985 #f 8205 #f 9985 #t)
 "÷ [0.2] UPPER BLADE SCISSORS (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) × [11.0] UPPER BLADE SCISSORS (Other) ÷ [0.3]"
 )
(601 (#t 97 #f 8205 #t 9985 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Other) × [9.0] ZERO WIDTH JOINER (ZWJ_ExtCccZwj) ÷ [999.0] UPPER BLADE SCISSORS (Other) ÷ [0.3]"
 )
))

;; auxiliary/WordBreakTest.txt
(define *word-break-tests* '(
(0 (#t 1 #t 1 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1 (#t 1 #f 776 #t 1 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(2 (#t 1 #t 13 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(3 (#t 1 #f 776 #t 13 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(4 (#t 1 #t 10 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(5 (#t 1 #f 776 #t 10 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(6 (#t 1 #t 11 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(7 (#t 1 #f 776 #t 11 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(8 (#t 1 #t 12337 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(9 (#t 1 #f 776 #t 12337 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(10 (#t 1 #t 65 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(11 (#t 1 #f 776 #t 65 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(12 (#t 1 #t 58 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(13 (#t 1 #f 776 #t 58 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(14 (#t 1 #t 44 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(15 (#t 1 #f 776 #t 44 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(16 (#t 1 #t 46 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]")
(17 (#t 1 #f 776 #t 46 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(18 (#t 1 #t 48 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(19 (#t 1 #f 776 #t 48 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(20 (#t 1 #t 95 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]")
(21 (#t 1 #f 776 #t 95 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(22 (#t 1 #t 127462 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(23 (#t 1 #f 776 #t 127462 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(24 (#t 1 #t 1488 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(25 (#t 1 #f 776 #t 1488 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(26 (#t 1 #t 34 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(27 (#t 1 #f 776 #t 34 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(28 (#t 1 #t 39 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(29 (#t 1 #f 776 #t 39 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(30 (#t 1 #t 8986 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(31 (#t 1 #f 776 #t 8986 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(32 (#t 1 #t 32 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(33 (#t 1 #f 776 #t 32 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(34 (#t 1 #f 173 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(35 (#t 1 #f 776 #f 173 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(36 (#t 1 #f 768 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(37 (#t 1 #f 776 #f 768 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(38 (#t 1 #f 8205 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(39 (#t 1 #f 776 #f 8205 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(40 (#t 1 #t 97 #f 8288 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(41 (#t 1 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(42 (#t 1 #t 97 #t 58 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(43 (#t 1 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(44 (#t 1 #t 97 #t 39 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(45 (#t 1 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(46 (#t 1 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(47 (#t 1 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(48 (#t 1 #t 97 #t 44 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(49 (#t 1 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(50 (#t 1 #t 49 #t 58 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(51 (#t 1 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(52 (#t 1 #t 49 #t 39 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(53 (#t 1 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(54 (#t 1 #t 49 #t 44 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(55 (#t 1 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(56 (#t 1 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] <START OF HEADING> (Other) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(57 (#t 1 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(58 (#t 13 #t 1 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] <START OF HEADING> (Other) ÷ [0.3]"
 )
(59 (#t 13 #t 776 #t 1 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(60 (#t 13 #t 13 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(61 (#t 13 #t 776 #t 13 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(62 (#t 13 #f 10 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) × [3.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(63 (#t 13 #t 776 #t 10 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(64 (#t 13 #t 11 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(65 (#t 13 #t 776 #t 11 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(66 (#t 13 #t 12337 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(67 (#t 13 #t 776 #t 12337 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(68 (#t 13 #t 65 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(69 (#t 13 #t 776 #t 65 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(70 (#t 13 #t 58 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COLON (MidLetter) ÷ [0.3]")
(71 (#t 13 #t 776 #t 58 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(72 (#t 13 #t 44 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMMA (MidNum) ÷ [0.3]")
(73 (#t 13 #t 776 #t 44 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(74 (#t 13 #t 46 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] FULL STOP (MidNumLet) ÷ [0.3]")
(75 (#t 13 #t 776 #t 46 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(76 (#t 13 #t 48 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] DIGIT ZERO (Numeric) ÷ [0.3]")
(77 (#t 13 #t 776 #t 48 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(78 (#t 13 #t 95 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] LOW LINE (ExtendNumLet) ÷ [0.3]")
(79 (#t 13 #t 776 #t 95 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(80 (#t 13 #t 127462 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(81 (#t 13 #t 776 #t 127462 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(82 (#t 13 #t 1488 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(83 (#t 13 #t 776 #t 1488 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(84 (#t 13 #t 34 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(85 (#t 13 #t 776 #t 34 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(86 (#t 13 #t 39 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(87 (#t 13 #t 776 #t 39 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(88 (#t 13 #t 8986 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] WATCH (ExtPict) ÷ [0.3]")
(89 (#t 13 #t 776 #t 8986 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(90 (#t 13 #t 32 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] SPACE (WSegSpace) ÷ [0.3]")
(91 (#t 13 #t 776 #t 32 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(92 (#t 13 #t 173 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(93 (#t 13 #t 776 #f 173 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(94 (#t 13 #t 768 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(95 (#t 13 #t 776 #f 768 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(96 (#t 13 #t 8205 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(97 (#t 13 #t 776 #f 8205 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(98 (#t 13 #t 97 #f 8288 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(99 (#t 13 #t 776 #t 97 #f 8288 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(100 (#t 13 #t 97 #t 58 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(101 (#t 13 #t 776 #t 97 #t 58 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(102 (#t 13 #t 97 #t 39 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(103 (#t 13 #t 776 #t 97 #t 39 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(104 (#t 13 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(105 (#t 13 #t 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(106 (#t 13 #t 97 #t 44 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(107 (#t 13 #t 776 #t 97 #t 44 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(108 (#t 13 #t 49 #t 58 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(109 (#t 13 #t 776 #t 49 #t 58 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(110 (#t 13 #t 49 #t 39 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(111 (#t 13 #t 776 #t 49 #t 39 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(112 (#t 13 #t 49 #t 44 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(113 (#t 13 #t 776 #t 49 #t 44 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(114 (#t 13 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(115 (#t 13 #t 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(116 (#t 10 #t 1 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] <START OF HEADING> (Other) ÷ [0.3]")
(117 (#t 10 #t 776 #t 1 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(118 (#t 10 #t 13 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(119 (#t 10 #t 776 #t 13 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(120 (#t 10 #t 10 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] <LINE FEED (LF)> (LF) ÷ [0.3]")
(121 (#t 10 #t 776 #t 10 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(122 (#t 10 #t 11 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] <LINE TABULATION> (Newline) ÷ [0.3]")
(123 (#t 10 #t 776 #t 11 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(124 (#t 10 #t 12337 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(125 (#t 10 #t 776 #t 12337 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(126 (#t 10 #t 65 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(127 (#t 10 #t 776 #t 65 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(128 (#t 10 #t 58 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COLON (MidLetter) ÷ [0.3]")
(129 (#t 10 #t 776 #t 58 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(130 (#t 10 #t 44 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMMA (MidNum) ÷ [0.3]")
(131 (#t 10 #t 776 #t 44 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(132 (#t 10 #t 46 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] FULL STOP (MidNumLet) ÷ [0.3]")
(133 (#t 10 #t 776 #t 46 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(134 (#t 10 #t 48 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] DIGIT ZERO (Numeric) ÷ [0.3]")
(135 (#t 10 #t 776 #t 48 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(136 (#t 10 #t 95 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] LOW LINE (ExtendNumLet) ÷ [0.3]")
(137 (#t 10 #t 776 #t 95 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(138 (#t 10 #t 127462 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(139 (#t 10 #t 776 #t 127462 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(140 (#t 10 #t 1488 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(141 (#t 10 #t 776 #t 1488 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(142 (#t 10 #t 34 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] QUOTATION MARK (Double_Quote) ÷ [0.3]")
(143 (#t 10 #t 776 #t 34 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(144 (#t 10 #t 39 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] APOSTROPHE (Single_Quote) ÷ [0.3]")
(145 (#t 10 #t 776 #t 39 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(146 (#t 10 #t 8986 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] WATCH (ExtPict) ÷ [0.3]")
(147 (#t 10 #t 776 #t 8986 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(148 (#t 10 #t 32 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] SPACE (WSegSpace) ÷ [0.3]")
(149 (#t 10 #t 776 #t 32 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(150 (#t 10 #t 173 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(151 (#t 10 #t 776 #f 173 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(152 (#t 10 #t 768 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(153 (#t 10 #t 776 #f 768 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(154 (#t 10 #t 8205 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]")
(155 (#t 10 #t 776 #f 8205 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(156 (#t 10 #t 97 #f 8288 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(157 (#t 10 #t 776 #t 97 #f 8288 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(158 (#t 10 #t 97 #t 58 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(159 (#t 10 #t 776 #t 97 #t 58 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(160 (#t 10 #t 97 #t 39 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(161 (#t 10 #t 776 #t 97 #t 39 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(162 (#t 10 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(163 (#t 10 #t 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(164 (#t 10 #t 97 #t 44 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(165 (#t 10 #t 776 #t 97 #t 44 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(166 (#t 10 #t 49 #t 58 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(167 (#t 10 #t 776 #t 49 #t 58 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(168 (#t 10 #t 49 #t 39 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(169 (#t 10 #t 776 #t 49 #t 39 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(170 (#t 10 #t 49 #t 44 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(171 (#t 10 #t 776 #t 49 #t 44 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(172 (#t 10 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(173 (#t 10 #t 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(174 (#t 11 #t 1 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] <START OF HEADING> (Other) ÷ [0.3]"
 )
(175 (#t 11 #t 776 #t 1 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(176 (#t 11 #t 13 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(177 (#t 11 #t 776 #t 13 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(178 (#t 11 #t 10 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] <LINE FEED (LF)> (LF) ÷ [0.3]")
(179 (#t 11 #t 776 #t 10 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(180 (#t 11 #t 11 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(181 (#t 11 #t 776 #t 11 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(182 (#t 11 #t 12337 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(183 (#t 11 #t 776 #t 12337 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(184 (#t 11 #t 65 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(185 (#t 11 #t 776 #t 65 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(186 (#t 11 #t 58 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COLON (MidLetter) ÷ [0.3]")
(187 (#t 11 #t 776 #t 58 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(188 (#t 11 #t 44 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMMA (MidNum) ÷ [0.3]")
(189 (#t 11 #t 776 #t 44 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(190 (#t 11 #t 46 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] FULL STOP (MidNumLet) ÷ [0.3]")
(191 (#t 11 #t 776 #t 46 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(192 (#t 11 #t 48 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] DIGIT ZERO (Numeric) ÷ [0.3]")
(193 (#t 11 #t 776 #t 48 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(194 (#t 11 #t 95 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] LOW LINE (ExtendNumLet) ÷ [0.3]")
(195 (#t 11 #t 776 #t 95 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(196 (#t 11 #t 127462 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(197 (#t 11 #t 776 #t 127462 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(198 (#t 11 #t 1488 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(199 (#t 11 #t 776 #t 1488 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(200 (#t 11 #t 34 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(201 (#t 11 #t 776 #t 34 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(202 (#t 11 #t 39 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(203 (#t 11 #t 776 #t 39 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(204 (#t 11 #t 8986 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] WATCH (ExtPict) ÷ [0.3]")
(205 (#t 11 #t 776 #t 8986 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(206 (#t 11 #t 32 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] SPACE (WSegSpace) ÷ [0.3]")
(207 (#t 11 #t 776 #t 32 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(208 (#t 11 #t 173 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(209 (#t 11 #t 776 #f 173 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(210 (#t 11 #t 768 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(211 (#t 11 #t 776 #f 768 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(212 (#t 11 #t 8205 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(213 (#t 11 #t 776 #f 8205 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(214 (#t 11 #t 97 #f 8288 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(215 (#t 11 #t 776 #t 97 #f 8288 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(216 (#t 11 #t 97 #t 58 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(217 (#t 11 #t 776 #t 97 #t 58 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(218 (#t 11 #t 97 #t 39 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(219 (#t 11 #t 776 #t 97 #t 39 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(220 (#t 11 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(221 (#t 11 #t 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(222 (#t 11 #t 97 #t 44 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(223 (#t 11 #t 776 #t 97 #t 44 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(224 (#t 11 #t 49 #t 58 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(225 (#t 11 #t 776 #t 49 #t 58 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(226 (#t 11 #t 49 #t 39 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(227 (#t 11 #t 776 #t 49 #t 39 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(228 (#t 11 #t 49 #t 44 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(229 (#t 11 #t 776 #t 49 #t 44 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(230 (#t 11 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(231 (#t 11 #t 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] <LINE TABULATION> (Newline) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(232 (#t 12337 #t 1 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(233 (#t 12337 #f 776 #t 1 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(234 (#t 12337 #t 13 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(235 (#t 12337 #f 776 #t 13 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(236 (#t 12337 #t 10 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(237 (#t 12337 #f 776 #t 10 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(238 (#t 12337 #t 11 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(239 (#t 12337 #f 776 #t 11 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(240 (#t 12337 #f 12337 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [13.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(241 (#t 12337 #f 776 #f 12337 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(242 (#t 12337 #t 65 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(243 (#t 12337 #f 776 #t 65 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(244 (#t 12337 #t 58 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(245 (#t 12337 #f 776 #t 58 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(246 (#t 12337 #t 44 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(247 (#t 12337 #f 776 #t 44 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(248 (#t 12337 #t 46 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(249 (#t 12337 #f 776 #t 46 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(250 (#t 12337 #t 48 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(251 (#t 12337 #f 776 #t 48 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(252 (#t 12337 #f 95 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(253 (#t 12337 #f 776 #f 95 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(254 (#t 12337 #t 127462 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(255 (#t 12337 #f 776 #t 127462 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(256 (#t 12337 #t 1488 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(257 (#t 12337 #f 776 #t 1488 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(258 (#t 12337 #t 34 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(259 (#t 12337 #f 776 #t 34 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(260 (#t 12337 #t 39 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(261 (#t 12337 #f 776 #t 39 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(262 (#t 12337 #t 8986 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(263 (#t 12337 #f 776 #t 8986 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(264 (#t 12337 #t 32 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(265 (#t 12337 #f 776 #t 32 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(266 (#t 12337 #f 173 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(267 (#t 12337 #f 776 #f 173 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(268 (#t 12337 #f 768 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(269 (#t 12337 #f 776 #f 768 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(270 (#t 12337 #f 8205 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(271 (#t 12337 #f 776 #f 8205 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(272 (#t 12337 #t 97 #f 8288 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(273 (#t 12337 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(274 (#t 12337 #t 97 #t 58 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(275 (#t 12337 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(276 (#t 12337 #t 97 #t 39 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(277 (#t 12337 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(278 (#t 12337 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(279 (#t 12337 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(280 (#t 12337 #t 97 #t 44 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(281 (#t 12337 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(282 (#t 12337 #t 49 #t 58 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(283 (#t 12337 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(284 (#t 12337 #t 49 #t 39 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(285 (#t 12337 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(286 (#t 12337 #t 49 #t 44 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(287 (#t 12337 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(288 (#t 12337 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(289 (#t 12337 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(290 (#t 65 #t 1 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(291 (#t 65 #f 776 #t 1 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(292 (#t 65 #t 13 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(293 (#t 65 #f 776 #t 13 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(294 (#t 65 #t 10 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(295 (#t 65 #f 776 #t 10 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(296 (#t 65 #t 11 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(297 (#t 65 #f 776 #t 11 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(298 (#t 65 #t 12337 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(299 (#t 65 #f 776 #t 12337 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(300 (#t 65 #f 65 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [5.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(301 (#t 65 #f 776 #f 65 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(302 (#t 65 #t 58 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(303 (#t 65 #f 776 #t 58 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(304 (#t 65 #t 44 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(305 (#t 65 #f 776 #t 44 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(306 (#t 65 #t 46 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(307 (#t 65 #f 776 #t 46 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(308 (#t 65 #f 48 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [9.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(309 (#t 65 #f 776 #f 48 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(310 (#t 65 #f 95 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(311 (#t 65 #f 776 #f 95 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(312 (#t 65 #t 127462 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(313 (#t 65 #f 776 #t 127462 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(314 (#t 65 #f 1488 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [5.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(315 (#t 65 #f 776 #f 1488 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(316 (#t 65 #t 34 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(317 (#t 65 #f 776 #t 34 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(318 (#t 65 #t 39 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(319 (#t 65 #f 776 #t 39 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(320 (#t 65 #t 8986 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(321 (#t 65 #f 776 #t 8986 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(322 (#t 65 #t 32 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(323 (#t 65 #f 776 #t 32 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(324 (#t 65 #f 173 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(325 (#t 65 #f 776 #f 173 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(326 (#t 65 #f 768 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(327 (#t 65 #f 776 #f 768 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(328 (#t 65 #f 8205 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(329 (#t 65 #f 776 #f 8205 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(330 (#t 65 #f 97 #f 8288 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [5.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(331 (#t 65 #f 776 #f 97 #f 8288 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(332 (#t 65 #f 97 #t 58 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(333 (#t 65 #f 776 #f 97 #t 58 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(334 (#t 65 #f 97 #t 39 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(335 (#t 65 #f 776 #f 97 #t 39 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(336 (#t 65 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(337 (#t 65 #f 776 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(338 (#t 65 #f 97 #t 44 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(339 (#t 65 #f 776 #f 97 #t 44 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(340 (#t 65 #f 49 #t 58 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(341 (#t 65 #f 776 #f 49 #t 58 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(342 (#t 65 #f 49 #t 39 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(343 (#t 65 #f 776 #f 49 #t 39 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(344 (#t 65 #f 49 #t 44 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(345 (#t 65 #f 776 #f 49 #t 44 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(346 (#t 65 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(347 (#t 65 #f 776 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(348 (#t 58 #t 1 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]")
(349 (#t 58 #f 776 #t 1 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(350 (#t 58 #t 13 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(351 (#t 58 #f 776 #t 13 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(352 (#t 58 #t 10 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(353 (#t 58 #f 776 #t 10 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(354 (#t 58 #t 11 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]")
(355 (#t 58 #f 776 #t 11 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(356 (#t 58 #t 12337 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(357 (#t 58 #f 776 #t 12337 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(358 (#t 58 #t 65 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]")
(359 (#t 58 #f 776 #t 65 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(360 (#t 58 #t 58 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(361 (#t 58 #f 776 #t 58 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(362 (#t 58 #t 44 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(363 (#t 58 #f 776 #t 44 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(364 (#t 58 #t 46 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]")
(365 (#t 58 #f 776 #t 46 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(366 (#t 58 #t 48 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(367 (#t 58 #f 776 #t 48 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(368 (#t 58 #t 95 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]")
(369 (#t 58 #f 776 #t 95 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(370 (#t 58 #t 127462 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(371 (#t 58 #f 776 #t 127462 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(372 (#t 58 #t 1488 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(373 (#t 58 #f 776 #t 1488 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(374 (#t 58 #t 34 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]")
(375 (#t 58 #f 776 #t 34 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(376 (#t 58 #t 39 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]")
(377 (#t 58 #f 776 #t 39 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(378 (#t 58 #t 8986 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(379 (#t 58 #f 776 #t 8986 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(380 (#t 58 #t 32 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(381 (#t 58 #f 776 #t 32 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(382 (#t 58 #f 173 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(383 (#t 58 #f 776 #f 173 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(384 (#t 58 #f 768 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]")
(385 (#t 58 #f 776 #f 768 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(386 (#t 58 #f 8205 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]")
(387 (#t 58 #f 776 #f 8205 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(388 (#t 58 #t 97 #f 8288 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(389 (#t 58 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(390 (#t 58 #t 97 #t 58 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(391 (#t 58 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(392 (#t 58 #t 97 #t 39 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(393 (#t 58 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(394 (#t 58 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(395 (#t 58 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(396 (#t 58 #t 97 #t 44 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(397 (#t 58 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(398 (#t 58 #t 49 #t 58 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(399 (#t 58 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(400 (#t 58 #t 49 #t 39 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(401 (#t 58 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(402 (#t 58 #t 49 #t 44 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(403 (#t 58 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(404 (#t 58 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(405 (#t 58 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(406 (#t 44 #t 1 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]")
(407 (#t 44 #f 776 #t 1 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(408 (#t 44 #t 13 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(409 (#t 44 #f 776 #t 13 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(410 (#t 44 #t 10 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(411 (#t 44 #f 776 #t 10 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(412 (#t 44 #t 11 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]")
(413 (#t 44 #f 776 #t 11 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(414 (#t 44 #t 12337 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(415 (#t 44 #f 776 #t 12337 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(416 (#t 44 #t 65 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]")
(417 (#t 44 #f 776 #t 65 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(418 (#t 44 #t 58 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(419 (#t 44 #f 776 #t 58 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(420 (#t 44 #t 44 #t) "÷ [0.2] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(421 (#t 44 #f 776 #t 44 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(422 (#t 44 #t 46 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]")
(423 (#t 44 #f 776 #t 46 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(424 (#t 44 #t 48 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(425 (#t 44 #f 776 #t 48 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(426 (#t 44 #t 95 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]")
(427 (#t 44 #f 776 #t 95 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(428 (#t 44 #t 127462 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(429 (#t 44 #f 776 #t 127462 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(430 (#t 44 #t 1488 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]")
(431 (#t 44 #f 776 #t 1488 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(432 (#t 44 #t 34 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]")
(433 (#t 44 #f 776 #t 34 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(434 (#t 44 #t 39 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]")
(435 (#t 44 #f 776 #t 39 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(436 (#t 44 #t 8986 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(437 (#t 44 #f 776 #t 8986 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(438 (#t 44 #t 32 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(439 (#t 44 #f 776 #t 32 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(440 (#t 44 #f 173 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(441 (#t 44 #f 776 #f 173 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(442 (#t 44 #f 768 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]")
(443 (#t 44 #f 776 #f 768 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(444 (#t 44 #f 8205 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]")
(445 (#t 44 #f 776 #f 8205 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(446 (#t 44 #t 97 #f 8288 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(447 (#t 44 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(448 (#t 44 #t 97 #t 58 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(449 (#t 44 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(450 (#t 44 #t 97 #t 39 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(451 (#t 44 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(452 (#t 44 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(453 (#t 44 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(454 (#t 44 #t 97 #t 44 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(455 (#t 44 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(456 (#t 44 #t 49 #t 58 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(457 (#t 44 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(458 (#t 44 #t 49 #t 39 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(459 (#t 44 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(460 (#t 44 #t 49 #t 44 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(461 (#t 44 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(462 (#t 44 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(463 (#t 44 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(464 (#t 46 #t 1 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]")
(465 (#t 46 #f 776 #t 1 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(466 (#t 46 #t 13 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(467 (#t 46 #f 776 #t 13 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(468 (#t 46 #t 10 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(469 (#t 46 #f 776 #t 10 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(470 (#t 46 #t 11 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]")
(471 (#t 46 #f 776 #t 11 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(472 (#t 46 #t 12337 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(473 (#t 46 #f 776 #t 12337 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(474 (#t 46 #t 65 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(475 (#t 46 #f 776 #t 65 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(476 (#t 46 #t 58 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(477 (#t 46 #f 776 #t 58 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(478 (#t 46 #t 44 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(479 (#t 46 #f 776 #t 44 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(480 (#t 46 #t 46 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]")
(481 (#t 46 #f 776 #t 46 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(482 (#t 46 #t 48 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(483 (#t 46 #f 776 #t 48 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(484 (#t 46 #t 95 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]")
(485 (#t 46 #f 776 #t 95 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(486 (#t 46 #t 127462 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(487 (#t 46 #f 776 #t 127462 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(488 (#t 46 #t 1488 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(489 (#t 46 #f 776 #t 1488 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(490 (#t 46 #t 34 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(491 (#t 46 #f 776 #t 34 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(492 (#t 46 #t 39 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]")
(493 (#t 46 #f 776 #t 39 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(494 (#t 46 #t 8986 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(495 (#t 46 #f 776 #t 8986 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(496 (#t 46 #t 32 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(497 (#t 46 #f 776 #t 32 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(498 (#t 46 #f 173 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(499 (#t 46 #f 776 #f 173 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(500 (#t 46 #f 768 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(501 (#t 46 #f 776 #f 768 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(502 (#t 46 #f 8205 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]")
(503 (#t 46 #f 776 #f 8205 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(504 (#t 46 #t 97 #f 8288 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(505 (#t 46 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(506 (#t 46 #t 97 #t 58 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(507 (#t 46 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(508 (#t 46 #t 97 #t 39 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(509 (#t 46 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(510 (#t 46 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(511 (#t 46 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(512 (#t 46 #t 97 #t 44 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(513 (#t 46 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(514 (#t 46 #t 49 #t 58 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(515 (#t 46 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(516 (#t 46 #t 49 #t 39 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(517 (#t 46 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(518 (#t 46 #t 49 #t 44 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(519 (#t 46 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(520 (#t 46 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(521 (#t 46 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] FULL STOP (MidNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(522 (#t 48 #t 1 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]")
(523 (#t 48 #f 776 #t 1 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(524 (#t 48 #t 13 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(525 (#t 48 #f 776 #t 13 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(526 (#t 48 #t 10 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(527 (#t 48 #f 776 #t 10 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(528 (#t 48 #t 11 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]")
(529 (#t 48 #f 776 #t 11 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(530 (#t 48 #t 12337 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(531 (#t 48 #f 776 #t 12337 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(532 (#t 48 #f 65 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [10.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(533 (#t 48 #f 776 #f 65 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [10.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(534 (#t 48 #t 58 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(535 (#t 48 #f 776 #t 58 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(536 (#t 48 #t 44 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(537 (#t 48 #f 776 #t 44 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(538 (#t 48 #t 46 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]")
(539 (#t 48 #f 776 #t 46 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(540 (#t 48 #f 48 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [8.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(541 (#t 48 #f 776 #f 48 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [8.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(542 (#t 48 #f 95 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]")
(543 (#t 48 #f 776 #f 95 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(544 (#t 48 #t 127462 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(545 (#t 48 #f 776 #t 127462 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(546 (#t 48 #f 1488 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [10.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(547 (#t 48 #f 776 #f 1488 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [10.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(548 (#t 48 #t 34 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]")
(549 (#t 48 #f 776 #t 34 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(550 (#t 48 #t 39 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]")
(551 (#t 48 #f 776 #t 39 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(552 (#t 48 #t 8986 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(553 (#t 48 #f 776 #t 8986 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(554 (#t 48 #t 32 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(555 (#t 48 #f 776 #t 32 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(556 (#t 48 #f 173 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(557 (#t 48 #f 776 #f 173 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(558 (#t 48 #f 768 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(559 (#t 48 #f 776 #f 768 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(560 (#t 48 #f 8205 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]")
(561 (#t 48 #f 776 #f 8205 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(562 (#t 48 #f 97 #f 8288 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [10.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(563 (#t 48 #f 776 #f 97 #f 8288 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [10.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(564 (#t 48 #f 97 #t 58 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [10.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(565 (#t 48 #f 776 #f 97 #t 58 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [10.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(566 (#t 48 #f 97 #t 39 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [10.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(567 (#t 48 #f 776 #f 97 #t 39 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [10.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(568 (#t 48 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [10.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(569 (#t 48 #f 776 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [10.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(570 (#t 48 #f 97 #t 44 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [10.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(571 (#t 48 #f 776 #f 97 #t 44 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [10.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(572 (#t 48 #f 49 #t 58 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [8.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(573 (#t 48 #f 776 #f 49 #t 58 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [8.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(574 (#t 48 #f 49 #t 39 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [8.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(575 (#t 48 #f 776 #f 49 #t 39 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [8.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(576 (#t 48 #f 49 #t 44 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [8.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(577 (#t 48 #f 776 #f 49 #t 44 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [8.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(578 (#t 48 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [8.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(579 (#t 48 #f 776 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [4.0] COMBINING DIAERESIS (Extend_FE) × [8.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(580 (#t 95 #t 1 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]")
(581 (#t 95 #f 776 #t 1 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(582 (#t 95 #t 13 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(583 (#t 95 #f 776 #t 13 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(584 (#t 95 #t 10 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(585 (#t 95 #f 776 #t 10 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(586 (#t 95 #t 11 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]")
(587 (#t 95 #f 776 #t 11 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(588 (#t 95 #f 12337 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(589 (#t 95 #f 776 #f 12337 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(590 (#t 95 #f 65 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(591 (#t 95 #f 776 #f 65 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(592 (#t 95 #t 58 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(593 (#t 95 #f 776 #t 58 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(594 (#t 95 #t 44 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(595 (#t 95 #f 776 #t 44 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(596 (#t 95 #t 46 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]")
(597 (#t 95 #f 776 #t 46 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(598 (#t 95 #f 48 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] DIGIT ZERO (Numeric) ÷ [0.3]")
(599 (#t 95 #f 776 #f 48 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(600 (#t 95 #f 95 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]")
(601 (#t 95 #f 776 #f 95 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(602 (#t 95 #t 127462 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(603 (#t 95 #f 776 #t 127462 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(604 (#t 95 #f 1488 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(605 (#t 95 #f 776 #f 1488 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(606 (#t 95 #t 34 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(607 (#t 95 #f 776 #t 34 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(608 (#t 95 #t 39 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]")
(609 (#t 95 #f 776 #t 39 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(610 (#t 95 #t 8986 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(611 (#t 95 #f 776 #t 8986 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(612 (#t 95 #t 32 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(613 (#t 95 #f 776 #t 32 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(614 (#t 95 #f 173 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(615 (#t 95 #f 776 #f 173 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(616 (#t 95 #f 768 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(617 (#t 95 #f 776 #f 768 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(618 (#t 95 #f 8205 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]")
(619 (#t 95 #f 776 #f 8205 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(620 (#t 95 #f 97 #f 8288 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(621 (#t 95 #f 776 #f 97 #f 8288 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(622 (#t 95 #f 97 #t 58 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(623 (#t 95 #f 776 #f 97 #t 58 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(624 (#t 95 #f 97 #t 39 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(625 (#t 95 #f 776 #f 97 #t 39 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(626 (#t 95 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(627 (#t 95 #f 776 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(628 (#t 95 #f 97 #t 44 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(629 (#t 95 #f 776 #f 97 #t 44 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(630 (#t 95 #f 49 #t 58 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(631 (#t 95 #f 776 #f 49 #t 58 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(632 (#t 95 #f 49 #t 39 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(633 (#t 95 #f 776 #f 49 #t 39 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(634 (#t 95 #f 49 #t 44 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(635 (#t 95 #f 776 #f 49 #t 44 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(636 (#t 95 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(637 (#t 95 #f 776 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] LOW LINE (ExtendNumLet) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(638 (#t 127462 #t 1 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(639 (#t 127462 #f 776 #t 1 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(640 (#t 127462 #t 13 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(641 (#t 127462 #f 776 #t 13 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(642 (#t 127462 #t 10 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(643 (#t 127462 #f 776 #t 10 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(644 (#t 127462 #t 11 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(645 (#t 127462 #f 776 #t 11 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(646 (#t 127462 #t 12337 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(647 (#t 127462 #f 776 #t 12337 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(648 (#t 127462 #t 65 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(649 (#t 127462 #f 776 #t 65 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(650 (#t 127462 #t 58 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(651 (#t 127462 #f 776 #t 58 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(652 (#t 127462 #t 44 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(653 (#t 127462 #f 776 #t 44 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(654 (#t 127462 #t 46 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(655 (#t 127462 #f 776 #t 46 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(656 (#t 127462 #t 48 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(657 (#t 127462 #f 776 #t 48 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(658 (#t 127462 #t 95 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(659 (#t 127462 #f 776 #t 95 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(660 (#t 127462 #f 127462 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [15.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(661 (#t 127462 #f 776 #f 127462 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) × [15.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(662 (#t 127462 #t 1488 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(663 (#t 127462 #f 776 #t 1488 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(664 (#t 127462 #t 34 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(665 (#t 127462 #f 776 #t 34 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(666 (#t 127462 #t 39 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(667 (#t 127462 #f 776 #t 39 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(668 (#t 127462 #t 8986 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(669 (#t 127462 #f 776 #t 8986 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(670 (#t 127462 #t 32 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(671 (#t 127462 #f 776 #t 32 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(672 (#t 127462 #f 173 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(673 (#t 127462 #f 776 #f 173 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(674 (#t 127462 #f 768 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(675 (#t 127462 #f 776 #f 768 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(676 (#t 127462 #f 8205 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(677 (#t 127462 #f 776 #f 8205 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(678 (#t 127462 #t 97 #f 8288 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(679 (#t 127462 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(680 (#t 127462 #t 97 #t 58 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(681 (#t 127462 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(682 (#t 127462 #t 97 #t 39 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(683 (#t 127462 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(684 (#t 127462 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(685 (#t 127462 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(686 (#t 127462 #t 97 #t 44 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(687 (#t 127462 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(688 (#t 127462 #t 49 #t 58 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(689 (#t 127462 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(690 (#t 127462 #t 49 #t 39 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(691 (#t 127462 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(692 (#t 127462 #t 49 #t 44 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(693 (#t 127462 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(694 (#t 127462 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(695 (#t 127462 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(696 (#t 1488 #t 1 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(697 (#t 1488 #f 776 #t 1 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(698 (#t 1488 #t 13 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(699 (#t 1488 #f 776 #t 13 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(700 (#t 1488 #t 10 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(701 (#t 1488 #f 776 #t 10 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(702 (#t 1488 #t 11 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(703 (#t 1488 #f 776 #t 11 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(704 (#t 1488 #t 12337 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(705 (#t 1488 #f 776 #t 12337 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(706 (#t 1488 #f 65 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [5.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(707 (#t 1488 #f 776 #f 65 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(708 (#t 1488 #t 58 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(709 (#t 1488 #f 776 #t 58 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(710 (#t 1488 #t 44 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(711 (#t 1488 #f 776 #t 44 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(712 (#t 1488 #t 46 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(713 (#t 1488 #f 776 #t 46 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(714 (#t 1488 #f 48 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [9.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(715 (#t 1488 #f 776 #f 48 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(716 (#t 1488 #f 95 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(717 (#t 1488 #f 776 #f 95 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(718 (#t 1488 #t 127462 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(719 (#t 1488 #f 776 #t 127462 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(720 (#t 1488 #f 1488 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [5.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(721 (#t 1488 #f 776 #f 1488 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(722 (#t 1488 #t 34 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(723 (#t 1488 #f 776 #t 34 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(724 (#t 1488 #f 39 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [7.1] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(725 (#t 1488 #f 776 #f 39 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.1] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(726 (#t 1488 #t 8986 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(727 (#t 1488 #f 776 #t 8986 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(728 (#t 1488 #t 32 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(729 (#t 1488 #f 776 #t 32 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(730 (#t 1488 #f 173 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(731 (#t 1488 #f 776 #f 173 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(732 (#t 1488 #f 768 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(733 (#t 1488 #f 776 #f 768 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(734 (#t 1488 #f 8205 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(735 (#t 1488 #f 776 #f 8205 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(736 (#t 1488 #f 97 #f 8288 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [5.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(737 (#t 1488 #f 776 #f 97 #f 8288 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(738 (#t 1488 #f 97 #t 58 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(739 (#t 1488 #f 776 #f 97 #t 58 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(740 (#t 1488 #f 97 #t 39 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(741 (#t 1488 #f 776 #f 97 #t 39 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(742 (#t 1488 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(743 (#t 1488 #f 776 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(744 (#t 1488 #f 97 #t 44 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(745 (#t 1488 #f 776 #f 97 #t 44 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(746 (#t 1488 #f 49 #t 58 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(747 (#t 1488 #f 776 #f 49 #t 58 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(748 (#t 1488 #f 49 #t 39 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(749 (#t 1488 #f 776 #f 49 #t 39 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(750 (#t 1488 #f 49 #t 44 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(751 (#t 1488 #f 776 #f 49 #t 44 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(752 (#t 1488 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(753 (#t 1488 #f 776 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(754 (#t 34 #t 1 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(755 (#t 34 #f 776 #t 1 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(756 (#t 34 #t 13 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(757 (#t 34 #f 776 #t 13 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(758 (#t 34 #t 10 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(759 (#t 34 #f 776 #t 10 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(760 (#t 34 #t 11 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(761 (#t 34 #f 776 #t 11 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(762 (#t 34 #t 12337 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(763 (#t 34 #f 776 #t 12337 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(764 (#t 34 #t 65 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(765 (#t 34 #f 776 #t 65 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(766 (#t 34 #t 58 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(767 (#t 34 #f 776 #t 58 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(768 (#t 34 #t 44 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(769 (#t 34 #f 776 #t 44 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(770 (#t 34 #t 46 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(771 (#t 34 #f 776 #t 46 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(772 (#t 34 #t 48 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(773 (#t 34 #f 776 #t 48 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(774 (#t 34 #t 95 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(775 (#t 34 #f 776 #t 95 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(776 (#t 34 #t 127462 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(777 (#t 34 #f 776 #t 127462 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(778 (#t 34 #t 1488 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(779 (#t 34 #f 776 #t 1488 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(780 (#t 34 #t 34 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(781 (#t 34 #f 776 #t 34 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(782 (#t 34 #t 39 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(783 (#t 34 #f 776 #t 39 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(784 (#t 34 #t 8986 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(785 (#t 34 #f 776 #t 8986 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(786 (#t 34 #t 32 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(787 (#t 34 #f 776 #t 32 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(788 (#t 34 #f 173 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(789 (#t 34 #f 776 #f 173 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(790 (#t 34 #f 768 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(791 (#t 34 #f 776 #f 768 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(792 (#t 34 #f 8205 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(793 (#t 34 #f 776 #f 8205 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(794 (#t 34 #t 97 #f 8288 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(795 (#t 34 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(796 (#t 34 #t 97 #t 58 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(797 (#t 34 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(798 (#t 34 #t 97 #t 39 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(799 (#t 34 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(800 (#t 34 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(801 (#t 34 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(802 (#t 34 #t 97 #t 44 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(803 (#t 34 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(804 (#t 34 #t 49 #t 58 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(805 (#t 34 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(806 (#t 34 #t 49 #t 39 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(807 (#t 34 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(808 (#t 34 #t 49 #t 44 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(809 (#t 34 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(810 (#t 34 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(811 (#t 34 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] QUOTATION MARK (Double_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(812 (#t 39 #t 1 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(813 (#t 39 #f 776 #t 1 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(814 (#t 39 #t 13 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(815 (#t 39 #f 776 #t 13 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(816 (#t 39 #t 10 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(817 (#t 39 #f 776 #t 10 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(818 (#t 39 #t 11 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(819 (#t 39 #f 776 #t 11 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(820 (#t 39 #t 12337 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(821 (#t 39 #f 776 #t 12337 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(822 (#t 39 #t 65 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(823 (#t 39 #f 776 #t 65 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(824 (#t 39 #t 58 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(825 (#t 39 #f 776 #t 58 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(826 (#t 39 #t 44 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(827 (#t 39 #f 776 #t 44 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(828 (#t 39 #t 46 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]")
(829 (#t 39 #f 776 #t 46 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(830 (#t 39 #t 48 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(831 (#t 39 #f 776 #t 48 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(832 (#t 39 #t 95 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]")
(833 (#t 39 #f 776 #t 95 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(834 (#t 39 #t 127462 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(835 (#t 39 #f 776 #t 127462 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(836 (#t 39 #t 1488 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(837 (#t 39 #f 776 #t 1488 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(838 (#t 39 #t 34 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(839 (#t 39 #f 776 #t 34 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(840 (#t 39 #t 39 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(841 (#t 39 #f 776 #t 39 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(842 (#t 39 #t 8986 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(843 (#t 39 #f 776 #t 8986 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(844 (#t 39 #t 32 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(845 (#t 39 #f 776 #t 32 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(846 (#t 39 #f 173 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(847 (#t 39 #f 776 #f 173 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(848 (#t 39 #f 768 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(849 (#t 39 #f 776 #f 768 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(850 (#t 39 #f 8205 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]")
(851 (#t 39 #f 776 #f 8205 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(852 (#t 39 #t 97 #f 8288 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(853 (#t 39 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(854 (#t 39 #t 97 #t 58 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(855 (#t 39 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(856 (#t 39 #t 97 #t 39 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(857 (#t 39 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(858 (#t 39 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(859 (#t 39 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(860 (#t 39 #t 97 #t 44 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(861 (#t 39 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(862 (#t 39 #t 49 #t 58 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(863 (#t 39 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(864 (#t 39 #t 49 #t 39 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(865 (#t 39 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(866 (#t 39 #t 49 #t 44 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(867 (#t 39 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(868 (#t 39 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(869 (#t 39 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(870 (#t 8986 #t 1 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]")
(871 (#t 8986 #f 776 #t 1 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(872 (#t 8986 #t 13 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(873 (#t 8986 #f 776 #t 13 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(874 (#t 8986 #t 10 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(875 (#t 8986 #f 776 #t 10 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(876 (#t 8986 #t 11 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]")
(877 (#t 8986 #f 776 #t 11 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(878 (#t 8986 #t 12337 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(879 (#t 8986 #f 776 #t 12337 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(880 (#t 8986 #t 65 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]")
(881 (#t 8986 #f 776 #t 65 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(882 (#t 8986 #t 58 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(883 (#t 8986 #f 776 #t 58 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(884 (#t 8986 #t 44 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(885 (#t 8986 #f 776 #t 44 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(886 (#t 8986 #t 46 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]")
(887 (#t 8986 #f 776 #t 46 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(888 (#t 8986 #t 48 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(889 (#t 8986 #f 776 #t 48 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(890 (#t 8986 #t 95 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]")
(891 (#t 8986 #f 776 #t 95 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(892 (#t 8986 #t 127462 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(893 (#t 8986 #f 776 #t 127462 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(894 (#t 8986 #t 1488 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]")
(895 (#t 8986 #f 776 #t 1488 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(896 (#t 8986 #t 34 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]")
(897 (#t 8986 #f 776 #t 34 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(898 (#t 8986 #t 39 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]")
(899 (#t 8986 #f 776 #t 39 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(900 (#t 8986 #t 8986 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(901 (#t 8986 #f 776 #t 8986 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(902 (#t 8986 #t 32 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(903 (#t 8986 #f 776 #t 32 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(904 (#t 8986 #f 173 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(905 (#t 8986 #f 776 #f 173 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(906 (#t 8986 #f 768 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]")
(907 (#t 8986 #f 776 #f 768 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(908 (#t 8986 #f 8205 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]")
(909 (#t 8986 #f 776 #f 8205 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(910 (#t 8986 #t 97 #f 8288 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(911 (#t 8986 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(912 (#t 8986 #t 97 #t 58 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(913 (#t 8986 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(914 (#t 8986 #t 97 #t 39 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(915 (#t 8986 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(916 (#t 8986 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(917 (#t 8986 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(918 (#t 8986 #t 97 #t 44 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(919 (#t 8986 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(920 (#t 8986 #t 49 #t 58 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(921 (#t 8986 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(922 (#t 8986 #t 49 #t 39 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(923 (#t 8986 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(924 (#t 8986 #t 49 #t 44 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(925 (#t 8986 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(926 (#t 8986 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] WATCH (ExtPict) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(927 (#t 8986 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] WATCH (ExtPict) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(928 (#t 32 #t 1 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]")
(929 (#t 32 #f 776 #t 1 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(930 (#t 32 #t 13 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(931 (#t 32 #f 776 #t 13 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(932 (#t 32 #t 10 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(933 (#t 32 #f 776 #t 10 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(934 (#t 32 #t 11 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]")
(935 (#t 32 #f 776 #t 11 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(936 (#t 32 #t 12337 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(937 (#t 32 #f 776 #t 12337 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(938 (#t 32 #t 65 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]")
(939 (#t 32 #f 776 #t 65 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(940 (#t 32 #t 58 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(941 (#t 32 #f 776 #t 58 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(942 (#t 32 #t 44 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(943 (#t 32 #f 776 #t 44 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(944 (#t 32 #t 46 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]")
(945 (#t 32 #f 776 #t 46 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(946 (#t 32 #t 48 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(947 (#t 32 #f 776 #t 48 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(948 (#t 32 #t 95 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]")
(949 (#t 32 #f 776 #t 95 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(950 (#t 32 #t 127462 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(951 (#t 32 #f 776 #t 127462 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(952 (#t 32 #t 1488 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(953 (#t 32 #f 776 #t 1488 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(954 (#t 32 #t 34 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]")
(955 (#t 32 #f 776 #t 34 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(956 (#t 32 #t 39 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]")
(957 (#t 32 #f 776 #t 39 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(958 (#t 32 #t 8986 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(959 (#t 32 #f 776 #t 8986 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(960 (#t 32 #f 32 #t)
 "÷ [0.2] SPACE (WSegSpace) × [3.4] SPACE (WSegSpace) ÷ [0.3]")
(961 (#t 32 #f 776 #t 32 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(962 (#t 32 #f 173 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(963 (#t 32 #f 776 #f 173 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(964 (#t 32 #f 768 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]")
(965 (#t 32 #f 776 #f 768 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(966 (#t 32 #f 8205 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]")
(967 (#t 32 #f 776 #f 8205 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(968 (#t 32 #t 97 #f 8288 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(969 (#t 32 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(970 (#t 32 #t 97 #t 58 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(971 (#t 32 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(972 (#t 32 #t 97 #t 39 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(973 (#t 32 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(974 (#t 32 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(975 (#t 32 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(976 (#t 32 #t 97 #t 44 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(977 (#t 32 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(978 (#t 32 #t 49 #t 58 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(979 (#t 32 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(980 (#t 32 #t 49 #t 39 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(981 (#t 32 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(982 (#t 32 #t 49 #t 44 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(983 (#t 32 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(984 (#t 32 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] SPACE (WSegSpace) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(985 (#t 32 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(986 (#t 173 #t 1 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]")
(987 (#t 173 #f 776 #t 1 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(988 (#t 173 #t 13 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(989 (#t 173 #f 776 #t 13 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(990 (#t 173 #t 10 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(991 (#t 173 #f 776 #t 10 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(992 (#t 173 #t 11 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]")
(993 (#t 173 #f 776 #t 11 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(994 (#t 173 #t 12337 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(995 (#t 173 #f 776 #t 12337 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(996 (#t 173 #t 65 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(997 (#t 173 #f 776 #t 65 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(998 (#t 173 #t 58 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(999 (#t 173 #f 776 #t 58 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1000 (#t 173 #t 44 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(1001 (#t 173 #f 776 #t 44 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1002 (#t 173 #t 46 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]")
(1003 (#t 173 #f 776 #t 46 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1004 (#t 173 #t 48 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(1005 (#t 173 #f 776 #t 48 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1006 (#t 173 #t 95 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]")
(1007 (#t 173 #f 776 #t 95 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1008 (#t 173 #t 127462 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1009 (#t 173 #f 776 #t 127462 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1010 (#t 173 #t 1488 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1011 (#t 173 #f 776 #t 1488 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1012 (#t 173 #t 34 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1013 (#t 173 #f 776 #t 34 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1014 (#t 173 #t 39 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]")
(1015 (#t 173 #f 776 #t 39 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1016 (#t 173 #t 8986 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(1017 (#t 173 #f 776 #t 8986 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1018 (#t 173 #t 32 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(1019 (#t 173 #f 776 #t 32 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1020 (#t 173 #f 173 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(1021 (#t 173 #f 776 #f 173 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1022 (#t 173 #f 768 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1023 (#t 173 #f 776 #f 768 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1024 (#t 173 #f 8205 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]")
(1025 (#t 173 #f 776 #f 8205 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1026 (#t 173 #t 97 #f 8288 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1027 (#t 173 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1028 (#t 173 #t 97 #t 58 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1029 (#t 173 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1030 (#t 173 #t 97 #t 39 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1031 (#t 173 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1032 (#t 173 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1033 (#t 173 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1034 (#t 173 #t 97 #t 44 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1035 (#t 173 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1036 (#t 173 #t 49 #t 58 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1037 (#t 173 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1038 (#t 173 #t 49 #t 39 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1039 (#t 173 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1040 (#t 173 #t 49 #t 44 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1041 (#t 173 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1042 (#t 173 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1043 (#t 173 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1044 (#t 768 #t 1 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1045 (#t 768 #f 776 #t 1 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1046 (#t 768 #t 13 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1047 (#t 768 #f 776 #t 13 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1048 (#t 768 #t 10 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1049 (#t 768 #f 776 #t 10 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1050 (#t 768 #t 11 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1051 (#t 768 #f 776 #t 11 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1052 (#t 768 #t 12337 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1053 (#t 768 #f 776 #t 12337 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1054 (#t 768 #t 65 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1055 (#t 768 #f 776 #t 65 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1056 (#t 768 #t 58 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1057 (#t 768 #f 776 #t 58 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1058 (#t 768 #t 44 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(1059 (#t 768 #f 776 #t 44 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1060 (#t 768 #t 46 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1061 (#t 768 #f 776 #t 46 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1062 (#t 768 #t 48 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1063 (#t 768 #f 776 #t 48 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1064 (#t 768 #t 95 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1065 (#t 768 #f 776 #t 95 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1066 (#t 768 #t 127462 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1067 (#t 768 #f 776 #t 127462 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1068 (#t 768 #t 1488 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1069 (#t 768 #f 776 #t 1488 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1070 (#t 768 #t 34 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1071 (#t 768 #f 776 #t 34 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1072 (#t 768 #t 39 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1073 (#t 768 #f 776 #t 39 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1074 (#t 768 #t 8986 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]")
(1075 (#t 768 #f 776 #t 8986 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1076 (#t 768 #t 32 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1077 (#t 768 #f 776 #t 32 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1078 (#t 768 #f 173 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1079 (#t 768 #f 776 #f 173 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1080 (#t 768 #f 768 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1081 (#t 768 #f 776 #f 768 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1082 (#t 768 #f 8205 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1083 (#t 768 #f 776 #f 8205 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1084 (#t 768 #t 97 #f 8288 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1085 (#t 768 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1086 (#t 768 #t 97 #t 58 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1087 (#t 768 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1088 (#t 768 #t 97 #t 39 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1089 (#t 768 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1090 (#t 768 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1091 (#t 768 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1092 (#t 768 #t 97 #t 44 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1093 (#t 768 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1094 (#t 768 #t 49 #t 58 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1095 (#t 768 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1096 (#t 768 #t 49 #t 39 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1097 (#t 768 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1098 (#t 768 #t 49 #t 44 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1099 (#t 768 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1100 (#t 768 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1101 (#t 768 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1102 (#t 8205 #t 1 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1103 (#t 8205 #f 776 #t 1 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1104 (#t 8205 #t 13 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1105 (#t 8205 #f 776 #t 13 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1106 (#t 8205 #t 10 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]")
(1107 (#t 8205 #f 776 #t 10 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1108 (#t 8205 #t 11 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1109 (#t 8205 #f 776 #t 11 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1110 (#t 8205 #t 12337 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1111 (#t 8205 #f 776 #t 12337 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1112 (#t 8205 #t 65 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1113 (#t 8205 #f 776 #t 65 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1114 (#t 8205 #t 58 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]")
(1115 (#t 8205 #f 776 #t 58 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1116 (#t 8205 #t 44 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]")
(1117 (#t 8205 #f 776 #t 44 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1118 (#t 8205 #t 46 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]")
(1119 (#t 8205 #f 776 #t 46 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1120 (#t 8205 #t 48 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(1121 (#t 8205 #f 776 #t 48 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1122 (#t 8205 #t 95 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]")
(1123 (#t 8205 #f 776 #t 95 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1124 (#t 8205 #t 127462 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1125 (#t 8205 #f 776 #t 127462 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1126 (#t 8205 #t 1488 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1127 (#t 8205 #f 776 #t 1488 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1128 (#t 8205 #t 34 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1129 (#t 8205 #f 776 #t 34 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1130 (#t 8205 #t 39 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1131 (#t 8205 #f 776 #t 39 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1132 (#t 8205 #f 8986 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [3.3] WATCH (ExtPict) ÷ [0.3]")
(1133 (#t 8205 #f 776 #t 8986 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1134 (#t 8205 #t 32 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]")
(1135 (#t 8205 #f 776 #t 32 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1136 (#t 8205 #f 173 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(1137 (#t 8205 #f 776 #f 173 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1138 (#t 8205 #f 768 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1139 (#t 8205 #f 776 #f 768 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1140 (#t 8205 #f 8205 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1141 (#t 8205 #f 776 #f 8205 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1142 (#t 8205 #t 97 #f 8288 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1143 (#t 8205 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1144 (#t 8205 #t 97 #t 58 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1145 (#t 8205 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1146 (#t 8205 #t 97 #t 39 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1147 (#t 8205 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1148 (#t 8205 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1149 (#t 8205 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1150 (#t 8205 #t 97 #t 44 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1151 (#t 8205 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1152 (#t 8205 #t 49 #t 58 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1153 (#t 8205 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1154 (#t 8205 #t 49 #t 39 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1155 (#t 8205 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1156 (#t 8205 #t 49 #t 44 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1157 (#t 8205 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1158 (#t 8205 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1159 (#t 8205 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1160 (#t 97 #f 8288 #t 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1161 (#t 97 #f 8288 #f 776 #t 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1162 (#t 97 #f 8288 #t 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1163 (#t 97 #f 8288 #f 776 #t 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1164 (#t 97 #f 8288 #t 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1165 (#t 97 #f 8288 #f 776 #t 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1166 (#t 97 #f 8288 #t 11 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1167 (#t 97 #f 8288 #f 776 #t 11 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1168 (#t 97 #f 8288 #t 12337 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1169 (#t 97 #f 8288 #f 776 #t 12337 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1170 (#t 97 #f 8288 #f 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [5.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1171 (#t 97 #f 8288 #f 776 #f 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1172 (#t 97 #f 8288 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1173 (#t 97 #f 8288 #f 776 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1174 (#t 97 #f 8288 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1175 (#t 97 #f 8288 #f 776 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1176 (#t 97 #f 8288 #t 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1177 (#t 97 #f 8288 #f 776 #t 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1178 (#t 97 #f 8288 #f 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [9.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1179 (#t 97 #f 8288 #f 776 #f 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1180 (#t 97 #f 8288 #f 95 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1181 (#t 97 #f 8288 #f 776 #f 95 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1182 (#t 97 #f 8288 #t 127462 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1183 (#t 97 #f 8288 #f 776 #t 127462 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1184 (#t 97 #f 8288 #f 1488 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [5.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1185 (#t 97 #f 8288 #f 776 #f 1488 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1186 (#t 97 #f 8288 #t 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1187 (#t 97 #f 8288 #f 776 #t 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1188 (#t 97 #f 8288 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1189 (#t 97 #f 8288 #f 776 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1190 (#t 97 #f 8288 #t 8986 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1191 (#t 97 #f 8288 #f 776 #t 8986 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1192 (#t 97 #f 8288 #t 32 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1193 (#t 97 #f 8288 #f 776 #t 32 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1194 (#t 97 #f 8288 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1195 (#t 97 #f 8288 #f 776 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1196 (#t 97 #f 8288 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1197 (#t 97 #f 8288 #f 776 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1198 (#t 97 #f 8288 #f 8205 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1199 (#t 97 #f 8288 #f 776 #f 8205 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1200 (#t 97 #f 8288 #f 97 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [5.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1201 (#t 97 #f 8288 #f 776 #f 97 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1202 (#t 97 #f 8288 #f 97 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1203 (#t 97 #f 8288 #f 776 #f 97 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1204 (#t 97 #f 8288 #f 97 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1205 (#t 97 #f 8288 #f 776 #f 97 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1206 (#t 97 #f 8288 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1207 (#t 97 #f 8288 #f 776 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1208 (#t 97 #f 8288 #f 97 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1209 (#t 97 #f 8288 #f 776 #f 97 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1210 (#t 97 #f 8288 #f 49 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1211 (#t 97 #f 8288 #f 776 #f 49 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1212 (#t 97 #f 8288 #f 49 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1213 (#t 97 #f 8288 #f 776 #f 49 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1214 (#t 97 #f 8288 #f 49 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1215 (#t 97 #f 8288 #f 776 #f 49 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1216 (#t 97 #f 8288 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1217 (#t 97 #f 8288 #f 776 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [9.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1218 (#t 97 #t 58 #t 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1219 (#t 97 #t 58 #f 776 #t 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1220 (#t 97 #t 58 #t 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1221 (#t 97 #t 58 #f 776 #t 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1222 (#t 97 #t 58 #t 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1223 (#t 97 #t 58 #f 776 #t 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1224 (#t 97 #t 58 #t 11 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1225 (#t 97 #t 58 #f 776 #t 11 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1226 (#t 97 #t 58 #t 12337 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1227 (#t 97 #t 58 #f 776 #t 12337 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1228 (#t 97 #f 58 #f 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [7.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1229 (#t 97 #f 58 #f 776 #f 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1230 (#t 97 #t 58 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1231 (#t 97 #t 58 #f 776 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1232 (#t 97 #t 58 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1233 (#t 97 #t 58 #f 776 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1234 (#t 97 #t 58 #t 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1235 (#t 97 #t 58 #f 776 #t 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1236 (#t 97 #t 58 #t 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1237 (#t 97 #t 58 #f 776 #t 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1238 (#t 97 #t 58 #t 95 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1239 (#t 97 #t 58 #f 776 #t 95 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1240 (#t 97 #t 58 #t 127462 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1241 (#t 97 #t 58 #f 776 #t 127462 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1242 (#t 97 #f 58 #f 1488 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [7.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1243 (#t 97 #f 58 #f 776 #f 1488 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1244 (#t 97 #t 58 #t 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1245 (#t 97 #t 58 #f 776 #t 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1246 (#t 97 #t 58 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1247 (#t 97 #t 58 #f 776 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1248 (#t 97 #t 58 #t 8986 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1249 (#t 97 #t 58 #f 776 #t 8986 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1250 (#t 97 #t 58 #t 32 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1251 (#t 97 #t 58 #f 776 #t 32 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1252 (#t 97 #t 58 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1253 (#t 97 #t 58 #f 776 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1254 (#t 97 #t 58 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1255 (#t 97 #t 58 #f 776 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1256 (#t 97 #t 58 #f 8205 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1257 (#t 97 #t 58 #f 776 #f 8205 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1258 (#t 97 #f 58 #f 97 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [7.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1259 (#t 97 #f 58 #f 776 #f 97 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1260 (#t 97 #f 58 #f 97 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1261 (#t 97 #f 58 #f 776 #f 97 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1262 (#t 97 #f 58 #f 97 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1263 (#t 97 #f 58 #f 776 #f 97 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1264 (#t 97 #f 58 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1265 (#t 97 #f 58 #f 776 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1266 (#t 97 #f 58 #f 97 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1267 (#t 97 #f 58 #f 776 #f 97 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1268 (#t 97 #t 58 #t 49 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1269 (#t 97 #t 58 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1270 (#t 97 #t 58 #t 49 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1271 (#t 97 #t 58 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1272 (#t 97 #t 58 #t 49 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1273 (#t 97 #t 58 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1274 (#t 97 #t 58 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1275 (#t 97 #t 58 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1276 (#t 97 #t 39 #t 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1277 (#t 97 #t 39 #f 776 #t 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1278 (#t 97 #t 39 #t 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1279 (#t 97 #t 39 #f 776 #t 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1280 (#t 97 #t 39 #t 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1281 (#t 97 #t 39 #f 776 #t 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1282 (#t 97 #t 39 #t 11 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1283 (#t 97 #t 39 #f 776 #t 11 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1284 (#t 97 #t 39 #t 12337 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1285 (#t 97 #t 39 #f 776 #t 12337 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1286 (#t 97 #f 39 #f 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [7.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1287 (#t 97 #f 39 #f 776 #f 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1288 (#t 97 #t 39 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1289 (#t 97 #t 39 #f 776 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1290 (#t 97 #t 39 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1291 (#t 97 #t 39 #f 776 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1292 (#t 97 #t 39 #t 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1293 (#t 97 #t 39 #f 776 #t 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1294 (#t 97 #t 39 #t 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1295 (#t 97 #t 39 #f 776 #t 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1296 (#t 97 #t 39 #t 95 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1297 (#t 97 #t 39 #f 776 #t 95 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1298 (#t 97 #t 39 #t 127462 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1299 (#t 97 #t 39 #f 776 #t 127462 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1300 (#t 97 #f 39 #f 1488 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [7.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1301 (#t 97 #f 39 #f 776 #f 1488 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1302 (#t 97 #t 39 #t 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1303 (#t 97 #t 39 #f 776 #t 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1304 (#t 97 #t 39 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1305 (#t 97 #t 39 #f 776 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1306 (#t 97 #t 39 #t 8986 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1307 (#t 97 #t 39 #f 776 #t 8986 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1308 (#t 97 #t 39 #t 32 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1309 (#t 97 #t 39 #f 776 #t 32 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1310 (#t 97 #t 39 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1311 (#t 97 #t 39 #f 776 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1312 (#t 97 #t 39 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1313 (#t 97 #t 39 #f 776 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1314 (#t 97 #t 39 #f 8205 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1315 (#t 97 #t 39 #f 776 #f 8205 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1316 (#t 97 #f 39 #f 97 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [7.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1317 (#t 97 #f 39 #f 776 #f 97 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1318 (#t 97 #f 39 #f 97 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1319 (#t 97 #f 39 #f 776 #f 97 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1320 (#t 97 #f 39 #f 97 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1321 (#t 97 #f 39 #f 776 #f 97 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1322 (#t 97 #f 39 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1323 (#t 97 #f 39 #f 776 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1324 (#t 97 #f 39 #f 97 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1325 (#t 97 #f 39 #f 776 #f 97 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1326 (#t 97 #t 39 #t 49 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1327 (#t 97 #t 39 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1328 (#t 97 #t 39 #t 49 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1329 (#t 97 #t 39 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1330 (#t 97 #t 39 #t 49 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1331 (#t 97 #t 39 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1332 (#t 97 #t 39 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1333 (#t 97 #t 39 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1334 (#t 97 #t 39 #f 8288 #t 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1335 (#t 97 #t 39 #f 8288 #f 776 #t 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1336 (#t 97 #t 39 #f 8288 #t 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1337 (#t 97 #t 39 #f 8288 #f 776 #t 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1338 (#t 97 #t 39 #f 8288 #t 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1339 (#t 97 #t 39 #f 8288 #f 776 #t 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1340 (#t 97 #t 39 #f 8288 #t 11 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1341 (#t 97 #t 39 #f 8288 #f 776 #t 11 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1342 (#t 97 #t 39 #f 8288 #t 12337 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1343 (#t 97 #t 39 #f 8288 #f 776 #t 12337 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1344 (#t 97 #f 39 #f 8288 #f 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [7.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1345 (#t 97 #f 39 #f 8288 #f 776 #f 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1346 (#t 97 #t 39 #f 8288 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1347 (#t 97 #t 39 #f 8288 #f 776 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1348 (#t 97 #t 39 #f 8288 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1349 (#t 97 #t 39 #f 8288 #f 776 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1350 (#t 97 #t 39 #f 8288 #t 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1351 (#t 97 #t 39 #f 8288 #f 776 #t 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1352 (#t 97 #t 39 #f 8288 #t 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1353 (#t 97 #t 39 #f 8288 #f 776 #t 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1354 (#t 97 #t 39 #f 8288 #t 95 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1355 (#t 97 #t 39 #f 8288 #f 776 #t 95 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1356 (#t 97 #t 39 #f 8288 #t 127462 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1357 (#t 97 #t 39 #f 8288 #f 776 #t 127462 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1358 (#t 97 #f 39 #f 8288 #f 1488 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [7.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1359 (#t 97 #f 39 #f 8288 #f 776 #f 1488 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1360 (#t 97 #t 39 #f 8288 #t 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1361 (#t 97 #t 39 #f 8288 #f 776 #t 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1362 (#t 97 #t 39 #f 8288 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1363 (#t 97 #t 39 #f 8288 #f 776 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1364 (#t 97 #t 39 #f 8288 #t 8986 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1365 (#t 97 #t 39 #f 8288 #f 776 #t 8986 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1366 (#t 97 #t 39 #f 8288 #t 32 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1367 (#t 97 #t 39 #f 8288 #f 776 #t 32 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1368 (#t 97 #t 39 #f 8288 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1369 (#t 97 #t 39 #f 8288 #f 776 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1370 (#t 97 #t 39 #f 8288 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1371 (#t 97 #t 39 #f 8288 #f 776 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1372 (#t 97 #t 39 #f 8288 #f 8205 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1373 (#t 97 #t 39 #f 8288 #f 776 #f 8205 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1374 (#t 97 #f 39 #f 8288 #f 97 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [7.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1375 (#t 97 #f 39 #f 8288 #f 776 #f 97 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1376 (#t 97 #f 39 #f 8288 #f 97 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1377 (#t 97 #f 39 #f 8288 #f 776 #f 97 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1378 (#t 97 #f 39 #f 8288 #f 97 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1379 (#t 97 #f 39 #f 8288 #f 776 #f 97 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1380 (#t 97 #f 39 #f 8288 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1381 (#t 97 #f 39 #f 8288 #f 776 #f 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1382 (#t 97 #f 39 #f 8288 #f 97 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1383 (#t 97 #f 39 #f 8288 #f 776 #f 97 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [6.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [7.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1384 (#t 97 #t 39 #f 8288 #t 49 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1385 (#t 97 #t 39 #f 8288 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1386 (#t 97 #t 39 #f 8288 #t 49 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1387 (#t 97 #t 39 #f 8288 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1388 (#t 97 #t 39 #f 8288 #t 49 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1389 (#t 97 #t 39 #f 8288 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1390 (#t 97 #t 39 #f 8288 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1391 (#t 97 #t 39 #f 8288 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1392 (#t 97 #t 44 #t 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1393 (#t 97 #t 44 #f 776 #t 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1394 (#t 97 #t 44 #t 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1395 (#t 97 #t 44 #f 776 #t 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1396 (#t 97 #t 44 #t 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1397 (#t 97 #t 44 #f 776 #t 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1398 (#t 97 #t 44 #t 11 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1399 (#t 97 #t 44 #f 776 #t 11 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1400 (#t 97 #t 44 #t 12337 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1401 (#t 97 #t 44 #f 776 #t 12337 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1402 (#t 97 #t 44 #t 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1403 (#t 97 #t 44 #f 776 #t 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1404 (#t 97 #t 44 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1405 (#t 97 #t 44 #f 776 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1406 (#t 97 #t 44 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1407 (#t 97 #t 44 #f 776 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1408 (#t 97 #t 44 #t 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1409 (#t 97 #t 44 #f 776 #t 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1410 (#t 97 #t 44 #t 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1411 (#t 97 #t 44 #f 776 #t 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1412 (#t 97 #t 44 #t 95 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1413 (#t 97 #t 44 #f 776 #t 95 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1414 (#t 97 #t 44 #t 127462 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1415 (#t 97 #t 44 #f 776 #t 127462 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1416 (#t 97 #t 44 #t 1488 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1417 (#t 97 #t 44 #f 776 #t 1488 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1418 (#t 97 #t 44 #t 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1419 (#t 97 #t 44 #f 776 #t 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1420 (#t 97 #t 44 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1421 (#t 97 #t 44 #f 776 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1422 (#t 97 #t 44 #t 8986 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1423 (#t 97 #t 44 #f 776 #t 8986 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1424 (#t 97 #t 44 #t 32 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1425 (#t 97 #t 44 #f 776 #t 32 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1426 (#t 97 #t 44 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1427 (#t 97 #t 44 #f 776 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1428 (#t 97 #t 44 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1429 (#t 97 #t 44 #f 776 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1430 (#t 97 #t 44 #f 8205 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1431 (#t 97 #t 44 #f 776 #f 8205 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1432 (#t 97 #t 44 #t 97 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1433 (#t 97 #t 44 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1434 (#t 97 #t 44 #t 97 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1435 (#t 97 #t 44 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1436 (#t 97 #t 44 #t 97 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1437 (#t 97 #t 44 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1438 (#t 97 #t 44 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1439 (#t 97 #t 44 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1440 (#t 97 #t 44 #t 97 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1441 (#t 97 #t 44 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1442 (#t 97 #t 44 #t 49 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1443 (#t 97 #t 44 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1444 (#t 97 #t 44 #t 49 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1445 (#t 97 #t 44 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1446 (#t 97 #t 44 #t 49 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1447 (#t 97 #t 44 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1448 (#t 97 #t 44 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1449 (#t 97 #t 44 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1450 (#t 49 #t 58 #t 1 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1451 (#t 49 #t 58 #f 776 #t 1 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1452 (#t 49 #t 58 #t 13 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1453 (#t 49 #t 58 #f 776 #t 13 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1454 (#t 49 #t 58 #t 10 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1455 (#t 49 #t 58 #f 776 #t 10 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1456 (#t 49 #t 58 #t 11 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1457 (#t 49 #t 58 #f 776 #t 11 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1458 (#t 49 #t 58 #t 12337 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1459 (#t 49 #t 58 #f 776 #t 12337 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1460 (#t 49 #t 58 #t 65 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1461 (#t 49 #t 58 #f 776 #t 65 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1462 (#t 49 #t 58 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1463 (#t 49 #t 58 #f 776 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1464 (#t 49 #t 58 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1465 (#t 49 #t 58 #f 776 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1466 (#t 49 #t 58 #t 46 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1467 (#t 49 #t 58 #f 776 #t 46 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1468 (#t 49 #t 58 #t 48 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1469 (#t 49 #t 58 #f 776 #t 48 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1470 (#t 49 #t 58 #t 95 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1471 (#t 49 #t 58 #f 776 #t 95 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1472 (#t 49 #t 58 #t 127462 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1473 (#t 49 #t 58 #f 776 #t 127462 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1474 (#t 49 #t 58 #t 1488 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1475 (#t 49 #t 58 #f 776 #t 1488 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1476 (#t 49 #t 58 #t 34 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1477 (#t 49 #t 58 #f 776 #t 34 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1478 (#t 49 #t 58 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1479 (#t 49 #t 58 #f 776 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1480 (#t 49 #t 58 #t 8986 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1481 (#t 49 #t 58 #f 776 #t 8986 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1482 (#t 49 #t 58 #t 32 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1483 (#t 49 #t 58 #f 776 #t 32 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1484 (#t 49 #t 58 #f 173 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1485 (#t 49 #t 58 #f 776 #f 173 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1486 (#t 49 #t 58 #f 768 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1487 (#t 49 #t 58 #f 776 #f 768 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1488 (#t 49 #t 58 #f 8205 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1489 (#t 49 #t 58 #f 776 #f 8205 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1490 (#t 49 #t 58 #t 97 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1491 (#t 49 #t 58 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1492 (#t 49 #t 58 #t 97 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1493 (#t 49 #t 58 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1494 (#t 49 #t 58 #t 97 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1495 (#t 49 #t 58 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1496 (#t 49 #t 58 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1497 (#t 49 #t 58 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1498 (#t 49 #t 58 #t 97 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1499 (#t 49 #t 58 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1500 (#t 49 #t 58 #t 49 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1501 (#t 49 #t 58 #f 776 #t 49 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1502 (#t 49 #t 58 #t 49 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1503 (#t 49 #t 58 #f 776 #t 49 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1504 (#t 49 #t 58 #t 49 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1505 (#t 49 #t 58 #f 776 #t 49 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1506 (#t 49 #t 58 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1507 (#t 49 #t 58 #f 776 #t 49 #t 46 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1508 (#t 49 #t 39 #t 1 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1509 (#t 49 #t 39 #f 776 #t 1 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1510 (#t 49 #t 39 #t 13 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1511 (#t 49 #t 39 #f 776 #t 13 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1512 (#t 49 #t 39 #t 10 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1513 (#t 49 #t 39 #f 776 #t 10 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1514 (#t 49 #t 39 #t 11 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1515 (#t 49 #t 39 #f 776 #t 11 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1516 (#t 49 #t 39 #t 12337 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1517 (#t 49 #t 39 #f 776 #t 12337 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1518 (#t 49 #t 39 #t 65 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1519 (#t 49 #t 39 #f 776 #t 65 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1520 (#t 49 #t 39 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1521 (#t 49 #t 39 #f 776 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1522 (#t 49 #t 39 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1523 (#t 49 #t 39 #f 776 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1524 (#t 49 #t 39 #t 46 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1525 (#t 49 #t 39 #f 776 #t 46 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1526 (#t 49 #f 39 #f 48 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] APOSTROPHE (Single_Quote) × [11.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1527 (#t 49 #f 39 #f 776 #f 48 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1528 (#t 49 #t 39 #t 95 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1529 (#t 49 #t 39 #f 776 #t 95 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1530 (#t 49 #t 39 #t 127462 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1531 (#t 49 #t 39 #f 776 #t 127462 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1532 (#t 49 #t 39 #t 1488 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1533 (#t 49 #t 39 #f 776 #t 1488 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1534 (#t 49 #t 39 #t 34 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1535 (#t 49 #t 39 #f 776 #t 34 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1536 (#t 49 #t 39 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1537 (#t 49 #t 39 #f 776 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1538 (#t 49 #t 39 #t 8986 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1539 (#t 49 #t 39 #f 776 #t 8986 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1540 (#t 49 #t 39 #t 32 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1541 (#t 49 #t 39 #f 776 #t 32 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1542 (#t 49 #t 39 #f 173 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1543 (#t 49 #t 39 #f 776 #f 173 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1544 (#t 49 #t 39 #f 768 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1545 (#t 49 #t 39 #f 776 #f 768 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1546 (#t 49 #t 39 #f 8205 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1547 (#t 49 #t 39 #f 776 #f 8205 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1548 (#t 49 #t 39 #t 97 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1549 (#t 49 #t 39 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1550 (#t 49 #t 39 #t 97 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1551 (#t 49 #t 39 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1552 (#t 49 #t 39 #t 97 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1553 (#t 49 #t 39 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1554 (#t 49 #t 39 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1555 (#t 49 #t 39 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1556 (#t 49 #t 39 #t 97 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1557 (#t 49 #t 39 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1558 (#t 49 #f 39 #f 49 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] APOSTROPHE (Single_Quote) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1559 (#t 49 #f 39 #f 776 #f 49 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1560 (#t 49 #f 39 #f 49 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] APOSTROPHE (Single_Quote) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1561 (#t 49 #f 39 #f 776 #f 49 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1562 (#t 49 #f 39 #f 49 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] APOSTROPHE (Single_Quote) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1563 (#t 49 #f 39 #f 776 #f 49 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1564 (#t 49 #f 39 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] APOSTROPHE (Single_Quote) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1565 (#t 49 #f 39 #f 776 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] APOSTROPHE (Single_Quote) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1566 (#t 49 #t 44 #t 1 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1567 (#t 49 #t 44 #f 776 #t 1 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1568 (#t 49 #t 44 #t 13 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1569 (#t 49 #t 44 #f 776 #t 13 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1570 (#t 49 #t 44 #t 10 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1571 (#t 49 #t 44 #f 776 #t 10 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1572 (#t 49 #t 44 #t 11 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1573 (#t 49 #t 44 #f 776 #t 11 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1574 (#t 49 #t 44 #t 12337 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1575 (#t 49 #t 44 #f 776 #t 12337 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1576 (#t 49 #t 44 #t 65 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1577 (#t 49 #t 44 #f 776 #t 65 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1578 (#t 49 #t 44 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1579 (#t 49 #t 44 #f 776 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1580 (#t 49 #t 44 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1581 (#t 49 #t 44 #f 776 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1582 (#t 49 #t 44 #t 46 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1583 (#t 49 #t 44 #f 776 #t 46 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1584 (#t 49 #f 44 #f 48 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] COMMA (MidNum) × [11.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1585 (#t 49 #f 44 #f 776 #f 48 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1586 (#t 49 #t 44 #t 95 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1587 (#t 49 #t 44 #f 776 #t 95 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1588 (#t 49 #t 44 #t 127462 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1589 (#t 49 #t 44 #f 776 #t 127462 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1590 (#t 49 #t 44 #t 1488 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1591 (#t 49 #t 44 #f 776 #t 1488 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1592 (#t 49 #t 44 #t 34 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1593 (#t 49 #t 44 #f 776 #t 34 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1594 (#t 49 #t 44 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1595 (#t 49 #t 44 #f 776 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1596 (#t 49 #t 44 #t 8986 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1597 (#t 49 #t 44 #f 776 #t 8986 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1598 (#t 49 #t 44 #t 32 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1599 (#t 49 #t 44 #f 776 #t 32 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1600 (#t 49 #t 44 #f 173 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1601 (#t 49 #t 44 #f 776 #f 173 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1602 (#t 49 #t 44 #f 768 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1603 (#t 49 #t 44 #f 776 #f 768 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1604 (#t 49 #t 44 #f 8205 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1605 (#t 49 #t 44 #f 776 #f 8205 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1606 (#t 49 #t 44 #t 97 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1607 (#t 49 #t 44 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1608 (#t 49 #t 44 #t 97 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1609 (#t 49 #t 44 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1610 (#t 49 #t 44 #t 97 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1611 (#t 49 #t 44 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1612 (#t 49 #t 44 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1613 (#t 49 #t 44 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1614 (#t 49 #t 44 #t 97 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1615 (#t 49 #t 44 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1616 (#t 49 #f 44 #f 49 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] COMMA (MidNum) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1617 (#t 49 #f 44 #f 776 #f 49 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1618 (#t 49 #f 44 #f 49 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] COMMA (MidNum) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1619 (#t 49 #f 44 #f 776 #f 49 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1620 (#t 49 #f 44 #f 49 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] COMMA (MidNum) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1621 (#t 49 #f 44 #f 776 #f 49 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1622 (#t 49 #f 44 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] COMMA (MidNum) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1623 (#t 49 #f 44 #f 776 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] COMMA (MidNum) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1624 (#t 49 #t 46 #f 8288 #t 1 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1625 (#t 49 #t 46 #f 8288 #f 776 #t 1 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1626 (#t 49 #t 46 #f 8288 #t 13 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1627 (#t 49 #t 46 #f 8288 #f 776 #t 13 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(1628 (#t 49 #t 46 #f 8288 #t 10 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1629 (#t 49 #t 46 #f 8288 #f 776 #t 10 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(1630 (#t 49 #t 46 #f 8288 #t 11 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1631 (#t 49 #t 46 #f 8288 #f 776 #t 11 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [3.2] <LINE TABULATION> (Newline) ÷ [0.3]"
 )
(1632 (#t 49 #t 46 #f 8288 #t 12337 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1633 (#t 49 #t 46 #f 8288 #f 776 #t 12337 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1634 (#t 49 #t 46 #f 8288 #t 65 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1635 (#t 49 #t 46 #f 8288 #f 776 #t 65 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1636 (#t 49 #t 46 #f 8288 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1637 (#t 49 #t 46 #f 8288 #f 776 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1638 (#t 49 #t 46 #f 8288 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1639 (#t 49 #t 46 #f 8288 #f 776 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1640 (#t 49 #t 46 #f 8288 #t 46 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1641 (#t 49 #t 46 #f 8288 #f 776 #t 46 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] FULL STOP (MidNumLet) ÷ [0.3]"
 )
(1642 (#t 49 #f 46 #f 8288 #f 48 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [11.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1643 (#t 49 #f 46 #f 8288 #f 776 #f 48 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1644 (#t 49 #t 46 #f 8288 #t 95 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1645 (#t 49 #t 46 #f 8288 #f 776 #t 95 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1646 (#t 49 #t 46 #f 8288 #t 127462 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1647 (#t 49 #t 46 #f 8288 #f 776 #t 127462 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) ÷ [0.3]"
 )
(1648 (#t 49 #t 46 #f 8288 #t 1488 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1649 (#t 49 #t 46 #f 8288 #f 776 #t 1488 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1650 (#t 49 #t 46 #f 8288 #t 34 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1651 (#t 49 #t 46 #f 8288 #f 776 #t 34 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] QUOTATION MARK (Double_Quote) ÷ [0.3]"
 )
(1652 (#t 49 #t 46 #f 8288 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1653 (#t 49 #t 46 #f 8288 #f 776 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1654 (#t 49 #t 46 #f 8288 #t 8986 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1655 (#t 49 #t 46 #f 8288 #f 776 #t 8986 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] WATCH (ExtPict) ÷ [0.3]"
 )
(1656 (#t 49 #t 46 #f 8288 #t 32 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1657 (#t 49 #t 46 #f 8288 #f 776 #t 32 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1658 (#t 49 #t 46 #f 8288 #f 173 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1659 (#t 49 #t 46 #f 8288 #f 776 #f 173 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(1660 (#t 49 #t 46 #f 8288 #f 768 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1661 (#t 49 #t 46 #f 8288 #f 776 #f 768 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(1662 (#t 49 #t 46 #f 8288 #f 8205 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1663 (#t 49 #t 46 #f 8288 #f 776 #f 8205 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [0.3]"
 )
(1664 (#t 49 #t 46 #f 8288 #t 97 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1665 (#t 49 #t 46 #f 8288 #f 776 #t 97 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1666 (#t 49 #t 46 #f 8288 #t 97 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1667 (#t 49 #t 46 #f 8288 #f 776 #t 97 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1668 (#t 49 #t 46 #f 8288 #t 97 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1669 (#t 49 #t 46 #f 8288 #f 776 #t 97 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1670 (#t 49 #t 46 #f 8288 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1671 (#t 49 #t 46 #f 8288 #f 776 #t 97 #t 39 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] APOSTROPHE (Single_Quote) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1672 (#t 49 #t 46 #f 8288 #t 97 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1673 (#t 49 #t 46 #f 8288 #f 776 #t 97 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1674 (#t 49 #f 46 #f 8288 #f 49 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1675 (#t 49 #f 46 #f 8288 #f 776 #f 49 #t 58 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [0.3]"
 )
(1676 (#t 49 #f 46 #f 8288 #f 49 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1677 (#t 49 #f 46 #f 8288 #f 776 #f 49 #t 39 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1678 (#t 49 #f 46 #f 8288 #f 49 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1679 (#t 49 #f 46 #f 8288 #f 776 #f 49 #t 44 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [0.3]"
 )
(1680 (#t 49 #f 46 #f 8288 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1681 (#t 49 #f 46 #f 8288 #f 776 #f 49 #t 46 #f 8288 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [12.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [11.0] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) × [4.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(1682 (#t 13 #f 10 #t 97 #t 10 #t 776 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) × [3.0] <LINE FEED (LF)> (LF) ÷ [3.1] LATIN SMALL LETTER A (ALetter) ÷ [3.2] <LINE FEED (LF)> (LF) ÷ [3.1] COMBINING DIAERESIS (Extend_FE) ÷ [0.3]"
 )
(1683 (#t 97 #f 776 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [0.3]"
 )
(1684 (#t 32 #f 8205 #t 1606 #t)
 "÷ [0.2] SPACE (WSegSpace) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] ARABIC LETTER NOON (ALetter) ÷ [0.3]"
 )
(1685 (#t 1606 #f 8205 #t 32 #t)
 "÷ [0.2] ARABIC LETTER NOON (ALetter) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] SPACE (WSegSpace) ÷ [0.3]"
 )
(1686 (#t 65 #f 65 #f 65 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [5.0] LATIN CAPITAL LETTER A (ALetter) × [5.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1687 (#t 65 #f 58 #f 65 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [6.0] COLON (MidLetter) × [7.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1688 (#t 65 #t 58 #t 58 #t 65 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1689 (#t 1488 #f 39 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [7.1] APOSTROPHE (Single_Quote) ÷ [0.3]"
 )
(1690 (#t 1488 #f 34 #f 1488 #t)
 "÷ [0.2] HEBREW LETTER ALEF (Hebrew_Letter) × [7.2] QUOTATION MARK (Double_Quote) × [7.3] HEBREW LETTER ALEF (Hebrew_Letter) ÷ [0.3]"
 )
(1691 (#t 65 #f 48 #f 48 #f 65 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [9.0] DIGIT ZERO (Numeric) × [8.0] DIGIT ZERO (Numeric) × [10.0] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1692 (#t 48 #f 44 #f 48 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [12.0] COMMA (MidNum) × [11.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1693 (#t 48 #t 44 #t 44 #t 48 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(1694 (#t 12337 #f 12337 #t)
 "÷ [0.2] VERTICAL KANA REPEAT MARK (Katakana) × [13.0] VERTICAL KANA REPEAT MARK (Katakana) ÷ [0.3]"
 )
(1695 (#t 65 #f 95 #f 48 #f 95 #f 12337 #f 95 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ZERO (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] VERTICAL KANA REPEAT MARK (Katakana) × [13.1] LOW LINE (ExtendNumLet) ÷ [0.3]"
 )
(1696 (#t 65 #f 95 #f 95 #f 65 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN CAPITAL LETTER A (ALetter) ÷ [0.3]"
 )
(1697 (#t 127462 #f 127463 #t 127464 #t 98 #t)
 "÷ [0.2] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [15.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (ALetter) ÷ [0.3]"
 )
(1698 (#t 97 #t 127462 #f 127463 #t 127464 #t 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [16.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (ALetter) ÷ [0.3]"
 )
(1699 (#t 97 #t 127462 #f 127463 #f 8205 #t 127464 #t 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [16.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (ALetter) ÷ [0.3]"
 )
(1700 (#t 97 #t 127462 #f 8205 #f 127463 #t 127464 #t 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) × [16.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) ÷ [999.0] LATIN SMALL LETTER B (ALetter) ÷ [0.3]"
 )
(1701 (#t 97 #t 127462 #f 127463 #t 127464 #f 127465 #t 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER A (RI) × [16.0] REGIONAL INDICATOR SYMBOL LETTER B (RI) ÷ [999.0] REGIONAL INDICATOR SYMBOL LETTER C (RI) × [16.0] REGIONAL INDICATOR SYMBOL LETTER D (RI) ÷ [999.0] LATIN SMALL LETTER B (ALetter) ÷ [0.3]"
 )
(1702 (#t 128118 #f 127999 #t 128118 #t)
 "÷ [0.2] BABY (ExtPict) × [4.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend_FE) ÷ [999.0] BABY (ExtPict) ÷ [0.3]"
 )
(1703 (#t 128721 #f 8205 #f 128721 #t)
 "÷ [0.2] OCTAGONAL SIGN (ExtPict) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) × [3.3] OCTAGONAL SIGN (ExtPict) ÷ [0.3]"
 )
(1704 (#t 97 #f 8205 #f 128721 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) × [3.3] OCTAGONAL SIGN (ExtPict) ÷ [0.3]"
 )
(1705 (#t 9985 #f 8205 #f 9985 #t)
 "÷ [0.2] UPPER BLADE SCISSORS (Other) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) × [3.3] UPPER BLADE SCISSORS (Other) ÷ [0.3]"
 )
(1706 (#t 97 #f 8205 #f 9985 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) × [3.3] UPPER BLADE SCISSORS (Other) ÷ [0.3]"
 )
(1707 (#t 128118 #f 127999 #f 776 #f 8205 #f 128118 #f 127999 #t)
 "÷ [0.2] BABY (ExtPict) × [4.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) × [3.3] BABY (ExtPict) × [4.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend_FE) ÷ [0.3]"
 )
(1708 (#t 128721 #f 127999 #t)
 "÷ [0.2] OCTAGONAL SIGN (ExtPict) × [4.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend_FE) ÷ [0.3]"
 )
(1709 (#t 8205 #f 128721 #f 127999 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [3.3] OCTAGONAL SIGN (ExtPict) × [4.0] EMOJI MODIFIER FITZPATRICK TYPE-6 (Extend_FE) ÷ [0.3]"
 )
(1710 (#t 8205 #f 128721 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [3.3] OCTAGONAL SIGN (ExtPict) ÷ [0.3]")
(1711 (#t 8205 #f 128721 #t)
 "÷ [0.2] ZERO WIDTH JOINER (ZWJ_FE) × [3.3] OCTAGONAL SIGN (ExtPict) ÷ [0.3]")
(1712 (#t 128721 #t 128721 #t)
 "÷ [0.2] OCTAGONAL SIGN (ExtPict) ÷ [999.0] OCTAGONAL SIGN (ExtPict) ÷ [0.3]")
(1713 (#t 97 #f 776 #f 8205 #f 776 #f 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [4.0] COMBINING DIAERESIS (Extend_FE) × [4.0] ZERO WIDTH JOINER (ZWJ_FE) × [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] LATIN SMALL LETTER B (ALetter) ÷ [0.3]"
 )
(1714 (#t 97 #t 32 #f 32 #t 98 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] SPACE (WSegSpace) × [3.4] SPACE (WSegSpace) ÷ [999.0] LATIN SMALL LETTER B (ALetter) ÷ [0.3]"
 )
(1715 (#t 49 #t 58 #t 58 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1716 (#t 49 #f 95 #f 49 #t 58 #t 58 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1717 (#t 49 #f 95 #f 97 #t 58 #t 58 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1718 (#t 49 #t 58 #t 58 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1719 (#t 49 #f 95 #f 49 #t 58 #t 58 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1720 (#t 49 #f 95 #f 97 #t 58 #t 58 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1721 (#t 49 #t 58 #t 46 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1722 (#t 49 #f 95 #f 49 #t 58 #t 46 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1723 (#t 49 #f 95 #f 97 #t 58 #t 46 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1724 (#t 49 #t 58 #t 46 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1725 (#t 49 #f 95 #f 49 #t 58 #t 46 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1726 (#t 49 #f 95 #f 97 #t 58 #t 46 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1727 (#t 49 #t 58 #t 44 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1728 (#t 49 #f 95 #f 49 #t 58 #t 44 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1729 (#t 49 #f 95 #f 97 #t 58 #t 44 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1730 (#t 49 #t 58 #t 44 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1731 (#t 49 #f 95 #f 49 #t 58 #t 44 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1732 (#t 49 #f 95 #f 97 #t 58 #t 44 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1733 (#t 49 #t 46 #t 58 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1734 (#t 49 #f 95 #f 49 #t 46 #t 58 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1735 (#t 49 #f 95 #f 97 #t 46 #t 58 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1736 (#t 49 #t 46 #t 58 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1737 (#t 49 #f 95 #f 49 #t 46 #t 58 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1738 (#t 49 #f 95 #f 97 #t 46 #t 58 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1739 (#t 49 #t 46 #t 46 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1740 (#t 49 #f 95 #f 49 #t 46 #t 46 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1741 (#t 49 #f 95 #f 97 #t 46 #t 46 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1742 (#t 49 #t 46 #t 46 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1743 (#t 49 #f 95 #f 49 #t 46 #t 46 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1744 (#t 49 #f 95 #f 97 #t 46 #t 46 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1745 (#t 49 #t 46 #t 44 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1746 (#t 49 #f 95 #f 49 #t 46 #t 44 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1747 (#t 49 #f 95 #f 97 #t 46 #t 44 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1748 (#t 49 #t 46 #t 44 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1749 (#t 49 #f 95 #f 49 #t 46 #t 44 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1750 (#t 49 #f 95 #f 97 #t 46 #t 44 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1751 (#t 49 #t 44 #t 58 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1752 (#t 49 #f 95 #f 49 #t 44 #t 58 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1753 (#t 49 #f 95 #f 97 #t 44 #t 58 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1754 (#t 49 #t 44 #t 58 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1755 (#t 49 #f 95 #f 49 #t 44 #t 58 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1756 (#t 49 #f 95 #f 97 #t 44 #t 58 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1757 (#t 49 #t 44 #t 46 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1758 (#t 49 #f 95 #f 49 #t 44 #t 46 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1759 (#t 49 #f 95 #f 97 #t 44 #t 46 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1760 (#t 49 #t 44 #t 46 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1761 (#t 49 #f 95 #f 49 #t 44 #t 46 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1762 (#t 49 #f 95 #f 97 #t 44 #t 46 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1763 (#t 49 #t 44 #t 44 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1764 (#t 49 #f 95 #f 49 #t 44 #t 44 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1765 (#t 49 #f 95 #f 97 #t 44 #t 44 #t 49 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1766 (#t 49 #t 44 #t 44 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1767 (#t 49 #f 95 #f 49 #t 44 #t 44 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1768 (#t 49 #f 95 #f 97 #t 44 #t 44 #t 97 #t)
 "÷ [0.2] DIGIT ONE (Numeric) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1769 (#t 97 #t 58 #t 58 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1770 (#t 97 #f 95 #f 49 #t 58 #t 58 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1771 (#t 97 #f 95 #f 97 #t 58 #t 58 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1772 (#t 97 #t 58 #t 58 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1773 (#t 97 #f 95 #f 49 #t 58 #t 58 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1774 (#t 97 #f 95 #f 97 #t 58 #t 58 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1775 (#t 97 #t 58 #t 46 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1776 (#t 97 #f 95 #f 49 #t 58 #t 46 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1777 (#t 97 #f 95 #f 97 #t 58 #t 46 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1778 (#t 97 #t 58 #t 46 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1779 (#t 97 #f 95 #f 49 #t 58 #t 46 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1780 (#t 97 #f 95 #f 97 #t 58 #t 46 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1781 (#t 97 #t 58 #t 44 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1782 (#t 97 #f 95 #f 49 #t 58 #t 44 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1783 (#t 97 #f 95 #f 97 #t 58 #t 44 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1784 (#t 97 #t 58 #t 44 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1785 (#t 97 #f 95 #f 49 #t 58 #t 44 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1786 (#t 97 #f 95 #f 97 #t 58 #t 44 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COLON (MidLetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1787 (#t 97 #t 46 #t 58 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1788 (#t 97 #f 95 #f 49 #t 46 #t 58 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1789 (#t 97 #f 95 #f 97 #t 46 #t 58 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1790 (#t 97 #t 46 #t 58 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1791 (#t 97 #f 95 #f 49 #t 46 #t 58 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1792 (#t 97 #f 95 #f 97 #t 46 #t 58 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1793 (#t 97 #t 46 #t 46 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1794 (#t 97 #f 95 #f 49 #t 46 #t 46 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1795 (#t 97 #f 95 #f 97 #t 46 #t 46 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1796 (#t 97 #t 46 #t 46 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1797 (#t 97 #f 95 #f 49 #t 46 #t 46 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1798 (#t 97 #f 95 #f 97 #t 46 #t 46 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1799 (#t 97 #t 46 #t 44 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1800 (#t 97 #f 95 #f 49 #t 46 #t 44 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1801 (#t 97 #f 95 #f 97 #t 46 #t 44 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1802 (#t 97 #t 46 #t 44 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1803 (#t 97 #f 95 #f 49 #t 46 #t 44 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1804 (#t 97 #f 95 #f 97 #t 46 #t 44 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1805 (#t 97 #t 44 #t 58 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1806 (#t 97 #f 95 #f 49 #t 44 #t 58 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1807 (#t 97 #f 95 #f 97 #t 44 #t 58 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1808 (#t 97 #t 44 #t 58 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1809 (#t 97 #f 95 #f 49 #t 44 #t 58 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1810 (#t 97 #f 95 #f 97 #t 44 #t 58 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COLON (MidLetter) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1811 (#t 97 #t 44 #t 46 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1812 (#t 97 #f 95 #f 49 #t 44 #t 46 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1813 (#t 97 #f 95 #f 97 #t 44 #t 46 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1814 (#t 97 #t 44 #t 46 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1815 (#t 97 #f 95 #f 49 #t 44 #t 46 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1816 (#t 97 #f 95 #f 97 #t 44 #t 46 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] FULL STOP (MidNumLet) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1817 (#t 97 #t 44 #t 44 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1818 (#t 97 #f 95 #f 49 #t 44 #t 44 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1819 (#t 97 #f 95 #f 97 #t 44 #t 44 #t 49 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] DIGIT ONE (Numeric) ÷ [0.3]"
 )
(1820 (#t 97 #t 44 #t 44 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1821 (#t 97 #f 95 #f 49 #t 44 #t 44 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] DIGIT ONE (Numeric) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
(1822 (#t 97 #f 95 #f 97 #t 44 #t 44 #t 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (ALetter) × [13.1] LOW LINE (ExtendNumLet) × [13.2] LATIN SMALL LETTER A (ALetter) ÷ [999.0] COMMA (MidNum) ÷ [999.0] COMMA (MidNum) ÷ [999.0] LATIN SMALL LETTER A (ALetter) ÷ [0.3]"
 )
))

;; auxiliary/SentenceBreakTest.txt
(define *sentence-break-tests* '(
(0 (#t 1 #f 1 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(1 (#t 1 #f 776 #f 1 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(2 (#t 1 #f 13 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(3 (#t 1 #f 776 #f 13 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(4 (#t 1 #f 10 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(5 (#t 1 #f 776 #f 10 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(6 (#t 1 #f 133 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]")
(7 (#t 1 #f 776 #f 133 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(8 (#t 1 #f 9 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(9 (#t 1 #f 776 #f 9 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(10 (#t 1 #f 97 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(11 (#t 1 #f 776 #f 97 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(12 (#t 1 #f 65 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(13 (#t 1 #f 776 #f 65 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(14 (#t 1 #f 443 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(15 (#t 1 #f 776 #f 443 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(16 (#t 1 #f 48 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(17 (#t 1 #f 776 #f 48 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(18 (#t 1 #f 46 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] FULL STOP (ATerm) ÷ [0.3]")
(19 (#t 1 #f 776 #f 46 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(20 (#t 1 #f 33 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(21 (#t 1 #f 776 #f 33 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(22 (#t 1 #f 34 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] QUOTATION MARK (Close) ÷ [0.3]")
(23 (#t 1 #f 776 #f 34 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(24 (#t 1 #f 44 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [998.0] COMMA (SContinue) ÷ [0.3]")
(25 (#t 1 #f 776 #f 44 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(26 (#t 1 #f 173 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(27 (#t 1 #f 776 #f 173 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(28 (#t 1 #f 768 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(29 (#t 1 #f 776 #f 768 #t)
 "÷ [0.2] <START OF HEADING> (Other) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(30 (#t 13 #t 1 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(31 (#t 13 #t 776 #f 1 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(32 (#t 13 #t 13 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(33 (#t 13 #t 776 #f 13 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(34 (#t 13 #f 10 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) × [3.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(35 (#t 13 #t 776 #f 10 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(36 (#t 13 #t 133 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]")
(37 (#t 13 #t 776 #f 133 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(38 (#t 13 #t 9 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(39 (#t 13 #t 776 #f 9 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(40 (#t 13 #t 97 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(41 (#t 13 #t 776 #f 97 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(42 (#t 13 #t 65 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(43 (#t 13 #t 776 #f 65 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(44 (#t 13 #t 443 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(45 (#t 13 #t 776 #f 443 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(46 (#t 13 #t 48 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(47 (#t 13 #t 776 #f 48 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(48 (#t 13 #t 46 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] FULL STOP (ATerm) ÷ [0.3]")
(49 (#t 13 #t 776 #f 46 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(50 (#t 13 #t 33 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] EXCLAMATION MARK (STerm) ÷ [0.3]")
(51 (#t 13 #t 776 #f 33 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(52 (#t 13 #t 34 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] QUOTATION MARK (Close) ÷ [0.3]")
(53 (#t 13 #t 776 #f 34 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(54 (#t 13 #t 44 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMMA (SContinue) ÷ [0.3]")
(55 (#t 13 #t 776 #f 44 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(56 (#t 13 #t 173 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(57 (#t 13 #t 776 #f 173 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(58 (#t 13 #t 768 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(59 (#t 13 #t 776 #f 768 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(60 (#t 10 #t 1 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <START OF HEADING> (Other) ÷ [0.3]")
(61 (#t 10 #t 776 #f 1 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(62 (#t 10 #t 13 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(63 (#t 10 #t 776 #f 13 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(64 (#t 10 #t 10 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(65 (#t 10 #t 776 #f 10 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(66 (#t 10 #t 133 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]")
(67 (#t 10 #t 776 #f 133 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(68 (#t 10 #t 9 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]")
(69 (#t 10 #t 776 #f 9 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(70 (#t 10 #t 97 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]")
(71 (#t 10 #t 776 #f 97 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(72 (#t 10 #t 65 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]")
(73 (#t 10 #t 776 #f 65 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(74 (#t 10 #t 443 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(75 (#t 10 #t 776 #f 443 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(76 (#t 10 #t 48 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(77 (#t 10 #t 776 #f 48 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(78 (#t 10 #t 46 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] FULL STOP (ATerm) ÷ [0.3]")
(79 (#t 10 #t 776 #f 46 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(80 (#t 10 #t 33 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] EXCLAMATION MARK (STerm) ÷ [0.3]")
(81 (#t 10 #t 776 #f 33 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(82 (#t 10 #t 34 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] QUOTATION MARK (Close) ÷ [0.3]")
(83 (#t 10 #t 776 #f 34 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(84 (#t 10 #t 44 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMMA (SContinue) ÷ [0.3]")
(85 (#t 10 #t 776 #f 44 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(86 (#t 10 #t 173 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(87 (#t 10 #t 776 #f 173 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(88 (#t 10 #t 768 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(89 (#t 10 #t 776 #f 768 #t)
 "÷ [0.2] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(90 (#t 133 #t 1 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] <START OF HEADING> (Other) ÷ [0.3]")
(91 (#t 133 #t 776 #f 1 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(92 (#t 133 #t 13 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(93 (#t 133 #t 776 #f 13 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(94 (#t 133 #t 10 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(95 (#t 133 #t 776 #f 10 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(96 (#t 133 #t 133 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]")
(97 (#t 133 #t 776 #f 133 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(98 (#t 133 #t 9 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]")
(99 (#t 133 #t 776 #f 9 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(100 (#t 133 #t 97 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]")
(101 (#t 133 #t 776 #f 97 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(102 (#t 133 #t 65 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(103 (#t 133 #t 776 #f 65 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(104 (#t 133 #t 443 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(105 (#t 133 #t 776 #f 443 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(106 (#t 133 #t 48 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(107 (#t 133 #t 776 #f 48 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(108 (#t 133 #t 46 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] FULL STOP (ATerm) ÷ [0.3]")
(109 (#t 133 #t 776 #f 46 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(110 (#t 133 #t 33 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] EXCLAMATION MARK (STerm) ÷ [0.3]")
(111 (#t 133 #t 776 #f 33 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(112 (#t 133 #t 34 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] QUOTATION MARK (Close) ÷ [0.3]")
(113 (#t 133 #t 776 #f 34 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(114 (#t 133 #t 44 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMMA (SContinue) ÷ [0.3]")
(115 (#t 133 #t 776 #f 44 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(116 (#t 133 #t 173 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(117 (#t 133 #t 776 #f 173 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(118 (#t 133 #t 768 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(119 (#t 133 #t 776 #f 768 #t)
 "÷ [0.2] <NEXT LINE (NEL)> (Sep) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(120 (#t 9 #f 1 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(121 (#t 9 #f 776 #f 1 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(122 (#t 9 #f 13 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(123 (#t 9 #f 776 #f 13 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(124 (#t 9 #f 10 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(125 (#t 9 #f 776 #f 10 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(126 (#t 9 #f 133 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(127 (#t 9 #f 776 #f 133 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(128 (#t 9 #f 9 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(129 (#t 9 #f 776 #f 9 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(130 (#t 9 #f 97 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(131 (#t 9 #f 776 #f 97 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(132 (#t 9 #f 65 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(133 (#t 9 #f 776 #f 65 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(134 (#t 9 #f 443 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(135 (#t 9 #f 776 #f 443 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(136 (#t 9 #f 48 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(137 (#t 9 #f 776 #f 48 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(138 (#t 9 #f 46 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] FULL STOP (ATerm) ÷ [0.3]")
(139 (#t 9 #f 776 #f 46 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(140 (#t 9 #f 33 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(141 (#t 9 #f 776 #f 33 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(142 (#t 9 #f 34 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] QUOTATION MARK (Close) ÷ [0.3]")
(143 (#t 9 #f 776 #f 34 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(144 (#t 9 #f 44 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [998.0] COMMA (SContinue) ÷ [0.3]")
(145 (#t 9 #f 776 #f 44 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(146 (#t 9 #f 173 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(147 (#t 9 #f 776 #f 173 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(148 (#t 9 #f 768 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(149 (#t 9 #f 776 #f 768 #t)
 "÷ [0.2] <CHARACTER TABULATION> (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(150 (#t 97 #f 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(151 (#t 97 #f 776 #f 1 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(152 (#t 97 #f 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(153 (#t 97 #f 776 #f 13 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(154 (#t 97 #f 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(155 (#t 97 #f 776 #f 10 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(156 (#t 97 #f 133 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(157 (#t 97 #f 776 #f 133 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(158 (#t 97 #f 9 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(159 (#t 97 #f 776 #f 9 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(160 (#t 97 #f 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(161 (#t 97 #f 776 #f 97 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(162 (#t 97 #f 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(163 (#t 97 #f 776 #f 65 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(164 (#t 97 #f 443 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(165 (#t 97 #f 776 #f 443 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(166 (#t 97 #f 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(167 (#t 97 #f 776 #f 48 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(168 (#t 97 #f 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] FULL STOP (ATerm) ÷ [0.3]")
(169 (#t 97 #f 776 #f 46 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(170 (#t 97 #f 33 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(171 (#t 97 #f 776 #f 33 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(172 (#t 97 #f 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(173 (#t 97 #f 776 #f 34 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(174 (#t 97 #f 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [998.0] COMMA (SContinue) ÷ [0.3]")
(175 (#t 97 #f 776 #f 44 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(176 (#t 97 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(177 (#t 97 #f 776 #f 173 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(178 (#t 97 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(179 (#t 97 #f 776 #f 768 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(180 (#t 65 #f 1 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(181 (#t 65 #f 776 #f 1 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(182 (#t 65 #f 13 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(183 (#t 65 #f 776 #f 13 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(184 (#t 65 #f 10 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(185 (#t 65 #f 776 #f 10 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(186 (#t 65 #f 133 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(187 (#t 65 #f 776 #f 133 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(188 (#t 65 #f 9 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(189 (#t 65 #f 776 #f 9 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(190 (#t 65 #f 97 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(191 (#t 65 #f 776 #f 97 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(192 (#t 65 #f 65 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(193 (#t 65 #f 776 #f 65 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(194 (#t 65 #f 443 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(195 (#t 65 #f 776 #f 443 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(196 (#t 65 #f 48 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(197 (#t 65 #f 776 #f 48 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(198 (#t 65 #f 46 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] FULL STOP (ATerm) ÷ [0.3]")
(199 (#t 65 #f 776 #f 46 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(200 (#t 65 #f 33 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(201 (#t 65 #f 776 #f 33 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(202 (#t 65 #f 34 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(203 (#t 65 #f 776 #f 34 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(204 (#t 65 #f 44 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [998.0] COMMA (SContinue) ÷ [0.3]")
(205 (#t 65 #f 776 #f 44 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(206 (#t 65 #f 173 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(207 (#t 65 #f 776 #f 173 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(208 (#t 65 #f 768 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(209 (#t 65 #f 776 #f 768 #t)
 "÷ [0.2] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(210 (#t 443 #f 1 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(211 (#t 443 #f 776 #f 1 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(212 (#t 443 #f 13 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(213 (#t 443 #f 776 #f 13 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(214 (#t 443 #f 10 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(215 (#t 443 #f 776 #f 10 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(216 (#t 443 #f 133 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(217 (#t 443 #f 776 #f 133 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(218 (#t 443 #f 9 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(219 (#t 443 #f 776 #f 9 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(220 (#t 443 #f 97 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(221 (#t 443 #f 776 #f 97 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(222 (#t 443 #f 65 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(223 (#t 443 #f 776 #f 65 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(224 (#t 443 #f 443 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(225 (#t 443 #f 776 #f 443 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(226 (#t 443 #f 48 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(227 (#t 443 #f 776 #f 48 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(228 (#t 443 #f 46 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(229 (#t 443 #f 776 #f 46 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(230 (#t 443 #f 33 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(231 (#t 443 #f 776 #f 33 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(232 (#t 443 #f 34 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(233 (#t 443 #f 776 #f 34 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(234 (#t 443 #f 44 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(235 (#t 443 #f 776 #f 44 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(236 (#t 443 #f 173 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(237 (#t 443 #f 776 #f 173 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(238 (#t 443 #f 768 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(239 (#t 443 #f 776 #f 768 #t)
 "÷ [0.2] LATIN LETTER TWO WITH STROKE (OLetter) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(240 (#t 48 #f 1 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] <START OF HEADING> (Other) ÷ [0.3]")
(241 (#t 48 #f 776 #f 1 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(242 (#t 48 #f 13 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(243 (#t 48 #f 776 #f 13 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(244 (#t 48 #f 10 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(245 (#t 48 #f 776 #f 10 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(246 (#t 48 #f 133 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]")
(247 (#t 48 #f 776 #f 133 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(248 (#t 48 #f 9 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]")
(249 (#t 48 #f 776 #f 9 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(250 (#t 48 #f 97 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]")
(251 (#t 48 #f 776 #f 97 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(252 (#t 48 #f 65 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(253 (#t 48 #f 776 #f 65 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(254 (#t 48 #f 443 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(255 (#t 48 #f 776 #f 443 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(256 (#t 48 #f 48 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(257 (#t 48 #f 776 #f 48 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(258 (#t 48 #f 46 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] FULL STOP (ATerm) ÷ [0.3]")
(259 (#t 48 #f 776 #f 46 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(260 (#t 48 #f 33 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]")
(261 (#t 48 #f 776 #f 33 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(262 (#t 48 #f 34 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] QUOTATION MARK (Close) ÷ [0.3]")
(263 (#t 48 #f 776 #f 34 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(264 (#t 48 #f 44 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [998.0] COMMA (SContinue) ÷ [0.3]")
(265 (#t 48 #f 776 #f 44 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(266 (#t 48 #f 173 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(267 (#t 48 #f 776 #f 173 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(268 (#t 48 #f 768 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(269 (#t 48 #f 776 #f 768 #t)
 "÷ [0.2] DIGIT ZERO (Numeric) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(270 (#t 46 #t 1 #t)
 "÷ [0.2] FULL STOP (ATerm) ÷ [11.0] <START OF HEADING> (Other) ÷ [0.3]")
(271 (#t 46 #f 776 #t 1 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(272 (#t 46 #f 13 #t)
 "÷ [0.2] FULL STOP (ATerm) × [9.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(273 (#t 46 #f 776 #f 13 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [9.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(274 (#t 46 #f 10 #t)
 "÷ [0.2] FULL STOP (ATerm) × [9.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(275 (#t 46 #f 776 #f 10 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [9.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(276 (#t 46 #f 133 #t)
 "÷ [0.2] FULL STOP (ATerm) × [9.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]")
(277 (#t 46 #f 776 #f 133 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [9.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(278 (#t 46 #f 9 #t)
 "÷ [0.2] FULL STOP (ATerm) × [9.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]")
(279 (#t 46 #f 776 #f 9 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [9.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(280 (#t 46 #f 97 #t)
 "÷ [0.2] FULL STOP (ATerm) × [8.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]")
(281 (#t 46 #f 776 #f 97 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [8.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(282 (#t 46 #t 65 #t)
 "÷ [0.2] FULL STOP (ATerm) ÷ [11.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]")
(283 (#t 46 #f 776 #t 65 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(284 (#t 46 #t 443 #t)
 "÷ [0.2] FULL STOP (ATerm) ÷ [11.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(285 (#t 46 #f 776 #t 443 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(286 (#t 46 #f 48 #t)
 "÷ [0.2] FULL STOP (ATerm) × [6.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(287 (#t 46 #f 776 #f 48 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [6.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(288 (#t 46 #f 46 #t)
 "÷ [0.2] FULL STOP (ATerm) × [8.1] FULL STOP (ATerm) ÷ [0.3]")
(289 (#t 46 #f 776 #f 46 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [8.1] FULL STOP (ATerm) ÷ [0.3]"
 )
(290 (#t 46 #f 33 #t)
 "÷ [0.2] FULL STOP (ATerm) × [8.1] EXCLAMATION MARK (STerm) ÷ [0.3]")
(291 (#t 46 #f 776 #f 33 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [8.1] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(292 (#t 46 #f 34 #t)
 "÷ [0.2] FULL STOP (ATerm) × [9.0] QUOTATION MARK (Close) ÷ [0.3]")
(293 (#t 46 #f 776 #f 34 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [9.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(294 (#t 46 #f 44 #t)
 "÷ [0.2] FULL STOP (ATerm) × [8.1] COMMA (SContinue) ÷ [0.3]")
(295 (#t 46 #f 776 #f 44 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [8.1] COMMA (SContinue) ÷ [0.3]"
 )
(296 (#t 46 #f 173 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(297 (#t 46 #f 776 #f 173 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(298 (#t 46 #f 768 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]")
(299 (#t 46 #f 776 #f 768 #t)
 "÷ [0.2] FULL STOP (ATerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(300 (#t 33 #t 1 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) ÷ [11.0] <START OF HEADING> (Other) ÷ [0.3]")
(301 (#t 33 #f 776 #t 1 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(302 (#t 33 #f 13 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [9.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(303 (#t 33 #f 776 #f 13 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [9.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(304 (#t 33 #f 10 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [9.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(305 (#t 33 #f 776 #f 10 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [9.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(306 (#t 33 #f 133 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [9.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]")
(307 (#t 33 #f 776 #f 133 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [9.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(308 (#t 33 #f 9 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [9.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]")
(309 (#t 33 #f 776 #f 9 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [9.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(310 (#t 33 #t 97 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) ÷ [11.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(311 (#t 33 #f 776 #t 97 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(312 (#t 33 #t 65 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) ÷ [11.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(313 (#t 33 #f 776 #t 65 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(314 (#t 33 #t 443 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) ÷ [11.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(315 (#t 33 #f 776 #t 443 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(316 (#t 33 #t 48 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) ÷ [11.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(317 (#t 33 #f 776 #t 48 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(318 (#t 33 #f 46 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [8.1] FULL STOP (ATerm) ÷ [0.3]")
(319 (#t 33 #f 776 #f 46 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [8.1] FULL STOP (ATerm) ÷ [0.3]"
 )
(320 (#t 33 #f 33 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [8.1] EXCLAMATION MARK (STerm) ÷ [0.3]")
(321 (#t 33 #f 776 #f 33 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [8.1] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(322 (#t 33 #f 34 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [9.0] QUOTATION MARK (Close) ÷ [0.3]")
(323 (#t 33 #f 776 #f 34 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [9.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(324 (#t 33 #f 44 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [8.1] COMMA (SContinue) ÷ [0.3]")
(325 (#t 33 #f 776 #f 44 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [8.1] COMMA (SContinue) ÷ [0.3]"
 )
(326 (#t 33 #f 173 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(327 (#t 33 #f 776 #f 173 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(328 (#t 33 #f 768 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(329 (#t 33 #f 776 #f 768 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(330 (#t 34 #f 1 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] <START OF HEADING> (Other) ÷ [0.3]")
(331 (#t 34 #f 776 #f 1 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(332 (#t 34 #f 13 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(333 (#t 34 #f 776 #f 13 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(334 (#t 34 #f 10 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(335 (#t 34 #f 776 #f 10 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(336 (#t 34 #f 133 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]")
(337 (#t 34 #f 776 #f 133 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(338 (#t 34 #f 9 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]")
(339 (#t 34 #f 776 #f 9 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(340 (#t 34 #f 97 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(341 (#t 34 #f 776 #f 97 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(342 (#t 34 #f 65 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(343 (#t 34 #f 776 #f 65 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(344 (#t 34 #f 443 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(345 (#t 34 #f 776 #f 443 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(346 (#t 34 #f 48 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(347 (#t 34 #f 776 #f 48 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(348 (#t 34 #f 46 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] FULL STOP (ATerm) ÷ [0.3]")
(349 (#t 34 #f 776 #f 46 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(350 (#t 34 #f 33 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]")
(351 (#t 34 #f 776 #f 33 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(352 (#t 34 #f 34 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] QUOTATION MARK (Close) ÷ [0.3]")
(353 (#t 34 #f 776 #f 34 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(354 (#t 34 #f 44 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [998.0] COMMA (SContinue) ÷ [0.3]")
(355 (#t 34 #f 776 #f 44 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(356 (#t 34 #f 173 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(357 (#t 34 #f 776 #f 173 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(358 (#t 34 #f 768 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(359 (#t 34 #f 776 #f 768 #t)
 "÷ [0.2] QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(360 (#t 44 #f 1 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] <START OF HEADING> (Other) ÷ [0.3]")
(361 (#t 44 #f 776 #f 1 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(362 (#t 44 #f 13 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]")
(363 (#t 44 #f 776 #f 13 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(364 (#t 44 #f 10 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(365 (#t 44 #f 776 #f 10 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(366 (#t 44 #f 133 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]")
(367 (#t 44 #f 776 #f 133 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(368 (#t 44 #f 9 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]")
(369 (#t 44 #f 776 #f 9 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(370 (#t 44 #f 97 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]")
(371 (#t 44 #f 776 #f 97 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(372 (#t 44 #f 65 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]")
(373 (#t 44 #f 776 #f 65 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(374 (#t 44 #f 443 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(375 (#t 44 #f 776 #f 443 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(376 (#t 44 #f 48 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(377 (#t 44 #f 776 #f 48 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(378 (#t 44 #f 46 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] FULL STOP (ATerm) ÷ [0.3]")
(379 (#t 44 #f 776 #f 46 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(380 (#t 44 #f 33 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]")
(381 (#t 44 #f 776 #f 33 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(382 (#t 44 #f 34 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] QUOTATION MARK (Close) ÷ [0.3]")
(383 (#t 44 #f 776 #f 34 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(384 (#t 44 #f 44 #t)
 "÷ [0.2] COMMA (SContinue) × [998.0] COMMA (SContinue) ÷ [0.3]")
(385 (#t 44 #f 776 #f 44 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(386 (#t 44 #f 173 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(387 (#t 44 #f 776 #f 173 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(388 (#t 44 #f 768 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]")
(389 (#t 44 #f 776 #f 768 #t)
 "÷ [0.2] COMMA (SContinue) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(390 (#t 173 #f 1 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]")
(391 (#t 173 #f 776 #f 1 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(392 (#t 173 #f 13 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(393 (#t 173 #f 776 #f 13 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(394 (#t 173 #f 10 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]")
(395 (#t 173 #f 776 #f 10 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(396 (#t 173 #f 133 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]")
(397 (#t 173 #f 776 #f 133 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(398 (#t 173 #f 9 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(399 (#t 173 #f 776 #f 9 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(400 (#t 173 #f 97 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(401 (#t 173 #f 776 #f 97 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(402 (#t 173 #f 65 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(403 (#t 173 #f 776 #f 65 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(404 (#t 173 #f 443 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(405 (#t 173 #f 776 #f 443 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(406 (#t 173 #f 48 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]")
(407 (#t 173 #f 776 #f 48 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(408 (#t 173 #f 46 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]")
(409 (#t 173 #f 776 #f 46 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(410 (#t 173 #f 33 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]")
(411 (#t 173 #f 776 #f 33 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(412 (#t 173 #f 34 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]")
(413 (#t 173 #f 776 #f 34 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(414 (#t 173 #f 44 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [998.0] COMMA (SContinue) ÷ [0.3]")
(415 (#t 173 #f 776 #f 44 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(416 (#t 173 #f 173 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]")
(417 (#t 173 #f 776 #f 173 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(418 (#t 173 #f 768 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(419 (#t 173 #f 776 #f 768 #t)
 "÷ [0.2] SOFT HYPHEN (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(420 (#t 768 #f 1 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(421 (#t 768 #f 776 #f 1 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <START OF HEADING> (Other) ÷ [0.3]"
 )
(422 (#t 768 #f 13 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(423 (#t 768 #f 776 #f 13 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CARRIAGE RETURN (CR)> (CR) ÷ [0.3]"
 )
(424 (#t 768 #f 10 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(425 (#t 768 #f 776 #f 10 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <LINE FEED (LF)> (LF) ÷ [0.3]"
 )
(426 (#t 768 #f 133 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(427 (#t 768 #f 776 #f 133 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <NEXT LINE (NEL)> (Sep) ÷ [0.3]"
 )
(428 (#t 768 #f 9 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(429 (#t 768 #f 776 #f 9 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] <CHARACTER TABULATION> (Sp) ÷ [0.3]"
 )
(430 (#t 768 #f 97 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(431 (#t 768 #f 776 #f 97 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN SMALL LETTER A (Lower) ÷ [0.3]"
 )
(432 (#t 768 #f 65 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(433 (#t 768 #f 776 #f 65 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER A (Upper) ÷ [0.3]"
 )
(434 (#t 768 #f 443 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(435 (#t 768 #f 776 #f 443 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN LETTER TWO WITH STROKE (OLetter) ÷ [0.3]"
 )
(436 (#t 768 #f 48 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(437 (#t 768 #f 776 #f 48 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] DIGIT ZERO (Numeric) ÷ [0.3]"
 )
(438 (#t 768 #f 46 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(439 (#t 768 #f 776 #f 46 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(440 (#t 768 #f 33 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(441 (#t 768 #f 776 #f 33 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] EXCLAMATION MARK (STerm) ÷ [0.3]"
 )
(442 (#t 768 #f 34 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(443 (#t 768 #f 776 #f 34 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] QUOTATION MARK (Close) ÷ [0.3]"
 )
(444 (#t 768 #f 44 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(445 (#t 768 #f 776 #f 44 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [998.0] COMMA (SContinue) ÷ [0.3]"
 )
(446 (#t 768 #f 173 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(447 (#t 768 #f 776 #f 173 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] SOFT HYPHEN (Format_FE) ÷ [0.3]"
 )
(448 (#t 768 #f 768 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(449 (#t 768 #f 776 #f 768 #t)
 "÷ [0.2] COMBINING GRAVE ACCENT (Extend_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) ÷ [0.3]"
 )
(450 (#t 13 #f 10 #t 97 #f 10 #t 776 #t)
 "÷ [0.2] <CARRIAGE RETURN (CR)> (CR) × [3.0] <LINE FEED (LF)> (LF) ÷ [4.0] LATIN SMALL LETTER A (Lower) × [998.0] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) ÷ [0.3]"
 )
(451 (#t 97 #f 776 #t)
 "÷ [0.2] LATIN SMALL LETTER A (Lower) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [0.3]"
 )
(452 (#t 32 #f 8205 #f 1606 #t)
 "÷ [0.2] SPACE (Sp) × [5.0] ZERO WIDTH JOINER (Extend_FE) × [998.0] ARABIC LETTER NOON (OLetter) ÷ [0.3]"
 )
(453 (#t 1606 #f 8205 #f 32 #t)
 "÷ [0.2] ARABIC LETTER NOON (OLetter) × [5.0] ZERO WIDTH JOINER (Extend_FE) × [998.0] SPACE (Sp) ÷ [0.3]"
 )
(454
 (#t 40 #f 34 #f 71 #f 111 #f 46 #f 34 #f 41 #f 32 #t 40 #f 72 #f 101 #f 32 #f
  100 #f 105 #f 100 #f 46 #f 41 #t)
 "÷ [0.2] LEFT PARENTHESIS (Close) × [998.0] QUOTATION MARK (Close) × [998.0] LATIN CAPITAL LETTER G (Upper) × [998.0] LATIN SMALL LETTER O (Lower) × [998.0] FULL STOP (ATerm) × [9.0] QUOTATION MARK (Close) × [9.0] RIGHT PARENTHESIS (Close) × [9.0] SPACE (Sp) ÷ [11.0] LEFT PARENTHESIS (Close) × [998.0] LATIN CAPITAL LETTER H (Upper) × [998.0] LATIN SMALL LETTER E (Lower) × [998.0] SPACE (Sp) × [998.0] LATIN SMALL LETTER D (Lower) × [998.0] LATIN SMALL LETTER I (Lower) × [998.0] LATIN SMALL LETTER D (Lower) × [998.0] FULL STOP (ATerm) × [9.0] RIGHT PARENTHESIS (Close) ÷ [0.3]")
(455
 (#t 40 #f 8220 #f 71 #f 111 #f 63 #f 8221 #f 41 #f 32 #t 40 #f 72 #f 101 #f 32
  #f 100 #f 105 #f 100 #f 46 #f 41 #t)
 "÷ [0.2] LEFT PARENTHESIS (Close) × [998.0] LEFT DOUBLE QUOTATION MARK (Close) × [998.0] LATIN CAPITAL LETTER G (Upper) × [998.0] LATIN SMALL LETTER O (Lower) × [998.0] QUESTION MARK (STerm) × [9.0] RIGHT DOUBLE QUOTATION MARK (Close) × [9.0] RIGHT PARENTHESIS (Close) × [9.0] SPACE (Sp) ÷ [11.0] LEFT PARENTHESIS (Close) × [998.0] LATIN CAPITAL LETTER H (Upper) × [998.0] LATIN SMALL LETTER E (Lower) × [998.0] SPACE (Sp) × [998.0] LATIN SMALL LETTER D (Lower) × [998.0] LATIN SMALL LETTER I (Lower) × [998.0] LATIN SMALL LETTER D (Lower) × [998.0] FULL STOP (ATerm) × [9.0] RIGHT PARENTHESIS (Close) ÷ [0.3]")
(456 (#t 85 #f 46 #f 83 #f 46 #f 65 #f 768 #f 46 #f 32 #f 105 #f 115 #t)
 "÷ [0.2] LATIN CAPITAL LETTER U (Upper) × [998.0] FULL STOP (ATerm) × [7.0] LATIN CAPITAL LETTER S (Upper) × [998.0] FULL STOP (ATerm) × [7.0] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] FULL STOP (ATerm) × [8.0] SPACE (Sp) × [8.0] LATIN SMALL LETTER I (Lower) × [998.0] LATIN SMALL LETTER S (Lower) ÷ [0.3]"
 )
(457 (#t 85 #f 46 #f 83 #f 46 #f 65 #f 768 #f 63 #f 32 #t 72 #f 101 #t)
 "÷ [0.2] LATIN CAPITAL LETTER U (Upper) × [998.0] FULL STOP (ATerm) × [7.0] LATIN CAPITAL LETTER S (Upper) × [998.0] FULL STOP (ATerm) × [7.0] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] QUESTION MARK (STerm) × [9.0] SPACE (Sp) ÷ [11.0] LATIN CAPITAL LETTER H (Upper) × [998.0] LATIN SMALL LETTER E (Lower) ÷ [0.3]"
 )
(458 (#t 85 #f 46 #f 83 #f 46 #f 65 #f 768 #f 46 #t)
 "÷ [0.2] LATIN CAPITAL LETTER U (Upper) × [998.0] FULL STOP (ATerm) × [7.0] LATIN CAPITAL LETTER S (Upper) × [998.0] FULL STOP (ATerm) × [7.0] LATIN CAPITAL LETTER A (Upper) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] FULL STOP (ATerm) ÷ [0.3]"
 )
(459 (#t 51 #f 46 #f 52 #t)
 "÷ [0.2] DIGIT THREE (Numeric) × [998.0] FULL STOP (ATerm) × [6.0] DIGIT FOUR (Numeric) ÷ [0.3]"
 )
(460 (#t 99 #f 46 #f 100 #t)
 "÷ [0.2] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) × [8.0] LATIN SMALL LETTER D (Lower) ÷ [0.3]"
 )
(461 (#t 67 #f 46 #f 100 #t)
 "÷ [0.2] LATIN CAPITAL LETTER C (Upper) × [998.0] FULL STOP (ATerm) × [8.0] LATIN SMALL LETTER D (Lower) ÷ [0.3]"
 )
(462 (#t 99 #f 46 #f 68 #t)
 "÷ [0.2] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) × [7.0] LATIN CAPITAL LETTER D (Upper) ÷ [0.3]"
 )
(463 (#t 67 #f 46 #f 68 #t)
 "÷ [0.2] LATIN CAPITAL LETTER C (Upper) × [998.0] FULL STOP (ATerm) × [7.0] LATIN CAPITAL LETTER D (Upper) ÷ [0.3]"
 )
(464 (#t 101 #f 116 #f 99 #f 46 #f 41 #f 8217 #f 160 #f 116 #f 104 #f 101 #t)
 "÷ [0.2] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) × [8.0] RIGHT PARENTHESIS (Close) × [8.0] RIGHT SINGLE QUOTATION MARK (Close) × [8.0] NO-BREAK SPACE (Sp) × [8.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER H (Lower) × [998.0] LATIN SMALL LETTER E (Lower) ÷ [0.3]"
 )
(465 (#t 101 #f 116 #f 99 #f 46 #f 41 #f 8217 #f 160 #t 84 #f 104 #f 101 #t)
 "÷ [0.2] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) × [9.0] RIGHT PARENTHESIS (Close) × [9.0] RIGHT SINGLE QUOTATION MARK (Close) × [9.0] NO-BREAK SPACE (Sp) ÷ [11.0] LATIN CAPITAL LETTER T (Upper) × [998.0] LATIN SMALL LETTER H (Lower) × [998.0] LATIN SMALL LETTER E (Lower) ÷ [0.3]"
 )
(466
 (#t 101 #f 116 #f 99 #f 46 #f 41 #f 8217 #f 160 #f 8216 #f 40 #f 116 #f 104 #f
  101 #t)
 "÷ [0.2] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) × [8.0] RIGHT PARENTHESIS (Close) × [8.0] RIGHT SINGLE QUOTATION MARK (Close) × [8.0] NO-BREAK SPACE (Sp) × [8.0] LEFT SINGLE QUOTATION MARK (Close) × [998.0] LEFT PARENTHESIS (Close) × [998.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER H (Lower) × [998.0] LATIN SMALL LETTER E (Lower) ÷ [0.3]")
(467
 (#t 101 #f 116 #f 99 #f 46 #f 41 #f 8217 #f 160 #t 8216 #f 40 #f 84 #f 104 #f
  101 #t)
 "÷ [0.2] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) × [9.0] RIGHT PARENTHESIS (Close) × [9.0] RIGHT SINGLE QUOTATION MARK (Close) × [9.0] NO-BREAK SPACE (Sp) ÷ [11.0] LEFT SINGLE QUOTATION MARK (Close) × [998.0] LEFT PARENTHESIS (Close) × [998.0] LATIN CAPITAL LETTER T (Upper) × [998.0] LATIN SMALL LETTER H (Lower) × [998.0] LATIN SMALL LETTER E (Lower) ÷ [0.3]")
(468
 (#t 101 #f 116 #f 99 #f 46 #f 41 #f 8217 #f 160 #f 776 #f 116 #f 104 #f 101 #t)
 "÷ [0.2] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) × [8.0] RIGHT PARENTHESIS (Close) × [8.0] RIGHT SINGLE QUOTATION MARK (Close) × [8.0] NO-BREAK SPACE (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) × [8.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER H (Lower) × [998.0] LATIN SMALL LETTER E (Lower) ÷ [0.3]")
(469
 (#t 101 #f 116 #f 99 #f 46 #f 41 #f 8217 #f 160 #f 776 #t 84 #f 104 #f 101 #t)
 "÷ [0.2] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) × [9.0] RIGHT PARENTHESIS (Close) × [9.0] RIGHT SINGLE QUOTATION MARK (Close) × [9.0] NO-BREAK SPACE (Sp) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] LATIN CAPITAL LETTER T (Upper) × [998.0] LATIN SMALL LETTER H (Lower) × [998.0] LATIN SMALL LETTER E (Lower) ÷ [0.3]")
(470 (#t 101 #f 116 #f 99 #f 46 #f 41 #f 8217 #f 776 #t 84 #f 104 #f 101 #t)
 "÷ [0.2] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) × [9.0] RIGHT PARENTHESIS (Close) × [9.0] RIGHT SINGLE QUOTATION MARK (Close) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] LATIN CAPITAL LETTER T (Upper) × [998.0] LATIN SMALL LETTER H (Lower) × [998.0] LATIN SMALL LETTER E (Lower) ÷ [0.3]"
 )
(471 (#t 101 #f 116 #f 99 #f 46 #f 41 #f 10 #t 776 #f 84 #f 104 #f 101 #t)
 "÷ [0.2] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) × [9.0] RIGHT PARENTHESIS (Close) × [9.0] <LINE FEED (LF)> (LF) ÷ [4.0] COMBINING DIAERESIS (Extend_FE) × [998.0] LATIN CAPITAL LETTER T (Upper) × [998.0] LATIN SMALL LETTER H (Lower) × [998.0] LATIN SMALL LETTER E (Lower) ÷ [0.3]"
 )
(472
 (#t 116 #f 104 #f 101 #f 32 #f 114 #f 101 #f 115 #f 112 #f 46 #f 32 #f 108 #f
  101 #f 97 #f 100 #f 101 #f 114 #f 115 #f 32 #f 97 #f 114 #f 101 #t)
 "÷ [0.2] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER H (Lower) × [998.0] LATIN SMALL LETTER E (Lower) × [998.0] SPACE (Sp) × [998.0] LATIN SMALL LETTER R (Lower) × [998.0] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER S (Lower) × [998.0] LATIN SMALL LETTER P (Lower) × [998.0] FULL STOP (ATerm) × [8.0] SPACE (Sp) × [8.0] LATIN SMALL LETTER L (Lower) × [998.0] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER A (Lower) × [998.0] LATIN SMALL LETTER D (Lower) × [998.0] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER R (Lower) × [998.0] LATIN SMALL LETTER S (Lower) × [998.0] SPACE (Sp) × [998.0] LATIN SMALL LETTER A (Lower) × [998.0] LATIN SMALL LETTER R (Lower) × [998.0] LATIN SMALL LETTER E (Lower) ÷ [0.3]")
(473 (#t 23383 #f 46 #t 23383 #t)
 "÷ [0.2] CJK UNIFIED IDEOGRAPH-5B57 (OLetter) × [998.0] FULL STOP (ATerm) ÷ [11.0] CJK UNIFIED IDEOGRAPH-5B57 (OLetter) ÷ [0.3]"
 )
(474 (#t 101 #f 116 #f 99 #f 46 #t 23427 #t)
 "÷ [0.2] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) ÷ [11.0] CJK UNIFIED IDEOGRAPH-5B83 (OLetter) ÷ [0.3]"
 )
(475 (#t 101 #f 116 #f 99 #f 46 #f 12290 #t)
 "÷ [0.2] LATIN SMALL LETTER E (Lower) × [998.0] LATIN SMALL LETTER T (Lower) × [998.0] LATIN SMALL LETTER C (Lower) × [998.0] FULL STOP (ATerm) × [8.1] IDEOGRAPHIC FULL STOP (STerm) ÷ [0.3]"
 )
(476 (#t 23383 #f 12290 #t 23427 #t)
 "÷ [0.2] CJK UNIFIED IDEOGRAPH-5B57 (OLetter) × [998.0] IDEOGRAPHIC FULL STOP (STerm) ÷ [11.0] CJK UNIFIED IDEOGRAPH-5B83 (OLetter) ÷ [0.3]"
 )
(477 (#t 33 #f 32 #f 32 #t)
 "÷ [0.2] EXCLAMATION MARK (STerm) × [9.0] SPACE (Sp) × [10.0] SPACE (Sp) ÷ [0.3]"
 )
(478
 (#t 8288 #f 40 #f 8288 #f 34 #f 8288 #f 71 #f 8288 #f 111 #f 8288 #f 46 #f
  8288 #f 34 #f 8288 #f 41 #f 8288 #f 32 #f 8288 #t 40 #f 8288 #f 72 #f 8288 #f
  101 #f 8288 #f 32 #f 8288 #f 100 #f 8288 #f 105 #f 8288 #f 100 #f 8288 #f 46
  #f 8288 #f 41 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LEFT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [998.0] QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN CAPITAL LETTER G (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER O (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [9.0] QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] SPACE (Sp) × [5.0] WORD JOINER (Format_FE) ÷ [11.0] LEFT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN CAPITAL LETTER H (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER D (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER I (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER D (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(479
 (#t 8288 #f 40 #f 8288 #f 8220 #f 8288 #f 71 #f 8288 #f 111 #f 8288 #f 63 #f
  8288 #f 8221 #f 8288 #f 41 #f 8288 #f 32 #f 8288 #t 40 #f 8288 #f 72 #f 8288
  #f 101 #f 8288 #f 32 #f 8288 #f 100 #f 8288 #f 105 #f 8288 #f 100 #f 8288 #f
  46 #f 8288 #f 41 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LEFT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [998.0] LEFT DOUBLE QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN CAPITAL LETTER G (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER O (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] QUESTION MARK (STerm) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT DOUBLE QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] SPACE (Sp) × [5.0] WORD JOINER (Format_FE) ÷ [11.0] LEFT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN CAPITAL LETTER H (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER D (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER I (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER D (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(480
 (#t 8288 #f 85 #f 8288 #f 46 #f 8288 #f 83 #f 8288 #f 46 #f 8288 #f 65 #f 8288
  #f 768 #f 46 #f 8288 #f 32 #f 8288 #f 105 #f 8288 #f 115 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN CAPITAL LETTER U (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [7.0] LATIN CAPITAL LETTER S (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [7.0] LATIN CAPITAL LETTER A (Upper) × [5.0] WORD JOINER (Format_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [8.0] SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [8.0] LATIN SMALL LETTER I (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER S (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(481
 (#t 8288 #f 85 #f 8288 #f 46 #f 8288 #f 83 #f 8288 #f 46 #f 8288 #f 65 #f 8288
  #f 768 #f 63 #f 8288 #f 32 #f 8288 #t 72 #f 8288 #f 101 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN CAPITAL LETTER U (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [7.0] LATIN CAPITAL LETTER S (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [7.0] LATIN CAPITAL LETTER A (Upper) × [5.0] WORD JOINER (Format_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] QUESTION MARK (STerm) × [5.0] WORD JOINER (Format_FE) × [9.0] SPACE (Sp) × [5.0] WORD JOINER (Format_FE) ÷ [11.0] LATIN CAPITAL LETTER H (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(482
 (#t 8288 #f 85 #f 8288 #f 46 #f 8288 #f 83 #f 8288 #f 46 #f 8288 #f 65 #f 8288
  #f 768 #f 46 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN CAPITAL LETTER U (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [7.0] LATIN CAPITAL LETTER S (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [7.0] LATIN CAPITAL LETTER A (Upper) × [5.0] WORD JOINER (Format_FE) × [5.0] COMBINING GRAVE ACCENT (Extend_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(483 (#t 8288 #f 51 #f 8288 #f 46 #f 8288 #f 52 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] DIGIT THREE (Numeric) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [6.0] DIGIT FOUR (Numeric) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(484 (#t 8288 #f 99 #f 8288 #f 46 #f 8288 #f 100 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [8.0] LATIN SMALL LETTER D (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(485 (#t 8288 #f 67 #f 8288 #f 46 #f 8288 #f 100 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN CAPITAL LETTER C (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [8.0] LATIN SMALL LETTER D (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(486 (#t 8288 #f 99 #f 8288 #f 46 #f 8288 #f 68 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [7.0] LATIN CAPITAL LETTER D (Upper) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(487 (#t 8288 #f 67 #f 8288 #f 46 #f 8288 #f 68 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN CAPITAL LETTER C (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [7.0] LATIN CAPITAL LETTER D (Upper) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(488
 (#t 8288 #f 101 #f 8288 #f 116 #f 8288 #f 99 #f 8288 #f 46 #f 8288 #f 41 #f
  8288 #f 8217 #f 8288 #f 160 #f 8288 #f 116 #f 8288 #f 104 #f 8288 #f 101 #f
  8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [8.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [8.0] RIGHT SINGLE QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [8.0] NO-BREAK SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [8.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER H (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(489
 (#t 8288 #f 101 #f 8288 #f 116 #f 8288 #f 99 #f 8288 #f 46 #f 8288 #f 41 #f
  8288 #f 8217 #f 8288 #f 160 #f 8288 #t 84 #f 8288 #f 104 #f 8288 #f 101 #f
  8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT SINGLE QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] NO-BREAK SPACE (Sp) × [5.0] WORD JOINER (Format_FE) ÷ [11.0] LATIN CAPITAL LETTER T (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER H (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(490
 (#t 8288 #f 101 #f 8288 #f 116 #f 8288 #f 99 #f 8288 #f 46 #f 8288 #f 41 #f
  8288 #f 8217 #f 8288 #f 160 #f 8288 #f 8216 #f 8288 #f 40 #f 8288 #f 116 #f
  8288 #f 104 #f 8288 #f 101 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [8.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [8.0] RIGHT SINGLE QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [8.0] NO-BREAK SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [8.0] LEFT SINGLE QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [998.0] LEFT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER H (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(491
 (#t 8288 #f 101 #f 8288 #f 116 #f 8288 #f 99 #f 8288 #f 46 #f 8288 #f 41 #f
  8288 #f 8217 #f 8288 #f 160 #f 8288 #t 8216 #f 8288 #f 40 #f 8288 #f 84 #f
  8288 #f 104 #f 8288 #f 101 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT SINGLE QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] NO-BREAK SPACE (Sp) × [5.0] WORD JOINER (Format_FE) ÷ [11.0] LEFT SINGLE QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [998.0] LEFT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN CAPITAL LETTER T (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER H (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(492
 (#t 8288 #f 101 #f 8288 #f 116 #f 8288 #f 99 #f 8288 #f 46 #f 8288 #f 41 #f
  8288 #f 8217 #f 8288 #f 160 #f 8288 #f 776 #f 116 #f 8288 #f 104 #f 8288 #f
  101 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [8.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [8.0] RIGHT SINGLE QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [8.0] NO-BREAK SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [8.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER H (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(493
 (#t 8288 #f 101 #f 8288 #f 116 #f 8288 #f 99 #f 8288 #f 46 #f 8288 #f 41 #f
  8288 #f 8217 #f 8288 #f 160 #f 8288 #f 776 #t 84 #f 8288 #f 104 #f 8288 #f
  101 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT SINGLE QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] NO-BREAK SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] LATIN CAPITAL LETTER T (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER H (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(494
 (#t 8288 #f 101 #f 8288 #f 116 #f 8288 #f 99 #f 8288 #f 46 #f 8288 #f 41 #f
  8288 #f 8217 #f 8288 #f 776 #t 84 #f 8288 #f 104 #f 8288 #f 101 #f 8288 #f
  8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT SINGLE QUOTATION MARK (Close) × [5.0] WORD JOINER (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) ÷ [11.0] LATIN CAPITAL LETTER T (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER H (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(495
 (#t 8288 #f 101 #f 8288 #f 116 #f 8288 #f 99 #f 8288 #f 46 #f 8288 #f 41 #f
  8288 #f 10 #t 8288 #f 776 #f 8288 #f 84 #f 8288 #f 104 #f 8288 #f 101 #f 8288
  #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [9.0] RIGHT PARENTHESIS (Close) × [5.0] WORD JOINER (Format_FE) × [9.0] <LINE FEED (LF)> (LF) ÷ [4.0] WORD JOINER (Format_FE) × [5.0] COMBINING DIAERESIS (Extend_FE) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN CAPITAL LETTER T (Upper) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER H (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(496
 (#t 8288 #f 116 #f 8288 #f 104 #f 8288 #f 101 #f 8288 #f 32 #f 8288 #f 114 #f
  8288 #f 101 #f 8288 #f 115 #f 8288 #f 112 #f 8288 #f 46 #f 8288 #f 32 #f 8288
  #f 108 #f 8288 #f 101 #f 8288 #f 97 #f 8288 #f 100 #f 8288 #f 101 #f 8288 #f
  114 #f 8288 #f 115 #f 8288 #f 32 #f 8288 #f 97 #f 8288 #f 114 #f 8288 #f 101
  #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER H (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER R (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER S (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER P (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [8.0] SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [8.0] LATIN SMALL LETTER L (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER A (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER D (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER R (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER S (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER A (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER R (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(497 (#t 8288 #f 23383 #f 8288 #f 46 #f 8288 #t 23383 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] CJK UNIFIED IDEOGRAPH-5B57 (OLetter) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) ÷ [11.0] CJK UNIFIED IDEOGRAPH-5B57 (OLetter) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(498
 (#t 8288 #f 101 #f 8288 #f 116 #f 8288 #f 99 #f 8288 #f 46 #f 8288 #t 23427 #f
  8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) ÷ [11.0] CJK UNIFIED IDEOGRAPH-5B83 (OLetter) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(499
 (#t 8288 #f 101 #f 8288 #f 116 #f 8288 #f 99 #f 8288 #f 46 #f 8288 #f 12290 #f
  8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER E (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER T (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] LATIN SMALL LETTER C (Lower) × [5.0] WORD JOINER (Format_FE) × [998.0] FULL STOP (ATerm) × [5.0] WORD JOINER (Format_FE) × [8.1] IDEOGRAPHIC FULL STOP (STerm) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]")
(500 (#t 8288 #f 23383 #f 8288 #f 12290 #f 8288 #t 23427 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] CJK UNIFIED IDEOGRAPH-5B57 (OLetter) × [5.0] WORD JOINER (Format_FE) × [998.0] IDEOGRAPHIC FULL STOP (STerm) × [5.0] WORD JOINER (Format_FE) ÷ [11.0] CJK UNIFIED IDEOGRAPH-5B83 (OLetter) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
(501 (#t 8288 #f 33 #f 8288 #f 32 #f 8288 #f 32 #f 8288 #f 8288 #t)
 "÷ [0.2] WORD JOINER (Format_FE) × [998.0] EXCLAMATION MARK (STerm) × [5.0] WORD JOINER (Format_FE) × [9.0] SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [10.0] SPACE (Sp) × [5.0] WORD JOINER (Format_FE) × [5.0] WORD JOINER (Format_FE) ÷ [0.3]"
 )
))
