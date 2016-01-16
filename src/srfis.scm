;; -*- coding:utf-8 -*-
;;
;; Master list of supported srfis.
;;
;; Run this script with $top_builddir as an argument, and the following
;; files are generated:
;;
;;   src/libsrfis.scm  - sets up cond-expand feature identifier list
;;                       and exported symbols for built-in srfis.
;;   lib/srfi/*.scm    - Dummy modules to allow (import (srfi N))
;;                       to work.
;;   doc/srfis.texi    - Incorporated into concepts.texi for the
;;                       list of supported srfis.

(use util.match)
(use file.util)
(use gauche.cgen.literal)
(use srfi-1)
(use gauche.generator)

;;; Parser

;; returns ((num module exports doc-en doc-ja) ...)
(define (parse self)
  (let1 gen ($ record-generator $ file->string-list self :encoding 'utf-8)
    ($ generator->list
       $ gmap ($ paragraphs->final-record $ split-by-empty-line $) gen)))

;; split input by chunks starting with "srfi-"
(define (record-generator lines)
  (define buf (drop-while #/^(?!srfi-)/ lines))
  (^[] (if (null? buf)
         (eof-object)
         (receive (record rest) (break #/^srfi-/ (cdr buf))
           (begin0 (cons (car buf) record)
             (set! buf rest))))))

;; gather paragraphs separated by emtpy line(s)
(define (split-by-empty-line lines)
  (let loop ([lines lines] [s '()] [t '()])
    (cond [(null? lines)
           (if (null? t) (reverse s) (reverse (cons (reverse t) s)))]
          [(#/^\s*$/ (car lines))
           (if (null? t)
             (loop (cdr lines) s '())
             (loop (cdr lines) (cons (reverse t) s) '()))]
          [else (loop (cdr lines) s (cons (car lines) t))])))

;; from given paragraph list, create the final record
(define (paragraphs->final-record ps)
  (match ps
    [(entry-line doc-en doc-ja)
     `(,@(parse-entry-line (car entry-line))  ; (N "module") or (N #f)
       ,(read-from-string (cadr entry-line))   ; list of exported symbols
       ,doc-en ,doc-ja)]
    [else
     (error "Invalid entry for ~a" (car ps))]))

;; "srfi-N, module" -> (N "module")
;; "srfi-N"  -> (N #f)
(define (parse-entry-line line)
  (rxmatch-let (#/srfi-(\d+)(?:,\s+(\S+))?/ line) (#f n mod)
    (list (x->integer n) mod)))

;;; Generator

(define (generate-texi records top_builddir)
  (with-output-to-file (build-path top_builddir "doc/srfis.texi")
    (^[] (print "@c -*- coding:utf-8 -*-") (print)
      (dolist [rec records]
        (match-let1 (n _ _ doc-en doc-ja) rec
          (print "@c EN")
          (print #"@item SRFI-~|n|, ~(car doc-en)")
          (for-each print (cdr doc-en))
          (print "@c JP")
          (print #"@item SRFI-~|n|, ~(car doc-ja)")
          (for-each print (cdr doc-ja))
          (print "@c COMMON")
          (print))))))

(define (generate-libsrfis records top_builddir)
  (with-output-to-file (build-path top_builddir "src/libsrfis.scm")
    (^[]
      (print "(inline-stub (initcode")
      (dolist [rec records]
        (match-let1 (n mod . _) rec
          (format #t "(Scm_AddFeature \"srfi-~d\" ~s)\n" n (or mod 'NULL))))
      (print "))")
      (dolist [rec records]
        (match-let1 (n _ exports . _) rec
          (unless (null? exports)
            (write `(define-module ,(string->symbol #"srfi-~n")
                      (export ,@exports)))
            (newline))))
      )))

(define (generate-srfi-modules records top_builddir)
  (make-directory* (build-path top_builddir "lib/srfi"))
  (dolist [rec records]
    (let1 n (car rec)
      (with-output-to-file (build-path top_builddir "lib/srfi" #"~|n|.scm")
        (^[]
          (print ";; Generated automatically.  Do not edit.")
          (print #"(define-module srfi.~n (extend srfi-~n))"))))))


(define (generate self top_builddir)
  (let1 records (parse self)
    (generate-libsrfis records top_builddir)
    (generate-texi records top_builddir)
    (generate-srfi-modules records top_builddir)))

;;; Main

;; we don't use 'main' in order to put the data after (exit 0) line.
(match (command-line)
  [(self top_builddir) (generate self top_builddir)]
  [(self . _) (exit 1 #"Usage: gosh ~self $top_builddir")])

(exit 0)

;;; Data

;; The following is data to be processed.
;; #/^srfi-\d+/ starts a new record.
;; If the srfi requires importing a module, list the module name
;; after the srfi's name.
;; The second line lists exported symbols from the srfi, *if* the srfi
;; is built-in.  Since we don't actually have srfi-N.scm file for built-in
;; srfis, we have to set up the module exports at the initialization.
;;
;; It is followed by two paragraphs in English and then in Japanese,
;; delimited by an empty line.
;; In each paragraph, the first line is the header, then description
;; follows.

srfi-0
()

Feature-based conditional expansion construct.
Built-in.   @xref{Feature conditional}.

機能ベースの条件展開
組み込みです。@ref{Feature conditional}参照。


srfi-1, srfi-1
()

List library.
Supported by the module @code{srfi-1}.  @xref{List library}.
(Some of SRFI-1 procedures are built-in).

リストライブラリ
モジュール@code{srfi-1}でサポートされます。@ref{List library}参照。
SRFI-1の手続きのうちいくつかは組み込みになっています。

srfi-2
(and-let*)

AND-LET*: an AND with local bindings, a guarded LET* special form.
Supported natively.  @xref{Binding constructs}.

AND-LET*: 局所束縛をともなう AND、ガード付 LET* 特殊フォーム
組み込みです。@ref{Binding constructs}参照。

srfi-4, gauche.uvector
()

Homogeneous numeric vector datatypes.
The module @code{gauche.uvector} provides a superset of
@code{srfi-4} procedures, including arithmetic operations and
generic interface on the SRFI-4 vectors.  @xref{Uniform vectors}.

一様な数値ベクタ型
モジュール@code{gauche.uvector}が
@code{srfi-4}の上位互換手続きを提供します。
同モジュールにはSRFI-4の手続きに加え、
算術演算やジェネリックなインタフェースが定義されています。@ref{Uniform vectors}参照。

srfi-5, srfi-5
()

A compatible let form with signatures and rest arguments
Supported by the module @code{srfi-5}.
@xref{A compatible let form with signatures and rest arguments}.

シグネチャとrest引数に互換性のあるlet形式
モジュール@code{srfi-5}でサポートされます。
@ref{A compatible let form with signatures and rest arguments}参照。


srfi-6
(open-input-string open-output-string get-output-string)

Basic String Ports.
SRFI-6 procedures are built-in.  @xref{String ports}.

基本文字列ポート
SRFI-6の手続きは組み込みになっています。@ref{String ports}参照。

srfi-7
()

Feature-based program configuration language
Supported as an autoloaded macro.
@xref{Feature-based program configuration language}.

機能ベースプログラム設定言語
オートロードされるマクロとしてサポートされています。
@ref{Feature-based program configuration language}参照。

srfi-8
(receive)

receive: Binding to multiple values.
Syntax @code{receive} is built-in.  @xref{Binding constructs}.

receive: 多値束縛
構文@code{receive}は組み込みになっています。@ref{Binding constructs}参照。


srfi-9, gauche.record
()

Defining record types.
Supported by the module @code{gauche.record}.  @xref{Record types}.

レコード型の定義
モジュール@code{gauche.record}でサポートされます。@ref{Record types}参照。


srfi-10
(define-reader-ctor)

Sharp-comma external form.
Built-in.  @xref{Read-time constructor}.

Sharp-comma外部フォーム
組み込みです。@ref{Read-time constructor}参照。


srfi-11, srfi-11
()

Syntax for receiving multiple values.
Supported by the module @code{srfi-11}.  @xref{Let-values}.

多値を受け取るための構文
モジュール@code{srfi-11}でサポートされます。@ref{Let-values}参照。

srfi-13, srfi-13
()

String library
Supported by the module @code{srfi-13}.  @xref{String library}.
(Some of SRFI-13 procedures are built-in).

文字列ライブラリ
モジュール@code{srfi-13}でサポートされます。@ref{String library}参照。
(SRFI-13の手続きのいくつかは組み込みになっています。)

srfi-14, srfi-14
()

Character-set library
Character-set object and a few SRFI-14 procedures are built-in.
@xref{Character set}.
Complete set of SRFI-14 is supported by the module @code{srfi-14}.
@xref{Character-set library}.

文字集合のライブラリ
文字集合と基本的なSRFI-14手続きは組み込みになっています。
@ref{Character set}参照。SRFI-14の完全なサポートはモジュール@code{srfi-14}
で提供されています。@ref{Character-set library}参照。

srfi-16
(case-lambda)

Syntax for procedures of variable arity (case-lambda)
Built-in.  @xref{Making Procedures}.

可変長引数手続き構文 (case-lambda)
組み込みです。@ref{Making Procedures}参照。


srfi-17
(setter getter-with-setter)

Generalized set!
Built-in.  @xref{Assignments}.

一般化された set!
組み込みです。@ref{Assignments}参照。


srfi-18, gauche.threads
()

Multithreading support
Some SRFI-18 features are built-in, and the rest is in @code{gauche.threads}
module.  @xref{Threads}.

マルチスレッドのサポート
いくつかのSRFI-18の機能は組み込みであり、、残りのAPIは
@code{gauche.threads}モジュールで提供されます。@ref{Threads}参照。


srfi-19, srfi-19
()

Time Data Types and Procedures.
Time data type is Gauche built-in (@pxref{Time}).
Complete set of SRFI-19 is supported by the module @code{srfi-19}.
@xref{Time data types and procedures}.

時間データの型と手続き
時間のデータ型はGauche組み込みです(@ref{Time}参照)。
SRFI-19の完全なサポートはモジュール@code{srfi-19}で提供されています。
@ref{Time data types and procedures}参照。


srfi-22
()

Running Scheme scripts on Unix
Supported.  @xref{Writing Scheme scripts}.

UNIX 上の Scheme スクリプトの実行
サポートされています。@ref{Writing Scheme scripts}参照。


srfi-23
(error)

Error reporting mechanism.
Built-in.   @xref{Signaling exceptions}.

エラー報告機構
組み込みです。@ref{Signaling exceptions}参照。


srfi-25, gauche.array
()

Multi-dimensional array primitives.
Supported by the module @code{gauche.array}, which defines
superset of SRFI-25.  @xref{Arrays}.

多次元配列のプリミティブ
モジュール@code{gauche.array}が、SRFI-25の上位互換と
なっています。@ref{Arrays}参照。

srfi-26, srfi-26
()

Notation for specializing parameters without currying.
As an autoloaded macro.  @xref{Making Procedures}.

カリー化をともなわないパラメータの特殊化記法
オートロードされるマクロとして定義されています。@ref{Making Procedures}参照。

srfi-27, srfi-27
()

Sources of Random Bits.
Supported by the module @code{srfi-27}.  @xref{Sources of random bits}.

ランダムビットのソース
モジュール@code{srfi-27}でサポートされます。@ref{Sources of random bits}参照。

srfi-28
(format)

Basic format strings.
Gauche's built-in @code{format} procedure is a superset of
SRFI-28 @code{format}.  @xref{Output}.

基本フォーマット文字列
Gauche組み込みの@code{format}がSRFI-28のものの上位互換に
なっています。@ref{Output}参照。

srfi-29, srfi-29
()

Localization
Supported by the module @code{srfi-29}.
@xref{Localization}.

地域化
モジュール@code{srfi-29}でサポートされます。
@ref{Localization}参照。


srfi-30
()

Nested multi-line comments.
Supported by the native reader.  @xref{Lexical structure}.

ネストした複数行コメント
ネイティブのリーダでサポートされています。@ref{Lexical structure}参照。


srfi-31
()

A special form rec for recursive evaluation
Defined as an autoloaded macro.  @xref{Binding constructs}.

再帰評価用の特殊フォーム rec
オートロードされるマクロとして定義されています。@ref{Binding constructs}参照。

srfi-34
(with-exception-handler guard raise)

Exception Handling for Programs
Built-in.  @xref{Exceptions}.
(However, Gauche implements srfi-18's semantics of @code{raise} literally,
which differs slightly from srfi-34's.  This may be changed in future.)

プログラムの例外処理
組み込みです。@ref{Exceptions}参照。
(但し、Gaucheは@code{raise}に関してはsrfi-18のセマンティクスを文字通り
実装していて、それはsrfi-34と若干異なります。将来はsrfi-34に合わせるかもしれません。)


srfi-35
(make-condition-type condition-type? make-condition condition? condition-has-type? condition-ref make-compound-condition extract-condition define-condition-type condition &condition &message &serious &error)

Conditions
Built-in.  @xref{Conditions}.

コンディション
組み込みです。@ref{Conditions}参照。


srfi-36
(&i/o-error &i/o-port-error &i/o-read-error &i/o-write-error &i/o-closed-error &read-error) ; Need filename-related errors

I/O Conditions
Partly supported.  @xref{Conditions}.

I/O コンディション
部分的にサポートされています.  @ref{Conditions}参照。


srfi-37, srfi-37
()

args-fold: a program argument processor
Supported by the module @code{srfi-37}.
@xref{A program argument processor}.

args-fold: プログラム引数処理
モジュール@code{srfi-37}でサポートされます。
@ref{A program argument processor}参照。


srfi-38
(write-with-shared-structure write/ss read-with-shared-structure read/ss)

External Representation for Data With Shared Structure
Built-in.  See @ref{Reading data} and @ref{Output}.

共有されるデータの外部表現
組み込みです。@ref{Reading data}と@ref{Output}参照。

srfi-39, gauche.parameter
()

Parameter objects
Supported by the module @code{gauche.parameter}.
@xref{Parameters}.

パラメータオブジェクト
モジュール@code{gauche.parameter}でサポートされます。
@ref{Parameters}参照。

srfi-40, util.stream
()

A Library of Streams
Supported by the module @code{util.stream}.
@xref{Stream library}.

ストリームライブラリ
モジュール@code{util.stream}でサポートされています。
@xref{Stream library}.

srfi-42, srfi-42
()

Eager comprehensions
Supported by the module @code{srfi-42}.
@xref{Eager comprehensions}.

先行評価的内包表記
モジュール@code{srfi-42}でサポートされます。
@ref{Eager comprehensions}参照。


srfi-43, srfi-43
()

Vector library
Supported by the module @code{srfi-43}.
@xref{Vector library}.

ベクタライブラリ
モジュール@code{srfi-43}でサポートされます。
@ref{Vector library}参照。


srfi-45
(delay force lazy eager)

Primitives for Expressing Iterative Lazy Algorithms
Built-in.
@xref{Lazy evaluation}.

反復的 Lazy アルゴリズムのための基本関数
組み込みです。
@xref{Lazy evaluation}.


srfi-46
(syntax-rules ...)

Basic Syntax-rules Extensions
Built-in.
@xref{Hygienic macros}.

基本的なsyntax-rulesの拡張
組み込みです。
@xref{Hygienic macros}.


srfi-55, srfi-55
()

require-extension
Supported as an autoloaded macro.
@xref{Requiring extensions}.

requireの拡張
オートロードマクロとしてサポートされます。
@ref{Requiring extensions}参照。


srfi-60, srfi-60
()

Integers as bits
Most procedures are built-in: @xref{Bitwise operations}.
The complete support is in @code{srfi-60} module: @xref{Integers as bits}.

整数に対するビット操作
ほとんどの手続きは組み込みになっています。@ref{Bitwise operations}参照。
完全なサポートは@code{srfi-60}モジュールで提供されます。
@ref{Integers as bits}参照。

srfi-61
(cond else)

A more general @code{cond} clause
Supported natively.  @xref{Conditionals}.

より汎用的な@code{cond}節
組み込みです。@ref{Conditionals}参照。


srfi-62
()

S-expression comments
Supported by the native reader.  @xref{Lexical structure}.

S式コメント
ネイティブのリーダでサポートされています。@ref{Lexical structure}参照。


srfi-69
()

Basic hash tables
Supported by the module @code{srfi-69} (@pxref{Basic hash tables}).
Note that Gauche provides
built-in hashtable support (@pxref{Hashtables}).
They just use different names in some procedures.

基本的なハッシュテーブル
モジュール@code{srfi-69}でサポートされます (@ref{Basic hash tables}参照)。
Gaucheにも組み込みのハッシュテーブルサポートがありますが(@ref{Hashtables}参照)、
いくつかの手続きにおいて異なる名前を使っています。


srfi-78
()

Lightweight testing
Supported by the module @code{srfi-78}.
Since Gauche already has its own test framework (@pxref{Unit testing}),
this is mainly for third-party modules that adopt srfi-78 for testing.

軽いテストフレームワーク
モジュール@code{srfi-78}でサポートされます。
Gaucheは既に独自のテストフレームワークを持っているので(@ref{Unit testing}参照)、
このモジュールは主としてサードパーティ製のモジュールがsrfi-78をテストに
使っている場合をサポートするためのものです。


srfi-87
(case => else)

@code{=>} in case clauses
Supported natively.  @xref{Conditionals}.

case節での@code{=>}
組込みです。@ref{Conditionals}参照。

srfi-95
(sorted? merge merge! sort sort!)

Sorting and merging
Supported natively.  @xref{Sorting and merging}.

ソートとマージ
組み込みです。@ref{Sorting and merging}参照。

srfi-98, srfi-98
()

An interface to access environment variables
Supported by the module @code{srfi-98}.  @xref{Accessing environment variables}.

環境変数にアクセスするためのインタフェース
モジュール@code{srfi-98}でサポートされます。@ref{Accessing environment variables}参照。

srfi-99, gauche.record
()

ERR5RS Records
Supported by the module @code{gauche.record}.  @xref{Record types}.

ERR5RS レコード
モジュール@code{gauche.record}でサポートされます。@ref{Record types}参照。

srfi-106, srfi-106
()

Basic socket interface
Supported by the module @code{srfi-106}.  @xref{Basic socket interface}.

基本的なソケットインタフェース
モジュール@code{srfi-106}でサポートされます。@ref{Basic socket interface}参照。


srfi-111, srfi-111
(box box? unbox set-box!)

Boxes
Supported by the module @code{srfi-111}.  @xref{Boxes}.

ボックス
モジュール@code{srfi-111}でサポートされます。@ref{Boxes}参照。


srfi-112, srfi-112
()

Environment inquiry
Supported by the module @code{srfi-112}.
@xref{Portable runtime environment inquiry}.

環境の問い合わせ
モジュール@code{srfi-112}でサポートされます。
@ref{Portable runtime environment inquiry}参照。



srfi-113, srfi-113
()

Sets and Bags
Supported by the module @code{srfi-113}.  @xref{Sets and bags}.

セットとバッグ
モジュール@code{srfi-113}でサポートされます。 @ref{Sets and bags}参照。


srfi-114, srfi-114
()

Comparators
Some of the features are built-in (@pxref{Basic comparators}).  Full srfi spec is supported by the module @code{srfi-114} (@pxref{Comparators}).

比較器
いくつかの機能は組み込みです(@ref{Basic comparators}参照)。srfiの完全な仕様はモジュール@code{srfi-114}でサポートされます(@ref{Comparators}参照)。


srfi-117, srfi-117
()

Queues based on lists.
Supported by the module @code{srfi-117}, which is implemented on top
of @code{data.queue}.
(@pxref{Queues based on lists})

リストを元にしたキュー
@code{data.queue}上に実装されたモジュール@code{srfi-117}でサポートされます。
(@ref{リストを元にしたキュー}参照)

srfi-118, srfi-118
()

Simple adjustable-size strings
Supported by the module @code{srfi-118}.
(@pxref{Simple adjustable-size strings})

簡単な可変長文字列
モジュール@code{srfi-118}でサポートされます。
(@ref{Simple adjustable-size strings}参照)


srfi-121, gauche.generator
()

Generators
Gauche's @code{gauche.generator} is superset of srfi-121
(@pxref{Generators}).

ジェネレータ
Gaucheの@code{gauche.generator}がsrfi-121のスーパーセットになっています。
(@ref{Generators}参照)
