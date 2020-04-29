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
  (let1 gen ($ record-generator 
               $ remove #/^\;/
               $ file->string-list self :encoding 'utf-8)
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
     (error "Invalid entry for " (car ps))]))

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
(cond-expand)

Feature-based conditional expansion construct.
This has become a part of R7RS small.  Gauche supports this as
Built-in.   @xref{Feature conditional}.

機能ベースの条件展開
R7RS smallに取り込まれました。Gaucheは組み込みでサポートしています。
@ref{Feature conditional}参照。


srfi-1, srfi-1
()

List library (R7RS lists)
This has become a part of R7RS large.  @xref{R7RS lists}.
(Some of SRFI-1 procedures are built-in).

リストライブラリ
これはR7RS largeの一部になりました。@ref{R7RS lists}参照。
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
This has become a part of R7RS small.
Gauche supports this as built-in.  @xref{String ports}.

基本文字列ポート
これはR7RS smallの一部になりました。
Gaucheは組み込みでサポートしています。@ref{String ports}参照。


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


srfi-11
(let-values let*-values)

Syntax for receiving multiple values.
This has become a part of R7RS small.  Gauche supports it
as built-in.  @xref{Binding constructs}.

多値を受け取るための構文
これはR7RS smallの一部になりました。Gaucheは組み込みでサポートしています。
@ref{Binding constructs}参照。


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
This has become a part of R7RS large.
Character-set object and a few procedures are built-in.
@xref{Character set}.
Complete set of SRFI-14 is supported by the module @code{scheme.charset}.
@xref{R7RS character sets}.

文字集合のライブラリ
これはR7RS largeの一部になりました。
文字集合と基本的な手続きは組み込みになっています。
@ref{Character set}参照。
SRFI-14の完全なサポートはモジュール@code{scheme.charset}
で提供されています。@ref{R7RS character sets}参照。


srfi-16
(case-lambda)

Syntax for procedures of variable arity (case-lambda)
This has become a part of R7RS small.
Built-in.  @xref{Making procedures}.

可変長引数手続き構文 (case-lambda)
これはR7RS smallの一部になりました。
組み込みです。@ref{Making procedures}参照。


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
This has become a part of R7RS small.
Built-in.   @xref{Signaling exceptions}.

エラー報告機構
これはR7RS smallの一部になりました。
組み込みです。@ref{Signaling exceptions}参照。


srfi-25, gauche.array
()

Multi-dimensional array primitives.
Supported by the module @code{gauche.array}, which defines
superset of SRFI-25.  @xref{Arrays}.

多次元配列のプリミティブ
モジュール@code{gauche.array}が、SRFI-25の上位互換と
なっています。@ref{Arrays}参照。


srfi-26
(cut cute)

Notation for specializing parameters without currying.
Built-in.  @xref{Making procedures}.

カリー化をともなわないパラメータの特殊化記法。組み込みです。
@ref{Making procedures}参照。


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
This has become a part of R7RS small.
Supported by the native reader.  @xref{Lexical structure}.

ネストした複数行コメント
これはR7RS smallの一部になりました。
ネイティブのリーダでサポートされています。@ref{Lexical structure}参照。


srfi-31
(rec)

A special form rec for recursive evaluation.  Built-in.
@xref{Binding constructs}.

再帰評価用の特殊フォーム@code{rec}. 組み込みです。
@ref{Binding constructs}参照。


srfi-34
(with-exception-handler guard raise)

Exception Handling for Programs
This has become a part of R7RS small.
Built-in.  @xref{Exceptions}.
(However, Gauche implements srfi-18's semantics of @code{raise} literally,
which differs slightly from srfi-34's.  This may be changed in future.)

プログラムの例外処理
これはR7RS smallの一部になりました。
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
This has become a part of R7RS small.
Supported by the module @code{gauche.parameter}.
@xref{Parameters}.

パラメータオブジェクト
これはR7RS smallの一部になりました。
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


srfi-41, srfi-41
()

Streams
This has become a part of R7RS large.  @xref{R7RS stream}.
Most of stream procedures are also in @code{util.stream}
(@pxref{Stream library}).

ストリーム
これはR7RS largeの一部になりました。@ref{R7RS stream}参照。
ストリーム手続きのほぼ全ては@code{util.stream}からも提供されています
(@ref{Stream library}参照)。


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
@xref{Vector library (Legacy)}.
Note that this srfi is superseded by R7RS @code{scheme.vector}
library (formerly known as @code{srfi-133}).  @xref{R7RS vectors}.

ベクタライブラリ
モジュール@code{srfi-43}でサポートされます。
@ref{Vector library (Legacy)}参照。
註：このsrfiはR7RSの@code{scheme.vector}ライブラリ
(@code{srfi-133}として導入されたもの)により置き換えられました。
@ref{R7RS vectors}参照。


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
This has become a part of R7RS small.
Built-in.
@xref{Hygienic macros}.

基本的なsyntax-rulesの拡張
これはR7RS smallの一部となりました。
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
Most procedures are built-in: @xref{R7RS bitwise operations}.
The complete support is in @code{srfi-60} module: @xref{Integers as bits}.

整数に対するビット操作
ほとんどの手続きは組み込みになっています。@ref{R7RS bitwise operations}参照。
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
This has become a part of R7RS small.
Supported by the native reader.  @xref{Lexical structure}.

S式コメント
これはR7RS smallの一部となりました。
ネイティブのリーダでサポートされています。@ref{Lexical structure}参照。


srfi-64, srfi-64
()

A Scheme API for test suites
Supported by the module @code{srfi-64}.

テストスイートのためのScheme API
モジュール@code{srfi-64}でサポートされます。


srfi-66, srfi-66
()

Octet vectors
Supported by the module @code{srfi-66} (@pxref{Octet vectors}).
This is mostly a subset of @code{gauche.uvector}, but has one
slight difference.

オクテットベクタ
モジュール@code{srfi-66}でサポートされます (@ref{Octet vectors}参照)。
ほぼ@code{gauche.uvector}のサブセットですが、
一ヶ所だけ違いがあります。


srfi-69, srfi-69
()

Basic hash tables
Supported by the module @code{srfi-69} (@pxref{Basic hash tables}).
Note that this srfi is superseded by R7RS @code{scheme.hash-table}
library (formerly known as @code{srfi-125}).
@xref{R7RS hash tables}.

基本的なハッシュテーブル
モジュール@code{srfi-69}でサポートされます (@ref{Basic hash tables}参照)。
このsrfiはR7RSの@code{scheme.hash-table}
(@code{srfi-125}として導入されたもの) で置き換えられました。
詳しくは@ref{R7RS hash tables}を参照してください。


srfi-74, srfi-74
()

Octet-addressed binary blocks
Supported by the module @code{srfi-74} (@pxref{Octet-addressed binary blocks}).

オクテット毎にアクセス可能なバイナリブロック
モジュール@code{srfi-74}でサポートされます (@ref{Octet-addressed binary blocks}参照)。



srfi-78, srfi-78
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
This has become a part of R7RS small.
Supported natively.  @xref{Conditionals}.

case節での@code{=>}
これはR7RS smallの一部となりました。
組込みです。@ref{Conditionals}参照。


srfi-95
(sorted? merge merge! sort sort!)

Sorting and merging
Supported natively.  @xref{Sorting and merging}.

ソートとマージ
組み込みです。@ref{Sorting and merging}参照。


srfi-96, srfi-96
()

SLIB Prerequisites
This srfi is not exactly a library, but
rather a description of features the platform should provide
to support SLIB.  In order to load this module, SLIB must be
already installed.  @xref{SLIB}, for the details.  

SLIB必要事項
このsrfiはライブラリというよりは、SLIBをサポートするためにプラットフォームが
提供しなければならない機能要件を記述するものです。このモジュールをロードするためには、
SLIBが既にインストールされていることが必要です。
詳しくは@ref{SLIB}を参照してください。


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


;; We don't load srfi-101 on (cond-expand ((srfi-101) ...)) for now, since
;; it would shadow many list primitives and may create wreck havoc.  Especialy
;; it would make SLIB unusable, for it uses cond-expand to check library
;; availability.
srfi-101
()

Purely functional random-access pairs and lists
This has become a part of R7RS large.
Supported by the module @code{scheme.rlist}.

純粋に関数的でランダムアクセス可能なペアとリスト
これはR7RS largeの一部となりました。
モジュール@code{scheme.rlist}でサポートされます。


srfi-106, srfi-106
()

Basic socket interface
Supported by the module @code{srfi-106}.  @xref{Basic socket interface}.

基本的なソケットインタフェース
モジュール@code{srfi-106}でサポートされます。@ref{Basic socket interface}参照。


srfi-111, srfi-111
(box box? unbox set-box!)

Boxes
This has become a part of R7RS large.
Supported by the module @code{scheme.box}.  @xref{R7RS boxes}.

ボックス
これはR7RS largeの一部となりました。
モジュール@code{scheme.box}でサポートされます。@ref{R7RS boxes}参照。


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
This has become a part of R7RS large.
Supported by the module @code{scheme.set}.  @xref{R7RS sets}.

セットとバッグ
これはR7RS largeの一部となりました。
モジュール@code{scheme.set}でサポートされます。 @ref{R7RS sets}参照。


srfi-114, srfi-114
()

Comparators
Some of the features are built-in (@pxref{Basic comparators}).  Full srfi spec is supported by the module @code{srfi-114} (@pxref{Comparators}).

比較器
いくつかの機能は組み込みです(@ref{Basic comparators}参照)。srfiの完全な仕様はモジュール@code{srfi-114}でサポートされます(@ref{Comparators}参照)。


srfi-115, srfi-115
()

Scheme Regular Expressions
This has become a part of R7RS large.
Supported by the module @code{scheme.regex}.  @xref{R7RS regular expressions}.

Scheme正規表現
これはR7RS largeの一部となりました。
モジュール@code{scheme.regex}でサポートされます。@ref{R7RS regular expressions}参照。


srfi-116, srfi-116
()

Immutable List Library
This has become a part of R7RS large.
Immutable pairs are supported natively (@pxref{Mutable and immutable pairs}).
Full set of APIs are available
in the module @code{scheme.ilist} (@pxref{R7RS immutable lists}).

変更不可リストのライブラリ
これはR7RS largeの一部となりました。
変更不可なペアはGauche本体で直接サポートされます(@ref{Mutable and immutable pairs}参照)。
フルセットのAPIは モジュール@code{scheme.ilist}でサポートされます(@ref{R7RS immutable lists}参照)。


srfi-117, srfi-117
()

Queues based on lists.
This has become a part of R7RS large.
Supported by the module @code{scheme.list-queue}, which is implemented on top
of @code{data.queue}.
(@pxref{R7RS list queues})

リストを元にしたキュー
これはR7RS largeの一部となりました。
@code{data.queue}上に実装されたモジュール@code{scheme.list-queue}でサポートされます。
(@ref{R7RS list queues}参照)

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
This has become a part of R7RS large.
Gauche's @code{gauche.generator} is superset of srfi-121
(@pxref{Generators}).

ジェネレータ
これはR7RS largeの一部となりました。
Gaucheの@code{gauche.generator}がsrfi-121のスーパーセットになっています。
(@ref{Generators}参照)


srfi-124, srfi-124
()

Ephemerons
This has become a part of R7RS large.
Supported by @code{scheme.ephemeron}.
Note: Current Gauche's implementation isn't optimal.
@xref{R7RS ephemerons}.

Ephemeron
これはR7RS largeの一部となりました。
@code{scheme.ephemeron}モジュールでサポートされます
(註: 現在のGaucheでの実装は理想的ではありません。)
@ref{R7RS ephemerons}参照。


srfi-125, srfi-125
()

Intermediate hash tables
This has become a part of R7RS large.
Supported by @code{scheme.hash-table}
(@pxref{R7RS hash tables}).
Note that Gauche's native interface provides the same functionalities, but
under slightly different names for the backward compatibility.
@xref{Hashtables}.

ハッシュテーブル中位層
これはR7RS largeの一部となりました。
@code{scheme.hash-table}モジュールでサポートされます
(@ref{R7RS hash tables}参照)。
Gaucheの組み込みインタフェースも同じ機能を提供していますが、
既存のコードとの互換性のために名前が若干異なります (@ref{Hashtables}参照)。


srfi-127, srfi-127
()

Lazy sequences
This has become a part of R7RS large.
Supported by @code{scheme.lseq} (@pxref{R7RS lazy sequences}).

遅延シーケンス
これはR7RS largeの一部となりました。
@code{scheme.lseq}モジュールでサポートされます。
@ref{R7RS lazy sequences}参照。


srfi-128, srfi-128
()

Comparators (reduced)
This has become a part of R7RS large.
Built-in.  @xref{Basic comparators}, for the details.

比較器 (縮小版)
これはR7RS largeの一部となりました。
組み込みです。@ref{Basic comparators}参照。


srfi-129, srfi-129
()

Titlecase procedures
The procedures @code{char-title-case?} and @code{char-titlecase} are
built-in, and @code{string-titlecase} is in @code{gauche.unicode}.
For the compatibility, you can @code{(use srfi-129)} or
@code{(import (srfi 129))} to get these three procedures.

Titlecase手続き
@code{char-title-case?}と@code{char-titlecase}は組み込みで、
@code{string-titlecase}は@code{gauche.unicode}モジュールにあります。
互換性のため、@code{(use srfi-129)}や@code{(import (srfi 129))}とすれば
この3つの手続きがインポートされます。


srfi-130, srfi-130
()

Cursor-based string library
String cursors are supported natively (@pxref{String cursors}).
Most of built-in and srfi-13 string procedures accept cursors in addition
to indexes.  The module srfi-130 provides several procedures that
has the same name as srfi-13 but returns a string cursor instead
of an index (@pxref{Cursor-based string library}).

カーソルベースの文字列ライブラリ
文字列カーソルは組み込みでサポートされています (@ref{String cursors}参照)。
また、多くの組み込みおよびsrfi-13手続きはインデックスに加えてカーソルも
取れるようになっています。モジュールsrfi-130は、srfi-13と同名だけれど
インデックスの代わりにカーソルを返す手続きを提供します
(@ref{Cursor-based string library}参照)。


srfi-131, gauche.record
()

ERR5RS Record Syntax (reduced)
This srfi is a pure subset of srfi-99, and @code{gauche.record}'s
@code{define-record-type} covers it.  @xref{Record types}.

ERR5RSレコード構文 (簡易版)
このsrfiはsrfi-99のサブセットで、@code{gauche.record}の
@code{define-record-type}でカバーされます。@ref{Record types}参照。


srfi-132, srfi-132
()

Sort libraries
This has become a part of R7RS large.
Supported by the module @code{scheme.sort}.
@xref{R7RS sort}.

ソートライブラリ
これはR7RS largeの一部となりました。
モジュール@code{scheme.sort}でサポートされます。
@ref{R7RS sort}参照。


srfi-133, srfi-133
()

Vector library (R7RS-compatible)
This has become a part of R7RS large.
Supported by the module @code{scheme.vector}.  @xref{R7RS vectors}.

ベクタライブラリ (R7RS互換)
これはR7RS largeの一部となりました。
モジュール@code{scheme.vector}でサポートされます。 @ref{R7RS vectors}参照。


srfi-134, data.ideque
()

Immutable Deques
This has become a part of R7RS large.
The module @code{data.ideque} is compatible to srfi-134.
@xref{Immutable deques}.

変更不可な両端キュー
これはR7RS largeの一部となりました。
モジュール@code{data.ideque}がsrfi-134と互換です。
@ref{変更不可な両端キュー}参照。


srfi-135, srfi-135
()

Immutable Texts
This has become a part of R7RS large.
In Gauche, the text type is not disjoint from the string type.  Instead,
a text is simply an immutable and indexed string.
@xref{String indexing}, for the detail of indexed string.
The API is described in @ref{R7RS immutable texts}.

変更不可なテキスト
これはR7RS largeの一部となりました。
Gaucheではtext型はstring型と独立してはおらず、
textは単に「変更不可でインデクスされた文字列」にすぎません。
文字列のインデクシングについては@ref{String indexing}を参照してください。
APIは@ref{R7RS immutable texts}で説明しています。


srfi-141, srfi-141
()


Integer division
This has become a part of R7RS large.
Supported by the module @code{scheme.division}.  @xref{R7RS integer division}.

整数除算
これはR7RS largeの一部となりました。
モジュール@code{scheme.division}でサポートされます。@ref{R7RS integer division}参照。


srfi-143, srfi-143
()

Finxums
This has become a part of R7RS large.
Supported by the module @code{scheme.fixnum}.  @xref{R7RS fixnum}.

Fixnums
これはR7RS largeの一部となりました。
モジュール@code{scheme.fixnum}でサポートされます。@ref{R7RS fixnum}参照。


srfi-144, srfi-144
()

Flonums
This has become a part of R7RS large.
Supported by the module @code{scheme.flonum}.  @xref{R7RS flonum}.

Flonums
これはR7RS largeの一部となりました。
モジュール@code{scheme.flonum}でサポートされます。@ref{R7RS flonum}参照。


srfi-145
(assume)

Assumptions
Built-in.  @xref{Conditionals}.

アサンプション
組み込みです。@ref{Conditionals}参照。


srfi-146, srfi-146
()

Mappings
This has become a part of R7RS large.
Supported by the module @code{scheme.mapping}.
@xref{R7RS mappings}.

マッピング
これはR7RS largeの一部となりました。
モジュール@code{scheme.mapping}でサポートされます。
@ref{R7RS mappings}参照。


srfi-149
(syntax-rules)

Basic syntax-rules template extensions
The built-in @code{syntax-rules} support @code{srfi-149}.

基本的なsyntax-rulesの拡張
組み込みの@code{syntax-rules}は@code{srfi-149}に対応しています。


srfi-151, srfi-151
()

Bitwise operations
Supported by the module @code{srfi-151} (@pxref{R7RS bitwise operations}).
Note that many equivalent procedures are provided built-in
(@pxref{Basic bitwise operations}).

ビット演算
モジュール@code{srfi-151}でサポートされます (@ref{R7RS bitwise operations}参照)。
なお、組み込みでも多くの手続きについては同等の手続きが提供されています
(@ref{Basic bitwise operations}参照).


srfi-152, srfi-152
()

String library (reduced)
Supported by the module @code{srfi-152} (@pxref{String library (reduced)}).

文字列ライブラリ(簡略版)
モジュール@code{srfi-152}でサポートされます (@ref{String library (reduced)}参照)。


srfi-154, srfi-154
()

First-class dynamic extents
Suppored by the module @code{srfi-154}.  (@pxref{First-class dynamic extents}).

第一級の動的エクステント
モジュール@code{srfi-154}でサポートされます。(@ref{First-class dynamic extents}参照)。


srfi-158, srfi-158
()

Generators and accumulators
This has become a part of R7RS large.
Supported by the module @code{scheme.generator}
(@pxref{R7RS generators}).
Note that most of generator procedures are supported by @code{gauche.generator}
(@pxref{Generators}).

ジェネレータとアキュムレータ
これはR7RS largeの一部となりました。
モジュール@code{scheme.generator}でサポートされます
 (@ref{R7RS generators}参照)。
ジェネレータ手続きのほとんどは、@code{gauche.generator}でもサポートされています
(@ref{Generators}参照)。


srfi-159, srfi-159
()

Combinator Formatting
This has become a part of R7RS large.  @xref{R7RS combinator formatting}.

コンビネータによるフォーマッタ
これはR7RS largeの一部になりました。@ref{R7RS combinator formatting}参照。

srfi-160, srfi-160
()

Homogeneous numeric vector libraries
This has become a part of R7RS large,
supported by the module @code{scheme.vector.@@} where
@code{@@} is one of @code{base},
@code{u8}, @code{s8}, @code{u16}, @code{s16},
@code{u32}, @code{s32}, @code{u64}, @code{s64}, @code{f32}, @code{f64},
@code{c64}, or @code{c128} (@pxref{R7RS uniform vectors}).

均質な数値ベクタライブラリ
これはR7RS largeの一部となりました。
モジュール@code{scheme.vector.@@}でサポートされます
(但し@code{@@}は@code{base},
@code{u8}, @code{s8}, @code{u16}, @code{s16},
@code{u32}, @code{s32}, @code{u64}, @code{s64}, @code{f32}, @code{f64},
@code{c64}, @code{c128})。
@ref{R7RS uniform vectors}参照。


srfi-162, srfi-162
()

Comparators sublibrary
Supported by the module @code{srfi-162}.  @xref{Comparator sublibrary}.

比較器のサブライブラリ
モジュール@code{srfi-162}でサポートされます。@ref{Comparator sublibrary}参照。


srfi-173, srfi-173
()

Hooks
Supported by the module @code{srfi-173} (@pxref{Hooks (srfi)}),
which is a thin layer on top of @code{gauche.hook} (@pxref{Hooks})

フック
モジュール@code{srfi-173}でサポートされます (@ref{Hooks (srfi)}参照) 。
それ自体は@code{gauche.hook}の上にかぶせた薄いレイヤです (@ref{Hooks}参照)。


srfi-174, srfi-174
()

POSIX Timespecs
Supported by the module @code{srfi-174} (@pxref{POSIX timespecs}).
In Gauche, the timespec type is the same as built-in @code{<time>}
object, which is also the same as srfi-19 time.

POSIX Timespec
モジュール@code{srfi-174}でサポートされます (@ref{POSIX timespecs}参照)。
Gaucheでは、timespecオブジェクトは組み込みの@code{<time>}オブジェクトであり、
またsrfi-19のtimeオブジェクトとも共通です。


srfi-176, srfi-176
()

Version flag
Supported as a command-line flag of @code{gosh}.
The @code{version-alist} procedure is built-in.

バージョンフラグ
@code{gosh}のコマンドライン引数としてサポートされます。
@code{version-alist}手続きは組み込みです。


srfi-185, srfi-185
()

Linear adjustable-length strings
Supported by the module @code{srfi-185}.

線形更新な可変長文字列
モジュール@code{srfi-185}でサポートされます。
