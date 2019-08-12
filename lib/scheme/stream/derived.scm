(define-module scheme.stream.derived
  (use util.stream :except (stream-take stream-drop))
  (export define-stream list->stream port->stream
          (rename stream+ stream) ;; differs from srfi-40
          stream->list stream-append stream-concat stream-constant
          stream-drop stream-drop-while stream-filter stream-fold
          stream-for-each stream-from stream-iterate stream-length
          stream-let stream-map stream-match stream-of stream-range
          stream-ref stream-reverse stream-scan stream-take
          stream-take-while stream-unfold stream-unfolds
          stream-zip))
(select-module scheme.stream.derived)

(define (stream-take n s) (stream-take-safe s n))
(define (stream-drop n s) (stream-take-drop s n))
          
          
          
