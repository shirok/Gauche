;; JSON pretty printer for Emacs batch mode

(javascript-mode)

;; Scoop suggests 4-space indentation
(setq json-encoding-default-indentation "    ")
(json-pretty-print-buffer)

;; Scoop recommends UTF-8 with MSDOS style line feed
(set-buffer-file-coding-system 'utf-8-dos)
(save-buffer)
