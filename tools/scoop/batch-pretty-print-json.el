;; JSON pretty printer for Emacs batch mode

(javascript-mode)

(setq json-encoding-default-indentation "    ")
(json-pretty-print-buffer)

(set-buffer-file-coding-system 'utf-8-dos)
(save-buffer)
