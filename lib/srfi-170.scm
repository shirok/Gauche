;;;
;;;  Preliminary implementation of srfi-170
;;;

(define-module srfi-170
  (use file.util)
  (export fdes->textual-input-port
          fdes->binary-input-port
          fdes->textual-output-port
          fdes->binary-output-port
          port-fdes
          close-fdes
          
          create-directory
          create-fifo
          create-hard-link
          create-symlink
          read-symlink
          rename-file
          delete-directory

          set-file-mode
          set-file-owner
          set-file-group
          set-file-timespecs

          truncate-file

          file-info
          file-info?
          file-info:device
          file-info:inode
          file-info:mode
          file-info:nlinks
          file-info:uid
          file-info:gid
          file-info:rdev
          file-info:size
          file-info:blksize
          file-info:atime
          file-info:mtime
          file-info:ctime
          file-info-directory?
          file-info-fifo?
          file-info-symlink?
          file-info-regular?

          directory-files
          make-directory-files-generator
          open-directory
          read-directory
          close-directory

          real-path
          temp-file-prefix
          create-temp-file
          call-with-temporary-filename
          
          umask
          set-umask
          working-directory
          set-working-directory
          pid
          parent-pid
          process-group
          nice
          user-uid
          user-gid
          user-effective-uid
          user-effective-gid
          user-supplementary-gids

          user-info
          user-info?
          user-info:name
          user-info:uid
          user-info:gid
          user-info:home-dir
          user-info:shell
          user-info:full-name
          user-info:parsed-full-name

          groupinfo
          group-info?
          group-info:name
          group-info:gid

          posix-time
          monotonic-time

          terminal?
          terminal-file-name
          with-raw-mode
          with-rare-mode
          without-echo
          ))
(select-module srfi-170)

;; 3.1 I/O

(define (fdes->textual-input-port fd)  (open-input-fd-port fd))
(define (fdes->binary-input-port fd)   (open-input-fd-port fd))
(define (fdes->textual-output-port fd) (open-output-fd-port fd))
(define (fdes->binary-output-port fd)  (open-output-fd-port fd))
(define (port-fdes port) (port-file-number port))
;; (close-fdes fd)

;; 3.3 File system

(define (create-directory name :optional perm)
  (sys-mkdir name perm))
(define (create-fifo name :optional perm)
  (sys-mkfifo name perm))
(define (create-hard-link old new)
  (sys-link old new))
(define (create-symlink old new)
  (sys-symlink old new))



          
