(***********************************************************************)
(*                                                                     *)
(*                             Efuns                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(*
(define-scwm-option *xlock-query-program* "xlock"
  "The name of a program to run to output a list of available xlock modes.
It will be run with the -help option, and its output should
conform to the standard xlock output format."
  #:type 'command
  #:group 'system
  )

;;; --------------------------------------
;;; The screen saver and screen lock menus
;;; --------------------------------------

(define*-public (xlock-query-modes #&optional (xlock *xlock-query-program* ))
  "Returns a list of mode names queried from the given XLOCK program.
Special modes \"random\", \"bomb\", and \"blank\" are not included in this list.
Use *xlock-query-program* to specify what program's output we should
read to compute the list of modes.  See also `make-xlock-menu'."
  (let ((pipe (open-input-pipe (string-append xlock " -help 2>&1"))))
    (do ((line (read-line pipe) (read-line pipe))
         (start-re (make-regexp "where mode is one of:" regexp/icase)))
        ((or (eof-object? line) (regexp-exec start-re line))
         (if (eof-object? line) (display "xlock modes not found\n"))))
    (do ((line (read-line pipe) (read-line pipe)) (match #f) (ml '())
         (mode-re (make-regexp "^[ 	]*([a-zA-Z0-9]+)")))
        ((eof-object? line) (close-pipe pipe)
         (reverse! (delete! "random" (delete! "bomb" (delete! "blank" ml)))))
      (set! match (regexp-exec mode-re line))
      (if match (set! ml (cons (match:substring match 1) ml))))))

(define-public xlock-options
  "-nice 19 +mousemotion +timeelapsed -lockdelay 600 -timeout 30")

(define (run-xlock mode lock)	; returns a lambda!
  (exe (string-append "xlock " xlock-options " -mode " mode
                      (if lock "" " -nolock"))))

(define*-public (make-xlock-menu #&optional (lock? #f))
  "Create an xlock menu.
To use this, add the following to the menu of your choice:
   (menuitem \"Screensaver\" #:action (make-xlock-menu #f))
or (menuitem \"Lock Screen\" #:action (make-xlock-menu #t))"
  (menu (append!
	 (list (menu-title (if lock? "Lock Screen" "Screensaver")) menu-separator
	       (menuitem "Random!" #:action (run-xlock "random" lock?))
	       (menuitem "Blank" #:action (run-xlock "blank" lock?))
	       (menuitem "Bomb" #:action (run-xlock "bomb" lock?)))
	 (fold-menu-list
	  (map (lambda (str) (menuitem str #:action (run-xlock str lock?)))
	       (xlock-query-modes))))))
*)