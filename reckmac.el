;;; reckmac.el --- Recursive keyboard macros -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Nicholas Hubbard <nicholashubbard@posteo.net>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; Author: Nicholas Hubbard <nicholashubbard@posteo.net>
;; URL: http://github.com/NicholasBHubbard/recmac.el
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0
;; Created: 2022-03-19
;; By: Nicholas Hubbard <nicholashubbard@posteo.net>
;; Keywords: kmacro, convenience

;;; Commentary:

;; This package provides functionality for creating kbd macros incrementally by
;; chaining together previously recorded kbd macros.

;;; Code:

(require 'seq)


;;; --- internal state

(defvar reckmac--register-macro-alist nil
  "Internal data structure for reckmac macro state.")

(defvar reckmac--current-register nil
  "Internal variable to track the current register.")

(defvar reckmac--built-macro nil
  "Internal data structure used to build macros.")


;;; --- implementation convenience functions

(defun reckmac-vector-macro (macro)
  "Return the macro MACRO as a vector."
  (cond ((vectorp macro) macro)
        ((stringp macro) (string-to-vector macro))
        (t (error "The macro \"%s\" is not a string or vector" macro))))

(defun reckmac-register-p (register)
  "Return T if REGISTER is a valid reckmac register and return NIL otherwise."
  (characterp register))

(defun reckmac-register-occupied-p (register)
  "Return T if the reckmac register REGISTER contains a macro and return NIL
otherwise."
  (if (assoc register reckmac--register-macro-alist #'=) t nil))

(defun reckmac--append-to-built-macro (&rest macros)
  "Append every macro in MACROS to `reckmac--built-macro'."
  (dolist (macro macros)
    (let ((macro (reckmac-vector-macro macro)))
      (setq reckmac--built-macro (seq-concatenate 'vector reckmac--built-macro macro)))))

(defun reckmac-register-macro (register)
  "Return the macro stored in the register REGISTER."
  (cdr (assoc register reckmac--register-macro-alist #'=)))

(defun reckmac-occupied-registers ()
  "Return a list of all occupied registers."
  (mapcar #'car reckmac--register-macro-alist))


;;; --- core

(defun reckmac-start-or-end-macro (&optional register)
  "If not currently recording a macro then start recording to register REGISTER. 
If already recording a macro then finish recording."
  (interactive
   (if defining-kbd-macro
       (list nil)
     (list (read-char "Record to register: " t))))
  (if defining-kbd-macro
      (reckmac-end-macro)
    (reckmac-start-macro register)))

(defun reckmac-start-macro (register)
  "Begin recording a kbd macro that will be stored in the register REGISTER."
  (if (not (reckmac-register-p register))
      (user-error "Invalid reckmac register %s" register)
    (setq reckmac--built-macro (make-vector 0 0))
    (setq reckmac--current-register register)
    (let ((inhibit-message t))
      (start-kbd-macro nil))
    (message "Recording macro to register %s" (char-to-string register))))

(defun reckmac-end-macro ()
  "Finish recording a kbd macro."
  (let ((inhibit-message t))
    (end-kbd-macro))
  (reckmac--append-to-built-macro last-kbd-macro)
  (message "Finished recording macro to register %s" (char-to-string reckmac--current-register))
  (setf (alist-get reckmac--current-register reckmac--register-macro-alist) reckmac--built-macro))

(defun reckmac-execute-macro (register)
  "Execute the kbd macro stored in register REGISTER. If currently recording a
macro then recur on the macro in stored in REGISTER."
  (interactive (list (read-char "Execute macro in register: " t)))
  (let ((macro (reckmac-register-macro register)))
    (cond ((not macro)
           (user-error "No macro in register %s" (char-to-string register)))
          ((not defining-kbd-macro)
           (execute-kbd-macro macro))
          (t
           (reckmac-recur register)))))

(defun reckmac-execute-last-macro ()
  "Execute the most recently recorded reckmac kbd macro. If currently recording
a macro then recur on the most previously defined macro."
  (interactive)
  (unless reckmac--current-register
    (user-error "No reckmac macros have been recorded"))
  (reckmac-execute-macro reckmac--current-register))

(defun reckmac-recur (&optional register)
  "Call the macro in REGISTER such that it will be included in the new macro
that is currently being recorded. 

REGISTER defaults to `reckmac--current-register'."
  (if (not defining-kbd-macro) (user-error "Cannot recur unless currently recording a macro"))
  (if (null register) (setq register reckmac--current-register))
  (let ((recur-macro (reckmac-register-macro register))
        (inhibit-message t))
    (unless recur-macro
      (user-error "No macro in register %s" register))
    (end-kbd-macro)
    (reckmac--append-to-built-macro last-kbd-macro recur-macro)
    (execute-kbd-macro last-kbd-macro)
    (start-kbd-macro nil)))


;;; --- done

(provide 'reckmac)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; reckmac.el ends here
