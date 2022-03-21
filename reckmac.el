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


;;; --- minor mode

(defvar reckmac--recording-macro-mode-map (make-sparse-keymap)
  "Keymap for reckmac--recording-macro-mode.")

(define-minor-mode reckmac--recording-macro-mode
  "Toggle reckmac--recording-macro-mode.

This mode should not be turned on manually! Used for internal implementation of
reckmac."
  :global t
  :require 'reckmac
  :keymap reckmac--recording-macro-mode-map)


;;; --- implementation convenience functions

(defun reckmac-vector-macro (macro)
  "Return the macro MACRO as a vector."
  (cond ((vectorp macro) macro)
        ((stringp macro) (string-to-vector macro))
        (t (error "The macro \"%s\" is not a string or vector" macro))))

(defun reckmac-register-p (register)
  "Return T if REGISTER is a valid reckmac register and return NIL otherwise."
  (characterp register))

(defun reckmac-register-or-error (register)
  "Throw an error if REGISTER is not a valid reckmac register."
  (if (reckmac-register-p register)
      t
    (error "Invalid reckmac register: \"%s\"" register)))

(defun reckmac-register-occupied-p (register)
  "Return T if the reckmac register REGISTER contains a macro and return NIL
otherwise."
  (reckmac-register-or-error register)
  (if (assoc register reckmac--register-macro-alist #'=) t nil))

(defun reckmac--append-to-built-macro (&rest macros)
  "Append every macro in MACROS to `reckmac--built-macro'."
  (dolist (macro macros)
    (let ((macro (reckmac-vector-macro macro)))
      (setq reckmac--built-macro (seq-concatenate 'vector reckmac--built-macro macro)))))

(defun reckmac-register-macro (register)
  "Return the macro stored in the register REGISTER."
  (reckmac-register-or-error register)
  (cdr (assoc register reckmac--register-macro-alist #'=)))

(defun reckmac-occupied-registers ()
  "Return a list of all occupied registers."
  (mapcar #'car reckmac--register-macro-alist))

(defun reckmac-advice-end-kbd-macro ()
  "Advice `end-kbd-macro' to turn off `reckmac--recording-macro-mode' in case
macro recording aborts prematurely."
  (advice-add 'end-kbd-macro :after '(lambda (&rest _args)
                                       (reckmac--recording-macro-mode 0))))

(defun reckmac-unadvice-end-kbd-macro ()
  "Remove advice added by `reckmac-advice-end-kbd-macro'."
  (advice-remove 'end-kbd-macro '(lambda (&rest _args)
                                   (reckmac--recording-macro-mode 0))))


;;; --- core

(defun reckmac-start-or-end-macro (&optional register)
  "If not currently recording a macro then start recording to register REGISTER. 
If already recording a macro then finish recording."
  (interactive
   (if reckmac--recording-macro-mode
       (list nil)
     (list (read-char "Record to register: " t))))
  (if reckmac--recording-macro-mode
      (reckmac-end-macro)
    (reckmac-start-macro register)))

(defun reckmac-start-macro (register)
  "Begin recording a kbd macro that will be stored in the register REGISTER."
  (reckmac-register-or-error register)
  (reckmac-advice-end-kbd-macro)
  (setq reckmac--built-macro (make-vector 0 0))
  (setq reckmac--current-register register)
  (let ((inhibit-message t))
    (start-kbd-macro nil))
  (reckmac--recording-macro-mode 1)
  (message "Recording kbd macro to register \"%s\"" (char-to-string register)))

(defun reckmac-end-macro ()
  "Finish recording a kbd macro."
  (interactive)
  (let ((inhibit-message t))
    (end-kbd-macro))
  (reckmac--append-to-built-macro last-kbd-macro)
  (reckmac--recording-macro-mode 0)
  (reckmac-unadvice-end-kbd-macro)
  (message "Done recording kbd macro to register \"%s\"" (char-to-string reckmac--current-register))
  (setf (alist-get reckmac--current-register reckmac--register-macro-alist) reckmac--built-macro))

(defun reckmac-execute-macro (register)
  "Execute the kbd macro stored in register REGISTER."
  (interactive (list (read-char "Execute macro in register: " t)))
  (let ((macro (reckmac-register-macro register)))
    (if (not macro)
        (message "No macro in register \"%s\"" (char-to-string register))
      (execute-kbd-macro macro))))

(defun reckmac-execute-last-macro ()
  "Execute the most recently recorded reckmac kbd macro. If currently recording
a macro then recur on the most previously defined macro."
  (interactive)
  (if reckmac--recording-macro-mode
      (reckmac-recur reckmac--current-register)
    (reckmac-execute-macro reckmac--current-register)))

(defun reckmac-recur (&optional register)
  "Call the macro in REGISTER such that it will be included in the new macro
that is currently being recorded. 

REGISTER defaults to `reckmac--current-register'."
  (interactive "i")
  (if (null register) (setq register reckmac--current-register))
  (let ((recur-macro (reckmac-register-macro register))
        (inhibit-message t))
    (if (not recur-macro)
        (progn
          (message "Register \"%s\" is empty. Committing macro as is." (char-to-string register))
          (let ((inhibit-message t))
            (reckmac-end-macro)))
      (reckmac-unadvice-end-kbd-macro)
      (end-kbd-macro)
      (reckmac-advice-end-kbd-macro)
      (let ((recorded-macro last-kbd-macro))
        (execute-kbd-macro recur-macro)
        (reckmac--append-to-built-macro recorded-macro recur-macro)
        (start-kbd-macro nil)))))


;;; --- done

(provide 'reckmac)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; reckmac.el ends here
