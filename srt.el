;;; srt.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
;; Keywords: test

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar srt-version 0.5
  "srt.el version")

(defvar srt-errorp nil
  "When test fail, this flag will be t.")

(defvar srt-test-cases nil
  "Test list such as ((TEST-NAME VALUE) (TEST-NAME VALUE))")

(defvar srt-debug nil
  "If non nil, print debug messages")

(defvar srt-enable-color t ;; (when window-system)
  "If non nil, enable color message to output with meta character.")

(defvar srt-header-message (if srt-enable-color
			       "\n\e[33mRunning %d tests...\e[m\n"
			     "\nRunning %d tests...\n")
  "Header message")

(defvar srt-passed-label (if srt-enable-color
			     "\e[36mPASSED\e[m"
			   "[PASSED]")
  "Passed label.")

(defvar srt-fail-label (if srt-enable-color
			   "\e[31mFAILED\e[m"
			 "[FAILED]")
  "Fail label.")

(defvar srt-error-message (if srt-enable-color
			      "\e[31m===== Failed test(s) =====\e[m"
			    "===== Failed test(s) =====")
  "Error message")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  for old Emacs
;;

(unless (fboundp 'pp-to-string)
  (defalias 'pp-to-string 'print))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support functions
;;

(defun srt-testpass (name key form expect)
  (let* ((mesheader  (format "%s  %s\n" srt-passed-label name))
	 (mes        (concat mesheader)))
    (princ mes)))

(defun srt-testfail (name key form expect)
  (let* ((mesheader (format "%s  %s\n" srt-fail-label name))
	 (meskey    (format "< tested on %s >\n" key))
	 (mesform   (format "form:\n%s\n" (pp-to-string form)))
	 (mesexpect (format "expected:\n%s\n" (pp-to-string expect)))
	 (mes       (concat mesheader meskey mesform mesexpect)))
    (princ mes)
    (setq srt-errorp t)))

(defun srt-test (key form expect &optional special)
  (if (not special)
      (let* ((funcname
	      (replace-regexp-in-string "^:+" "" (symbol-name key)))
	     (funcsym (intern funcname)))
	(funcall funcsym form expect))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  main macro
;;

(defmacro srt-deftest (name value)
  (declare (indent 1))
  `(add-to-list 'srt-test-cases '(',name ,value) t))

(defun srt-run-tests-batch-and-exit ()
  (princ (format srt-header-message (length srt-test-cases)))
  (princ (format "%s\n" (emacs-version)))
  (mapc (lambda (x)
	  (let* ((name    (pop x))
		 (value   (pop x))
		 (key     (pop value))
		 (form    (pop value))
		 (expect  (pop value))
		 (special (pop value)))
	    
	    (if srt-debug
		(princ (format "name: %s\nvalue: %s\nkey: %s\nform: %s\nexpect: %s\nspecial: %s\n"
			       (pp-to-string name)
			       (pp-to-string value)
			       (pp-to-string key)
			       (pp-to-string form)
			       (pp-to-string expect)
			       (pp-to-string special))))
	    
	    (if (srt-test key form expect special)
		(srt-testpass name key form expect)
	      (srt-testfail name key form expect))))
	srt-test-cases)
  (princ "\n\n")
  (when srt-errorp
    (error srt-error-message)))

(provide 'srt)
;;; srt.el ends here
