;;; leaf-tests.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
;; Keywords: .emacs

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
(require 'leaf)

(cond
 ((require 'ert nil t)
  (message "test with ert.")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  test settings
  ;;
  
  (unless (fboundp 'macroexpand-1)
    (defun macroexpand-1 (form &optional environment)
      "Perform (at most) one step of macroexpansion."
      (cond
       ((consp form)
	(let* ((head (car form))
               (env-expander (assq head environment)))
          (if env-expander
              (if (cdr env-expander)
                  (apply (cdr env-expander) (cdr form))
		form)
            (if (not (and (symbolp head) (fboundp head)))
		form
              (let ((def (autoload-do-load (symbol-function head) head 'macro)))
		(cond
		 ;; Follow alias, but only for macros, otherwise we may end up
		 ;; skipping an important compiler-macro (e.g. cl--block-wrapper).
		 ((and (symbolp def) (macrop def)) (cons def (cdr form)))
		 ((not (consp def)) form)
		 (t
                  (if (eq 'macro (car def))
                      (apply (cdr def) (cdr form))
                    form))))))))
       (t form))))
  
  (defmacro expand-minimally (form)
    `(let ((leaf-verbose 'errors)
           (leaf-expand-minimally t))
       (macroexpand ',form)))

  (defmacro expand-maximally (form)
    `(let ((leaf-verbose 'debug)
           (leaf-expand-minimally nil))
       (macroexpand ',form)))
  
  (defmacro match-expansion (form value)
    `(should
      (equal (expand-minimally ,form) ,value)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  test definition
  ;;

  (ert-deftest leaf-test:/disabled-1 ()
    (match-expansion
     (leaf foo :disabled t)
     'nil))
  
  (ert-deftest leaf-test/:if-1 ()
    (match-expansion
     (leaf foo :if t)
     `(if t
	  (progn
	    (require (quote foo) nil nil)))))

  (ert-deftest leaf-test/:if-2 ()
    (match-expansion
     (leaf foo :if (and t t))
     `(if (and t t)
	  (progn
	    (require (quote foo) nil nil)))))

  (ert-deftest leaf-test/:if-3 ()
    (match-expansion
     (leaf foo :if nil)
     `(if nil
	  (progn
	    (require (quote foo) nil nil)))))

  (ert-deftest leaf-test/:when-1 ()
    (match-expansion
     (leaf foo :when t)
     `(if t
	  (progn
	    (progn
	      (require (quote foo) nil nil))))))

  (ert-deftest leaf-test/:when-2 ()
    (match-expansion
     (leaf foo :when (and t t))
     `(if (and t t)
	  (progn
	    (progn
	      (require (quote foo) nil nil))))))

  (ert-deftest leaf-test/:when-3 ()
    (match-expansion
     (leaf foo :when nil)
     `(if nil
	  (progn
	    (progn
	      (require (quote foo) nil nil))))))

  (ert-deftest leaf-test/:unless-1 ()
    (match-expansion
     (leaf foo :unless t)
     `(if t
	  nil
	(progn
	  (require (quote foo) nil nil)))))

  (ert-deftest leaf-test/:unless-2 ()
    (match-expansion
     (leaf foo :unless (and t t))
     `(if (and t t)
	  nil
	(progn
	  (require (quote foo) nil nil)))))

  (ert-deftest leaf-test/:unless-3 ()
    (match-expansion
     (leaf foo :unless nil)
     `(if nil
	  nil
	(progn
	  (require (quote foo) nil nil)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  entry point
  ;;
  
  (defun leaf-run-tests-batch-and-exit ()
    (message (format "\n%s" (emacs-version)))
    (ert-run-tests-batch-and-exit)))
 (t
  (message "test without ert.")

  (defun leaf-run-tests-batch-and-exit ()
    (message (format "\n%s" (emacs-version))))))

(provide 'leaf-tests)
;;; leaf-tests.el ends here
