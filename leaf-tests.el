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
    `(let ((use-package-verbose 'errors)
           (use-package-expand-minimally t))
       (macroexpand-1 ',form)))

  (defmacro expand-maximally (form)
    `(let ((use-package-verbose 'debug)
           (use-package-expand-minimally nil))
       (macroexpand-1 ',form)))
  
  (defmacro match-expansion (form &rest value)
    `(should (pcase (expand-minimally ,form)
               ,@(mapcar #'(lambda (x) (list x t)) value))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;  test definition
  ;;
  
  ;; (ert-deftest leaf ()
  ;;   (should (equal EXPECTED ACTUAL)))

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
