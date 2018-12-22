;;; leaf-polyfill.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
;; Keywords: settings

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  For legacy Emacs
;;

(defmacro leaf-case (fn var &rest conds)
  "Switch case macro with FN.
Emacs-22 doesn't support `pcase'."
  (declare (indent 2))
  (let ((lcond var))
    `(cond
      ,@(mapcar (lambda (x)
                  (let ((rcond (car x))
                        (form (cadr x)))
                    (if (eq rcond '_)
                        `(t ,form)
                      `((funcall ,fn ,lcond ,rcond) ,form))))
                conds)
      (t nil))))

(defun leaf-mapcaappend (func seq &rest rest)
  "Another implementation for `mapcan'.
`mapcan' uses `nconc', but Emacs-22 doesn't support it."
  (apply #'append (apply #'mapcar func seq rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Anaphoric macros
;;

(defmacro leaf-with-gensyms (syms &rest body)
  "Create `let' block with `gensym'ed variables.

\(fn (SYM...) &rest body)"
  (declare (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (gensym)))
                 syms)
     ,@body))

(defmacro leaf-asetq (sym* &optional body)
  "Anaphoric setq macro.

\(fn (ASYM SYM) &optional BODY)"
  (declare (indent 1))
  `(let ((,(car sym*) ,(cadr sym*)))
     (setq ,(cadr sym*) ,body)))

(defmacro leaf-alet (varlist* &rest body)
  "Anaphoric let macro. Return first arg value.
CAUTION:
`it' has first var value, it is NOT updated if var value changed.

(macroexpand
 '(leaf-alet (it ((result t)))
  (princ it)))
=> (let* ((result t)
          (it result))
     (progn (princ it))
     result)

\(fn (ASYM (VARLIST...)) &rest BODY)"
  (declare (debug t) (indent 1))
  `(let* (,@(cadr varlist*)
          (,(car varlist*) ,(caar (cadr varlist*))))
     (progn ,@body)
     ,(caar (cadr varlist*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General functions
;;

(defsubst leaf-truep (var)
  "Return t if var is non-nil."
  (not (not var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General list functions
;;

(defsubst leaf-list-memq (symlist list)
  "Return t if LIST contained element of SYMLIST."
  (leaf-truep
   (delq nil (mapcar (lambda (x) (memq x list)) symlist))))

(defun leaf-insert-before (lst target belm)
  "Insert TARGET before BELM in LST."
  (let ((retlst) (frg))
    (dolist (elm lst)
      (if (eq elm belm)
          (setq frg t
                retlst (append `(,belm ,target) retlst))
        (setq retlst (cons elm retlst))))
    (unless frg
      (warn (format "%s is not found in given list" belm)))
    (nreverse retlst)))

(defun leaf-insert-after (lst target aelm)
  "Insert TARGET after aelm in LST"
  (let ((retlst) (frg))
    (dolist (elm lst)
      (if (eq elm aelm)
          (setq frg t
                retlst (append `(,target ,aelm) retlst))
        (setq retlst (cons elm retlst))))
    (unless frg
      (warn (format "%s is not found in given list" aelm)))
    (nreverse retlst)))

(defun leaf-insert-list-before (lst targetlst belm)
  "Insert TARGETLST before BELM in LST."
  (let ((retlst) (frg))
    (dolist (elm lst)
      (if (eq elm belm)
          (setq frg t
                retlst (append `(,belm ,@(reverse targetlst)) retlst))
        (setq retlst (cons elm retlst))))
    (unless frg
      (warn (format "%s is not found in given list" belm)))
    (nreverse retlst)))

(defun leaf-insert-list-after (lst targetlst aelm)
  "Insert TARGETLST after aelm in LST"
  (let ((retlst) (frg))
    (dolist (elm lst)
      (if (eq elm aelm)
          (setq frg t
                retlst (append `(,@(reverse targetlst) ,aelm) retlst))
        (setq retlst (cons elm retlst))))
    (unless frg
      (warn (format "%s is not found in given list" aelm)))
    (nreverse retlst)))


(provide 'leaf-polyfill)
;;; leaf-polyfill.el ends here
