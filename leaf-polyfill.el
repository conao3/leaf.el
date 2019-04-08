;;; leaf-polyfill.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: settings
;; Version: 2.0.0
;; URL: https://github.com/conao3/leaf.el
;; Package-Requires: ((emacs "22.0"))

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
;;  General functions
;;

(defsubst leaf-truep (var)
  "Return t if var is non-nil."
  (not (not var)))

(defsubst leaf-pairp (var)
  "Return t if var is pair."
  (and (listp var) (atom (cdr var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General list functions
;;

(defsubst leaf-list-memq (symlist list)
  "Return t if LIST contained element of SYMLIST."
  (leaf-truep
   (delq nil (mapcar (lambda (x) (memq x list)) symlist))))

(defsubst leaf-list-add-to-list (destlst fromlst &optional append)
  "Add FROMLST to DESTLST with `add-to-list'.
Defaltly, add at the beginning of the list, but when APPEND is non-nil,
SOURCE-LST is added at the end.
this function is minor change from `add-to-list'."
  (mapc (lambda (x)
          (add-to-list destlst x append))
        (if append fromlst (reverse fromlst)))
  destlst)

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
  "Insert TARGET after AELM in LST."
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
  "Insert TARGETLST after AELM in LST."
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
