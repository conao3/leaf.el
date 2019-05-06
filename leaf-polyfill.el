;;; leaf-polyfill.el --- define polyfill for leaf.el    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/leaf.el

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

(defun leaf-mapcaappend (func seq &rest rest)
  "Another implementation for `mapcan' for FUNC SEQ REST.
`mapcan' uses `nconc', but Emacs-22 doesn't support it."
  (apply #'append (apply #'mapcar func seq rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General functions
;;

(defsubst leaf-truep (var)
  "Return t if VAR is non-nil."
  (not (not var)))

(defsubst leaf-pairp (var &optional allow)
  "Return t if VAR is pair.  If ALLOW is non-nil, allow nil as last element"
  (and (listp var)
       (atom (cdr var))
       (if allow t (not (null (cdr var))))))

(defsubst leaf-dotlistp (var)
  "Return t if VAR is doted list (last arg of list is not 'nil)."
  (not (eq nil (cdr (last var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General list functions
;;

(defsubst leaf-list-memq (symlist list)
  "Return t if LIST contained element of SYMLIST."
  (leaf-truep
   (delq nil (mapcar (lambda (x) (memq x list)) symlist))))

(defun leaf-flatten (lst)
  "Return flatten list of LST."
  (let ((fn))
    (if (fboundp 'mapcan)
        (setq fn (lambda (lst)
                   (if (atom lst) `(,lst) (mapcan fn lst))))
      (setq fn (lambda (lst)
                 (if (atom lst) `(,lst) (apply #'append (mapcar fn lst))))))
    (funcall fn lst)))

(defun leaf-subst (old new lst)
  "Substitute NEW for OLD in LST. "
  (declare (indent 2))
  (mapcar (lambda (elm) (if (eq elm old) new elm)) lst))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General plist functions
;;

(defun leaf-plist-keys (plist)
  (let ((count 1) ret)
    (dolist (elm plist)
      (when (= 1 (mod count 2))
        (setq ret (cons elm ret)))
      (setq count (1+ count)))
    (nreverse ret)))

(defun leaf-plist-get (key plist &optional default)
  "`plist-get' with DEFAULT value in PLIST search KEY."
  (declare (indent 1))
  (if (member key plist)
      (plist-get plist key)
    default))

(provide 'leaf-polyfill)
;;; leaf-polyfill.el ends here
