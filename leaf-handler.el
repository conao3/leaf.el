;;; leaf-handler.el ---                              -*- lexical-binding: t; -*-

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

(require 'leaf-polyfill)


(defun leaf-process-keywords (name plist)
  "Process keywords for NAME.

NOTE:
Not check PLIST, PLIST has already been carefully checked
parent funcitons.
Don't call this function directory."

  (when plist
    (let* ((key         (pop plist))
           (value       (pop plist))
           (rest        plist)
           (handler     (format "leaf-handler/%s" key))
           (handler-sym (intern handler)))
      (funcall handler-sym name value rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  meta handler
;;

(defun leaf-meta-handler/:mode-autoload (name value)
  "Meta handler to handle similar :mode."
  (let* ((namesym (eval name))
         (namestr (symbol-name namesym))
         (fnsym   (delq nil
                        (delete-dups
                         (cons namesym
                               (mapcar (lambda (x)
                                         (when (leaf-pairp x) (cdr x)))
                                       value))))))
    `(,@(mapcar (lambda (x) `(autoload #',x ,namestr nil t)) fnsym))))

(defun leaf-meta-handler/:mode (name value destlist)
  "Meta handler to handle similar :mode."
  (let* ((namesym (eval name))
         (namestr (symbol-name namesym)))
    `(,@(leaf-meta-handler/:mode-autoload name value)
      (leaf-list-add-to-list ',destlist
                             ',(mapcar (lambda (x)
                                         (if (listp x) x `(,x . ,namesym)))
                                       value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  :disabled keyword
;;

(defun leaf-handler/:disabled (name value rest)
  "Process :disabled.

This handler always return nil, and interrupt processing of
remaining arguments"
  (let ((body (leaf-process-keywords name rest)))
    (cond
     ((eq (car value) t)
      nil)
     ((eq (car value) nil)
      `(,@body))
     (t
      (if (eval (car value))
          nil
        `(,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Initialize keywords
;;

(defun leaf-handler/:load-path (name value rest)
  "Process :loadpath.

add loadpath located on `user-emacs-directory'"
  (let ((body (leaf-process-keywords name rest)))
    `((leaf-list-add-to-list 'load-path
                             ,(mapcar (lambda (x) (locate-user-emacs-file x))
                                      value))
      ,@body)))

(defun leaf-handler/:byte-compile-funcs (name value rest)
  "Process :commands

see `autoload'."
  (let ((body (leaf-process-keywords name rest))
        (value* (leaf-mapcaappend #'identity value)))
    `((eval-when-compile
        ,@(mapcar (lambda (x) `(autoload #',(car x) ,(symbol-name (cdr x)) nil t))
                value*))
      ,@body)))

(defun leaf-handler/:byte-compile-vars (name value rest)
  "Process :loadpath.

add loadpath located on `user-emacs-directory'"
  (let ((body (leaf-process-keywords name rest)))
    `((eval-when-compile
        ,@(mapcar (lambda (x) `(defvar ,x)) value))
      ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Condition keywords
;;

(defun leaf-handler/:if (name value rest)
  "Process :if.

This handler surround the processing of the remaining arguments
with an if block"
  (let ((body (leaf-process-keywords name rest)))
    (cond
     ((= 1 (length value))
      `((if ,@value (progn ,@body))))
     (t
      `((if (and ,@value) (progn ,@body)))))))

(defun leaf-handler/:when (name value rest)
  "Process :when.

This handler surround the processing of the remaining arguments
with an when block"
  (let ((body (leaf-process-keywords name rest)))
    (cond
     ((= 1 (length value))
      `((when ,@value ,@body)))
     (t
      `((when (and ,@value) ,@body))))))

(defun leaf-handler/:unless (name value rest)
  "Process :unless.

This handler surround the processing of the remaining arguments
with an unless block"
  (let ((body (leaf-process-keywords name rest)))
    (cond
     ((= 1 (length value))
      `((unless ,@value ,@body)))
     (t
      `((unless (and ,@value) ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Preparation keywords
;;

(defun leaf-handler/:ensure (name value rest)
  "Process :ensure.

Install package(s). If conditions keywords is nil, stop installation."
  (let ((body (leaf-process-keywords name rest)))
    `((leaf-meta-backend/:ensure ,name ',value)
      ,@body)))

(defun leaf-handler/:defaults (name value rest)
  "Process :defaults.

If you pass non-nil, tell feather.el to download and evaluate
the standard settings for that package."
  (let ((body   (leaf-process-keywords name rest)))
    (if (car value)
        `((feather-install-defaults ,name)
          ,@body)
      `(,@body))))

(defun leaf-handler/:pre-setq (name value rest)
  "Process :pre-setq.

Eval `setq' before `require' package."
  (let ((body (leaf-process-keywords name rest))
        (value* (leaf-mapcaappend (lambda (x) x) value)))
    `(,@(mapcar (lambda (x) `(setq ,(car x) ,(cdr x))) value*)
      ,@body)))

(defun leaf-handler/:init (name value rest)
  "Process :init.

This value is evaled before `require'."
  (let ((body (leaf-process-keywords name rest)))
    (cond
     ((eq (car value) nil)
      `((progn ,@body)))
     (t
      ;; remove last `nil' symbol from VALUE
      `((progn
          (progn ,@(butlast value))
          (progn ,@body)))))))

(defun leaf-handler/:commands (name value rest)
  "Process :commands

see `autoload'."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(mapcar (lambda (x) `(add-hook #',x ,(symbol-name (eval name)) nil t))
                value)
      ,@body)))

(defun leaf-handler/:hook (name value rest)
  "Process :hook

Add `auto-mode-alist' following value."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(leaf-meta-handler/:mode-autoload name value)
      ,@(mapcar (lambda (x)
                  `(add-hook ,@(if (leaf-pairp x)
                                   `(',(car x) #',(cdr x))
                                 `(',x #',(eval name)))))
                value)
      ,@body)))

(defun leaf-handler/:mode (name value rest)
  "Process :mode

Add `auto-mode-alist' following value."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(leaf-meta-handler/:mode name value 'auto-mode-alist)
      ,@body)))

(defun leaf-handler/:interpreter (name value rest)
  "Process :interpreter

Add `interpreter-mode-alist' following value."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(leaf-meta-handler/:mode name value 'interpreter-mode-alist)
      ,@body)))

(defun leaf-handler/:magic (name value rest)
  "Process :magic

Add `magic-mode-alist' following value."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(leaf-meta-handler/:mode name value 'magic-mode-alist)
      ,@body)))


(defun leaf-handler/:magic-fallback (name value rest)
  "Process :interpreter

Add `interpreter-mode-alist' following value."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(leaf-meta-handler/:mode name value 'magic-fallback-mode-alist)
      ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  :require keyword
;;

(defun leaf-handler/:require (name value rest)
  "Process :require.

This handler add require comamnd for name."
  (let ((body (leaf-process-keywords name rest)))
    (cond
     ((eq (car value) nil)
      `(,@body))
     ((eq (car value) t)
      `((require ,name)
        ,@body))
     (t
      `(,@(mapcar (lambda (x) `(require ,x)) value)
        ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Configuration keywords
;;

(defun leaf-handler/:bind (name value rest)
  "Process :bind

This handler return bind form.
TODO: :map keyword support."
  (let ((body (leaf-process-keywords name rest)))
    `((leaf-meta-backend/:bind ,name ',value)
      ,@body)))

(defun leaf-handler/:bind* (name value rest)
  "Process :bind*

This handler return bind form.
TODO: :map keyword support."
  (let ((body (leaf-process-keywords name rest)))
    `((leaf-meta-backend/:bind* ,name ',value)
      ,@body)))

(defun leaf-handler/:setq (name value rest)
  "Process :setq.

Eval `setq' after `require' package."
  (let ((body (leaf-process-keywords name rest))
        (value* (leaf-mapcaappend (lambda (x) x) value)))
    `(,@(mapcar (lambda (x) `(setq ,(car x) ,(cdr x))) value*)
      ,@body)))

(defun leaf-handler/:setq-default (name value rest)
  "Process :setq-default.

Eval `setq-default' before `require' package."
  (let ((body (leaf-process-keywords name rest))
        (value* (leaf-mapcaappend (lambda (x) x) value)))
    `(,@(mapcar (lambda (x) `(setq-default ,(car x) ,(cdr x))) value*)
      ,@body)))

(defun leaf-handler/:custom (name value rest)
  "Process :custom.

Eval `custom-set-variables' before `require' package."
  (let ((body (leaf-process-keywords name rest))
        (value* (leaf-mapcaappend (lambda (x) x) value)))
    `(,@(mapcar (lambda (x) `(custom-set-variables '(,(car x) ,(cdr x)))) value*)
      ,@body)))

(defun leaf-handler/:custom-face (name value rest)
  "Process :custom-face.

see `custom-set-faces'."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(mapcar (lambda (x) `(custom-set-faces ',x)) value)
      ,@body)))

(defun leaf-handler/:config (name value rest)
  "Process :config.

This handler return value with progn form."
  (let ((body (leaf-process-keywords name rest)))
    `(,@value ,@body)))


(provide 'leaf-handler)
;;; leaf-handler.el ends here
