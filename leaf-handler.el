;;; leaf-handler.el --- define leaf handler          -*- lexical-binding: t; -*-

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

;; define leaf handler

;;; Code:

(require 'leaf-polyfill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  meta handler
;;

(defun leaf-meta-handler/:mode-autoload (name value)
  "The meta handler to handle similar :mode for NAME with VALUE."
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
  "The meta handler to handle similar :mode for NAME with VALUE and DESTLIST."
  (let* ((namesym  (eval name))
         (_namestr (symbol-name namesym)))
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
  "The handler for :disabled for NAME with VALUE and REST.

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
  "The handler for :loadpath for NAME with VALUE and REST.

add loadpath located on `user-emacs-directory'"
  (let ((body (leaf-process-keywords name rest)))
    `((leaf-list-add-to-list 'load-path
                             ,(mapcar (lambda (x) (locate-user-emacs-file x))
                                      value))
      ,@body)))

(defun leaf-handler/:byte-compile-funcs (name value rest)
  "The handler for :commands for NAME with VALUE and REST.

see `autoload'."
  (let ((body (leaf-process-keywords name rest))
        (value* (leaf-mapcaappend #'identity value)))
    `((eval-when-compile
        ,@(mapcar (lambda (x) `(autoload #',(car x) ,(symbol-name (cdr x)) nil t))
                value*))
      ,@body)))

(defun leaf-handler/:byte-compile-vars (name value rest)
  "The handler for :loadpath for NAME with VALUE and REST.

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
  "The handler for :if for NAME with VALUE and REST.

This handler surround the processing of the remaining arguments
with an if block"
  (let ((body (leaf-process-keywords name rest)))
    (when body
      (cond
       ((= 1 (length value))
        `((if ,@value (progn ,@body))))
       (t
        `((if (and ,@value) (progn ,@body))))))))

(defun leaf-handler/:when (name value rest)
  "The handler for :when for NAME with VALUE and REST.

This handler surround the processing of the remaining arguments
with an when block"
  (let ((body (leaf-process-keywords name rest)))
    (when body
      (cond
       ((= 1 (length value))
        `((when ,@value ,@body)))
       (t
        `((when (and ,@value) ,@body)))))))

(defun leaf-handler/:unless (name value rest)
  "The handler for :unless for NAME with VALUE and REST.

This handler surround the processing of the remaining arguments
with an unless block"
  (let ((body (leaf-process-keywords name rest)))
    (when body
      (cond
       ((= 1 (length value))
        `((unless ,@value ,@body)))
       (t
        `((unless (and ,@value) ,@body)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Documentation keywords
;;

(defun leaf-handler/:doc (name _value rest)
  "The handler for :doc for NAME with VALUE and REST.

This handler just ignore this keyword."
  (let ((body (leaf-process-keywords name rest)))
    `(,@body)))

(defun leaf-handler/:file (name _value rest)
  "The handler for file for NAME with VALUE and REST.

This handler just ignore this keyword."
  (let ((body (leaf-process-keywords name rest)))
    `(,@body)))

(defun leaf-handler/:url (name _value rest)
  "The handler for :url for NAME with VALUE and REST.

This handler just ignore this keyword."
  (let ((body (leaf-process-keywords name rest)))
    `(,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Preparation keywords
;;

(defun leaf-handler/:ensure (name value rest)
  "The handler for :ensure for NAME with VALUE and REST.

Install package(s) if conditions keywords is nil, stop installation."
  (let ((body (leaf-process-keywords name rest)))
    `((leaf-meta-backend/:ensure ,name ',value)
      ,@body)))

(defun leaf-handler/:defaults (name value rest)
  "The handler for :defaults for NAME with VALUE and REST.

If you pass non-nil, tell feather.el to download and evaluate
the standard settings for that package."
  (let ((body   (leaf-process-keywords name rest)))
    (if (car value)
        `((feather-install-defaults ,name)
          ,@body)
      `(,@body))))

(defun leaf-handler/:pre-setq (name value rest)
  "The handler for :pre-setq for NAME with VALUE and REST.

Eval `setq' before `require' package."
  (let ((body (leaf-process-keywords name rest))
        (value* (leaf-mapcaappend (lambda (x) x) value)))
    `(,@(mapcar (lambda (x) `(setq ,(car x) ,(cdr x))) value*)
      ,@body)))

(defun leaf-handler/:init (name value rest)
  "The handler for :init for NAME with VALUE and REST.

This value is evaled before `require'."
  (let ((body (leaf-process-keywords name rest)))
    `(,@value ,@body)))

(defun leaf-handler/:commands (name value rest)
  "The handler for :commands for NAME with VALUE and REST.

see `autoload'."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(mapcar (lambda (x) `(add-hook #',x ,(symbol-name (eval name)) nil t))
                value)
      ,@body)))

(defun leaf-handler/:hook (name value rest)
  "The handler for :hook for NAME with VALUE and REST.

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
  "The handler for :mode for NAME with VALUE and REST.

Add `auto-mode-alist' following value."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(leaf-meta-handler/:mode name value 'auto-mode-alist)
      ,@body)))

(defun leaf-handler/:interpreter (name value rest)
  "The handler for :interpreter for NAME with VALUE and REST.

Add `interpreter-mode-alist' following value."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(leaf-meta-handler/:mode name value 'interpreter-mode-alist)
      ,@body)))

(defun leaf-handler/:magic (name value rest)
  "The handler for :magic for NAME with VALUE and REST.

Add `magic-mode-alist' following value."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(leaf-meta-handler/:mode name value 'magic-mode-alist)
      ,@body)))


(defun leaf-handler/:magic-fallback (name value rest)
  "The handler for :interpreter for NAME with VALUE and REST.

Add `interpreter-mode-alist' following value."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(leaf-meta-handler/:mode name value 'magic-fallback-mode-alist)
      ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  :require keyword
;;

(defun leaf-handler/:require (name value rest)
  "The handler for :require for NAME with VALUE and REST.

This handler add require comamnd for name."
  (let ((body (leaf-process-keywords name rest)))
    (cond
     ((eq (car value) nil)
      `(,@body))
     ((eq (car value) t)
      `((require ,name)
        ,@body))
     (t
      `(,@(mapcar (lambda (x) `(require ',x)) value)
        ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Configuration keywords
;;

(defun leaf-handler/:bind (name value rest)
  "The handler for :bind for NAME with VALUE and REST.

This handler return bind form.
TODO: :map keyword support."
  (let ((body (leaf-process-keywords name rest)))
    `((leaf-meta-backend/:bind ,name ',value)
      ,@body)))

(defun leaf-handler/:bind* (name value rest)
  "The handler for :bind* for NAME with VALUE and REST.

This handler return bind form.
TODO: :map keyword support."
  (let ((body (leaf-process-keywords name rest)))
    `((leaf-meta-backend/:bind* ,name ',value)
      ,@body)))

(defun leaf-handler/:setq (name value rest)
  "The handler for :setq for NAME with VALUE and REST.

Eval `setq' after `require' package."
  (let ((body (leaf-process-keywords name rest))
        (value* (leaf-mapcaappend (lambda (x) x) value)))
    `(,@(mapcar (lambda (x) `(setq ,(car x) ,(cdr x))) value*)
      ,@body)))

(defun leaf-handler/:setq-default (name value rest)
  "The handler for :setq-default for NAME with VALUE and REST.

Eval `setq-default' before `require' package."
  (let ((body (leaf-process-keywords name rest))
        (value* (leaf-mapcaappend (lambda (x) x) value)))
    `(,@(mapcar (lambda (x) `(setq-default ,(car x) ,(cdr x))) value*)
      ,@body)))

(defun leaf-handler/:custom (name value rest)
  "The handler for :custom for NAME with VALUE and REST.

Eval `custom-set-variables' before `require' package."
  (let ((body (leaf-process-keywords name rest))
        (value* (leaf-mapcaappend (lambda (x) x) value)))
    `(,@(mapcar (lambda (x) `(custom-set-variables '(,(car x) ,(cdr x)))) value*)
      ,@body)))

(defun leaf-handler/:custom-face (name value rest)
  "The handler for :custom-face for NAME with VALUE and REST.

see `custom-set-faces'."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(mapcar (lambda (x) `(custom-set-faces ',x)) value)
      ,@body)))

(defun leaf-handler/:config (name value rest)
  "The handler for :config for NAME with VALUE and REST.

This handler return value with progn form."
  (let ((body (leaf-process-keywords name rest)))
    `(,@value ,@body)))


(provide 'leaf-handler)
;;; leaf-handler.el ends here
