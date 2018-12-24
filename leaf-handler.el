;;; leaf-handler.el ---                              -*- lexical-binding: t; -*-

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

(require 'leaf-polyfill)

;; `leaf-core'
(defvar leaf-backend/:ensure)
(defvar leaf-backend/:bind)
(defvar leaf-backend/:bind*)


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
  (let ((body   (leaf-process-keywords name rest))
        (funsym `#',(intern
                     (format "leaf-backend/:ensure-%s" leaf-backend/:ensure)))
        (value* (if (and (eq (car value) t) (= (length value) 1))
                    (list (eval name))  ; unify as unquote value.
                  value)))
    (if leaf-backend/:ensure
        `(,@(mapcar (lambda (x) `(funcall ,funsym ,name ',x)) value*)
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
      `(;; remove last `t' symbol from VALUE
        ,@(mapcar (lambda (x) `(require ,x)) (butlast value))
        ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Configuration keywords
;;

(defun leaf-handler/:bind (name value rest)
  "Process :bind

This handler return bind form.
TODO: :map keyword support."
  (let ((body   (leaf-process-keywords name rest))
        (funsym `#',(intern
                     (format "leaf-backend/:bind-%s" leaf-backend/:bind))))
    (if leaf-backend/:bind
        `(,@(mapcar (lambda (x) `(funcall ,funsym ,name ',x)) value)
          ,@body)
      `(,@body))))

(defun leaf-handler/:bind* (name value rest)
  "Process :bind*

This handler return bind form.
TODO: :map keyword support."
  (let ((body   (leaf-process-keywords name rest))
        (funsym `#',(intern
                     (format "leaf-backend/:bind*-%s" leaf-backend/:bind*))))
    (if leaf-backend/:bind*
        `(,@(mapcar (lambda (x) `(funcall ,funsym ,name ',x)) value)
          ,@body)
      `(,@body))))

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

(defun leaf-handler/:config (name value rest)
  "Process :config.

This handler return value with progn form."
  (let ((body (leaf-process-keywords name rest)))
    `(,@value ,@body)))


(provide 'leaf-handler)
;;; leaf-handler.el ends here
