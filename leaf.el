;;; leaf.el ---                                      -*- lexical-binding: t; -*-

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

(defgroup leaf nil
  "leaf"
  :group 'lisp)

(defcustom leaf-keywords
  '(:disabled
    
    :if :when :unless
    :require
    ;; Any other keyword that also declares commands to be autoloaded
    ;; (such as :bind) must appear before this keyword.
    :init
    ;; This must occur almost last; the only forms which should appear after
    ;; are those that must happen directly after the config forms.
    :config)
  "Special keywords to be processed by `leaf'.
Sort by `leaf-sort-values-plist' in this order.
Each symbol must has handle function named as `leaf-handler/_:symbol_'."
  :type 'sexp
  :group 'leaf)

(defcustom leaf-defaults
  '(:require t)
  "Default values for each leaf packages."
  :type 'sexp
  :group 'leaf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support functions
;;

;;
;; pseudo-PLIST is list separated value with :keyword.
;;   such as (:key1 v1 v2 :key2 v3 :key3 v4 v5 v6)
;;
;; PLIST is normalized plist, and duplicate keys are allowed.
;;   such as (:key1 (v1 v2) :key2 v3 :key3 (v4 v5 v6)),
;;           (:key1 (v1 v2) :key2 v3 :key2 (v4 v5 v6))
;;
;; well-formed PLIST is normalized plst, and duplicate keys are NOT allowed.
;;   such as (:key1 (v1 v2) :key2 v3 :key3 (v4 v5 v6))
;;
;; list-valued PLIST is well-formed PLIST and value are ALWAYS list.
;; Duplicate keys are NOT allowed.
;;   such as (:key1 (v1 v2) :key2 (v3) :key2 (v4 v5 v6))
;;
;; sorted-list PLIST is list-valued PLIST and keys are sorted by `leaf-keywords'
;; Duplicate keys are NOT allowed.
;;   such as (:if (t) :config ((prin1 "a") (prin1 "b)))
;;

(defun leaf-append-defaults (plist)
  "Add leaf default values to plist."

  `(,@plist ,@leaf-defaults))

(defun leaf-sort-values-plist (plist)
  "Given a list-valued PLIST, return sorted-list PLIST.

EXAMPLE:
(leaf-sort-values-plist
  '(:config (message \"a\")
    :disabled (t)))
 -> (:disabled (t)
     :config (message \"a\"))"

  (let ((retplist))
    (dolist (key leaf-keywords)
      (if (plist-member plist key)
	  (setq retplist `(,@retplist ,key ,(plist-get plist key)))))
    retplist))

(defun leaf-merge-dupkey-values-plist (plist)
  "Given a PLIST, return list-valued PLIST.

EXAMPLE:
(leaf-merge-value-on-duplicate-key
  '(:defer (t)
    :config ((message \"a\") (message \"b\"))
    :config ((message \"c\"))))
 -> (:defer (t)
     :config ((message \"a\") (message \"b\") (message \"c\")))"

  (let ((retplist) (existkeys) (existvalue) (key) (value))
    (while plist
      (setq key (pop plist))
      (setq value (pop plist))

      (if (plist-member retplist key)
	  (plist-put retplist key `(,@(plist-get retplist key) ,@value))
	(setq retplist `(,@retplist ,key ,value))))
    retplist))

(defun leaf-normalize-plist (plist mergep)
  "Given a pseudo-PLIST, return PLIST,
if MERGEP is t, return well-formed PLIST.

EXAMPLE:
(leaf-normalize-plist
  '(:defer t
    :config (message \"a\") (message \"b\")
    :config (message \"c\")) nil)
 -> (:defer (t)
     :config ((message \"a\") (message \"b\"))
     :config ((message \"c\")))

(leaf-normalize-plist
  '(:defer t
    :config (message \"a\") (message \"b\")
    :config (message \"c\")) t)
 -> (:defer (t)
     :config ((message \"a\") (message \"b\") (message \"c\"))"

  ;; using reverse list, push (:keyword worklist) when find :keyword
  (let ((retplist) (worklist) (rlist (reverse plist)))
    (dolist (target rlist)
      (if (keywordp target)
	  (progn
	    (push worklist retplist)
	    (push target retplist)

	    ;; clean worklist for new keyword
	    (setq worklist nil))
	(push target worklist)))
    
    ;; merge value for duplicated key if MERGEP is t
    (if mergep (leaf-merge-dupkey-values-plist retplist) retplist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  keyword handlers
;;

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

(defun leaf-handler/:disabled (name value rest)
  "Process :disabled.

This handler always return nil, and interrupt processing of
remaining arguments"
  nil)

(defun leaf-handler/:require (name value rest)
  "Process :require.

This handler add require comamnd for name."
  (let ((body (leaf-process-keywords name rest)))
    `(progn
       (require ,name nil nil)
       ,@body)))

(defun leaf-handler/:if (name value rest)
  "Process :if.

This handler surround the processing of the remaining arguments
with an if block"
  (let ((body (leaf-process-keywords name rest)))
    `(if ,(car value) ,body)))

(defun leaf-handler/:when (name value rest)
  "Process :when.

This handler surround the processing of the remaining arguments
with an when block"
  (let ((body (leaf-process-keywords name rest)))
    `(when ,(car value) ,body)))

(defun leaf-handler/:unless (name value rest)
  "Process :unless.

This handler surround the processing of the remaining arguments
with an unless block"
  (let ((body (leaf-process-keywords name rest)))
    `(unless ,(car value) ,body)))

(defun leaf-handler/:config (name value rest)
  "Process :config.

This handler return value with progn form."
  (let ((body (leaf-process-keywords name rest)))
    `(progn ,@value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  main macros
;;

(defun leaf-macroexp-progn (exps)
  "Return an expression equivalent to \\=`(progn ,@EXPS).
Copy code from `macroexp-progn' for old Emacs."
  
  (if (cdr exps) `(progn ,@exps) (car exps)))

(defun leaf-core (name args)
  `(,(let* ((args* (leaf-sort-values-plist
		    (leaf-normalize-plist
		     (leaf-append-defaults args) t))))
       (leaf-process-keywords name args*))))

(defmacro leaf (name &rest args)
  (leaf-macroexp-progn
   (leaf-core `',name args)))

(provide 'leaf)
;;; leaf.el ends here
