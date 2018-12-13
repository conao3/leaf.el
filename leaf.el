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

(require 'leaf-backends)

(defgroup leaf nil
  "Symplifying your `.emacs' configuration."
  :group 'lisp)

(defconst leaf-version "1.1.8"
  "leaf.el version")

(defcustom leaf-keywords
  '(;; Always be placed at the top-level.
    ;; If this keyword activated, leaf block convert to nil.
    :disabled

    ;; Condition keywards.
    :if :when :unless

    ;; Preparation keywords.
    ;; Install package. (Condition isn't passed, not install)
    :ensure :init

    ;; Require package.
    :require

    ;; Configuration keywords.
    :config
    )
  "Special keywords to be processed by `leaf'.
Sort by `leaf-sort-values-plist' in this order.
Each symbol must has handle function named as `leaf-handler/_:symbol_'."
  :type 'sexp
  :group 'leaf)

(defcustom leaf-defaults
  '(:init nil :require t)
  "Default values for each leaf packages."
  :type 'sexp
  :group 'leaf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customize backend
;;

(defcustom leaf-backend/:ensure (if (require 'feather nil t) 'feather
                                  (if (require 'package nil t) 'package))
  "Backend to process `:ensure' keyword."
  :type '(choice (const :tag "Use `package.el'." 'package)
                 (const :tag "Use `feather.el'." 'feather)
                 (const :tag "No backend, disable `:ensure'." nil))
  :group 'leaf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support functions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  For legacy Emacs
;;

(unless (fboundp 'declare-function)
  (defmacro declare-function (_fn _file &rest _args)
    "Tell the byte-compiler that function FN is defined, in FILE."
    nil))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General list functions for leaf
;;

(defun leaf-append-defaults (plist)
  "Append leaf default values to plist."
  (append plist leaf-defaults))

(defun leaf-add-keyword-before (target belm)
  (if (memq target leaf-keywords)
      (warn (format "%s already exists in `leaf-keywords'" target))
    (leaf-asetq (it leaf-keywords)
      (funcall #'leaf-insert-before it target belm))))

(defun leaf-add-keyword-after (target aelm)
  (if (memq target leaf-keywords)
      (warn (format "%s already exists in `leaf-keywords'" target))
    (leaf-asetq (it leaf-keywords)
      (funcall #'leaf-insert-after it target aelm))))

(defun leaf-add-keyword-list-before (targetlst belm)
  (if (leaf-list-memq targetlst leaf-keywords)
      (warn (format "%s already exists in `leaf-keywords'" targetlst))
    (leaf-asetq (it leaf-keywords)
      (funcall #'leaf-insert-list-before it targetlst belm))))

(defun leaf-add-keyword-list-after (targetlst aelm)
  (if (leaf-list-memq targetlst leaf-keywords)
      (warn (format "%s already exists in `leaf-keywords'" targetlst))
    (leaf-asetq (it leaf-keywords)
      (funcall #'leaf-insert-list-after it targetlst aelm))))

(defun leaf-add-doc-keyword (key)
  "Add KEY to `leaf-keywords' as documentation keywords."
  (eval
   `(progn
      (leaf-add-keyword-after ,key :disabled)
      (defun ,(intern (format "leaf-handler/%s" key)) (name value rest)
        ,(format
          (concat "Process %s as documentation keyword.\n"
                  "This handler just ignore this keyword.")
          key)
        (let ((body (leaf-process-keywords name rest)))
          `(,@body))))))

;; top level operation, but don't do anything when don't need it.
;; (eg when loading multiple times)
(mapc (lambda (x)
        (unless (memq x leaf-keywords)
          (leaf-add-doc-keyword x)))
      (reverse '(:doc :file :url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Pseudo-plist functions
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
;;  Keyword handlers
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
        `(,@(mapcar (lambda (x) `(funcall ,funsym ',x)) value*)
          ,@body)
      `(,@body))))

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

(defun leaf-handler/:config (name value rest)
  "Process :config.

This handler return value with progn form."
  (let ((body (leaf-process-keywords name rest)))
    `(,@value ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main macro
;;

(defun leaf-macroexp-progn (exps)
  "Return an expression equivalent to \\=`(progn ,@EXPS).
Copy code from `macroexp-progn' for old Emacs."
  
  (if (cdr exps) `(progn ,@exps) (car exps)))

(defun leaf-core (name args)
  "leaf core process."
  (let* ((args* (leaf-sort-values-plist
                 (leaf-normalize-plist
                  (leaf-append-defaults args) t))))
    (leaf-process-keywords name args*)))

(defmacro leaf (name &rest args)
  "Symplifying your `.emacs' configuration."
  (declare (indent 1))
  (leaf-macroexp-progn
   (leaf-core `',name args)))

(provide 'leaf)
;;; leaf.el ends here
