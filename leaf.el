;;; leaf.el --- Symplify your init.el configuration       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp settings
;; Version: 2.1.4
;; URL: https://github.com/conao3/leaf.el
;; Package-Requires: ((emacs "24.0"))

;;   Abobe declared this package requires Emacs-24, but it's for warning
;;   suppression, and will actually work from Emacs-22.

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

;; simpify init.el

;;; Code:

(require 'leaf-polyfill)

(defgroup leaf nil
  "Symplifying your `.emacs' configuration."
  :group 'lisp)

(defcustom leaf-defaults '()
  "Default values for each leaf packages."
  :type 'sexp
  :group 'leaf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  leaf keywords definition
;;

(defvar leaf-keywords
  '(:dummy
    :disabled (unless (eval (car value)) `(,@body))
    :preface `(,@value ,@body)
    :if
    (when body
      `((if ,@(if (= 1 (length value)) value `((and ,@value)))
            (progn ,@body))))
    :when
    (when body
      `((when ,@(if (= 1 (length value)) value `((and ,@value)))
          ,@body)))
    :unless
    (when body
      `((unless ,@(if (= 1 (length value)) value `((and ,value)))
          ,@body)))
    :doc `(,@body) :file `(,@body) :url `(,@body)
    :init `(,@value ,@body)
    :require `(,@(mapcar (lambda (elm) `(require ',elm)) value) ,@body)
    :hook
    (progn
      (mapc (lambda (elm) (leaf-register-autoload (cdr elm) name)) value)
      `(,@(mapcar (lambda (elm) `(add-hook ',(car elm) #',(cdr elm))) value) ,@body))
    :mode
    (progn
      (mapc (lambda (elm) (leaf-register-autoload (cdr elm) name)) value)
      `(,@(mapcar (lambda (elm) `(add-to-list 'auto-mode-alist '(,(car elm) ,(cdr elm)))) value) ,@body))
    :interpreter
    (progn
      (mapc (lambda (elm) (leaf-register-autoload (cdr elm) name)) value)
      `(,@(mapcar (lambda (elm) `(add-to-list 'interpreter-mode-alist '(,(car elm) ,(cdr elm)))) value) ,@body))
    :config `(,@value ,@body)
    )
  "Special keywords and conversion rule to be processed by `leaf'.
Sort by `leaf-sort-values-plist' in this order.")

(defvar leaf-normarize
  '(((memq key '(:require))
     ;; Accept: 't, 'nil, symbol and list of these
     ;; Return: symbol list.
     ;; Note  : 't will convert to 'name and remove duplicate element
     (let ((ret) (fn))
       (setq fn (lambda (elm ret)
                  (cond
                   ((eq t elm)
                    (if (memq name ret)
                        ret
                      (cons name ret)))
                   ((eq nil elm)
                    ret)
                   ((atom elm)
                    (if (memq elm ret)
                        ret
                      (cons elm ret)))
                   ((listp elm)
                    (dolist (el elm)
                      (setq ret (funcall fn el ret)))
                    ret)
                   (t
                    (warn (format "Value %s is malformed." value))))))
       (dolist (elm value)
         (setq ret (funcall fn elm ret)))
       (nreverse ret)))
    ((memq key '(:hook :mode :interpreter))
     ;; Accept: func, (hook . func),
     ;;         ((hook hook ...) . func), (hook hook ... . func) and list of these
     ;; Return: list of pair (hook . func).
     ;; Note  : if omit hook, use {{name}}-hook as hook
     ;;         remove duplicate configure
     ;;         't and 'nil are just ignore
     (let ((ret) (fn))
       (setq fn (lambda (elm ret)
                  (cond
                   ((eq t elm)
                    ret)
                   ((eq nil elm)
                    ret)
                   ((atom elm)
                    (let ((sym `(,elm . ,name)))
                      (if (member sym ret)
                          ret
                        (cons sym ret))))
                   ((leaf-pairp elm)
                    (if (listp (car elm))
                        (progn
                          (dolist (el (car elm))
                            (setq ret (funcall fn `(,el . ,(cdr elm)) ret)))
                          ret)
                      (if (member elm ret)
                          ret
                        (cons elm ret))))
                   ((leaf-dotlistp elm)
                    (let ((tail (nthcdr (safe-length elm) elm)))
                      (while (not (atom elm))
                        (setq ret (funcall fn `(,(car elm) . ,tail) ret))
                        (pop elm))
                      ret))
                   ((listp elm)
                    (dolist (el elm)
                      (setq ret (funcall fn el ret)))
                    ret)
                   (t
                    (warn (format "Value %s is malformed." value))))))
       (dolist (elm value)
         (setq ret (funcall fn elm ret)))
       (nreverse ret)))
    (t
     value))
  "Normarize rule")

(defun leaf-process-keywords (name plist)
  "Process keywords for NAME.
NOTE:
Not check PLIST, PLIST has already been carefully checked
parent funcitons.
Don't call this function directory."
  (when plist
    (let* ((key   (pop plist))
           (value (leaf-normarize-args name key (pop plist)))
           (body  (leaf-process-keywords key plist)))
      (eval
       `(let ((name  ',name)
              (key   ',key)
              (value ',value)
              (body  ',body)
              (rest  ',plist))
          ,(plist-get (cdr leaf-keywords) key))))))

(defun leaf-normarize-args (name key value)
  "Normarize for NAME, KEY and VALUE."
  (eval
   `(let ((name  ',name)
          (key   ',key)
          (value ',value))
      (cond
       ,@leaf-normarize))))

(defvar leaf--autoload)
(defun leaf-register-autoload (fn pkg)
  "Registry FN as autoload for PKG."
  (let ((target `(,fn . ,(symbol-name pkg))))
    (when (not (member target leaf--autoload))
      (setq leaf--autoload (cons target leaf--autoload)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support functions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Formatting leaf
;;

;;;###autoload
(defun leaf-to-string (sexp)
  "Return format string of `leaf' SEXP like `pp-to-string'."
  (with-temp-buffer
    (insert (replace-regexp-in-string
             (eval
              `(rx (group
                    (or ,@(mapcar #'symbol-name leaf-keywords)))))
             "\n\\1"
             (prin1-to-string sexp)))
    (delete-trailing-whitespace)
    (emacs-lisp-mode)
    (indent-region (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General list functions for leaf
;;

(defun leaf-append-defaults (plist)
  "Append leaf default values to PLIST."
  (append plist leaf-defaults))

(defun leaf-add-keyword-before (target belm)
  "Add leaf keyword as name TARGET before BELM."
  (if (memq target leaf-keywords)
      (warn (format "%s already exists in `leaf-keywords'" target))
    (setq leaf-keywords
          (leaf-insert-before leaf-keywords target belm))))

(defun leaf-add-keyword-after (target aelm)
  "Add leaf keyword as name TARGET after AELM."
  (if (memq target leaf-keywords)
      (warn (format "%s already exists in `leaf-keywords'" target))
    (setq leaf-keywords
          (leaf-insert-after leaf-keywords target aelm))))

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
  => (:disabled (t)
      :config (message \"a\"))"
  (let ((retplist))
    (dolist (key (leaf-plist-keys (cdr leaf-keywords)))
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
  => (:defer (t)
      :config ((message \"a\") (message \"b\") (message \"c\")))"
  (let ((retplist) (key) (value))
    (while plist
      (setq key (pop plist))
      (setq value (pop plist))

      (if (plist-member retplist key)
          (plist-put retplist key `(,@(plist-get retplist key) ,@value))
        (setq retplist `(,@retplist ,key ,value))))
    retplist))

(defun leaf-normalize-plist (plist &optional mergep)
  "Given a pseudo-PLIST, return PLIST.
if MERGEP is t, return well-formed PLIST.

EXAMPLE:
  (leaf-normalize-plist
    '(:defer t
      :config (message \"a\") (message \"b\")
      :config (message \"c\")) nil)
  => (:defer (t)
      :config ((message \"a\") (message \"b\"))
      :config ((message \"c\")))

  (leaf-normalize-plist
    '(:defer t
      :config (message \"a\") (message \"b\")
      :config (message \"c\")) t)
  => (:defer (t)
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
;;  Main macro
;;

(defmacro leaf (name &rest args)
  "Symplify your `.emacs' configuration for package NAME with ARGS."
  (declare (indent defun))
  (let* ((leaf--autoload)
         (args* (leaf-sort-values-plist
                 (leaf-normalize-plist
                  (leaf-append-defaults args) t)))
         (body (leaf-process-keywords name args*)))
    (when body
      `(progn
         ,@(mapcar
            (lambda (elm) `(autoload #',(car elm) ,(cdr elm) nil t)) leaf--autoload)
         ,@body))))

(provide 'leaf)
;;; leaf.el ends here
