;;; leaf.el ---                                      -*- lexical-binding: t; -*-

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

(require 'leaf-handler)
(require 'leaf-backend)

(defgroup leaf nil
  "Symplifying your `.emacs' configuration."
  :group 'lisp)

(defconst leaf-version "2.0.8"
  "leaf.el version")

(defcustom leaf-keywords
  '(;; Always be placed at the top-level.
    ;; If this keyword activated, leaf block convert to nil.
    :disabled

    ;; Initialize phase
    :load-path
    :byte-compile-funcs :byte-compile-vars

    ;; Condition keywards.
    :if :when :unless

    ;; Preparation keywords.
    ;; Install package. (Condition isn't passed, not install)
    :ensure :defaults
    :pre-setq :init
    :commands :hook
    :mode :interpreter
    :magic :magic-fallback

    ;; Require package.
    :require

    ;; Configuration keywords.
    :bind :bind*
    :setq :setq-default
    :custom :custom-face
    :config
    )
  "Special keywords to be processed by `leaf'.
Sort by `leaf-sort-values-plist' in this order.
Each symbol must has handle function named as `leaf-handler/_:symbol_'."
  :type 'sexp
  :group 'leaf)

(defcustom leaf-defaults '()
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

(defcustom leaf-backend/:bind (if (require 'bind-key nil t) 'bind-key)
  "Backend to process `:bind' keyword."
  :type '(choice (const :tag "Use `bind-key.el'." 'bind-key)
                 (const :tag "No backend, disable `:bind'." nil))
  :group 'leaf)

(defcustom leaf-backend/:bind* (if (require 'bind-key nil t) 'bind-key)
  "Backend to process `:bind*' keyword."
  :type '(choice (const :tag "Use `bind-key.el'." 'bind-key)
                 (const :tag "No backend, disable `:bind'." nil))
  :group 'leaf)


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
;;  Main macro
;;

(defun leaf-macroexp-progn (exps)
  "Return an expression equivalent to \\=`(progn ,@EXPS).
Copy code from `macroexp-progn' for old Emacs."

  (when exps `(progn ,@exps)))

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
