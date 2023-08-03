;;; leaf.el --- Simplify your init.el configuration, extended use-package       -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp settings
;; Version: 4.5.5
;; URL: https://github.com/conao3/leaf.el
;; Package-Requires: ((emacs "24.1"))

;; This program is free software: you can redistribute it and/or modify
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

;; Provides macros that allow you to declaratively configure
;; settings typical of an Elisp package with various keywords.

;; By separating the settings of a package and combining many
;; 'leaves' of a package's settings, you could make a 'Yggdrasill'
;; on your Emacs.

;; A leaf can consist of multiple packages, in which case you can
;; disable all dependent child packages by disabling one parent's
;; package.

;; It also has a key management system and package management
;; uses the package.el.  With minimal external dependencies and
;; careful implementation, this package is guaranteed to be fully
;; functional from Emacs-24.4, now, and in future Emacs.

;; More information is [[https://github.com/conao3/leaf.el][here]]


;;; Code:

(defgroup leaf nil
  "Symplifying your `.emacs' configuration."
  :group 'lisp)

(defmacro leaf-list (&rest args)
  "Make list from ARGS.
Same as `list' but this macro does not evaluate any arguments."
  `(quote ,args))

(defvar leaf--paths nil)
(defvar leaf--raw)
(defvar leaf--name)
(defvar leaf--key)
(defvar leaf--keyname)
(defvar leaf--value)
(defvar leaf--body)
(defvar leaf--rest)
(defvar leaf--autoload)
(defvar leaf--load-file-name nil)

(defvar leaf-keywords
  (leaf-list
   :disabled          (unless (eval (car leaf--value)) `(,@leaf--body))
   :leaf-path         (if (and leaf--body (eval (car leaf--value))) `((leaf-handler-leaf-path ,leaf--name) ,@leaf--body) `(,@leaf--body))
   :leaf-protect      (if (and leaf--body (eval (car leaf--value))) `((leaf-handler-leaf-protect ,leaf--name ,@leaf--body)) `(,@leaf--body))
   :load-path         `(,@(mapcar (lambda (elm) `(add-to-list 'load-path ,elm)) leaf--value) ,@leaf--body)
   :load-path*        `(,@(mapcar (lambda (elm) `(add-to-list 'load-path (locate-user-emacs-file ,elm))) leaf--value) ,@leaf--body)
   :leaf-autoload     `(,@(when (car leaf--value) (mapcar (lambda (elm) `(unless (fboundp ',(car elm)) (autoload #',(car elm) ,(cdr elm) nil t))) (reverse leaf--autoload))) ,@leaf--body)

   :defun             `(,@(mapcar (lambda (elm) `(declare-function ,(car elm) ,(symbol-name (cdr elm)))) leaf--value) ,@leaf--body)
   :defvar            `(,@(mapcar (lambda (elm) `(defvar ,elm)) leaf--value) ,@leaf--body)
   :leaf-defun        `(,@(when (car leaf--value) (mapcar (lambda (elm) `(declare-function ,(car elm) ,(cdr elm))) (reverse leaf--autoload))) ,@leaf--body)
   :leaf-defvar       `(,@(mapcar (lambda (elm) `(defvar ,elm)) leaf--value) ,@leaf--body)
   :preface           `(,@leaf--value ,@leaf--body)

   :when              (when leaf--body `((when   ,@(if (= 1 (length leaf--value)) leaf--value `((and ,@leaf--value))) ,@leaf--body)))
   :unless            (when leaf--body `((unless ,@(if (= 1 (length leaf--value)) leaf--value `((and ,@leaf--value))) ,@leaf--body)))
   :if                (when leaf--body `((if     ,@(if (= 1 (length leaf--value)) leaf--value `((and ,@leaf--value))) (progn ,@leaf--body))))

   :doc               `(,@leaf--body)
   :req               `(,@leaf--body)
   :tag               `(,@leaf--body)
   :file              `(,@leaf--body)
   :url               `(,@leaf--body)
   :added             `(,@leaf--body)

   :emacs<            (when leaf--body `((when (version<  emacs-version ,leaf--value)  ,@leaf--body)))
   :emacs<=           (when leaf--body `((when (version<= emacs-version ,leaf--value)  ,@leaf--body)))
   :emacs=            (when leaf--body `((when (version=  emacs-version ,leaf--value)  ,@leaf--body)))
   :emacs>            (when leaf--body `((when (version<  ,leaf--value  emacs-version) ,@leaf--body)))
   :emacs>=           (when leaf--body `((when (version<= ,leaf--value  emacs-version) ,@leaf--body)))

   :package           `(,@(mapcar (lambda (elm) `(leaf-handler-package ,leaf--name ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
   :vc                `(,@(mapcar (lambda (elm) `(leaf-handler-vc ,leaf--name ,elm)) leaf--value) ,@leaf--body)

   :after             (when leaf--body (let ((ret `(progn ,@leaf--body)))
                                         (dolist (elm leaf--value) (setq ret `(eval-after-load ',elm ',ret)))
                                         `(,ret)))

   :commands          (progn (leaf-register-autoload leaf--value leaf--name) `(,@leaf--body))

   :bind              (progn
                        (leaf-register-autoload (cadr leaf--value) leaf--name)
                        `((leaf-keys ,(car leaf--value)) ,@leaf--body))
   :bind*             (progn
                        (leaf-register-autoload (cadr leaf--value) leaf--name)
                        `((leaf-keys* ,(car leaf--value)) ,@leaf--body))
   :bind-keymap       `((leaf-keys-bind-keymap ,(car leaf--value) nil ',leaf--name) ,@leaf--body)
   :bind-keymap*      `((leaf-keys-bind-keymap* ,(car leaf--value) nil ',leaf--name) ,@leaf--body)

   :mode              (progn
                        (leaf-register-autoload (mapcar #'cdr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(add-to-list 'auto-mode-alist '(,(car elm) . ,(cdr elm)))) leaf--value) ,@leaf--body))
   :interpreter       (progn
                        (leaf-register-autoload (mapcar #'cdr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(add-to-list 'interpreter-mode-alist '(,(car elm) . ,(cdr elm)))) leaf--value) ,@leaf--body))
   :magic             (progn
                        (leaf-register-autoload (mapcar #'cdr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(add-to-list 'magic-mode-alist '(,(car elm) . ,(cdr elm)))) leaf--value) ,@leaf--body))
   :magic-fallback    (progn
                        (leaf-register-autoload (mapcar #'cdr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(add-to-list 'magic-fallback-mode-alist '(,(car elm) . ,(cdr elm)))) leaf--value) ,@leaf--body))
   :hook              (progn
                        (leaf-register-autoload (mapcar #'cdr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(add-hook ',(car elm) #',(cdr elm))) leaf--value) ,@leaf--body))
   :advice            (progn
                        (leaf-register-autoload (cadr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(advice-add ,@elm)) (car leaf--value)) ,@leaf--body))
   :advice-remove     (progn
                        (leaf-register-autoload (cadr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(advice-remove ,@elm)) (car leaf--value)) ,@leaf--body))

   :pre-setq          `(,@(mapcar (lambda (elm) `(setq ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
   :pre-setf          `(,@(mapcar (lambda (elm) `(setf ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
   :pre-push          `(,@(mapcar (lambda (elm) `(push ,(cdr elm) ,(car elm))) leaf--value) ,@leaf--body)
   :pl-pre-setq       `(,@(mapcar (lambda (elm) `(setq ,(car elm) (leaf-handler-auth ,leaf--name ,(car elm) ,(cdr elm)))) leaf--value) ,@leaf--body)
   :auth-pre-setq     `(,@(mapcar (lambda (elm) `(setq ,(car elm) (leaf-handler-auth ,leaf--name ,(car elm) ,(cdr elm)))) leaf--value) ,@leaf--body)

   :custom            `(,@(mapcar (lambda (elm) `(customize-set-variable ',(car elm) ,(cdr elm) ,(leaf--create-custom-comment :custom))) leaf--value) ,@leaf--body)
   :custom*           `(,@(mapcar (lambda (elm) `(customize-set-variable ',(car elm) ,(cdr elm) ,(leaf--create-custom-comment :custom*))) leaf--value) ,@leaf--body)
   :pl-custom         `(,@(mapcar (lambda (elm) `(customize-set-variable ',(car elm) (leaf-handler-auth ,leaf--name ,(car elm) ,(cdr elm)) ,(leaf--create-custom-comment :pl-custom (cdr elm)))) leaf--value) ,@leaf--body)
   :auth-custom       `(,@(mapcar (lambda (elm) `(customize-set-variable ',(car elm) (leaf-handler-auth ,leaf--name ,(car elm) ,(cdr elm)) ,(leaf--create-custom-comment :auth-custom (cdr elm)))) leaf--value) ,@leaf--body)
   :custom-face       `((custom-set-faces ,@(mapcar (lambda (elm) `'(,(car elm) ,(car (cddr elm)) nil ,(leaf--create-custom-comment :custom-face))) leaf--value)) ,@leaf--body)
   :init              `(,@leaf--value ,@leaf--body)

   :require           `(,@(mapcar (lambda (elm) `(require ',elm)) leaf--value) ,@leaf--body)
   :global-minor-mode (progn
                        (mapc (lambda (elm) (leaf-register-autoload (car elm) (cdr elm))) leaf--value)
                        `(,@(mapcar (lambda (elm) `(,(car elm) 1)) leaf--value) ,@leaf--body))
   :leaf-defer        (let* ((eval-after-p (and leaf--body (eval (car leaf--value)) (leaf-list-memq leaf-defer-keywords (leaf-plist-keys leaf--raw))))
                             (file (leaf-this-file))
                             (let-or-progn (if file `(let ((leaf--load-file-name ,file))) '(progn))))
                        (if eval-after-p `((eval-after-load ',leaf--name '(,@let-or-progn ,@leaf--body))) `(,@leaf--body)))
   :setq              `(,@(mapcar (lambda (elm) `(setq ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
   :setq-default      `(,@(mapcar (lambda (elm) `(setq-default ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
   :setf              `(,@(mapcar (lambda (elm) `(setf ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
   :push              `(,@(mapcar (lambda (elm) `(push ,(cdr elm) ,(car elm))) leaf--value) ,@leaf--body)
   :pl-setq           `(,@(mapcar (lambda (elm) `(setq ,(car elm) (leaf-handler-auth ,leaf--name ,(car elm) ,(cdr elm)))) leaf--value) ,@leaf--body)
   :auth-setq         `(,@(mapcar (lambda (elm) `(setq ,(car elm) (leaf-handler-auth ,leaf--name ,(car elm) ,(cdr elm)))) leaf--value) ,@leaf--body)
   :pl-setq-default   `(,@(mapcar (lambda (elm) `(setq-default ,(car elm) (leaf-handler-auth ,leaf--name ,(car elm) ,(cdr elm)))) leaf--value) ,@leaf--body)
   :auth-setq-default `(,@(mapcar (lambda (elm) `(setq-default ,(car elm) (leaf-handler-auth ,leaf--name ,(car elm) ,(cdr elm)))) leaf--value) ,@leaf--body)
   :config            `(,@leaf--value ,@leaf--body)
   :defer-config      `((eval-after-load ',leaf--name '(progn ,@leaf--value)) ,@leaf--body))
  "Special keywords and conversion rule to be processed by `leaf'.
Sort by `leaf-sort-leaf--values-plist' in this order.")

(defvar leaf-normalize
  '(((memq leaf--key '(:require))
     ;; Accept: 't, 'nil, symbol and list of these (and nested)
     ;; Return: symbol list.
     ;; Note  : 't will convert to 'leaf--name
     ;;         if 'nil placed on top, ignore all argument
     ;;         remove duplicate element
     (let ((ret (leaf-flatten leaf--value)))
       (if (eq nil (car ret))
           nil
         (delete-dups (delq nil (leaf-subst t leaf--name ret))))))

    ((memq leaf--key '(:load-path :load-path* :commands :after :defvar))
     ;; Accept: 't, 'nil, symbol and list of these (and nested)
     ;; Return: symbol list.
     ;; Note  : 'nil is just ignored
     ;;         remove duplicate element
     (mapcar (lambda (elm)
               (cond
                ((memq leaf--key '(:after))
                 (if (eq elm t)
                     leaf--name
                   elm))
                (t
                 elm)))
             (delete-dups (delq nil (leaf-flatten leaf--value)))))

    ((memq leaf--key (list
                      :package
                      :global-minor-mode
                      :hook :mode :interpreter :magic :magic-fallback
                      :defun
                      :pl-setq :pl-pre-setq :pl-setq-default :pl-custom
                      :auth-custom :auth-pre-setq :auth-setq :auth-setq-default
                      :setq :pre-setq :setq-default :custom :custom-face))
     ;; Accept: (sym . val), ((sym sym ...) . val), (sym sym ... . val)
     ;; Return: list of pair (sym . val)
     ;; Note  : atom ('t, 'nil, symbol) is just ignored
     ;;         remove duplicate configure
     (mapcar (lambda (elm)
               (cond
                ((leaf-pairp elm)
                 (if (eq t (car elm)) `(,leaf--name . ,(cdr elm)) elm))
                ((memq leaf--key '(:package))
                 (if (equal '(t) elm) `(,leaf--name . nil) `(,@elm . nil)))
                ((memq leaf--key '(:global-minor-mode))
                 `(,(leaf-mode-sym (if (equal '(t) elm) leaf--name (car elm))) . ,leaf--name))
                ((memq leaf--key '(:hook :mode :interpreter :magic :magic-fallback))
                 `(,@elm . ,(leaf-mode-sym leaf--name)))
                ((memq leaf--key '(:defun))
                 `(,@elm . ,leaf--name))
                ((memq leaf--key (list :pl-custom :pl-pre-setq :pl-setq :pl-setq-default
                                       :auth-custom :auth-pre-setq :auth-setq :auth-setq-default))
                 `(,@elm . leaf-default-plstore))
                ((memq leaf--key '(:setq :pre-setq :setq-default :custom :custom-face))
                 elm)
                (t
                 elm)))
             (mapcan
              (lambda (elm) (leaf-normalize-list-in-list elm 'dotlistp))
              leaf--value)))

    ;; Accept: ("25.1") (25.1) ('25.1)
    ;; Return: string
    ((memq leaf--key '(:emacs< :emacs<= :emacs= :emacs> :emacs>=))
     (let ((arg (if (listp leaf--value) (leaf-flatten leaf--value) (list leaf--value))))
       (if (not (<= (length arg) 2))
           (leaf-error "%s could handle only one argument, %s" leaf--key leaf--value)
         (let ((val (if (= 1 (length arg)) (car arg) (eval arg))))
           (cond
            ((stringp val) val)
            ((numberp val) (number-to-string val))
            ((eq 'quote (car-safe val)) (number-to-string (eval val)))
            (t
             (leaf-error "%s recieve unknown type argument, %s" leaf--key val)))))))

    ;; Accept: ((sym val) (sym val)... )
    ;; Return: list of pair (sym . val)
    ;; NOTE  : This keyword does not allow distribution feature etc.
    ;;         If you use this keyword, must check macroexpansion form!
    ((memq leaf--key '(:custom*))
     (mapcar (lambda (elm)
               (cons (car elm) (cadr elm)))
             (mapcan 'identity leaf--value)))

    ((memq leaf--key '(:setf :push :pre-setf :pre-push))
     ;; Just merge leaf--value normalizer.
     (apply #'append leaf--value))

    ((memq leaf--key '(:bind :bind* :bind-keymap :bind-keymap*))
     ;; Accept: `leaf-keys' accept form
     ;; Return: a pair like (leaf--value . (fn fn ...))
     (eval `(leaf-keys ,leaf--value ,leaf--name)))

    ((memq leaf--key '(:leaf-defvar))
     ;; Accept: 't, 'nil, symbol and list of these
     ;; Return: :bind, :bind* specified map variables
     (when (eval (car leaf--value))
       (let ((args (append (leaf-plist-get :bind leaf--raw)
                           (leaf-plist-get :bind* leaf--raw))))
         (when args
           (mapcan (lambda (elm)
                     (when (symbolp (car elm))
                       `(,(leaf-sym-from-keyword (car elm)))))
                   (car (eval `(leaf-keys ,args ,leaf--name))))))))

    ((memq leaf--key '(:advice))
     ;; Accept: (:where symbol fn), ((:where symbol fn) (:where symbol fn) ...)
     ;; Return: (((advice) (advice) ...) (fn fn ...))
     ;; Note  : fn is also accept lambda form
     ;;         the arguments for `advice-add' and `:advice' are in different order.
     (let (val fns)
       (setq val (mapcan
                  (lambda (elm)
                    (cond
                     ((and (listp elm) (listp (car elm)))
                      (mapcar
                       (lambda (el)
                         (let ((where (nth 0 el)) (sym (nth 1 el)) (fn (nth 2 el)))
                           (setq fns (append fns `(,(nth 2 el)))) `(',sym ,where #',fn)))
                       elm))
                     ((listp elm)
                      (let ((where (nth 0 elm)) (sym (nth 1 elm)) (fn (nth 2 elm)))
                        (setq fns (append fns `(,(nth 2 elm)))) `((',sym ,where #',fn))))))
                  leaf--value))
       `(,val ,(delq nil (mapcar (lambda (elm) (when (symbolp elm) elm)) fns)))))

    ((memq leaf--key '(:advice-remove))
     ;; Accept: (:where symbol fn), ((:where symbol fn) (:where symbol fn) ...)
     ;; Return: (((advice) (advice) ...) (fn fn ...))
     ;; Note  : fn is also accept lambda form
     ;;         the arguments for `advice-add' and `:advice' are in different order.
     (let (val fns)
       (setq val (mapcan
                  (lambda (elm)
                    (cond
                     ((and (listp elm) (listp (car elm)))
                      (mapcar
                       (lambda (el)
                         (let ((sym (nth 0 el)) (fn (nth 1 el)))
                           (push fn fns) `(',sym #',fn)))
                       elm))
                     ((listp elm)
                      (let ((sym (nth 0 elm)) (fn (nth 1 elm)))
                        (push fn fns) `((',sym #',fn))))))
                  leaf--value))
       `(,val ,(delq nil (mapcar (lambda (elm) (when (symbolp elm) elm)) fns)))))

    ((memq leaf--key '(:vc))
     (mapcar (lambda (elm)
               (if (keywordp (car elm))
                   `(,leaf--name ,elm)
                 `(,(car elm) ,(cdr elm))))
             leaf--value))

    (t
     leaf--value))
  "Normalize rule.")

(defvar leaf-verify
  '(((memq leaf--key '(:package))
     (if (not (equal '(nil) (car leaf--value))) leaf--value nil))
    ((memq leaf--key '(:after))
     (delq nil
           (mapcar
            (lambda (elm)
              (cond
               ((eq elm nil)
                (prog1 nil
                  (leaf-error "Error occurs in leaf block: %s" leaf--name)
                  (leaf-error "Attempt wait constant: nil;  Please check your specification")))
               ((keywordp elm)
                (prog1 nil
                  (leaf-error "Error occurs in leaf block: %s" leaf--name)
                  (leaf-error "Attempt wait constant keyword: %s;  Please check your specification" elm)))
               (t
                elm)))
            leaf--value)))
    ((memq leaf--key (list
                      :hook :defun
                      :pl-setq :pl-pre-setq :pl-setq-default :pl-custom
                      :auth-custom :auth-pre-setq :auth-setq :auth-setq-default
                      :setq :pre-setq :setq-default :custom :custom-face))
     (delq nil
           (mapcar
            (lambda (elm)
              (let ((var (car elm)))
                (cond
                 ((eq t var)
                  (prog1 nil
                    (leaf-error "Error occurs in leaf block: %s" leaf--name)
                    (leaf-error "Attempt modify constant: t;  Please check your specification")))
                 ((eq nil var)
                  (prog1 nil
                    (leaf-error "Error occurs in leaf block: %s" leaf--name)
                    (leaf-error "Attempt modify constant: nil;  Please check your specification")))
                 ((keywordp var)
                  (prog1 nil
                    (leaf-error "Error occurs in leaf block: %s" leaf--name)
                    (leaf-error "Attempt modify constant keyword: %s;  Please check your specification" var)))
                 ((not (symbolp var))
                  (prog1 nil
                    (leaf-error "Error occurs in leaf block: %s" leaf--name)
                    (leaf-error "Attempt modify list;  Please check your specification")))
                 (t
                  elm))))
            leaf--value)))
    (t
     leaf--value))
  "Verify rule.")


;;;; Customize variables

(defcustom leaf-defaults '()
  "The value that are interpreted as specified for all `leaf' blocks."
  :type '(plist :key-type (choice (const :leaf-autoload)
                                  (const :leaf-defer)
                                  (const :leaf-protect)
                                  (const :leaf-defun)
                                  (const :leaf-defvar)
                                  (const :leaf-path)
                                  (symbol :tag "A keyword in `M-x leaf-available-keywords`"))
                :value-type (choice boolean
                                    (sexp :tag "Default value of the keyword")))
  :group 'leaf)

(defvar leaf-system-defaults (list
                              :leaf-autoload t :leaf-defer t :leaf-protect t
                              :leaf-defun t :leaf-defvar t :leaf-path t)
  "The value for all `leaf' blocks for leaf system.")

(defcustom leaf-defer-keywords (list
                                :bind :bind*
                                :mode :interpreter :magic :magic-fallback
                                :hook :commands)
  "The specified keyword is interpreted as a defer keyword.
`leaf' blocks containing the keywords are interpreted as lazy loadable.
To stop this function, specify ':leaf-defer nil'"
  :type 'sexp
  :group 'leaf)

(defcustom leaf-alias-keyword-alist '((:ensure . :package))
  "The alias keyword.  KEY is treated as an alias for VALUE."
  :type 'sexp
  :group 'leaf)

(defcustom leaf-expand-minimally nil
  "If non-nil, make the expanded code as minimal as possible.
If non-nil, disabled keywords of `leaf-expand-minimally-suppress-keywords'."
  :type 'boolean
  :group 'leaf)

(defcustom leaf-expand-minimally-suppress-keywords '(:leaf-protect :leaf-defun :leaf-defvar :leaf-path)
  "Suppress keywords when `leaf-expand-minimally' is non-nil."
  :type 'sexp
  :group 'leaf)

(defcustom leaf-options-ensure-default-pin nil
  "Set the default pin with :package.
'nil is using package manager default.
This feature is not yet implemented."
  :type 'sexp
  :group 'leaf)

(defcustom leaf-use-authinfo nil
  "If non-nil value, use raw authinfo file as encripted file.
If nil, use authinfo.plist powerd by `plstore' for :auth-* keywords'"
  :type 'boolean
  :group 'leaf)

(defcustom leaf-default-plstore
  (let ((path (locate-user-emacs-file "leaf-plstore.plist")))
    (when (file-readable-p path)
      (plstore-open path)))
  "Default value if omit store variable in plstore related keywords.
This variable must be result of `plstore-open'."
  :type 'sexp
  :group 'leaf)

(defcustom leaf-find-regexp ".*([[:space:]]*leaf[[:space:]]+\\(%s\\)"
  "The regexp used by `leaf-find' to search for a leaf block.
Note it must contain a `%s' at the place where `format'
should insert the leaf name."
  :type 'regexp
  :group 'leaf)

(defcustom leaf-enable-imenu-support t
  "If non-nil, enable `imenu' integrations.
Ref: `lisp-imenu-generic-expression'."
  :type 'boolean
  :set (lambda (sym value)
         (set sym value)
         (eval-after-load 'lisp-mode
           (let ((regexp (eval-when-compile
                           (require 'rx)
                           (rx-to-string
                            `(and (* any) "(" (* space) "leaf" (+ space)
                                  (group
                                   (regexp
                                    ,(or (bound-and-true-p lisp-mode-symbol-regexp)
                                         "\\(?:\\sw\\|\\s_\\|\\\\.\\)+"))))
                            'nogroup))))
             (if value
                 `(add-to-list 'lisp-imenu-generic-expression
                               '("Leaf" ,regexp 1))
               `(setq lisp-imenu-generic-expression
                      (delete '("Leaf" ,regexp 1)
                              lisp-imenu-generic-expression))))))
  :group 'leaf)

(defcustom leaf-find-function-support t
  "If non-nil, enable `find-func' integrations.
Ref: `find-function-regexp-alist'."
  :type 'boolean
  :set (lambda (sym value)
         (set sym value)
         (eval-after-load 'find-func
           (if value
               `(add-to-list 'find-function-regexp-alist '(leaf . leaf-find-regexp))
             `(setq find-function-regexp-alist
                    (delete '(leaf . leaf-find-regexp) find-function-regexp-alist)))))
  :group 'leaf)


;;;; General polyfill

;;; Polyfill for legacy Emacs

(defun leaf-mapcaappend (func seq)
  "Another implementation for `mapcan'.
`mapcan' uses `nconc', but Emacs-22 doesn't support it.

Apply FUNC to each element of SEQ, and concatenate
the results by altering them (using `nconc').
SEQ may be a list, a vector, a 'bool-vector, or a string."
  (declare (indent 2))
  (apply #'append (apply #'mapcar func seq nil)))

(eval-and-compile
  (unless (fboundp 'mapcan)
    (defalias 'mapcan 'leaf-mapcaappend)))

;;; predictors

(defun leaf-pairp (var &optional allow)
  "Return t if VAR is pair.
If ALLOW is non-nil, allow nil as the last element."
  (and (listp var)
       (or (atom (cdr var))                  ; (a . b)
           (member 'lambda var)              ; (a . (lambda (elm) elm)) => (a lambda elm elm)
           (and (= 3 (safe-length var))      ; (a . 'b) => (a quote b)
                (member `',(cadr var) `('quote ',backquote-backquote-symbol 'function))))
       (if allow t (not (null (cdr var)))))) ; (a . nil) => (a)

(defun leaf-dotlistp (var &optional allow)
  "Return t if VAR is doted list.
If ALLOW is non-nil, allow nil as the last element."
  (or (leaf-pairp var allow)                 ; (a b c . (lambda (v) v)) => (pairp '(c . (lambda (v) v)))
      (leaf-pairp (last var) allow)          ; (a b c . d) => (pairp '(c . d))
      (leaf-pairp (last var 3) allow)))      ; (a b c . 'd) => (pairp '(c . 'd))

;;; General list functions

(defun leaf-list-memq (symlist list)
  "Return t if LIST contained element of SYMLIST."
  (delq nil (mapcar (lambda (x) (memq x list)) symlist)))

(defun leaf-flatten (lst)
  "Return flatten list of LST."
  (let (fn)
    (setq fn (lambda (lst) (if (atom lst) `(,lst) (mapcan fn lst))))
    (funcall fn lst)))

(defun leaf-subst (old new lst)
  "Substitute NEW for OLD in LST."
  (declare (indent 2))
  (mapcar (lambda (elm) (if (eq elm old) new elm)) lst))

(defun leaf-copy-list (list)
  "Return a copy of LIST, which may be a dotted list.  see `cl-copy-list'.
The elements of LIST are not copied, just the list structure itself."
  (if (consp list)
      (let (res)
        (while (consp list) (push (pop list) res))
        (prog1 (nreverse res) (setcdr res list)))
    (car list)))

(defun leaf-insert-after (value list index)
  "Insert VALUE into LIST after INDEX."
  (push value (cdr (nthcdr index list)))
  list)

(defun leaf-insert-after-value (value list target &optional searchfn)
  "Insert VALUE into LIST after TARGET search with SEARCHFN."
  (let ((len (length list))
        (part (funcall (or searchfn #'memq) target list)))
    (if (not part)
        (leaf-error "%s is not found in given list; %s" target list)
      (leaf-insert-after value list (- len (length part))))))

(defun leaf-safe-mapcar (fn seq)
  "Apply FN to each element of SEQ, and make a list of the results.
The result is a list just as long as SEQUENCE.
SEQ may be a list, a vector, a 'bool-vector, or a string.
Unlike `mapcar', it works well with dotlist (last cdr is non-nil list)."
  (when (cdr (last seq))
    (setq seq (leaf-copy-list seq))
    (setcdr (last seq) nil))
  (mapcar fn seq))

(defun leaf-safe-butlast (list &optional n)
  "Return a copy of LIST with the last N elements removed.
If N is omitted or nil, the last element is removed from the copy.
Unlike `butlast', it works well with dotlist (last cdr is non-nil list)."
  (when (cdr (last list))
    (setq list (leaf-copy-list list))
    (setcdr (last list) nil))
  (butlast list n))

;;; General plist functions

(defun leaf-plist-keys (plist)
  "Get all keys of PLIST."
  (let (ret)
    (while plist
      (setq ret (cons (pop plist) ret))
      (pop plist))
    (nreverse ret)))

(defun leaf-plist-get (key plist &optional default)
  "`plist-get' with DEFAULT value in PLIST search KEY."
  (declare (indent 1))
  (or (and (plist-member plist key) (plist-get plist key)) default))

(when (version<= "24.3" emacs-version)
  (gv-define-expander leaf-plist-get
    (lambda (do key plist &optional default)
      (macroexp-let2 macroexp-copyable-p k key
        (gv-letplace (getter setter) plist
          (macroexp-let2 nil p `(plist-member ,getter ,k)
            (funcall
             do
             (if (null default) `(cadr ,p)
               `(if ,p (cadr ,p) ,default))
             (lambda (val)
               `(if (plist-member ,plist ,k)
                    (setcar (cdr (plist-member ,plist ,k)) ,val)
                  ,(funcall setter `(cons ,k (cons ,val ,getter))))))))))))

;;; General alist functions

;; for Emacs < 25.1
(defun leaf-alist-get (key alist &optional default _remove testfn)
  "Find the first element of ALIST whose `car' equals KEY and return its `cdr'.
If KEY is not found in ALIST, return DEFAULT.
Equality with KEY is tested by TESTFN, defaulting to `eq'.
see `alist-get'."
  (let ((x (if (not testfn)
               (assq key alist)
             (assoc key alist testfn))))
    (if x (cdr x) default)))

;;; et cetera

(defun leaf-sym-from-keyword (keyword)
  "Return normalizied `intern'ed symbol from KEYWORD or SYMBOL."
  (if (keywordp keyword)
      (intern (substring (symbol-name keyword) 1))
    keyword))

(defun leaf-mode-sym (sym)
  "Return mode like symbol from SYM."
  (let ((sym-str (symbol-name sym)))
    (intern (concat sym-str (unless (string-match-p "-mode$" sym-str) "-mode")))))

(defun leaf-error (message &rest args)
  "Raise error with type leaf.  MESSAGE and ARGS is same form as `lwarn'."
  (apply #'lwarn `(leaf :error ,message ,@args)))

(defun leaf--create-custom-comment (type &rest args)
  "Create message for TYPE using ARGS."
  (concat
   (format "Customized with leaf in `%s' block" leaf--name)
   (when (memq type '(:pl-custom :auth-custom))
     (let* ((store (pop args)))
       (format " using `%s' plstore" store)))
   (when load-file-name
     (format " at `%s'" load-file-name))))

(defsubst leaf-this-file ()
  "Return path to this file."
  (or (bound-and-true-p leaf--load-file-name)
      (bound-and-true-p byte-compile-current-file)
      load-file-name
      buffer-file-name))


;;;; General functions for leaf

;;;###autoload
(defun leaf-available-keywords ()
  "Return current available `leaf' keywords list."
  (interactive)
  (let* ((keywords (leaf-plist-keys leaf-keywords))
         (alias-from (delete-dups (mapcar #'car leaf-alias-keyword-alist)))
         (alias-alist
          (mapcar (lambda (elm) `(,elm . ,(leaf-alist-get elm leaf-alias-keyword-alist))) alias-from))
         (ret (progn
                (dolist (elm alias-alist)
                  (let ((from (car elm))
                        (to   (cdr elm)))
                    (if (not (memq to keywords))
                        (leaf-error "`leaf-alias-keyword-alist' is broken.  %s is missing from leaf-keywords; %s" to keywords)
                      (leaf-insert-after-value from keywords to))))
                keywords)))
    (if (called-interactively-p 'interactive)
        (message (prin1-to-string ret))
      ret)))

;;;###autoload
(defmacro leaf-pp-to-string (sexp)
  "Return format string of `leaf' SEXP like `pp-to-string'."
  `(with-output-to-string
     (leaf-pp ,sexp)))

;;;###autoload
(defun leaf-pp (sexp)
  "Output the pretty-printed representation of leaf SEXP."
  (prog1 nil
    (let ((str (with-temp-buffer
                 (insert (replace-regexp-in-string
                          (eval
                           `(rx (group
                                 (or ,@(mapcar #'symbol-name (leaf-available-keywords))))))
                          "\n\\1"
                          (prin1-to-string sexp)))
                 (delete-trailing-whitespace)
                 (emacs-lisp-mode)
                 (indent-region (point-min) (point-max))
                 (buffer-substring-no-properties (point-min) (point-max)))))
      (princ (concat str "\n")))))

(defvar leaf-expand-buffer-name "*Leaf Expand*")
(defvar leaf-expand-issue-template
  "## Description

<!-- Please write a description of your issue -->

## Issue leaf-block
```elisp
%s
```

## `macroexpand-1` leaf-block
```elisp
%s
```

## Expected leaf-block

<!-- Please write a Expected leaf-block -->

```elisp


```
")

;;;###autoload
(defun leaf-create-issue-template ()
  "Create issue template buffer."
  (interactive)
  (let* ((buf (get-buffer-create leaf-expand-buffer-name))
         (raw (save-excursion
                (condition-case _err
                    (while (not (looking-at "(leaf "))
                      (backward-up-list 1))
                  (error nil))
                (buffer-substring-no-properties
                 (line-beginning-position) (scan-sexps (point) 1))))
         (sexp (read raw))
         (leaf-expand-minimally t))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (format leaf-expand-issue-template
               raw
               (let ((eval-expression-print-length nil)
                     (eval-expression-print-level  nil)
                     (print-quoted t))
                 (pp-to-string
                  (funcall
                   (if (fboundp 'macroexpand-1) 'macroexpand-1 'macroexpand)
                   sexp)))))
      (display-buffer buf))))

;;;###autoload
(defun leaf-expand ()
  "Expand `leaf' at point."
  (interactive)
  (let* ((buf (get-buffer-create leaf-expand-buffer-name))
         (raw (save-excursion
                (condition-case _err
                    (while (not (looking-at "(leaf "))
                      (backward-up-list 1))
                  (error nil))
                (buffer-substring-no-properties
                 (line-beginning-position) (scan-sexps (point) 1))))
         (sexp (read raw))
         (leaf-expand-minimally t))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       (format "%s\n\n;; %s\n\n%s"
               raw
               (make-string 80 ?-)
               (let ((eval-expression-print-length nil)
                     (eval-expression-print-level  nil)
                     (print-quoted t))
                 (pp-to-string
                  (funcall
                   (if (fboundp 'macroexpand-1) 'macroexpand-1 'macroexpand)
                   sexp)))))
      (emacs-lisp-mode)
      (indent-region (point-min) (point-max))
      (display-buffer buf))))

(defmacro leaf-safe-push (newelt place &optional no-dup)
  "Safely add NEWELT to the list stored in the generalized variable PLACE.
This is equivalent to `push' if PLACE is bound, otherwise, `setq'
is used to define a new list.
If NO-DUP is non-nil, do not `push' if the element already exists."
  `(if (boundp ',place)
       ,(if (not no-dup)
            `(push ,newelt ,place)
          `(unless (memq ,newelt ,place)
             (push ,newelt ,place)))
     (setq ,place (list ,newelt))))


;;;; find-function

(defun leaf-find (name)
  "Find the leaf block of NAME."
  (interactive
   (let ((candidates (delete-dups (mapcar #'car leaf--paths))))
     (if (not candidates)
         (error "Leaf has no definition informations")
       (list (completing-read "Find leaf: " (delete-dups (mapcar #'car leaf--paths)))))))
  (require 'find-func)
  (let* ((name (intern name))
         (paths (mapcan (lambda (elm) (when (eq name (car elm)) (list (cdr elm)))) leaf--paths))
         (path (if (= (length paths) 1) (car paths) (completing-read "Select one: " paths)))
         (location (find-function-search-for-symbol name 'leaf path)))
    (when location
      (prog1 (pop-to-buffer (car location))
        (when (cdr location)
          (goto-char (cdr location)))
        (run-hooks 'find-function-after-hook)))))


;;;; Key management

(defvar leaf-key-override-global-map (make-keymap)
  "The leaf-override-global-mode keymap.")

(define-minor-mode leaf-key-override-global-mode
  "A minor mode so that keymap settings override other modes."
  :init-value t
  :lighter "")

;; the keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((leaf-key-override-global-mode . ,leaf-key-override-global-map)))

(defvar leaf-key-bindlist nil
  "List of bindings performed by `leaf-key'.
Elements have the form (MAP KEY CMD ORIGINAL-CMD PATH)")

(defmacro leaf-key (key command &optional keymap)
  "Bind KEY to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'.  Or it may be a string to be interpreted as spelled-out
keystrokes.  See documentation of `edmacro-mode' for details.

COMMAND must be an interactive function. lambda form, menu-item,
or the form that returned one of them also be accepted.

KEYMAP, if present, should be a keymap and not a quoted symbol.
For example:
  (leaf-key \"M-h\" #'some-interactive-function my-mode-map)

You can also use [remap COMMAND] as KEY.
For example:
  (leaf-key [remap backward-sentence] 'sh-beginning-of-command)"
  (let* ((key*     (eval key))
         (command* (eval command))
         (keymap*  (eval keymap))
         (bindto   (cond ((symbolp command*) command*)
                         ((eq (car-safe command*) 'lambda) '*lambda-function*)
                         ((eq (car-safe command*) 'menu-item) '*menu-item*)))
         (mmap     (or keymap* 'global-map))
         (vecp     (vectorp key*))
         (path     (leaf-this-file))
         (_mvec    (if (vectorp key*) key* (read-kbd-macro key*)))
         (mstr     (if (stringp key*) key* (key-description key*))))
    `(let* ((old (lookup-key ,mmap ,(if vecp key* `(kbd ,key*))))
            (value ,(list '\` `(,mmap ,mstr ,bindto ,',(and old (not (numberp old)) old) ,path))))
       (leaf-safe-push value leaf-key-bindlist)
       (define-key ,mmap ,(if vecp key* `(kbd ,key*))
                   ,(if (eq bindto '*lambda-function*) command* `',command*)))))

(defmacro leaf-key* (key command)
  "Similar to `leaf-key', but overrides any mode-specific bindings.
Bind COMMAND at KEY."
  `(leaf-key ,key ,command 'leaf-key-override-global-map))

(defmacro leaf-keys (bind &optional dryrun-name bind-keymap bind-keymap-pkg)
  "Bind multiple BIND for KEYMAP defined in PKG.
BIND is (KEY . COMMAND), (KEY . (lambda ...)). (KEY . nil) to unbind KEY.
If BIND-KEYMAP is non-nil generate `leaf-key-bind-keymap' instead of `leaf-key'.
If BIND-KEYMAP-PKG is passed, require it before binding.

OPTIONAL:
  BIND also accept below form.
    (:{{map}} :package {{pkg}} (KEY . COMMAND) (KEY . COMMAND))
  KEYMAP is keymap name.
  PKG is package name which define KEYMAP.
  (wrap `eval-after-load' PKG)

  If DRYRUN-NAME is non-nil, return list like
  (LEAF_KEYS-FORMS (FN FN ...))

  If omit :package of BIND, fill it in LEAF_KEYS-FORM.

NOTE: BIND can also accept list of these."
  (let ((pairp (lambda (x)
                 (condition-case _err
                     (and (listp x)
                          (or (stringp (eval (car x)))
                              (vectorp (eval (car x)))))
                   (error nil))))
        recurfn forms bds fns)
    (setq recurfn
          (lambda (bind)
            (cond
             ((funcall pairp bind)
              (push (if bind-keymap
                        `(leaf-key-bind-keymap ,(car bind) ,(cdr bind) nil ,bind-keymap-pkg)
                      (if (atom (cdr bind))
                          `(leaf-key ,(car bind) #',(cdr bind))
                        `(leaf-key ,(car bind) ,(cdr bind))))
                    forms)
              (push bind bds)
              (push (cdr bind) fns))
             ((or (keywordp (car bind))
                  (symbolp (car bind)))
              (let* ((map (leaf-sym-from-keyword (car bind)))
                     (pkg (leaf-plist-get :package (cdr bind)))
                     (elmbind (if pkg (nthcdr 3 bind) (nthcdr 1 bind)))
                     (elmbinds (if (funcall pairp (car elmbind)) elmbind (car elmbind)))
                     (form `(progn
                              ,@(mapcar
                                 (lambda (elm)
                                   (push (cdr elm) fns)
                                   (if bind-keymap
                                       `(leaf-key-bind-keymap ,(car elm) ,(cdr elm) ',map ,bind-keymap-pkg)
                                     (if (atom (cdr elm))
                                         `(leaf-key ,(car elm) #',(cdr elm) ',map)
                                       `(leaf-key ,(car elm) ,(cdr elm) ',map))))
                                 elmbinds))))
                (push `(,map :package ,(or `,pkg `,dryrun-name) ,@elmbinds) bds)
                (when pkg
                  (dolist (elmpkg (if (atom pkg) `(,pkg) pkg))
                    (unless bind-keymap
                      (setq form `(eval-after-load ',elmpkg ',form)))))
                (push form forms)))
             (t (mapcar recurfn bind)))))
    (funcall recurfn bind)
    (if dryrun-name
        `'(,(nreverse bds) ,(nreverse fns))
      (if (cdr forms) `(progn ,@(nreverse forms)) (car forms)))))

(defmacro leaf-keys* (bind)
  "Similar to `leaf-keys' but bind BIND to `leaf-key-override-global-map'.
BIND must not contain :{{map}}."
  (let ((binds (if (and (atom (car bind)) (atom (cdr bind))) `(,bind) bind)))
    `(leaf-keys (:leaf-key-override-global-map ,@binds))))

;;; leaf-keys-bind-keymap

(defmacro leaf-key-bind-keymap (key kmap &optional keymap pkg)
  "Bind KEY to KMAP in KEYMAP (`global-map' if not passed).
If PKG passed, require PKG before binding."
  `(progn
     ,(when pkg `(require ,pkg))
     (leaf-key ,key ,kmap ,keymap)))

(defmacro leaf-key-bind-keymap* (key keymap &optional pkg)
  "Similar to `leaf-keys-bind-keymap', but overrides any mode-specific bindings.
Bind KEYMAP at KEY.
If PKG passed, require PKG before binding."
  `(leaf-keys-bind-keymap ,key ,keymap 'leaf-key-override-global-map ,pkg))

(defmacro leaf-keys-bind-keymap (bind &optional dryrun-name pkg)
  "Bind multiple BIND for KEYMAP defined in PKG.
BIND is (KEY . KEYMAP) or (KEY . nil) to unbind KEY.
If PKG passed, require PKG before binding.

OPTIONAL:
  BIND also accept below form.
    (:{{map}} :package {{pkg}} (KEY . KEYMAP) (KEY . KEYMAP))
  KEYMAP is quoted keymap name.
  PKG is quoted package name which define KEYMAP.
  (wrap `eval-after-load' PKG)

  If DRYRUN-NAME is non-nil, return list like
  (LEAF_KEYS-FORMS (FN FN ...))

  If omit :package of BIND, fill it in LEAF_KEYS-FORM.

NOTE: BIND can also accept list of these."
  `(leaf-keys ,bind ,dryrun-name 'bind-keymap ,pkg))

(defmacro leaf-keys-bind-keymap* (bind &optional dryrun-name pkg)
  "Similar to `leaf-keys-bind-keymap' but overrides any mode-specific bindings.
BIND must not contain :{{map}}.
If PKG passed, require PKG before binding."
  (let ((binds (if (and (atom (car bind)) (atom (cdr bind))) `(,bind) bind)))
    `(leaf-keys (:leaf-key-override-global-map ,@binds) ,dryrun-name 'bind-keymap ,pkg)))

(define-derived-mode leaf-key-list-mode tabulated-list-mode "Leaf-key Bindings"
  "Major mode for listing bindings configured via `leaf-key'."
  (setq tabulated-list-format [("Map"     20 t)
                               ("Key"     20 t)
                               ("Command" 40 t)
                               ("Before Command" 40 t)
                               ("Path" 0 t)])
  (setq tabulated-list-entries
        (let ((id 0)
              (formatfn (lambda (elm)
                          (if (stringp elm)
                              elm
                            (prin1-to-string (if (eq elm nil) '--- elm)))))
              res)
          (dolist (elm leaf-key-bindlist)
            (setq id (1+ id))
            (push `(,id [,(funcall formatfn (nth 0 elm))
                         ,(funcall formatfn (nth 1 elm))
                         ,(funcall formatfn (nth 2 elm))
                         ,(funcall formatfn (nth 3 elm))
                         ,(funcall formatfn (nth 4 elm))])
                  res))
          (nreverse res)))
  (setq tabulated-list-sort-key '("Map" . nil))
  (tabulated-list-print)
  (tabulated-list-init-header))

;;;###autoload
(defun leaf-key-describe-bindings ()
  "Display all the bindings configured via `leaf-key'."
  (interactive)
  (require 'tabulated-list)
  (let ((buf (get-buffer-create "*Leaf-key bindings*")))
    (with-current-buffer buf
      (leaf-key-list-mode))
    (display-buffer buf)))


;;;; Handler

(defun leaf-register-autoload (fn pkg)
  "Registor FN as autoload for PKG.
FN also accept list of FN."
  (mapc
   (lambda (elm)
     (let ((target `(,elm . ,(symbol-name pkg))))
       (when (and elm (symbolp elm) (not (member target leaf--autoload)))
         (push target leaf--autoload))))
   (if (listp fn) fn `(,fn))))

(defun leaf-register-bind-keymap-autoload (fn pkg)
  "Registor FN as autoload for PKG.
FN also accept list of FN."
  (mapc
   (lambda (elm)
     (let ((target `(,(intern (format "leaf-key--bind-keymap--%s--%s" pkg elm)) . ,(symbol-name pkg))))
       (when (and elm (symbolp elm) (not (member target leaf--autoload)))
         (push target leaf--autoload))))
   (if (listp fn) fn `(,fn))))

(defmacro leaf-handler-leaf-protect (name &rest body)
  "Meta handler for :leaf-protect in NAME for BODY `leaf' block."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (display-warning 'leaf (format
                              ,(concat
                                (format "Error in `%s' block" name)
                                (when load-file-name
                                  (format " at `%s'" load-file-name))
                                "."
                                "  Error msg: %s")
                              (error-message-string err))))))

(defmacro leaf-handler-leaf-path (name)
  "Meta handler for :leaf-path for NAME."
  `(let ((file (leaf-this-file)))
     (unless (boundp 'leaf--paths) (defvar leaf--paths nil))
     (when file
      (add-to-list 'leaf--paths (cons ',name file)))))

(defmacro leaf-handler-package (name pkg _pin)
  "Handler for ensuring the installation of PKG with package.el
via PIN in the leaf block NAME."
  `(progn
     (leaf-safe-push ',pkg package-selected-packages 'no-dup)
     (unless (package-installed-p ',pkg)
       (unless (assoc ',pkg package-archive-contents)
         (package-refresh-contents))
       (condition-case _err
           (package-install ',pkg)
         (error
          (package-refresh-contents)
          (condition-case err
              (package-install ',pkg)
            (error
             (display-warning 'leaf
                              (format
                               ,(concat
                                 (format "In `%s' block" name)
                                 (when load-file-name
                                   (format " at `%s'" load-file-name))
                                 (format ", failed to :package of `%s'." pkg)
                                 "  Error msg: %s")
                               (error-message-string err))))))))))

(defmacro leaf-handler-vc (_name spec &optional _local-path)
  "Handler for :vc SPEC for leaf block NAME.
SPEC is a list of the form (PKG OPTIONS REVISION)"
  (declare (indent 1))
  (let ((pkg (nth 0 spec))
        (opts (nth 1 spec))
        (rev (nth 2 spec)))
    `(unless (package-installed-p ',pkg)
       (package-vc-install
        ',(if opts `(,pkg ,@opts) pkg)
        ,rev))))

(defmacro leaf-handler-auth (name sym store)
  "Handler auth-* to set SYM of NAME from STORE."
  (if leaf-use-authinfo
      `(let ((res (auth-source-search :host ,(format "leaf-%s" sym))))
         (if res
             (funcall (plist-get (car res) :secret))
           (error ,(format "Failed to search `leaf-%s' as machine/host name in auth-sources: '%%s" sym)
                  auth-sources)))
    `(if ,store
         (let ((res (cdr-safe (plstore-get ,store ,(format "leaf-%s" name)))))
           (if res
               (plist-get res ,(intern (format ":%s" sym)))
             (error ,(format "Failed to search `leaf-%s' in specified plstore" name))))
       (error "Right value is returns nil or `leaf-default-plstore' is not set"))))


;;; General list functions for leaf

(defun leaf-apply-keyword-alias (plist)
  "Apply keyword alias for PLIST."
  (let* ((alias-from (delete-dups (mapcar #'car leaf-alias-keyword-alist)))
         (alias-alist
          (mapcar (lambda (elm) `(,elm . ,(leaf-alist-get elm leaf-alias-keyword-alist))) alias-from)))
    (dolist (elm alias-alist)
      (let ((from (car elm))
            (to   (cdr elm)))
        (setq plist (leaf-subst from to plist))))
    plist))

(defun leaf-append-defaults (plist)
  "Append leaf default values to PLIST and return it."
  (append (when leaf-expand-minimally
            (mapcan (lambda (elm) (list elm nil))
                    leaf-expand-minimally-suppress-keywords))
          plist leaf-defaults leaf-system-defaults))

(defun leaf-normalize-list-in-list (lst &optional dotlistp provval)
  "Return normalized list from LST.
PROVVAL is provisionary value.

Example:
  - when DOTLISTP is nil
    a                 => (a)
    (a b c)           => (a b c)
    (a . b)           => (a . b)
    (a . nil)         => (a . nil)
    (a)               => (a . nil)
    ((a . b) (c . d)) => ((a . b) (c . d))
    ((a) (b) (c))     => ((a) (b) (c))
    ((a b c) . d)     => ((a b c) . d)

  - when DOTLISTP is non-nil
    a                 => ((a))
    (a b c)           => ((a) (b) (c))
    (a . b)           => ((a . b))
    (a . nil)         => ((a . nil))
    (a)               => ((a . nil))
    ((a . b) (c . d)) => ((a . b) (c . d))
    ((a) (b) (c))     => ((a) (b) (c))
    ((a b c) . d)           => ((a . d) (b . d) (c . d))
    ((x . y) ((a b c) . d)) => ((x . y) (a . d) (b . d) (c . d))"
  (if (not dotlistp)
      (if (atom lst) (list lst) lst)
    (cond
     ((atom lst) `((,lst . ,provval)))
     ((listp lst)
      (let* ((butlast-n 0)
             (prov
              (cond
               ((cdr (last lst))
                (cdr (last lst)))
               ((member 'lambda lst)
                (last lst (setq butlast-n (length (member 'lambda lst)))))
               ((member `',(car (last lst 2)) `('quote ',backquote-backquote-symbol 'function))
                (last lst (setq butlast-n 2))))))
        (mapcan
         (lambda (elm)
           (leaf-normalize-list-in-list elm t (or prov provval)))
         (leaf-safe-butlast lst butlast-n)))))))

;;; Pseudo-plist functions

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
  (let (retplist)
    (dolist (key (leaf-available-keywords))
      (when (plist-member plist key)
        (setq retplist `(,@retplist ,key ,(plist-get plist key)))
        (plist-put plist key nil)))
    (while plist
      (let* ((key (pop plist)) (val (pop plist)))
        (when val
          (signal 'error `(,(format "leaf: Unrecognized keyword %s" (symbol-name key)))))))
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
  (let (retplist)
    (while plist
      (let* ((key (pop plist)) (value (pop plist)))
        (if (plist-member retplist key)
            (plist-put retplist key `(,@(plist-get retplist key) ,@value))
          (setq retplist `(,@retplist ,key ,value)))))
    retplist))

(defun leaf-normalize-plist (plist &optional mergep evalp)
  "Given a pseudo-PLIST, return PLIST.
If MERGEP is t, return well-formed PLIST.
If EVALP is t, `eval' each element which have `quote' or `backquote'.

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
  (let ((rlist (reverse plist))
        retplist worklist)
    (dolist (target rlist)
      (if (keywordp target)
          (progn
            (push worklist retplist)
            (push target retplist)

            ;; clean worklist for new keyword
            (setq worklist nil))
        (push (if (and evalp
                       (listp target)
                       (member `',(car target) `('quote ',backquote-backquote-symbol)))
                  (eval target)
                target)
              worklist)))

    ;; merge value for duplicated key if MERGEP is t
    (if mergep (leaf-merge-dupkey-values-plist retplist) retplist)))


;;;; Main macro

(eval
 `(progn
    ,@(mapcar
       (lambda (elm)
         (let ((keyname (substring (symbol-name elm) 1)))
           `(defcustom ,(intern (format "leaf-expand-%s" keyname)) t
              ,(format "If nil, do not expand values for :%s." keyname)
              :type 'boolean
              :group 'leaf)))
       (leaf-available-keywords))))

(defun leaf-process-keywords (name plist raw)
  "Process keywords for NAME via argument PLIST, RAW.
NOTE:
  Not check PLIST, PLIST has already been carefully checked
  parent funcitons.
  Don't call this function directory."
  (when plist
    (let* ((leaf--name    name)
           (leaf--key     (pop plist))
           (leaf--keyname (substring (symbol-name leaf--key) 1))
           (leaf--value   (pop plist))
           (leaf--raw     raw)
           (leaf--rest    plist)
           leaf--body)
      ;; renew (normalize) leaf--value, save follow expansion in leaf--body
      (setq leaf--value (eval `(cond ,@leaf-normalize)))
      (setq leaf--value (eval `(cond ,@leaf-verify)))
      (setq leaf--body (leaf-process-keywords leaf--name leaf--rest leaf--raw))

      ;; if leaf-expand-no-error is nil, stop :no-error expansion.
      ;; unconditionally expands if leaf-expand is not declared,
      ;; as when only leaf-keyword is updated by the user or other packages.
      (let ((var (intern (format "leaf-expand-%s" leaf--keyname))))
        (if (boundp var)
            (if (eval var)
                (eval (plist-get leaf-keywords leaf--key))
              leaf--body)
          (eval (plist-get leaf-keywords leaf--key)))))))

;;;###autoload
(defmacro leaf (name &rest args)
  "Symplify your `.emacs' configuration for package NAME with ARGS."
  (declare (indent defun))
  (let* ((args* (leaf-sort-values-plist
                 (leaf-normalize-plist
                  (leaf-apply-keyword-alias
                   (leaf-append-defaults args)) 'merge 'eval)))
         leaf--autoload)
    `(prog1 ',name
       ,@(leaf-process-keywords name args* args*))))

(provide 'leaf)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf.el ends here
