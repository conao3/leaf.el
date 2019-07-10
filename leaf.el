;;; leaf.el --- Simplify your init.el configuration, extended use-package       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp settings
;; Version: 3.3.7
;; URL: https://github.com/conao3/leaf.el
;; Package-Requires: ((emacs "24.4"))

;;   Above declared this package requires Emacs-24, but it's for warning
;;   suppression, and will actually work from Emacs-23.
;;   But :advice, :advice-remove are not work Emacs-24.3 or lower.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the Affero GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the Affero
;; GNU General Public License for more details.

;; You should have received a copy of the Affero GNU General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides macros that allow you to declaratively configure settings typical
;; of an Elisp package with various keywords.
;;
;; By separating the settings of a package and combining many 'leaves' of a
;; package's settings, you could make a 'Yggdrasill' on top of your Emacs.
;;
;; A leaf can consist of multiple packages, in which case you can disable all
;; dependent child packages by disabling one parent's package.
;;
;; It also has a key management system and package management uses the
;; package.el.  With minimal external dependencies and careful implementation,
;; this package is guaranteed to be fully functional from Emacs-23, now, and
;; in future Emacs.
;;
;; More information is [[https://github.com/conao3/leaf.el][here]]

;;; Code:

(defgroup leaf nil
  "Symplifying your `.emacs' configuration."
  :group 'lisp)

(defcustom leaf-defaults '()
  "Default values for each leaf packages."
  :type 'sexp
  :group 'leaf)

(defcustom leaf-system-defaults '(:leaf-autoload t :leaf-defer t :leaf-protect t)
  "Default values for each leaf packages for `leaf' system."
  :type 'sexp
  :group 'leaf)

(defcustom leaf-defer-keywords (cdr '(:dummy
                                      :bind :bind*
                                      :mode :interpreter :magic :magic-fallback
                                      :hook :commands))
  "Specifies a keyword to perform a deferred load.
`leaf' blocks are lazily loaded by their package name
with values for these keywords."
  :type 'sexp
  :group 'leaf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customize backend
;;

(defcustom leaf-expand-minimally nil
  "If non-nil, make the expanded code as minimal as possible.
This disabled `leaf-expand-minimally-suppress-keywords'."
  :type 'boolean
  :group 'leaf)

(defcustom leaf-expand-minimally-suppress-keywords '(:leaf-protect)
  "Suppress keywords when `leaf-expand-minimally' is non-nil."
  :type 'sexp
  :group 'leaf)

(defcustom leaf-options-ensure-default-pin nil
  "Option :ensure pin default.
'nil is using package manager default."
  :type 'sexp
  :group 'leaf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support functions
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  For legacy Emacs
;;

(defun leaf-mapcaappend (func seq &rest rest)
  "Another implementation for `mapcan' for FUNC SEQ REST.
`mapcan' uses `nconc', but Emacs-22 doesn't support it."
  (declare (indent 2))
  (apply #'append (apply #'mapcar func seq rest)))

(unless (fboundp 'mapcan)
  (defalias 'mapcan 'leaf-mapcaappend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General functions
;;

(defsubst leaf-pairp (var &optional allow)
  "Return t if VAR is pair.  If ALLOW is non-nil, allow nil as last element."
  (and (listp var)
       (or (atom (cdr var))                  ; (a . b)
           (and (= 3 (safe-length var))      ; (a . 'b) => (a quote b)
                (member `',(cadr var) `('quote ',backquote-backquote-symbol 'function)))
           (and (= 4 (safe-length var))      ; (a . (lambda (elm) elm)) => (a lambda elm elm)
                (member `',(cadr var) '('lambda))))
       (if allow t (not (null (cdr var)))))) ; (a . nil) => (a)

(defsubst leaf-dotlistp (var &optional allow)
  "Return t if VAR is doted list.  If ALLOW is non-nil, allow nil as last element."
  (or (leaf-pairp (last var) allow)          ; (a b c . d) => (pairp '(c . d))
      (leaf-pairp (last var 3) allow)        ; (a b c . 'd) => (pairp '(c . 'd))
      (leaf-pairp (last var 4) allow)))      ; (a b c . (lambda (v) v)) => (pairp '(c . (lambda (v) v)))

(defsubst leaf-sym-or-keyword (keyword)
  "Return normalizied `intern'ed symbol from keyword or symbol."
  (if (keywordp keyword)
      (intern (substring (symbol-name keyword) 1))
    keyword))

(defun leaf-copy-list (list)
  "Return a copy of LIST, which may be a dotted list.  see `cl-copy-list'.
The elements of LIST are not copied, just the list structure itself."
  (if (consp list)
      (let ((res nil))
        (while (consp list) (push (pop list) res))
        (prog1 (nreverse res) (setcdr res list)))
    (car list)))

(defun leaf-safe-mapcar (fn seq)
  "Apply FN to each element of SEQ, and make a list of the results.
The result is a list just as long as SEQUENCE.
SEQUENCE may be a list, a vector, a bool-vector, or a string.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General list functions
;;

(defsubst leaf-list-memq (symlist list)
  "Return t if LIST contained element of SYMLIST."
  (delq nil (mapcar (lambda (x) (memq x list)) symlist)))

(defun leaf-flatten (lst)
  "Return flatten list of LST."
  (let ((fn))
    (if (fboundp 'mapcan)
        (setq fn (lambda (lst) (if (atom lst) `(,lst) (mapcan fn lst))))
      (setq fn (lambda (lst) (if (atom lst) `(,lst) (leaf-mapcaappend fn lst)))))
    (funcall fn lst)))

(defun leaf-subst (old new lst)
  "Substitute NEW for OLD in LST."
  (declare (indent 2))
  (mapcar (lambda (elm) (if (eq elm old) new elm)) lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General plist functions
;;

(defun leaf-plist-keys (plist)
  "Get all keys of PLIST."
  (let ((ret))
    (while plist
      (setq ret (cons (pop plist) ret))
      (pop plist))
    (nreverse ret)))

(defun leaf-plist-get (key plist &optional default)
  "`plist-get' with DEFAULT value in PLIST search KEY."
  (declare (indent 1))
  (or (and (plist-member plist key) (plist-get plist key)) default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  leaf keywords definition
;;

(defvar leaf--raw)
(defvar leaf--name)
(defvar leaf--key)
(defvar leaf--keyname)
(defvar leaf--value)
(defvar leaf--body)
(defvar leaf--rest)
(defvar leaf--autoload)

(defvar leaf-keywords
  (cdr
   '(:dummy
     :disabled        (unless (eval (car leaf--value)) `(,@leaf--body))
     :leaf-protect    (if (and leaf--body (eval (car leaf--value))) `((leaf-handler-leaf-protect ,leaf--name ,@leaf--body)) `(,@leaf--body))
     :load-path       `(,@(mapcar (lambda (elm) `(add-to-list 'load-path ,elm)) leaf--value) ,@leaf--body)
     :leaf-autoload   `(,@(when (car leaf--value) (mapcar (lambda (elm) `(autoload #',(car elm) ,(cdr elm) nil t)) (reverse leaf--autoload))) ,@leaf--body)

     :doc             `(,@leaf--body)
     :file            `(,@leaf--body)
     :url             `(,@leaf--body)

     :defun           `(,@(mapcar (lambda (elm) `(declare-function ,(car elm) ,(symbol-name (cdr elm)))) leaf--value) ,@leaf--body)
     :defvar          `(,@(mapcar (lambda (elm) `(defvar ,elm)) leaf--value) ,@leaf--body)

     :preface         `(,@leaf--value ,@leaf--body)
     :when            (when leaf--body `((when   ,@(if (= 1 (length leaf--value)) leaf--value `((and ,@leaf--value))) ,@leaf--body)))
     :unless          (when leaf--body `((unless ,@(if (= 1 (length leaf--value)) leaf--value `((and ,@leaf--value))) ,@leaf--body)))
     :if              (when leaf--body `((if     ,@(if (= 1 (length leaf--value)) leaf--value `((and ,@leaf--value))) (progn ,@leaf--body))))

     :ensure          `(,@(mapcar (lambda (elm) `(leaf-handler-package ,leaf--name ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
     :package         `(,@(mapcar (lambda (elm) `(leaf-handler-package ,leaf--name ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)

     :after           (when leaf--body (let ((ret `(progn ,@leaf--body)))
                                         (dolist (elm leaf--value) (setq ret `(eval-after-load ',elm ',ret)))
                                         `(,ret)))

     :commands        (progn (leaf-register-autoload leaf--value leaf--name) `(,@leaf--body))

     :bind            (progn
                        (leaf-register-autoload (cadr leaf--value) leaf--name)
                        `((leaf-keys ,(car leaf--value)) ,@leaf--body))
     :bind*           (progn
                        (leaf-register-autoload (cadr leaf--value) leaf--name)
                        `((leaf-keys* ,(car leaf--value)) ,@leaf--body))

     :mode            (progn
                        (leaf-register-autoload (mapcar #'cdr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(add-to-list 'auto-mode-alist '(,(car elm) . ,(cdr elm)))) leaf--value) ,@leaf--body))
     :interpreter     (progn
                        (leaf-register-autoload (mapcar #'cdr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(add-to-list 'interpreter-mode-alist '(,(car elm) . ,(cdr elm)))) leaf--value) ,@leaf--body))
     :magic           (progn
                        (leaf-register-autoload (mapcar #'cdr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(add-to-list 'magic-mode-alist '(,(car elm) . ,(cdr elm)))) leaf--value) ,@leaf--body))
     :magic-fallback  (progn
                        (leaf-register-autoload (mapcar #'cdr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(add-to-list 'magic-fallback-mode-alist '(,(car elm) . ,(cdr elm)))) leaf--value) ,@leaf--body))
     :hook            (progn
                        (leaf-register-autoload (mapcar #'cdr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(add-hook ',(car elm) #',(cdr elm))) leaf--value) ,@leaf--body))
     :advice          (progn
                        (leaf-register-autoload (cadr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(advice-add ,@elm)) (car leaf--value)) ,@leaf--body))
     :advice-remove   (progn
                        (leaf-register-autoload (cadr leaf--value) leaf--name)
                        `(,@(mapcar (lambda (elm) `(advice-remove ,@elm)) (car leaf--value)) ,@leaf--body))

     :leaf-defer      (if (and leaf--body (eval (car leaf--value)) (leaf-list-memq leaf-defer-keywords (leaf-plist-keys leaf--raw)))
                          `((eval-after-load ',leaf--name '(progn ,@leaf--body))) `(,@leaf--body))

     :pre-setq        `(,@(mapcar (lambda (elm) `(setq ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
     :pl-pre-setq     `(,@(mapcar (lambda (elm) `(setq ,(car elm) (plist-get (cdr (plstore-get ,(cdr elm) ,(format "leaf-%s" leaf--name))) ,(intern (format ":%s" (car elm)))))) leaf--value) ,@leaf--body)

     :init            `(,@leaf--value ,@leaf--body)

     :require         `(,@(mapcar (lambda (elm) `(require ',elm)) leaf--value) ,@leaf--body)

     :custom          `((custom-set-variables ,@(mapcar (lambda (elm) `'(,(car elm) ,(cdr elm) ,(format "Customized with leaf in %s block" leaf--name))) leaf--value)) ,@leaf--body)
     :custom-face     `((custom-set-faces     ,@(mapcar (lambda (elm) `'(,(car elm) ,(car (cddr elm)))) leaf--value)) ,@leaf--body)
     :pl-custom       `((custom-set-variables ,@(mapcar (lambda (elm) `'(,(car elm) (plist-get (cdr (plstore-get ,(cdr elm) ,(format "leaf-%s" leaf--name))) ,(intern (format ":%s" (car elm)))) ,(format "Customized in leaf `%s' from plstore `%s'" leaf--name (symbol-name (cdr elm))))) leaf--value)) ,@leaf--body)
     :setq            `(,@(mapcar (lambda (elm) `(setq ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
     :setq-default    `(,@(mapcar (lambda (elm) `(setq-default ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
     :pl-setq         `(,@(mapcar (lambda (elm) `(setq ,(car elm) (plist-get (cdr (plstore-get ,(cdr elm) ,(format "leaf-%s" leaf--name))) ,(intern (format ":%s" (car elm)))))) leaf--value) ,@leaf--body)
     :pl-setq-default `(,@(mapcar (lambda (elm) `(setq-default ,(car elm) (plist-get (cdr (plstore-get ,(cdr elm) ,(format "leaf-%s" leaf--name))) ,(intern (format ":%s" (car elm)))))) leaf--value) ,@leaf--body)

     :config          `(,@leaf--value ,@leaf--body)
     ))
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

    ((memq leaf--key '(:load-path :commands :after :defvar))
     ;; Accept: 't, 'nil, symbol and list of these (and nested)
     ;; Return: symbol list.
     ;; Note  : 'nil is just ignored
     ;;         remove duplicate element
     (delete-dups (delq nil (leaf-flatten leaf--value))))

    ((memq leaf--key (cdr '(:dummy
                            :ensure :package
                            :hook :mode :interpreter :magic :magic-fallback :defun
                            :setq :pre-setq :setq-default :custom :custom-face
                            :pl-setq :pl-pre-setq :pl-setq-default :pl-custom)))
     ;; Accept: (sym . val), ((sym sym ...) . val), (sym sym ... . val)
     ;; Return: list of pair (sym . val)
     ;; Note  : atom ('t, 'nil, symbol) is just ignored
     ;;         remove duplicate configure
     (mapcar (lambda (elm)
               (cond
                ((leaf-pairp elm)
                 (if (eq t (car elm)) `(,leaf--name . ,(cdr elm)) elm))
                ((memq leaf--key '(:ensure :package))
                 (if (equal '(t) elm) `(,leaf--name . nil) `(,@elm . nil)))
                ((memq leaf--key '(:hook :mode :interpreter :magic :magic-fallback :defun))
                 `(,@elm . ,leaf--name))
                ((memq leaf--key '(:setq :pre-setq :setq-default :custom :custom-face))
                 elm)
                (t
                 elm)))
             (mapcan
              (lambda (elm) (leaf-normalize-list-in-list elm 'dotlistp))
              leaf--value)))

    ((memq leaf--key '(:bind :bind*))
     ;; Accept: `leaf-keys' accept form
     ;; Return: a pair like (leaf--value . (fn fn ...))
     (eval `(leaf-keys ,leaf--value ,leaf--name)))

    ((memq leaf--key '(:advice))
     ;; Accept: (:where symbol fn), ((:where symbol fn) (:where symbol fn) ...)
     ;; Return: (((advice) (advice) ...) (fn fn ...))
     ;; Note  : fn is also accept lambda form
     ;;         the arguments for `advice-add' and `:advice' are in different order.
     (let ((val) (fns))
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
     (let ((val) (fns))
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

    ((memq leaf--key (cdr '(:dummy
                            :disabled :if :when :unless
                            :doc :file :url :preface :init :config
                            :leaf-autoload :leaf-defer :leaf-protect)))
     leaf--value)

    (t
     leaf--value))
  "Normalize rule.")

(eval
 `(progn
    ,@(mapcar
       (lambda (elm)
         (let ((keyname (substring (symbol-name elm) 1)))
           `(defcustom ,(intern (format "leaf-expand-%s" keyname)) t
              ,(format "If nil, do not expand values for :%s." keyname)
              :type 'boolean
              :group 'leaf)))
       (leaf-plist-keys leaf-keywords))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Key management
;;

(defvar leaf-key-override-global-map (make-keymap)
  "The leaf-override-global-mode keymap.")

(define-minor-mode leaf-key-override-global-mode
  "A minor mode so that keymap settings override other modes."
  t "")

;; the keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((leaf-key-override-global-mode . ,leaf-key-override-global-map)))

(defvar leaf-key-bindlist nil
  "List of bindings performed by `leaf-key'.
Elements have the form ((KEY . [MAP]) CMD ORIGINAL-CMD)")

(defmacro leaf-key (key command &optional keymap)
  "Bind KEY to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'.  Or it may be a string to be interpreted as spelled-out
keystrokes.  See documentation of `edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap and not a quoted symbol.
For example:
  (leaf-key \"M-h\" #'some-interactive-function my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when a
key should be bound. It must return non-nil in such cases.  Emacs can
evaluate this form at any time that it does redisplay or operates on
menu data structures, so you should write it so it can safely be
called at any time.

You can also use [remap COMMAND] as KEY.
For example:
  (leaf-key [remap backward-sentence] 'sh-beginning-of-command)"
  (let* ((key*     (eval key))
         (command* (eval command))
         (keymap*  (eval keymap))
         (mmap     (or keymap* 'global-map))
         (vecp     (vectorp key*))
         (_mvec    (if (vectorp key*) key* (read-kbd-macro key*)))
         (mstr     (if (stringp key*) key* (key-description key*))))
    `(let* ((old (lookup-key ,mmap ,(if vecp key `(kbd ,key))))
            (value ,(list '\` `((,mstr . ,mmap) ,command*  ,',(and old (not (numberp old)) old)))))
       (push value leaf-key-bindlist)
       (define-key ,mmap ,(if vecp key `(kbd ,key)) ',command*))))

(defmacro leaf-key* (key command)
  "Similar to `leaf-key', but overrides any mode-specific bindings.
Bind COMMAND at KEY."
  `(leaf-key ,key ,command 'leaf-key-override-global-map))

(defmacro leaf-keys (bind &optional dryrun-name)
  "Bind multiple BIND for KEYMAP defined in PKG.
BIND is (KEY . COMMAND) or (KEY . nil) to unbind KEY.

OPTIONAL:
  BIND also accept below form.
    (:{{map}} :package {{pkg}} (KEY . COMMAND) (KEY . COMMAND))
  KEYMAP is quoted keymap name.
  PKG is quoted package name which define KEYMAP.
  (wrap `eval-after-load' PKG)

  If DRYRUN-NAME is non-nil, return list like
  (LEAF_KEYS-FORMS (FN FN ...))

  If omit :package of BIND, fill it in LEAF_KEYS-FORM.

NOTE: BIND can also accept list of these."
  (let ((pairp (lambda (x)
                 (condition-case _err
                     (and (listp x)
                          (or (stringp (eval (car x)))
                              (vectorp (eval (car x))))
                          (atom (cdr x)))
                   (error nil))))
        (recurfn) (forms) (bds) (fns))
    (setq recurfn
          (lambda (bind)
            (cond
             ((funcall pairp bind)
              (push `(leaf-key ,(car bind) #',(cdr bind)) forms)
              (push bind bds)
              (push (cdr bind) fns))
             ((and (listp (car bind))
                   (funcall pairp (car bind)))
              (mapcar (lambda (elm)
                        (if (funcall pairp elm)
                            (progn
                              (push `(leaf-key ,(car elm) #',(cdr elm)) forms)
                              (push elm bds)
                              (push (cdr elm) fns))
                          (funcall recurfn elm)))
                      bind))
             ((or (keywordp (car bind))
                  (symbolp (car bind)))
              (let* ((map (leaf-sym-or-keyword (car bind)))
                     (pkg (leaf-plist-get :package (cdr bind)))
                     (pkgs (if (atom pkg) `(,pkg) pkg))
                     (elmbind (if pkg (nthcdr 3 bind) (nthcdr 1 bind)))
                     (elmbinds (if (funcall pairp (car elmbind))
                                   elmbind (car elmbind)))
                     (form `(progn
                              ,@(mapcar
                                 (lambda (elm)
                                   (push (cdr elm) fns)
                                   `(leaf-key ,(car elm) #',(cdr elm) ',map))
                                 elmbinds))))
                (push (if pkg bind
                        `(,(intern (concat ":" (symbol-name map)))
                          :package ,dryrun-name
                          ,@elmbinds))
                      bds)
                (when pkg
                  (dolist (elmpkg pkgs)
                    (setq form `(eval-after-load ',elmpkg ',form))))
                (push form forms)))
             (t
              (mapcar (lambda (elm) (funcall recurfn elm)) bind)))))
    (funcall recurfn bind)
    (if dryrun-name `'(,(nreverse bds) ,(nreverse fns))
      (if (cdr forms) `(progn ,@(nreverse forms)) (car forms)))))

(defmacro leaf-keys* (bind)
  "Similar to `leaf-keys' but bind BIND to `leaf-key-override-global-map'.
BIND must not contain :{{map}}."
  (let ((binds (if (and (atom (car bind)) (atom (cdr bind)))
                   `(,bind) bind)))
    `(leaf-keys (:leaf-key-override-global-map ,@binds))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Handler
;;

(defun leaf-register-autoload (fn pkg)
  "Registry FN as autoload for PKG.
FN also accept list of FN."
  (mapc
   (lambda (elm)
     (let ((target `(,elm . ,(symbol-name pkg))))
       (when (and elm (not (member target leaf--autoload)))
         (push target leaf--autoload))))
   (if (listp fn) fn `(,fn))))

(defmacro leaf-handler-leaf-protect (name &rest body)
  "Meta handler for :leaf-no-erorr in NAME for BODY leaf block."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (display-warning 'leaf (format ,(format "Error in `%s' block.  Error msg: %%s" name)
                                     (error-message-string err))))))

(defmacro leaf-handler-package (name pkg _pin)
  "Handler ensure PKG via PIN in NAME leaf block."
  `(unless (package-installed-p ',pkg)
     (unless (assoc ',pkg package-archive-contents)
       (package-refresh-contents))
     (condition-case err
         (package-install ',pkg)
       (error
        (condition-case err
            (progn
              (package-refresh-contents)
              (package-install ',pkg))
          (error
           (signal 'error
                   (format
                    ,(format "In `%s' block, failed to :package of %s.  Error msg: %%s"
                             name pkg)
                    (error-message-string err)))))))))

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

;;;###autoload
(defun leaf-available-keywords ()
  (interactive)
  "Return current available `leaf' keywords list."
  (let ((ret (leaf-plist-keys leaf-keywords)))
    (if (called-interactively-p 'interactive)
        (message (prin1-to-string ret))
      ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  General list functions for leaf
;;

(defun leaf-append-defaults (plist)
  "Append leaf default values to PLIST."
  (append (and leaf-expand-minimally
               (funcall (if (fboundp 'mapcan) #'mapcan #'leaf-mapcaappend)
                        (lambda (elm) `(,elm nil))
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
               ((member `',(car (last lst 2)) `('quote ',backquote-backquote-symbol 'function))
                (last lst (setq butlast-n 2)))
               ((member `',(car (last lst 3)) `('lambda))
                (last lst (setq butlast-n 3))))))
        (funcall (if (fboundp 'mapcan) #'mapcan #'leaf-mapcaappend)
                 (lambda (elm)
                   (leaf-normalize-list-in-list elm t (or prov provval)))
                 (leaf-safe-butlast lst butlast-n)))))))

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
    (dolist (key (leaf-plist-keys leaf-keywords))
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
  (let ((retplist))
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
  (let ((retplist) (worklist) (rlist (reverse plist)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main macro
;;

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
           (leaf--body))
      ;; renew (normalize) leaf--value, save follow expansion in leaf--body
      (setq leaf--value (eval `(cond ,@leaf-normalize)))
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
  (let* ((leaf--autoload)
         (args* (leaf-sort-values-plist
                 (leaf-normalize-plist
                  (leaf-append-defaults args) 'merge 'eval))))
    `(prog1 ',name
       ,@(leaf-process-keywords name args* args*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Font lock
;;

(defconst leaf-warn-font-lock-keywords
  `((,(rx (group "leaf-" (or "warn" "error")))
     (1 font-lock-warning-face))))

(defconst leaf-font-lock-keywords
  '(("(\\(leaf\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode leaf-warn-font-lock-keywords)
(font-lock-add-keywords 'emacs-lisp-mode leaf-font-lock-keywords)

(provide 'leaf)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; leaf.el ends here
