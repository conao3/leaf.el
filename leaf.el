;;; leaf.el --- Symplify your init.el configuration       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp settings
;; Version: 2.4.4
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

(defcustom leaf-system-defaults '(:leaf-autoload t :leaf-defer t :leaf-no-error t)
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

(defcustom leaf-backend-ensure (if (require 'feather nil t) 'feather 'package)
  "Backend to process `:ensure' keyword."
  :type '(choice (const :tag "Use `package.el'." 'package)
                 (const :tag "Use `feather.el'." 'feather)
                 (const :tag "No backend, disable `:ensure'." nil))
  :group 'leaf)

(defcustom leaf-backend-bind (if (require 'leaf-key nil t) 'leaf-key 'bind-key)
  "Backend to process `:bind' keyword."
  :type '(choice (const :tag "Use `bind-key.el'." 'bind-key)
                 (const :tag "No backend, disable `:bind'." nil))
  :group 'leaf)

(defcustom leaf-options-ensure-default-pin nil
  "Option :ensure pin default.
'nil is using package manager default."
  :type 'sexp
  :group 'leaf)

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
     :disabled       (unless (eval (car leaf--value)) `(,@leaf--body))
     :leaf-no-error  (if (and leaf--body leaf--value) `((leaf-handler-leaf-no-error ,leaf--name ,@leaf--body)) `(,@leaf--body))
     :load-path      `(,@(mapcar (lambda (elm) `(add-to-list 'load-path ,elm)) leaf--value) ,@leaf--body)
     :leaf-autoload  `(,@(when (car leaf--value) (mapcar (lambda (elm) `(autoload #',(car elm) ,(cdr elm) nil t)) (nreverse leaf--autoload))) ,@leaf--body)

     :doc            `(,@leaf--body)
     :file           `(,@leaf--body)
     :url            `(,@leaf--body)

     :defun          `(,@(mapcar (lambda (elm) `(declare-function ,(car elm) ,(symbol-name (cdr elm)))) leaf--value) ,@leaf--body)
     :defvar         `(,@(mapcar (lambda (elm) `(defvar ,elm)) leaf--value) ,@leaf--body)

     :preface        `(,@leaf--value ,@leaf--body)
     :when           (when leaf--body `((when   ,@(if (= 1 (length leaf--value)) leaf--value `((and ,@leaf--value))) ,@leaf--body)))
     :unless         (when leaf--body `((unless ,@(if (= 1 (length leaf--value)) leaf--value `((and ,@leaf--value))) ,@leaf--body)))
     :if             (when leaf--body `((if     ,@(if (= 1 (length leaf--value)) leaf--value `((and ,@leaf--value))) (progn ,@leaf--body))))

     :ensure         `(,@(mapcar (lambda (elm) `(leaf-handler-ensure ,leaf--name ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)

     :after          (when leaf--body (let ((ret `(progn ,@leaf--body)))
                                        (dolist (elm leaf--value) (setq ret `(eval-after-load ',elm ',ret)))
                                        `(,ret)))

     :commands       (progn (mapc (lambda (elm) (leaf-register-autoload elm leaf--name)) leaf--value) `(,@leaf--body))
     :bind           (progn
                       (mapc (lambda (elm) (leaf-register-autoload (leaf-plist-get :func elm) leaf--name)) leaf--value)
                       `(,@(mapcar (lambda (elm) `(leaf-handler-bind ,leaf--name ',elm)) leaf--value) ,@leaf--body))
     :bind*          (progn
                       (mapc (lambda (elm) (leaf-register-autoload (leaf-plist-get :func elm) leaf--name)) leaf--value)
                       `(,@(mapcar (lambda (elm) `(leaf-handler-bind* ,leaf--name ',elm)) leaf--value) ,@leaf--body))

     :mode           (progn
                       (mapc (lambda (elm) (leaf-register-autoload (cdr elm) leaf--name)) leaf--value)
                       `(,@(mapcar (lambda (elm) `(add-to-list 'auto-mode-alist '(,(car elm) ,(cdr elm)))) leaf--value) ,@leaf--body))
     :interpreter    (progn
                       (mapc (lambda (elm) (leaf-register-autoload (cdr elm) leaf--name)) leaf--value)
                       `(,@(mapcar (lambda (elm) `(add-to-list 'interpreter-mode-alist '(,(car elm) ,(cdr elm)))) leaf--value) ,@leaf--body))
     :magic          (progn
                       (mapc (lambda (elm) (leaf-register-autoload (cdr elm) leaf--name)) leaf--value)
                       `(,@(mapcar (lambda (elm) `(add-to-list 'magic-mode-alist '(,(car elm) ,(cdr elm)))) leaf--value) ,@leaf--body))
     :magic-fallback (progn
                       (mapc (lambda (elm) (leaf-register-autoload (cdr elm) leaf--name)) leaf--value)
                       `(,@(mapcar (lambda (elm) `(add-to-list 'magic-fallback-mode-alist '(,(car elm) ,(cdr elm)))) leaf--value) ,@leaf--body))
     :hook           (progn
                       (mapc (lambda (elm) (leaf-register-autoload (cdr elm) leaf--name)) leaf--value)
                       `(,@(mapcar (lambda (elm) `(add-hook ',(car elm) #',(cdr elm))) leaf--value) ,@leaf--body))

     :leaf-defer     (if (and leaf--body leaf--value) `((eval-after-load ',leaf--name '(progn ,@leaf--body))) `(,@leaf--body))

     :custom         `((custom-set-variables ,@(mapcar (lambda (elm) `'(,(car elm) ,(cdr elm) ,(format "Customized with leaf in %s block" leaf--name))) leaf--value)) ,@leaf--body)
     :custom-face    `((custom-set-faces     ,@(mapcar (lambda (elm) `'(,(car elm) ,(car (cddr elm)))) leaf--value)) ,@leaf--body)
     
     :pre-setq       `(,@(mapcar (lambda (elm) `(setq ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
     :init           `(,@leaf--value ,@leaf--body)
     :require        `(,@(mapcar (lambda (elm) `(require ',elm)) leaf--value) ,@leaf--body)
     :setq           `(,@(mapcar (lambda (elm) `(setq ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
     :setq-default   `(,@(mapcar (lambda (elm) `(setq-default ,(car elm) ,(cdr elm))) leaf--value) ,@leaf--body)
     :config         `(,@leaf--value ,@leaf--body)
     ))
  "Special keywords and conversion rule to be processed by `leaf'.
Sort by `leaf-sort-leaf--values-plist' in this order.")

(defvar leaf-normarize
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

    ((memq leaf--key '(:leaf-defer))
     ;; Accept: 't, 'nil, symbol and list of these (and nested)
     ;; Return: 't, 'nil
     ;; Note  : 't is returned when specified `leaf-defer-keywords'.
     (and (delq nil (leaf-flatten leaf--value))
          (leaf-list-memq leaf-defer-keywords (leaf-plist-keys leaf--raw))))

    ((memq leaf--key '(:leaf-no-error))
     ;; Accept: 't, 'nil, symbol and list of these (and nested)
     ;; Return: 't, 'nil
     (leaf-truep (delq nil (leaf-flatten leaf--value))))

    ((memq leaf--key (cdr '(:dummy
                            :ensure
                            :hook :mode :interpreter :magic :magic-fallback :defun
                            :setq :pre-setq :setq-default :custom :custom-face)))
     ;; Accept: (sym . val), ((sym sym ...) . val), (sym sym ... . val)
     ;; Return: list of pair (sym . val)
     ;; Note  : atom ('t, 'nil, symbol) is just ignored
     ;;         remove duplicate configure
     (mapcar (lambda (elm)
               (cond
                ((leaf-pairp elm)
                 elm)
                ((memq leaf--key '(:ensure))
                 (if (eq t elm) `(,leaf--name . nil) `(,elm . nil)))
                ((memq leaf--key '(:hook :mode :interpreter :magic :magic-fallback :defun))
                 `(,elm . ,leaf--name))
                ((memq leaf--key '(:setq :pre-setq :setq-default :custom :custom-face))
                 elm)
                (t
                 elm)))
             (mapcan (lambda (elm)
                       (leaf-normalize-list-in-list elm 'dotlistp))
                     leaf--value)))

    ((memq leaf--key '(:bind :bind*))
     ;; Accept: list of pair (bind . func), (bind . nil)
     ;;         ([:{{hoge}}-map] [:package {{pkg}}](bind . func) (bind . func) ...)
     ;;         optional, [:{{hoge}}-map] [:package {{pkg}}]
     ;; Return: list of ([:{{hoge}}-map] [:package {{pkg}}] (bind . func))
     (mapcan (lambda (elm)
               (cond
                ((leaf-pairp elm 'allow-nil)
                 (list `(:package ,leaf--name :key ,(car elm) :func ,(cdr elm))))
                ((not (keywordp (car elm)))
                 (mapcar
                  (lambda (el) `(:package ,leaf--name :key ,(car el) :func ,(cdr el))) elm))
                (t
                 (delq nil
                       (mapcar (lambda (el)
                                 (when (leaf-pairp el 'allow-nil)
                                   (let ((map (intern (substring (symbol-name (car elm)) 1)))
                                         (pkg (leaf-plist-get :package (cdr elm))))
                                     (cdr `(:dummy
                                            :map ,map
                                            :package ,(if pkg pkg leaf--name)
                                            :key ,(car el)
                                            :func ,(cdr el))))))
                               (cdr elm))))))
             (mapcan (lambda (elm)
                       (if (or (and (listp elm) (keywordp (car elm)))
                               (and (listp elm) (atom (car elm)) (atom (cdr elm))))
                           (list elm)
                         elm))
                     leaf--value)))

    ((memq leaf--key '(:dummy :disabled :if :when :unless :doc :file :url :preface :init :config))
     leaf--value)

    (t
     leaf--value))
  "Normarize rule")

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

(defun leaf-process-keywords (name plist raw)
  "Process keywords for NAME.
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
      (setq leaf--value (eval `(cond ,@leaf-normarize)))
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

(defun leaf-register-autoload (fn pkg)
  "Registry FN as autoload for PKG."
  (let ((target `(,fn . ,(symbol-name pkg))))
    (when (and fn (not (member target leaf--autoload)))
      (setq leaf--autoload (cons target leaf--autoload)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support functions
;;

(defun leaf-warn (message &rest args)
  "Minor change from `warn' for `leaf'.
MESSAGE and ARGS are passed `format'."
  (display-warning 'leaf (apply #'format `(,message ,@args))))

(defun leaf-error (message &rest args)
  "Minor change from `error' for `leaf'.
MESSAGE and ARGS are passed `format'."
  (display-warning 'leaf (apply #'format `(,message ,@args)) :error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Key management
;;

(defvar leaf-key-override-global-map (make-keymap)
  "leaf-override-global-mode keymap")

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
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap and not a quoted symbol.
For example:
  (bind-key \"M-h\" #'some-interactive-function my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time.

You can also use [remap COMMAND] as KEY.
For example:
  (leaf-key [remap backward-sentence] 'sh-beginning-of-command)"
  (let* ((mmap (or (eval keymap) 'global-map))
         (vecp (vectorp key))
         (mvec (if (vectorp key) key (read-kbd-macro key)))
         (mstr (if (stringp key) key (key-description key))))
    `(let* ((old (lookup-key ,mmap ,(if vecp key `(kbd ,key))))
            (value ,(list '\` `((,mstr . ,mmap) ,(eval command) ,',(unless (numberp old) old)))))
       (push value leaf-key-bindlist)
       (define-key ,mmap ,(if vecp key `(kbd ,key)) ,command))))

(defmacro leaf-key* (key command)
  "Similar to `bind-key', but overrides any mode-specific bindings."
  `(leaf-key ,key ,command 'leaf-key-override-global-map))

(defmacro leaf-keys (&rest plist)
  "Bind multiple keys at once.

Accepts keyword arguments:
MUST:
  :bind (KEY . COMMAND) - bind KEY to COMMAND
        (KEY . nil)     - unbind KEY

OPTIONAL:
  :map MAP              - a keymap into which the keybind should be added
  :package PKG          - a package in which the MAP defined in
                          (wrap `eval-after-load' PKG)

NOTE: :package, :bind can accept list of these.
  :package (PKG ... PKG)
  :bind ((KEY . COMMAND) ... (KEY . COMMAND))"
  (let ((mmap  (leaf-plist-get :map plist))
        (mpkg  (leaf-plist-get :package plist))
        (mbind (leaf-plist-get :bind plist))
        (mform))
    (unless mbind (leaf-error "leaf-keys need :bind argument.  arg: %s" plist))
    (when (atom mpkg) (setq mpkg `(,mpkg)))
    (when (leaf-pairp mbind 'allow-nil) (setq mbind `(,mbind)))
    (setq mform `(progn
                  ,@(mapcar
                     (lambda (elm) `(leaf-key ,(car elm) ',(cdr elm) ',mmap))
                     mbind)))
    (if (equal '(nil) mpkg)
        mform
      (dolist (pkg mpkg) (setq mform `(eval-after-load ',pkg ',mform)))
      mform)))

(defmacro leaf-keys* (&rest plist)
  (when (leaf-plist-get :map plist)
    (leaf-error "leaf-keys* should not call with :map argument.  arg: %s" plist))
  `(leaf-key :map 'leaf-key-override-global-map ,@plist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Handler
;;

(defmacro leaf-handler-leaf-no-error (name &rest body)
  "Meta handler for :leaf-no-erorr in NAME leaf block."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (leaf-error ,(format "Error in `%s' block.  Error msg: %%s" name)
                  (error-message-string err)))))

(defmacro leaf-handler-ensure (name pkg _pin)
  "Meta handler for PKG from PIN in NAME leaf block."
  (cond
   ((eq leaf-backend-ensure 'package)
    `(unless
         (package-installed-p ',pkg)
       (condition-case err
           (progn
             (unless (assoc 'leaf package-archive-contents)
               (package-refresh-contents))
             (package-install ',pkg))
         (error
          (condition-case err
              (progn
                (package-refresh-contents)
                (package-install ',pkg))
            (error
             (leaf-error
              ,(format "In `%s' block, failed to :ensure of %s.  Error msg: %%s"
                       name pkg)
              (error-message-string err))))))))))

(defmacro leaf-handler-bind (_name elm)
  "Meta handler for NAME with ELM."
  (declare (indent 1))
  (cond
   ((eq leaf-backend-bind 'bind-key)
    (let* ((elm* (eval elm))
           (map (leaf-plist-get :map elm*))
           (pkg (leaf-plist-get :package elm*))
           (key (leaf-plist-get :key elm*))
           (fn  (leaf-plist-get :func elm*)))
      `(bind-keys ,@(when map `(:map ,map)) :package ,pkg (,key . ,fn))))))

(defmacro leaf-handler-bind* (_name elm)
  "Meta handler for NAME with ELM."
  (declare (indent 1))
  (cond
   ((eq leaf-backend-bind 'bind-key)
    (let* ((elm* (eval elm))
           (map (leaf-plist-get :map elm*))
           (pkg (leaf-plist-get :package elm*))
           (key (leaf-plist-get :key elm*))
           (fn  (leaf-plist-get :func elm*)))
      `(bind-keys* ,@(when map `(:map ,map)) :package ,pkg (,key . ,fn))))))

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
  (append plist leaf-defaults leaf-system-defaults))

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

(defun leaf-normalize-list-in-list (lst &optional dotlistp)
  "Return normarized list from LST.
Example:
  - when dotlistp is nil
  a       => (a)
  (a b c) => (a b c)

  - when dotlistp is t
  a                 => (a)
  (a b c)           => (a b c)
  (a . b)           => ((a . b))
  ((a . b) (c . d)) => ((a . b) (c . d))
  (a . nil)         => ((a . nil))
  (a)               => ((a . nil))
  ((a) (b) (c))     => ((a) (b) (c))"
  (if (or (atom lst)
          (and dotlistp
               (leaf-pairp lst dotlistp)
               (not (leaf-pairp (car lst) dotlistp))))
      (list lst)
    lst))

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
;;; leaf.el ends here
