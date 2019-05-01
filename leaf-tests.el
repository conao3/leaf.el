;;; leaf-tests.el ---                                -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'leaf)
(require 'cort)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test settings
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support legacy Emacs
;;

(when (not (fboundp 'autoload-do-load))
  (defun autoload-do-load (fundef &optional funname macro-only)
    (if (or (not (consp fundef)) (not (eql 'autoload (car fundef))))
        fundef
      (let ((kind (nth 4 fundef)))
        (if (and (eql macro-only 'macro)
                 (not (or (eql kind t)
                          (eql kind 'macro))))
            fundef)
        (if purify-flag
            (error "Attempt to autoload %s while preparing to dump" (symbol-name funnname)))
        (unwind-protect
            (let ((ignore-errors (if (or (eql kind t) (eql kind 'macro)) nil macro_only)))
              (load (cadr fundef) ignore-errors t nil t))
          ;; FIXME: revert partially performed defuns
          ())
        (if (or (not funname) ignore-errors)
            nil
          (let ((fun (indirect-function funname, nil)))
            (if (equal fun fundef)
                (error "Autoloading file %s failed to define function %s"
                       (caar load-history)
                       (symbol-name funname))
              fun)))))))

(when (not (fboundp 'macroexpand-1))
  (defun macroexpand-1 (form &optional environment)
    "Perform (at most) one step of macroexpansion."
    (cond
     ((consp form)
      (let* ((head (car form))
             (env-expander (assq head environment)))
        (if env-expander
            (if (cdr env-expander)
                (apply (cdr env-expander) (cdr form))
              form)
          (if (not (and (symbolp head) (fboundp head)))
              form
            (let ((def (autoload-do-load (symbol-function head) head 'macro)))
              (cond
               ;; Follow alias, but only for macros, otherwise we may end up
               ;; skipping an important compiler-macro (e.g. cl--block-wrapper).
               ((and (symbolp def) (macrop def)) (cons def (cdr form)))
               ((not (consp def)) form)
               (t
                (if (eq 'macro (car def))
                    (apply (cdr def) (cdr form))
                  form))))))))
     (t form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support macros for test definition
;;

(defmacro match-expansion (form expect)
  `(:equal (macroexpand-1 ',form) ,expect))

(defmacro match-expansion-let (letform form expect)
  (declare (indent 1))
  `(:equal (let ,letform (macroexpand-1 ',form)) ,expect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  sumple adding keyword(s)
;;

(leaf-add-keyword-before :message-pre-require :require)
(defun leaf-handler/:message-pre-require (name value rest)
  "process :message-pre-require."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(mapcar (lambda (x) `(message ,x)) value) ,@body)))

(leaf-add-keyword-after :message-post-require :require)
(defun leaf-handler/:message-post-require (name value rest)
  "process :message-post-require."
  (let ((body (leaf-process-keywords name rest)))
    `(,@(mapcar (lambda (x) `(message ,x)) value) ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Sumple functions
;;

(defun rt ()
  "test function every returns `t'."
  t)

(defun rnil ()
  "test function every returns `nil'."
  nil)

(defmacro mt ()
  "test macro every returns `rt'."
  `(rt))

(defmacro mnil ()
  "test macro every returns `rnil'"
  `(rnil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test definition
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  utility functions
;;

(cort-deftest leaf-test:/simple-format
  (:string= (leaf-to-string
             '(leaf real-auto-save
                :ensure t
                :custom ((real-auto-save-interval . 0.3))
                :commands real-auto-save-mode
                :hook (find-file-hook . real-auto-save-mode)))
            "(leaf real-auto-save
  :ensure t
  :custom ((real-auto-save-interval . 0.3))
  :commands real-auto-save-mode
  :hook (find-file-hook . real-auto-save-mode))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  simple test
;;

(cort-deftest leaf-test:/simple-none
  (match-expansion
   (leaf foo)
   'nil))

(cort-deftest leaf-test:/simple-disabled-t
  (match-expansion
   (leaf foo :disabled t)
   'nil))

(cort-deftest leaf-test:/simple-disabled-nil
  (match-expansion
   (leaf foo :disabled nil)
   'nil))

(cort-deftest leaf-test:/simple-if
  (match-expansion
   (leaf foo :if t)
   'nil))

(cort-deftest leaf-test/:simple-when
  (match-expansion
   (leaf foo :when t)
   'nil))

(cort-deftest leaf-test/:simple-unless
  (match-expansion
   (leaf foo :unless t)
   'nil))

(cort-deftest leaf-test/:simple-multi-if
  (match-expansion
   (leaf foo :if (rt) :if (rnil) (mt))
   'nil))

(cort-deftest leaf-test/:simple-multi-conds
  (match-expansion
   (leaf foo :if (rt) :when (rnil) (mt) :unless (rt) :if (rnil))
   'nil))

(cort-deftest leaf-test/:simple-init
  (match-expansion
   (leaf foo
     :init
     (setq bar1 'baz)
     (setq bar2 'baz))
   '(progn
      (setq bar1 'baz)
      (setq bar2 'baz))))

(cort-deftest leaf-test/:simple-init-config
  (match-expansion
   (leaf foo :require foo-hoge foo-piyo
         :init
         (setq bar1 'baz)
         (setq bar2 'baz)
         :config
         (setq bar3 'baz)
         (setq bar4 'baz))
   '(progn
      (setq bar1 'baz)
      (setq bar2 'baz)
      (require 'foo-hoge)
      (require 'foo-piyo)
      (setq bar3 'baz)
      (setq bar4 'baz))))

(cort-deftest leaf-test/:simple-config
  (match-expansion
   (leaf foo :config (setq bar 'baz))
   '(progn
      (setq bar 'baz))))

(cort-deftest leaf-test/:simple-require
  (match-expansion
   (leaf foo
     :require t
     :config (setq bar 'baz))
   '(progn
      (require 'foo)
      (setq bar 'baz))))

(cort-deftest leaf-test/:simple-require-nil
  (match-expansion
   (leaf foo
     :require nil
     :config (setq bar 'baz))
   '(progn
      (setq bar 'baz))))

(cort-deftest leaf-test/:simple-multi-require
  (match-expansion
   (leaf foo
     :require foo-hoge foo-piyo
     :config (setq bar 'baz))
   '(progn
      (require 'foo-hoge)
      (require 'foo-piyo)
      (setq bar 'baz))))

(cort-deftest leaf-test/:simple-keyword-add
  (match-expansion
   (leaf foo
     :require h s :message-post-require "foo!" :config (setq bar 'baz))
   '(progn
      (require 'h)
      (require 's)
      (message "foo!")
      (setq bar 'baz))))

(cort-deftest leaf-test/:simple-keyword-add-2
  (match-expansion
   (leaf foo
     :require h s
     :message-post-require "foo!"
     :config (setq bar 'baz)
     :message-post-require "post!"
     :message-pre-require "pre")
   '(progn
      (message "pre")
      (require 'h)
      (require 's)
      (message "foo!")
      (message "post!")
      (setq bar 'baz))))

(cort-deftest leaf-test/:simple-doc-keyword
  (match-expansion
   (leaf foo
     :doc "this package is awesome!!"
     :require nil
     :config (setq bar 'baz))
   '(progn
      (setq bar 'baz))))

(cort-deftest leaf-test/:simple-doc-keywords
  (match-expansion
   (leaf foo
     :doc "this package is awesome!!"
     :file "~/path/to/package/file.el"
     :url "https://www.example.com/"
     :require nil
     :config (setq bar 'baz))
   '(progn
      (setq bar 'baz))))

(cort-deftest leaf-test/:simple-bind
  (match-expansion
   (leaf foo
     :bind (("M-s O" . moccur)
            :map isearch-mode-map
            ("M-o" . isearch-moccur)
            ("M-O" . isearch-moccur-all))
     :init
     (setq isearch-lazy-highlight t)
     :config
     (leaf moccur-edit))
   '(progn
      (setq isearch-lazy-highlight t)
      (leaf-meta-backend/:bind 'foo
			       '((("M-s O" . moccur)
			          :map isearch-mode-map
			          ("M-o" . isearch-moccur)
			          ("M-O" . isearch-moccur-all))))
      (leaf moccur-edit))))

(cort-deftest leaf-test/:simple-pre-setq
  (match-expansion
   (leaf foo
     :pre-setq ((bar . 'baz))
     :init (foo-pre-init)
     :config (foo-post-init))
   '(progn
      (setq bar 'baz)
      (foo-pre-init)
      (foo-post-init))))

(cort-deftest leaf-test/:simple-post-setq
  (match-expansion
   (leaf foo
     :setq ((bar . 'baz))
     :init (foo-pre-init)
     :config (foo-post-init))
   '(progn
      (foo-pre-init)
      (setq bar 'baz)
      (foo-post-init))))

(cort-deftest leaf-test/:simple-custom-set-variables
  (match-expansion
   (leaf foo
     :custom ((bar . 'baz))
     :init (foo-pre-init)
     :config (foo-post-init))
   '(progn
      (foo-pre-init)
      (custom-set-variables
       '(bar 'baz))
      (foo-post-init))))

(cort-deftest leaf-test/:simple-mode
  (match-expansion
   (leaf ruby-mode
     :mode "\\.rb\\'"
     :interpreter "ruby")
   '(progn
      (autoload #'ruby-mode "ruby-mode" nil t)
      (leaf-list-add-to-list 'auto-mode-alist
                             '(("\\.rb\\'" . ruby-mode)))
      (autoload #'ruby-mode "ruby-mode" nil t)
      (leaf-list-add-to-list 'interpreter-mode-alist
                             '(("ruby" . ruby-mode))))))

(cort-deftest leaf-test/:simple-multi-mode
  (match-expansion
   (leaf ruby-mode
     :mode "\\.rb\\'" "\\.rb2\\'" ("\\.rbg\\'" . rb-mode)
     :interpreter "ruby")
   '(progn
      (autoload #'ruby-mode "ruby-mode" nil t)
      (autoload #'rb-mode "ruby-mode" nil t)
      (leaf-list-add-to-list 'auto-mode-alist
                             '(("\\.rb\\'" . ruby-mode)
                               ("\\.rb2\\'" . ruby-mode)
                               ("\\.rbg\\'" . rb-mode)))
      (autoload #'ruby-mode "ruby-mode" nil t)
      (leaf-list-add-to-list 'interpreter-mode-alist
                             '(("ruby" . ruby-mode))))))

(cort-deftest leaf-test/:simple-magic
  (match-expansion
   (leaf pdf-tools
     :magic ("%PDF" . pdf-view-mode)
     :config
     (pdf-tools-install))
   '(progn
      (autoload #'pdf-tools "pdf-tools" nil t)
      (autoload #'pdf-view-mode "pdf-tools" nil t)
      (leaf-list-add-to-list 'magic-mode-alist
                             '(("%PDF" . pdf-view-mode)))
      (pdf-tools-install))))

(cort-deftest leaf-test/:simple-magic-fallback
  (match-expansion
   (leaf pdf-tools
     :magic-fallback ("%PDF" . pdf-view-mode)
     :config
     (pdf-tools-install))
   '(progn
      (autoload #'pdf-tools "pdf-tools" nil t)
      (autoload #'pdf-view-mode "pdf-tools" nil t)
      (leaf-list-add-to-list 'magic-fallback-mode-alist
                             '(("%PDF" . pdf-view-mode)))
      (pdf-tools-install))))

(cort-deftest leaf-test/:simple-hook
  (match-expansion
   (leaf ace-jump-mode
     :hook cc-mode-hook)
   '(progn
      (autoload #'ace-jump-mode "ace-jump-mode" nil t)
      (add-hook 'cc-mode-hook #'ace-jump-mode))))

(cort-deftest leaf-test/:simple-multi-hook
  (match-expansion
   (leaf ace-jump-mode
     :hook cc-mode-hook (prog-mode-hook . ace-jump-mode))
   '(progn
      (autoload #'ace-jump-mode "ace-jump-mode" nil t)
      (add-hook 'cc-mode-hook #'ace-jump-mode)
      (add-hook 'prog-mode-hook #'ace-jump-mode))))

(cort-deftest leaf-test/:simple-commands
  (match-expansion
   (leaf ace-jump-mode
     :commands ace-jump-mode)
   '(progn
      (add-hook #'ace-jump-mode "ace-jump-mode" nil t))))

(cort-deftest leaf-test/:simple-multi-commands
  (match-expansion
   (leaf ace-jump-mode
     :commands ace-jump-mode command1 command2)
   '(progn
      (add-hook #'ace-jump-mode "ace-jump-mode" nil t)
      (add-hook #'command1 "ace-jump-mode" nil t)
      (add-hook #'command2 "ace-jump-mode" nil t))))

(cort-deftest leaf-test/:simple-custom-face
  (match-expansion
   (leaf eruby-mode
     :custom-face
     (eruby-standard-face ((t (:slant italic)))))
   '(progn
      (custom-set-faces
       '(eruby-standard-face
         ((t
           (:slant italic))))))))

(cort-deftest leaf-test/:simple-multi-custom-face
  (match-expansion
   (leaf eruby-mode
     :custom-face
     (eruby-standard-face ((t (:slant italic))))
     (eruby-standard-face2 ((t (:slant italic)))))
   '(progn
      (custom-set-faces
       '(eruby-standard-face
         ((t
           (:slant italic)))))
      (custom-set-faces
       '(eruby-standard-face2
         ((t
           (:slant italic))))))))

(cort-deftest leaf-test/:simple-byte-compile-vars
  (match-expansion
   (leaf for
     :byte-compile-vars for-var1)
   '(progn
      (eval-when-compile
        (defvar for-var1)))))

(cort-deftest leaf-test/:simple-multi-byte-compile-vars
  (match-expansion
   (leaf for
     :byte-compile-vars for-var1 for-var2)
   '(progn
      (eval-when-compile
        (defvar for-var1)
        (defvar for-var2)))))

(cort-deftest leaf-test/:simple-byte-compile-funcs
  (match-expansion
   (leaf for
     :byte-compile-funcs ((hoge-fn1 . hoge)))
   '(progn
      (eval-when-compile
        (autoload #'hoge-fn1 "hoge" nil t)))))

(cort-deftest leaf-test/:simple-multi-byte-compile-funcs
  (match-expansion
   (leaf for
     :byte-compile-funcs ((hoge-fn1 . hoge)
                          (hoge-fn2 . hoge)))
   '(progn
      (eval-when-compile
        (autoload #'hoge-fn1 "hoge" nil t)
        (autoload #'hoge-fn2 "hoge" nil t)))))

(cort-deftest leaf-test/:simple-ensure
  (match-expansion
   (leaf foo :ensure t)
   '(progn
      (leaf-meta-backend/:ensure 'foo '(t)))))

(cort-deftest leaf-test/:simple-defaults
  (match-expansion
   (leaf foo :ensure t :defaults t)
   '(progn
      (leaf-meta-backend/:ensure 'foo '(t))
      (feather-install-defaults 'foo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  :disabled keyword
;;

;; simple :disabled patterns, Finaly `t' will be adopted.
(cort-deftest leaf-test:/disabled-1-
  (match-expansion
   (leaf foo :disabled t)
   'nil))

(cort-deftest leaf-test:/disabled-2-
  (match-expansion
   (leaf foo :disabled t :config (message "bar"))
   'nil))

(cort-deftest laef-test:/disabled-3-
  (match-expansion
   (leaf foo :disabled t :init (message "bar") :config (message "baz"))
   'nil))

(cort-deftest leaf-test:/disabled-4-
  (match-expansion
   (leaf foo :disabled t nil :config (message "bar"))
   'nil))

(cort-deftest leaf-test:/disabled-5-
  (match-expansion
   (leaf foo :disabled t nil :config (message "bar") :disabled t)
   'nil))

(cort-deftest leaf-test:/disabled-6-
  (match-expansion
   (leaf foo :disabled t nil nil :config (message "bar"))
   'nil))

;; simple :disabled patterns, Finaly `nil' will be adopted.
(cort-deftest leaf-test:/disabled-1+
  (match-expansion
   (leaf foo :disabled nil)
   'nil))

(cort-deftest leaf-test:/disabled-2+
  (match-expansion
   (leaf foo :disabled nil :config (message "bar"))
   '(progn
      (message "bar"))))

(cort-deftest laef-test:/disabled-3+
  (match-expansion
   (leaf foo :disabled nil :init (message "bar") :config (message "baz"))
   '(progn
      (message "bar")
      (message "baz"))))

(cort-deftest leaf-test:/disabled-4+
  (match-expansion
   (leaf foo :disabled nil t :config (message "bar"))
   '(progn
      (message "bar"))))

(cort-deftest leaf-test:/disabled-5+
  (match-expansion
   (leaf foo :disabled nil t :config (message "bar") :disabled t)
   '(progn
      (message "bar"))))

(cort-deftest leaf-test:/disabled-6+
  (match-expansion
   (leaf foo :disabled nil :disabled t nil nil :config (message "bar"))
   '(progn
      (message "bar"))))

;; :disabled with boolean reterns funciton patterns, Finaly `t' will be adopted.
(cort-deftest leaf-test:/disabled-1--
  (match-expansion
   (leaf foo :disabled (rt))
   'nil))

(cort-deftest leaf-test:/disabled-2--
  (match-expansion
   (leaf foo :disabled (rt) :config (message "bar"))
   'nil))

(cort-deftest laef-test:/disabled-3--
  (match-expansion
   (leaf foo :disabled (rt) :init (message "bar") :config (message "baz"))
   'nil))

(cort-deftest leaf-test:/disabled-4--
  (match-expansion
   (leaf foo :disabled (rt) (rnil) :config (message "bar"))
   'nil))

(cort-deftest leaf-test:/disabled-5--
  (match-expansion
   (leaf foo :disabled (rt) (rnil) :config (message "bar") :disabled (rt))
   'nil))

(cort-deftest leaf-test:/disabled-6--
  (match-expansion
   (leaf foo :disabled (rt) (rnil) (rnil) :config (message "bar"))
   'nil))

;; :disabled with boolean returns funciton patterns, Finaly `nil' will be adopted.
(cort-deftest leaf-test:/disabled-1++
  (match-expansion
   (leaf foo :disabled (rnil))
   'nil))

(cort-deftest leaf-test:/disabled-2++
  (match-expansion
   (leaf foo :disabled (rnil) :config (message "bar"))
   '(progn
      (message "bar"))))

(cort-deftest laef-test:/disabled-3++
  (match-expansion
   (leaf foo :disabled (rnil) :init (message "bar") :config (message "baz"))
   '(progn
      (message "bar")
      (message "baz"))))

(cort-deftest leaf-test:/disabled-4++
  (match-expansion
   (leaf foo :disabled (rnil) (rt) :config (message "bar"))
   '(progn
      (message "bar"))))

(cort-deftest leaf-test:/disabled-5++
  (match-expansion
   (leaf foo :disabled (rnil) (rt) :config (message "bar") :disabled (rt))
   '(progn
      (message "bar"))))

(cort-deftest leaf-test:/disabled-6++
  (match-expansion
   (leaf foo :disabled (rnil) :disabled (rt) (rnil) (rnil) :config (message "bar"))
   '(progn
      (message "bar"))))

;; :disabled with boolean reterns funciton patterns, Finaly `t' will be adopted.
(cort-deftest leaf-test:/disabled-1---
  (match-expansion
   (leaf foo :disabled (mt))
   'nil))

(cort-deftest leaf-test:/disabled-2---
  (match-expansion
   (leaf foo :disabled (mt) :config (message "bar"))
   'nil))

(cort-deftest laef-test:/disabled-3---
  (match-expansion
   (leaf foo :disabled (mt) :init (message "bar") :config (message "baz"))
   'nil))

(cort-deftest leaf-test:/disabled-4---
  (match-expansion
   (leaf foo :disabled (mt) (mnil) :config (message "bar"))
   'nil))

(cort-deftest leaf-test:/disabled-5---
  (match-expansion
   (leaf foo :disabled (mt) (mnil) :config (message "bar") :disabled (mt))
   'nil))

(cort-deftest leaf-test:/disabled-6---
  (match-expansion
   (leaf foo :disabled (mt) (mnil) (mnil) :config (message "bar"))
   'nil))

;; :disabled with boolean returns funciton patterns, Finaly `nil' will be adopted.
(cort-deftest leaf-test:/disabled-1+++
  (match-expansion
   (leaf foo :disabled (mnil))
   'nil))

(cort-deftest leaf-test:/disabled-2+++
  (match-expansion
   (leaf foo :disabled (mnil) :config (message "bar"))
   '(progn
      (message "bar"))))

(cort-deftest laef-test:/disabled-3+++
  (match-expansion
   (leaf foo :disabled (mnil) :init (message "bar") :config (message "baz"))
   '(progn
      (message "bar")
      (message "baz"))))

(cort-deftest leaf-test:/disabled-4+++
  (match-expansion
   (leaf foo :disabled (mnil) (mt) :config (message "bar"))
   '(progn
      (message "bar"))))

(cort-deftest leaf-test:/disabled-5+++
  (match-expansion
   (leaf foo :disabled (mnil) (mt) :config (message "bar") :disabled (mt))
   '(progn
      (message "bar"))))

(cort-deftest leaf-test:/disabled-6+++
  (match-expansion
   (leaf foo :disabled (mnil) :disabled (mt) (mnil) (mnil) :config (message "bar"))
   '(progn
      (message "bar"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  conditions
;;

(cort-deftest leaf-test/:if-1
  (match-expansion
   (leaf foo :if t)
   'nil))

(cort-deftest leaf-test/:if-2
  (match-expansion
   (leaf foo :if (or (rt) (rnil)))
   'nil))

(cort-deftest leaf-test/:if-3
  (match-expansion
   (leaf foo :if nil)
   'nil))

(cort-deftest leaf-test/:when-1
  (match-expansion
   (leaf foo :when t)
   'nil))

(cort-deftest leaf-test/:when-2
  (match-expansion
   (leaf foo :when (or (rt) (rnil)))
   'nil))

(cort-deftest leaf-test/:when-3
  (match-expansion
   (leaf foo :when nil)
   'nil))

(cort-deftest leaf-test/:unless-1
  (match-expansion
   (leaf foo :unless t)
   'nil))

(cort-deftest leaf-test/:unless-2
  (match-expansion
   (leaf foo :unless (or (rt) (rnil)))
   'nil))

(cort-deftest leaf-test/:unless-3
  (match-expansion
   (leaf foo :unless nil)
   'nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  :require keyword
;;

(cort-deftest leaf-test/:require-0
  (match-expansion
   (leaf foo)
   'nil))

(cort-deftest leaf-test/:require-1
  (match-expansion
   (leaf foo :require t)
   '(progn
      (require 'foo))))

(cort-deftest leaf-test/:require-2
  (match-expansion
   (leaf foo :require nil)
   'nil))

(cort-deftest leaf-test/:require-3
  (match-expansion
   (leaf foo :require bar baz)
   '(progn
      (require 'bar)
      (require 'baz))))

(cort-deftest leaf-test/:require-4
  (match-expansion
   (leaf foo :require bar baz :if t)
   '(progn
      (if t
          (progn
	    (require 'bar)
	    (require 'baz))))))

(provide 'leaf-tests)
;;; leaf-tests.el ends here
