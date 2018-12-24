;;; leaf-tests.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
;; Keywords: .emacs

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

(leaf-add-keyword-list-after '(:tmp-pre :tmp-post) :config)
(defun leaf-handler/:tmp-pre (name value rest)
  "process :tmp-pre."
  (let ((body (leaf-process-keywords name rest)))
    `(,@value ,@body)))

(defun leaf-handler/:tmp-post (name value rest)
  "process :tmp-post."
  (let ((body (leaf-process-keywords name rest)))
    `(,@value ,@body)))

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
;;  simple test
;;

(cort-deftest leaf-test:/simple-none
  (match-expansion
   (leaf foo)
   '(progn
      (require 'foo))))

(cort-deftest leaf-test:/simple-disabled-t
  (match-expansion
   (leaf foo :disabled t)
   'nil))

(cort-deftest leaf-test:/simple-disabled-nil
  (match-expansion
   (leaf foo :disabled nil)
   '(progn
      (require 'foo))))

(cort-deftest leaf-test:/simple-if
  (match-expansion
   (leaf foo :if t)
   '(if t
        (progn
          (progn
            (require 'foo))))))

(cort-deftest leaf-test/:simple-when
  (match-expansion
   (leaf foo :when t)
   '(when t
       (progn
         (require 'foo)))))

(cort-deftest leaf-test/:simple-unless
  (match-expansion
   (leaf foo :unless t)
   '(unless t
       (progn
         (require 'foo)))))

(cort-deftest leaf-test/:simple-multi-if
  (match-expansion
   (leaf foo :if (rt) :if (rnil) (mt))
   '(if (and (rt) (rnil) (mt))
        (progn
          (progn
            (require 'foo))))))

(cort-deftest leaf-test/:simple-multi-conds
  (match-expansion
   (leaf foo :if (rt) :when (rnil) (mt) :unless (rt) :if (rnil))
   '(if (and (rt) (rnil))
        (progn
          (when (and (rnil) (mt))
            (unless (rt)
              (progn
                (require 'foo))))))))

(cort-deftest leaf-test/:simple-init
  (match-expansion
   (leaf foo
         :init
         (setq bar1 'baz)
         (setq bar2 'baz))
   '(progn
      (progn
        (setq bar1 'baz)
        (setq bar2 'baz))
      (progn
        (require 'foo)))))

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
      (progn
        (setq bar1 'baz)
        (setq bar2 'baz))
      (progn
        (require foo-hoge)
        (require foo-piyo)
        (setq bar3 'baz)
        (setq bar4 'baz)))))

(cort-deftest leaf-test/:simple-config
  (match-expansion
   (leaf foo :config (setq bar 'baz))
   '(progn
      (require 'foo)
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
      (require foo-hoge)
      (require foo-piyo)
      (setq bar 'baz))))

(cort-deftest leaf-test/:simple-keyword-add
  (match-expansion
   (leaf foo
     :require h s :message-post-require "foo!" :config (setq bar 'baz))
   '(progn
      (require h)
      (require s)
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
      (require h)
      (require s)
      (message "foo!")
      (message "post!")
      (setq bar 'baz))))

(cort-deftest leaf-test/:simple-keyword-list-add
  (match-expansion
   (leaf foo
     :require h s
     :tmp-pre (message "start tmp")
     :tmp-post (setq foo 'bar)
     :tmp-pre (message "really start tmp!"))
   '(progn
      (require h)
      (require s)
      (message "start tmp")
      (message "really start tmp!")
      (setq foo 'bar))))

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
  (match-expansion-let ((leaf-backend/:bind 'bind-key))
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
       (progn
         (setq isearch-lazy-highlight t))
       (progn
         (require 'foo)
         (funcall #'leaf-backend/:bind-bind-key 'foo
                  '(("M-s O" . moccur)
                    :map isearch-mode-map
                    ("M-o" . isearch-moccur)
                    ("M-O" . isearch-moccur-all)))
         (leaf moccur-edit)))))

(cort-deftest leaf-test/:simple-pre-setq
  (match-expansion
   (leaf foo
     :pre-setq ((bar . 'baz))
     :init (foo-pre-init)
     :config (foo-post-init))
   '(progn
      (setq bar 'baz)
      (progn
        (progn
          (foo-pre-init))
        (progn
          (require 'foo)
          (foo-post-init))))))

(cort-deftest leaf-test/:simple-post-setq
  (match-expansion
   (leaf foo
     :setq ((bar . 'baz))
     :init (foo-pre-init)
     :config (foo-post-init))
   '(progn
      (progn
        (foo-pre-init))
      (progn
        (require 'foo)
        (setq bar 'baz)
        (foo-post-init)))))

(cort-deftest leaf-test/:simple-custom-set-variables
  (match-expansion
   (leaf foo
     :custom ((bar . 'baz))
     :init (foo-pre-init)
     :config (foo-post-init))
   '(progn
      (progn
        (foo-pre-init))
      (progn
        (require 'foo)
        (custom-set-variables '(bar 'baz))
        (foo-post-init)))))

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
   '(progn
      (require 'foo))))

(cort-deftest leaf-test:/disabled-2+
  (match-expansion
   (leaf foo :disabled nil :config (message "bar"))
   '(progn
      (require 'foo)
      (message "bar"))))

(cort-deftest laef-test:/disabled-3+
  (match-expansion
   (leaf foo :disabled nil :init (message "bar") :config (message "baz"))
   '(progn
      (progn
        (message "bar"))
      (progn
        (require 'foo)
        (message "baz")))))

(cort-deftest leaf-test:/disabled-4+
  (match-expansion
   (leaf foo :disabled nil t :config (message "bar"))
   '(progn
      (require 'foo)
      (message "bar"))))

(cort-deftest leaf-test:/disabled-5+
  (match-expansion
   (leaf foo :disabled nil t :config (message "bar") :disabled t)
   '(progn
      (require 'foo)
      (message "bar"))))

(cort-deftest leaf-test:/disabled-6+
  (match-expansion
   (leaf foo :disabled nil :disabled t nil nil :config (message "bar"))
   '(progn
      (require 'foo)
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
   '(progn
      (require 'foo))))

(cort-deftest leaf-test:/disabled-2++
  (match-expansion
   (leaf foo :disabled (rnil) :config (message "bar"))
   '(progn
      (require 'foo)
      (message "bar"))))

(cort-deftest laef-test:/disabled-3++
  (match-expansion
   (leaf foo :disabled (rnil) :init (message "bar") :config (message "baz"))
   '(progn
      (progn
        (message "bar"))
      (progn
        (require 'foo)
        (message "baz")))))

(cort-deftest leaf-test:/disabled-4++
  (match-expansion
   (leaf foo :disabled (rnil) (rt) :config (message "bar"))
   '(progn
      (require 'foo)
      (message "bar"))))

(cort-deftest leaf-test:/disabled-5++
  (match-expansion
   (leaf foo :disabled (rnil) (rt) :config (message "bar") :disabled (rt))
   '(progn
      (require 'foo)
      (message "bar"))))

(cort-deftest leaf-test:/disabled-6++
  (match-expansion
   (leaf foo :disabled (rnil) :disabled (rt) (rnil) (rnil) :config (message "bar"))
   '(progn
      (require 'foo)
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
   '(progn
      (require 'foo))))

(cort-deftest leaf-test:/disabled-2+++
  (match-expansion
   (leaf foo :disabled (mnil) :config (message "bar"))
   '(progn
      (require 'foo)
      (message "bar"))))

(cort-deftest laef-test:/disabled-3+++
  (match-expansion
   (leaf foo :disabled (mnil) :init (message "bar") :config (message "baz"))
   '(progn
      (progn
        (message "bar"))
      (progn
        (require 'foo)
        (message "baz")))))

(cort-deftest leaf-test:/disabled-4+++
  (match-expansion
   (leaf foo :disabled (mnil) (mt) :config (message "bar"))
   '(progn
      (require 'foo)
      (message "bar"))))

(cort-deftest leaf-test:/disabled-5+++
  (match-expansion
   (leaf foo :disabled (mnil) (mt) :config (message "bar") :disabled (mt))
   '(progn
      (require 'foo)
      (message "bar"))))

(cort-deftest leaf-test:/disabled-6+++
  (match-expansion
   (leaf foo :disabled (mnil) :disabled (mt) (mnil) (mnil) :config (message "bar"))
   '(progn
      (require 'foo)
      (message "bar"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  conditions
;;

(cort-deftest leaf-test/:if-1
  (match-expansion
   (leaf foo :if t)
   '(if t
        (progn
          (progn
            (require 'foo))))))

(cort-deftest leaf-test/:if-2
  (match-expansion
   (leaf foo :if (or (rt) (rnil)))
   '(if (or (rt) (rnil))
        (progn
          (progn
            (require 'foo))))))

(cort-deftest leaf-test/:if-3
  (match-expansion
   (leaf foo :if nil)
   '(if nil
        (progn
          (progn
            (require 'foo))))))

(cort-deftest leaf-test/:when-1
  (match-expansion
   (leaf foo :when t)
   '(when t
       (progn
         (require 'foo)))))

(cort-deftest leaf-test/:when-2
  (match-expansion
   (leaf foo :when (or (rt) (rnil)))
   '(when (or (rt) (rnil))
       (progn
         (require 'foo)))))

(cort-deftest leaf-test/:when-3
  (match-expansion
   (leaf foo :when nil)
   '(when nil
       (progn
         (require 'foo)))))

(cort-deftest leaf-test/:unless-1
  (match-expansion
   (leaf foo :unless t)
   '(unless t
       (progn
         (require 'foo)))))

(cort-deftest leaf-test/:unless-2
  (match-expansion
   (leaf foo :unless (or (rt) (rnil)))
   '(unless (or (rt) (rnil))
       (progn
         (require 'foo)))))

(cort-deftest leaf-test/:unless-3
  (match-expansion
   (leaf foo :unless nil)
   '(unless nil
       (progn
         (require 'foo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  :require keyword
;;

(cort-deftest leaf-test/:require-0
  (match-expansion
   (leaf foo)
   '(progn
      (require 'foo))))

(cort-deftest leaf-test/:require-1
  (match-expansion
   (leaf foo :require t)
   '(progn
      (require 'foo))))

(cort-deftest leaf-test/:require-2
  (match-expansion
   (leaf foo :require nil)
   '(progn)))

(cort-deftest leaf-test/:require-3
  (match-expansion
   (leaf foo :require bar baz)
   '(progn
      (require bar)
      (require baz))))

(cort-deftest leaf-test/:require-4
  (match-expansion
   (leaf foo :require bar baz :if t)
   '(if t
        (progn
          (progn
            (require bar)
            (require baz))))))

(provide 'leaf-tests)
;;; leaf-tests.el ends here
