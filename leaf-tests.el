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

(when (and (not (fboundp 'macroexpand-1))
           (fboundp 'autoload-do-load))
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

(defmacro match-expansion (form expect)
  (if (fboundp 'macroexpand-1)
      `(:equal (macroexpand-1 ',form) ,expect)
    `(:equal (macroexpand ',form) ,expect)))

(defmacro leaf-match (form expect)
  "Return testcase for cort.

Since `macroexpand-1' is not defined in Emacs below 23.0, use this macro.
EXPECT is (expect-default expect-24)"
  `(match-expansion
    ,form
    (,(car expect)
     :cort-if ((not (fboundp 'macroexpand-1)) ,(cadr expect)))))

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
          (require 'foo)))))

(cort-deftest leaf-test/:simple-when
  (leaf-match
   (leaf foo :when t)
   ('(when t
       (progn
         (require 'foo)))
    '(if t
         (progn
           (progn
             (require 'foo)))))))

(cort-deftest leaf-test/:simple-unless
  (leaf-match
   (leaf foo :unless t)
   ('(unless t
       (progn
         (require 'foo)))
    '(if t
         nil
       (progn
         (require 'foo))))))

(cort-deftest leaf-test/:simple-multi-if
  (match-expansion
   (leaf foo :if (rt) :if (rnil) (mt))
   '(if (and (rt) (rnil) (mt))
        (progn
          (require 'foo)))))

(cort-deftest leaf-test/:simple-multi-conds
  (match-expansion
   (leaf foo :if (rt) :when (rnil) (mt) :unless (rt) :if (rnil))
   '(if (and (rt) (rnil))
        (when (and (rnil) (mt))
          (unless (rt)
            (progn
              (require 'foo)))))))

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
          (require 'foo)))))

(cort-deftest leaf-test/:if-2
  (match-expansion
   (leaf foo :if (or (rt) (rnil)))
   '(if (or (rt) (rnil))
        (progn
          (require 'foo)))))

(cort-deftest leaf-test/:if-3
  (match-expansion
   (leaf foo :if nil)
   '(if nil
        (progn
          (require 'foo)))))

(cort-deftest leaf-test/:when-1
  (leaf-match
   (leaf foo :when t)
   ('(when t
       (progn
         (require 'foo)))
    '(if t
         (progn
           (progn
             (require 'foo)))))))

(cort-deftest leaf-test/:when-2
  (leaf-match
   (leaf foo :when (or (rt) (rnil)))
   ('(when (or (rt) (rnil))
       (progn
         (require 'foo)))
    '(if (or (rt) (rnil))
         (progn
           (progn
             (require 'foo)))))))

(cort-deftest leaf-test/:when-3
  (leaf-match
   (leaf foo :when nil)
   ('(when nil
       (progn
         (require 'foo)))
    '(if nil
         (progn
           (progn
             (require 'foo)))))))

(cort-deftest leaf-test/:unless-1
  (leaf-match
   (leaf foo :unless t)
   ('(unless t
       (progn
         (require 'foo)))
    '(if t
         nil
       (progn
         (require 'foo))))))

(cort-deftest leaf-test/:unless-2
  (leaf-match
   (leaf foo :unless (or (rt) (rnil)))
   ('(unless (or (rt) (rnil))
       (progn
         (require 'foo)))
    '(if (or (rt) (rnil))
         nil
       (progn
         (require 'foo))))))

(cort-deftest leaf-test/:unless-3
  (leaf-match
   (leaf foo :unless nil)
   ('(unless nil
       (progn
         (require 'foo)))
    '(if nil nil
       (progn
         (require 'foo))))))

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
          (require bar)
          (require baz)))))

(provide 'leaf-tests)
;;; leaf-tests.el ends here
