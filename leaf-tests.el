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
(require 'cort-test)

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

(defmacro cort-deftest-with-macroexpand (name form)
  "Return `cort-deftest' compare by `equal' for NAME, FORM.

Example:
  (p (cort-deftest-with-equal leaf/disabled
       '((asdf asdf)
         (uiop uiop))))
   => (cort-deftest leaf/disabled
        '((:equal 'asdf
                  (macroexpand-1 'asdf))
          (:equal 'uiop
                  (macroexpand-1 'uiop))))"
  (declare (indent 1))
  `(cort-deftest ,name
     ',(mapcar (lambda (elm)
                 `(:equal
                   ',(cadr elm)
                   (macroexpand-1 ',(car elm))))
               (cadr form))))

(defmacro cort-deftest-with-macroexpand-let (name letform form)
  "Return `cort-deftest' compare by `equal' for NAME, LETFORM FORM.

Example:
  (p (cort-deftest-with-macroexpand-let leaf/leaf
         ((leaf-expand-leaf-protect t))
       '(((leaf leaf
            :config (leaf-init))
          (prog1 'leaf
            (leaf-handler-leaf-protect leaf
              (leaf-init)))))))
   => (cort-deftest leaf/leaf
        '((:equal
           '(prog1 'leaf
              (leaf-handler-leaf-protect leaf
                (leaf-init)))
           (let ((leaf-expand-leaf-protect t))
             (macroexpand-1
              '(leaf leaf
                 :config (leaf-init)))))))"
  (declare (indent 2))
  `(cort-deftest ,name
     ',(mapcar (lambda (elm)
                 `(:equal
                   ',(cadr elm)
                   (let ,letform (macroexpand-1 ',(car elm)))))
               (cadr form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test definition
;;

(custom-set-variables '(leaf-backend-bind   'bind-key)
                      '(leaf-expand-leaf-protect nil))

(cort-deftest-with-macroexpand leaf/none
  '(((leaf leaf)
     (prog1 'leaf))))

(cort-deftest-with-macroexpand leaf/disabled
  '(((leaf leaf :disabled t       :config (leaf-init))
     (prog1 'leaf))
    ((leaf leaf :disabled nil     :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled t t     :config (leaf-init))
     (prog1 'leaf))
    ((leaf leaf :disabled t nil   :config (leaf-init))
     (prog1 'leaf))
    ((leaf leaf :disabled nil t   :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled nil nil :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))

    ((leaf leaf :disabled t :disabled t       :config (leaf-init))
     (prog1 'leaf))
    ((leaf leaf :disabled t :disabled nil     :config (leaf-init))
     (prog1 'leaf))
    ((leaf leaf :disabled t :disabled t t     :config (leaf-init))
     (prog1 'leaf))
    ((leaf leaf :disabled t :disabled t nil   :config (leaf-init))
     (prog1 'leaf))
    ((leaf leaf :disabled t :disabled nil t   :config (leaf-init))
     (prog1 'leaf))
    ((leaf leaf :disabled t :disabled nil nil :config (leaf-init))
     (prog1 'leaf))

    ((leaf leaf :disabled nil :disabled t       :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled nil :disabled nil     :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled nil :disabled t t     :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled nil :disabled t nil   :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled nil :disabled nil t   :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled nil :disabled nil nil :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))

    ((leaf leaf :disabled t :disabled t       :config (leaf-init) :disabled t)
     (prog1 'leaf))
    ((leaf leaf :disabled t :disabled nil     :config (leaf-init) :disabled nil)
     (prog1 'leaf))
    ((leaf leaf :disabled t :disabled t t     :config (leaf-init) :disabled t t)
     (prog1 'leaf))
    ((leaf leaf :disabled t :disabled t nil   :config (leaf-init) :disabled t nil)
     (prog1 'leaf))
    ((leaf leaf :disabled t :disabled nil t   :config (leaf-init) :disabled nil t)
     (prog1 'leaf))
    ((leaf leaf :disabled t :disabled nil nil :config (leaf-init) :disabled nil nil)
     (prog1 'leaf))

    ((leaf leaf :disabled nil :disabled t       :config (leaf-init) :disabled t)
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled nil :disabled nil     :config (leaf-init) :disabled nil)
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled nil :disabled t t     :config (leaf-init) :disabled t t)
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled nil :disabled t nil   :config (leaf-init) :disabled t nil)
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled nil :disabled nil t   :config (leaf-init) :disabled nil t)
     (prog1 'leaf
       (leaf-init)))
    ((leaf leaf :disabled nil :disabled nil nil :config (leaf-init) :disabled nil nil)
     (prog1 'leaf
       (leaf-init)))))

(cort-deftest-with-macroexpand leaf/package
  '(((leaf leaf
       :package t
       :config (leaf-init))
     (prog1 'leaf
       (leaf-handler-package leaf leaf nil)
       (leaf-init)))

    ((leaf leaf
       :package t leaf-browser
       :config (leaf-init))
     (prog1 'leaf
       (leaf-handler-package leaf leaf nil)
       (leaf-handler-package leaf leaf-browser nil)
       (leaf-init)))

    ((leaf leaf
       :package feather leaf-key leaf-browser
       :config (leaf-init))
     (prog1 'leaf
       (leaf-handler-package leaf feather nil)
       (leaf-handler-package leaf leaf-key nil)
       (leaf-handler-package leaf leaf-browser nil)
       (leaf-init)))

    ;;    :package (((feather) leaf-key) leaf-browser)
    ;;    :config (leaf-init))
    ;;  (progn
    ;;    (leaf-handler-package leaf 'feather nil)
    ;;    (leaf-handler-package leaf 'leaf-key nil)
    ;;    (leaf-handler-package leaf 'leaf-browser nil)
    ;;    (leaf-init)))

    ;; ((leaf leaf
    ;;    :package (((feather . elpa-archive) leaf-key) leaf-browser . stable)
    ;;    :config (leaf-init))
    ;;  (progn
    ;;    (leaf-handler-package leaf 'feather elpa-archive)
    ;;    (leaf-handler-package leaf 'leaf-key stable)
    ;;    (leaf-handler-package leaf 'leaf-browser stable)
    ;;    (leaf-init)))
    ))

(cort-deftest-with-macroexpand leaf/doc
  '(((leaf leaf
       :doc "Symplify init.el configuration"
       :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))

    ((leaf leaf
       :file "~/.emacs.d/elpa/leaf.el/leaf.el"
       :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))

    ((leaf leaf
       :url "https://github.com/conao3/leaf.el"
       :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))

    ((leaf leaf
       :doc "Symplify init.el configuration"
       :file "~/.emacs.d/elpa/leaf.el/leaf.el"
       :url "https://github.com/conao3/leaf.el"
       :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))

    ((leaf leaf
       :doc "Symplify init.el configuration"
       "
(leaf leaf
  :doc \"Symplify init.el configuration\"
  :config (leaf-init))
 => (progn
      (leaf-init))"
       "
(leaf leaf
  :disabled nil
  :config (leaf-init))
 => (progn
      (leaf-init))"
       :file "~/.emacs.d/elpa/leaf.el/leaf.el"
       :url "https://github.com/conao3/leaf.el"
       :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))))

(cort-deftest-with-macroexpand leaf/load-path
  '(((leaf leaf
       :load-path "~/.emacs.d/elpa-archive/leaf.el/"
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf.el/")
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :load-path
       "~/.emacs.d/elpa-archive/leaf.el/"
       "~/.emacs.d/elpa-archive/leaf-browser.el/"
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf.el/")
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf-browser.el/")
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :load-path ("~/.emacs.d/elpa-archive/leaf.el/"
                   "~/.emacs.d/elpa-archive/leaf-browser.el/")
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf.el/")
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf-browser.el/")
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :load-path ("~/.emacs.d/elpa-archive/leaf.el/"
                   ("~/.emacs.d/elpa-archive/leaf.el/"
                    "~/.emacs.d/elpa-archive/leaf-browser.el/"))
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf.el/")
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf-browser.el/")
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :load-path ("~/.emacs.d/elpa-archive/leaf.el/"
                   ("~/.emacs.d/elpa-archive/leaf.el/"
                    ("~/.emacs.d/elpa-archive/leaf.el/"
                     ("~/.emacs.d/elpa-archive/leaf.el/"
                      ("~/.emacs.d/elpa-archive/leaf.el/")))))
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf.el/")
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :load-path ("~/.emacs.d/elpa-archive/leaf.el/")
       :load-path `(,(mapcar (lambda (elm)
                               (concat "~/.emacs.d/elpa-archive/" elm "/"))
                             '("leaf.el" "leaf-broser.el" "orglyth.el")))
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf.el/")
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf-broser.el/")
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/orglyth.el/")
       (require 'leaf)
       (leaf-init)))))

(cort-deftest-with-macroexpand leaf/defun
  '(((leaf leaf
       :defun leaf leaf-normalize-plist leaf-merge-dupkey-values-plist)
     (prog1 'leaf
       (declare-function leaf "leaf")
       (declare-function leaf-normalize-plist "leaf")
       (declare-function leaf-merge-dupkey-values-plist "leaf")))

    ((leaf leaf
       :defun (leaf leaf-normalize-plist leaf-merge-dupkey-values-plist))
     (prog1 'leaf
       (declare-function leaf "leaf")
       (declare-function leaf-normalize-plist "leaf")
       (declare-function leaf-merge-dupkey-values-plist "leaf")))

    ;; ((leaf leaf
    ;;    :defun (leaf
    ;;             (leaf-normalize-plist
    ;;              (leaf-merge-dupkey-values-plist))))
    ;;  (progn
    ;;    (declare-function leaf "leaf")
    ;;    (declare-function leaf-normalize-plist "leaf")
    ;;    (declare-function leaf-merge-dupkey-values-plist "leaf")))

    ;; ((leaf leaf
    ;;    :defun (lbrowser-open lbrowser-close . leaf-browser))
    ;;  (progn
    ;;    (declare-function lbrowser-open "leaf-browser")
    ;;    (declare-function lbrowser-close "leaf-browser")))

    ;; ((leaf leaf
    ;;    :defun ((lbrowser-open (lbrowser-close) . leaf) . leaf-browser))
    ;;  (progn
    ;;    (declare-function lbrowser-open "leaf")
    ;;    (declare-function lbrowser-close "leaf")))

    ;; ((leaf leaf
    ;;    :defun ((lbrowser-open (lbrowser-close) . leaf) leaf-asdf . leaf-browser))
    ;;  (progn
    ;;    (declare-function lbrowser-open "leaf")
    ;;    (declare-function lbrowser-close "leaf")
    ;;    (declare-function leaf-asdf "leaf-browser")))
    ))

(cort-deftest-with-macroexpand leaf/defvar
  '(((leaf leaf
       :defvar leaf leaf-normalize-plist leaf-merge-dupkey-values-plist)
     (prog1 'leaf
       (defvar leaf)
       (defvar leaf-normalize-plist)
       (defvar leaf-merge-dupkey-values-plist)))

    ((leaf leaf
       :defvar (leaf leaf-normalize-plist leaf-merge-dupkey-values-plist))
     (prog1 'leaf
       (defvar leaf)
       (defvar leaf-normalize-plist)
       (defvar leaf-merge-dupkey-values-plist)))

    ((leaf leaf
       :defvar (leaf
                 (leaf-normalize-plist
                  (leaf-merge-dupkey-values-plist))))
     (prog1 'leaf
       (defvar leaf)
       (defvar leaf-normalize-plist)
       (defvar leaf-merge-dupkey-values-plist)))))

(cort-deftest-with-macroexpand leaf/preface
  '(((leaf leaf
       :init (leaf-pre-init)
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :preface (progn
                  (leaf-pre-init)
                  (leaf-pre-init-after))
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (progn
         (leaf-pre-init)
         (leaf-pre-init-after))
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :preface
       (leaf-pre-init)
       (leaf-pre-init-after)
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (leaf-pre-init-after)
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :preface (preface-init)
       :when (some-condition)
       :require t
       :init (package-preconfig)
       :config (package-init))
     (prog1 'leaf
       (preface-init)
       (when (some-condition)
         (package-preconfig)
         (require 'leaf)
         (package-init))))))

(cort-deftest-with-macroexpand leaf/if
  '(((leaf leaf
       :if leafp
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (if leafp
           (progn
             (require 'leaf)
             (leaf-init)))))

    ((leaf leaf
       :if leafp leaf-avairablep (window-system)
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (if (and leafp leaf-avairablep (window-system))
           (progn
             (require 'leaf)
             (leaf-init)))))

    ((leaf leaf
       :if leafp leaf-avairablep (window-system)
       :when leaf-browserp
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (when leaf-browserp
         (if (and leafp leaf-avairablep (window-system))
             (progn
               (require 'leaf)
               (leaf-init))))))

    ((leaf leaf
       :if leafp leaf-avairablep (window-system)
       :when leaf-browserp
       :load-path "~/.emacs.d/elpa-archive/leaf.el/"
       :preface (leaf-load)
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf.el/")
       (leaf-load)
       (when leaf-browserp
         (if (and leafp leaf-avairablep (window-system))
             (progn
               (require 'leaf)
               (leaf-init))))))))

(cort-deftest-with-macroexpand leaf/when
  '(((leaf leaf
       :when leafp
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (when leafp
         (require 'leaf)
         (leaf-init))))

    ((leaf leaf
       :when leafp leaf-avairablep (window-system)
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (when (and leafp leaf-avairablep (window-system))
         (require 'leaf)
         (leaf-init))))))

(cort-deftest-with-macroexpand leaf/unless
  '(((leaf leaf
       :unless leafp
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (unless leafp
         (require 'leaf)
         (leaf-init))))

    ((leaf leaf
       :unless leafp leaf-avairablep (window-system)
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (unless (and leafp leaf-avairablep (window-system))
         (require 'leaf)
         (leaf-init))))))

(cort-deftest-with-macroexpand leaf/after
  '(((leaf leaf-browser
       :after leaf
       :require t
       :config (leaf-browser-init))
     (prog1 'leaf-browser
       (eval-after-load 'leaf
         '(progn
            (require 'leaf-browser)
            (leaf-browser-init)))))

    ((leaf leaf-browser
       :after leaf org orglyth
       :require t
       :config (leaf-browser-init))
     (prog1 'leaf-browser
       (eval-after-load 'orglyth
         '(eval-after-load 'org
            '(eval-after-load 'leaf
               '(progn
                  (require 'leaf-browser)
                  (leaf-browser-init)))))))

    ((leaf leaf-browser
       :after leaf (org orglyth)
       :require t
       :config (leaf-browser-init))
     (prog1 'leaf-browser
       (eval-after-load 'orglyth
         '(eval-after-load 'org
            '(eval-after-load 'leaf
               '(progn
                  (require 'leaf-browser)
                  (leaf-browser-init)))))))

    ((leaf leaf-browser
       :after leaf (org orglyth
                        (org
                         (org
                          (org-ex))))
       :require t
       :config (leaf-browser-init))
     (prog1 'leaf-browser
       (eval-after-load 'org-ex
         '(eval-after-load 'orglyth
            '(eval-after-load 'org
               '(eval-after-load 'leaf
                  '(progn
                     (require 'leaf-browser)
                     (leaf-browser-init))))))))))

(cort-deftest-with-macroexpand leaf/custom
  '(((leaf flyspell-correct-ivy
       :bind (("C-M-i" . flyspell-correct-wrapper))
       :custom ((flyspell-correct-interface . #'flyspell-correct-ivy)))
     (prog1 'flyspell-correct-ivy
       (autoload #'flyspell-correct-wrapper "flyspell-correct-ivy" nil t)
       (leaf-keys (("C-M-i" . flyspell-correct-wrapper)))
       (eval-after-load 'flyspell-correct-ivy
         '(progn
            (custom-set-variables
             '(flyspell-correct-interface #'flyspell-correct-ivy "Customized with leaf in flyspell-correct-ivy block"))))))

    ((leaf leaf
       :custom ((leaf-backend-ensure . 'feather)))
     (prog1 'leaf
       (custom-set-variables
        '(leaf-backend-ensure 'feather "Customized with leaf in leaf block"))))

    ((leaf leaf
       :custom ((leaf-backend-ensure . 'feather)
                (leaf-backend-bind   . 'bind-key)
                (leaf-backend-bind*  . 'bind-key)))
     (prog1 'leaf
       (custom-set-variables
        '(leaf-backend-ensure 'feather "Customized with leaf in leaf block")
        '(leaf-backend-bind 'bind-key "Customized with leaf in leaf block")
        '(leaf-backend-bind* 'bind-key "Customized with leaf in leaf block"))))

    ((leaf leaf
       :custom
       (leaf-backend-ensure . 'feather)
       (leaf-backend-bind   . 'bind-key)
       (leaf-backend-bind*  . 'bind-key))
     (prog1 'leaf
       (custom-set-variables
        '(leaf-backend-ensure 'feather "Customized with leaf in leaf block")
        '(leaf-backend-bind 'bind-key "Customized with leaf in leaf block")
        '(leaf-backend-bind* 'bind-key "Customized with leaf in leaf block"))))

    ((leaf buffer.c
       :custom ((cursor-type . nil)))
     (prog1 'buffer\.c
       (custom-set-variables
        '(cursor-type nil "Customized with leaf in buffer.c block"))))

    ;; ((leaf leaf
    ;;    :custom ((leaf-backend-bind leaf-backend-bind*) . 'bind-key))
    ;;  (progn
    ;;    (custom-set-variables
    ;;     '(leaf-backend-bind 'bind-key "Customized with leaf in leaf block")
    ;;     '(leaf-backend-bind* 'bind-key "Customized with leaf in leaf block"))))

    ;; ((leaf leaf
    ;;    :custom
    ;;    (leaf-backend-ensure . 'feather)
    ;;    ((leaf-backend-bind leaf-backend-bind*) . 'bind-key))
    ;;  (progn
    ;;    (custom-set-variables
    ;;     '(leaf-backend-ensure 'feather "Customized with leaf in leaf block")
    ;;     '(leaf-backend-bind 'bind-key "Customized with leaf in leaf block")
    ;;     '(leaf-backend-bind* 'bind-key "Customized with leaf in leaf block"))))

    ;; ((leaf leaf
    ;;    :custom ((leaf-backend-ensure . 'feather)
    ;;             ((leaf-backend-bind leaf-backend-bind*) . 'bind-key)))
    ;;  (progn
    ;;    (custom-set-variables
    ;;     '(leaf-backend-ensure 'feather "Customized with leaf in leaf block")
    ;;     '(leaf-backend-bind 'bind-key "Customized with leaf in leaf block")
    ;;     '(leaf-backend-bind* 'bind-key "Customized with leaf in leaf block"))))

    ;; ((leaf leaf
    ;;    :custom ((leaf-backend-ensure . 'feather)
    ;;             (((leaf-backend-bind leaf-backend-bind*) . 'leaf-key)
    ;;              leaf-backend-bind-key . 'bind-key)))
    ;;  (progn
    ;;    (custom-set-variables
    ;;     '(leaf-backend-ensure 'feather "Customized with leaf in leaf block")
    ;;     '(leaf-backend-bind 'leaf-key "Customized with leaf in leaf block")
    ;;     '(leaf-backend-bind* 'leaf-key "Customized with leaf in leaf block")
    ;;     '(leaf-backend-bind-key 'bind-key "Customized with leaf in leaf block"))))
    ))

(cort-deftest-with-macroexpand leaf/custom-face
  '(((leaf eruby-mode
       :custom-face
       (eruby-standard-face . '((t (:slant italic)))))
     (prog1 'eruby-mode
       (custom-set-faces
        '(eruby-standard-face ((t (:slant italic)))))))

    ;; ((leaf eruby-mode
    ;;    :custom-face
    ;;    ((default eruby-standard-face) . '((t (:slant italic)))))
    ;;  (progn
    ;;    (custom-set-faces
    ;;     '(default (((t (:slant italic)))))
    ;;     '(eruby-standard-face (((t (:slant italic))))))))
    ))

(cort-deftest-with-macroexpand leaf/bind
  '(((leaf macrostep
       :package t
       :bind (("C-c e" . macrostep-expand)))
     (prog1 'macrostep
       (autoload #'macrostep-expand "macrostep" nil t)
       (leaf-handler-package macrostep macrostep nil)
       (leaf-keys (("C-c e" . macrostep-expand)))))

    ((leaf macrostep
       :package t
       :bind ("C-c e" . macrostep-expand))
     (prog1 'macrostep
       (autoload #'macrostep-expand "macrostep" nil t)
       (leaf-handler-package macrostep macrostep nil)
       (leaf-keys
        (("C-c e" . macrostep-expand)))))

    ((leaf color-moccur
       :bind
       ("M-s O" . moccur)
       ("M-o" . isearch-moccur)
       ("M-O" . isearch-moccur-all))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-keys (("M-s O" . moccur)
                   ("M-o" . isearch-moccur)
                   ("M-O" . isearch-moccur-all)))))

    ((leaf color-moccur
       :bind (("M-s O" . moccur)
              ("M-o" . isearch-moccur)
              ("M-O" . isearch-moccur-all)))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-keys (("M-s O" . moccur)
                   ("M-o" . isearch-moccur)
                   ("M-O" . isearch-moccur-all)))))

    ((leaf color-moccur
       :bind
       ("M-s" . nil)
       ("M-s o" . isearch-moccur)
       ("M-s i" . isearch-moccur-all))
     (prog1 'color-moccur
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-keys (("M-s")
                   ("M-s o" . isearch-moccur)
                   ("M-s i" . isearch-moccur-all)))))

    ((leaf color-moccur
       :bind (("M-s" . nil)
              ("M-s o" . isearch-moccur)
              ("M-s i" . isearch-moccur-all)))
     (prog1 'color-moccur
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-keys (("M-s")
                   ("M-s o" . isearch-moccur)
                   ("M-s i" . isearch-moccur-all)))))

    ;; ((leaf color-moccur
    ;;    :bind (("M-s O" . moccur)
    ;;           (("M-o" . isearch-moccur)
    ;;            (("M-O" . isearch-moccur-all)))))
    ;;  (progn
    ;;    (autoload #'moccur "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur-all "color-moccur" nil t)
    ;;    (leaf-handler-bind color-moccur '(:package color-moccur ("M-s O" . moccur)))
    ;;    (leaf-handler-bind color-moccur '(:package color-moccur ("M-o" . isearch-moccur)))
    ;;    (leaf-handler-bind color-moccur '(:package color-moccur ("M-O" . isearch-moccur-all)))))

    ;; ((leaf color-moccur
    ;;    :bind (("M-s O" . moccur)
    ;;           (("M-o" . isearch-moccur)
    ;;            (("M-O" . isearch-moccur-all))
    ;;            ("M-s" . isearch-moccur-some))))
    ;;  (progn
    ;;    (autoload #'moccur "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur-all "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur-some "color-moccur" nil t)
    ;;    (leaf-handler-bind color-moccur '(:package color-moccur ("M-s O" . moccur)))
    ;;    (leaf-handler-bind color-moccur '(:package color-moccur ("M-o" . isearch-moccur)))
    ;;    (leaf-handler-bind color-moccur '(:package color-moccur ("M-O" . isearch-moccur-all)))
    ;;    (leaf-handler-bind color-moccur '(:package color-moccur ("M-s" . isearch-moccur-some)))))

    ;; ((leaf color-moccur
    ;;    :bind (("M-s O" . moccur)
    ;;           (:isearch-mode-map
    ;;            ("M-o" . isearch-moccur)
    ;;            ("M-O" . isearch-moccur-all))))
    ;;  (progn
    ;;    (autoload #'moccur "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur-all "color-moccur" nil t)
    ;;    (leaf-handler-bind color-moccur '(:package color-moccur ("M-s O" . moccur)))
    ;;    (leaf-handler-bind color-moccur '(:map isearch-mode-map :package color-moccur ("M-o" . isearch-moccur)))
    ;;    (leaf-handler-bind color-moccur '(:map isearch-mode-map :package color-moccur ("M-O" . isearch-moccur-all)))))

    ((leaf color-moccur
       :bind
       ("M-s O" . moccur)
       (:isearch-mode-map
        ("M-o" . isearch-moccur)
        ("M-O" . isearch-moccur-all)))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-keys (("M-s O" . moccur)
                   (:isearch-mode-map
                    :package color-moccur
                    ("M-o" . isearch-moccur)
                    ("M-O" . isearch-moccur-all))))))

    ((leaf color-moccur
       :bind
       ("M-s O" . moccur)
       (:isearch-mode-map
        :package isearch
        ("M-o" . isearch-moccur)
        ("M-O" . isearch-moccur-all)))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-keys (("M-s O" . moccur)
                   (:isearch-mode-map
                    :package isearch
                    ("M-o" . isearch-moccur)
                    ("M-O" . isearch-moccur-all))))))

    ((leaf color-moccur
       :bind (("M-s O" . moccur)
              (:isearch-mode-map
               :package isearch
               ("M-o" . isearch-moccur)
               ("M-O" . isearch-moccur-all))))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-keys (("M-s O" . moccur)
                   (:isearch-mode-map
                    :package isearch
                    ("M-o" . isearch-moccur)
                    ("M-O" . isearch-moccur-all))))))

    ((leaf color-moccur
       :bind (("M-s O" . moccur)
              (isearch-mode-map
               :package isearch
               ("M-o" . isearch-moccur)
               ("M-O" . isearch-moccur-all))))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-keys (("M-s O" . moccur)
                   (isearch-mode-map
                    :package isearch
                    ("M-o" . isearch-moccur)
                    ("M-O" . isearch-moccur-all))))))))

(cort-deftest-with-macroexpand leaf/bind*
  '(((leaf color-moccur
       :bind*
       ("M-s O" . moccur)
       ("M-o" . isearch-moccur)
       ("M-O" . isearch-moccur-all))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-keys* (("M-s O" . moccur)
                    ("M-o" . isearch-moccur)
                    ("M-O" . isearch-moccur-all)))))

    ((leaf color-moccur
       :bind* (("M-s O" . moccur)
               ("M-o" . isearch-moccur)
               ("M-O" . isearch-moccur-all)))
     (prog1 'color-moccur
       (autoload #'moccur "color-moccur" nil t)
       (autoload #'isearch-moccur "color-moccur" nil t)
       (autoload #'isearch-moccur-all "color-moccur" nil t)
       (leaf-keys* (("M-s O" . moccur)
                    ("M-o" . isearch-moccur)
                    ("M-O" . isearch-moccur-all)))))

    ;; ((leaf color-moccur
    ;;    :bind* (("M-s O" . moccur)
    ;;            (("M-o" . isearch-moccur)
    ;;             (("M-O" . isearch-moccur-all)))))
    ;;  (progn
    ;;    (autoload #'moccur "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur-all "color-moccur" nil t)
    ;;    (leaf-handler-bind* color-moccur '(:package color-moccur ("M-s O" . moccur)))
    ;;    (leaf-handler-bind* color-moccur '(:package color-moccur ("M-o" . isearch-moccur)))
    ;;    (leaf-handler-bind* color-moccur '(:package color-moccur ("M-O" . isearch-moccur-all)))))

    ;; ((leaf color-moccur
    ;;    :bind* (("M-s O" . moccur)
    ;;            (("M-o" . isearch-moccur)
    ;;             (("M-O" . isearch-moccur-all))
    ;;             ("M-s" . isearch-moccur-some))))
    ;;  (progn
    ;;    (autoload #'moccur "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur-all "color-moccur" nil t)
    ;;    (autoload #'isearch-moccur-some "color-moccur" nil t)
    ;;    (leaf-handler-bind* color-moccur '(:package color-moccur ("M-s O" . moccur)))
    ;;    (leaf-handler-bind* color-moccur '(:package color-moccur ("M-o" . isearch-moccur)))
    ;;    (leaf-handler-bind* color-moccur '(:package color-moccur ("M-O" . isearch-moccur-all)))
    ;;    (leaf-handler-bind* color-moccur '(:package color-moccur ("M-s" . isearch-moccur-some)))))
    ))

(cort-deftest-with-macroexpand leaf/mode
  '(((leaf web-mode
       :mode "\\.js\\'" "\\.p?html?\\'")
     (prog1 'web-mode
       (autoload #'web-mode "web-mode" nil t)
       (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
       (add-to-list 'auto-mode-alist '("\\.p?html?\\'" . web-mode))))

    ((leaf web-mode
       :mode ("\\.js\\'" "\\.p?html?\\'"))
     (prog1 'web-mode
       (autoload #'web-mode "web-mode" nil t)
       (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
       (add-to-list 'auto-mode-alist '("\\.p?html?\\'" . web-mode))))

    ;; ((leaf web-mode
    ;;    :mode ("\\.js\\'" ("\\.p?html?\\'")))
    ;;  (progn
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
    ;;    (add-to-list 'auto-mode-alist '("\\.p?html?\\'" . web-mode))))

    ;; ((leaf web-mode
    ;;    :mode (("\\.js\\'" "\\.p?html?\\'") . web-mode))
    ;;  (progn
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
    ;;    (add-to-list 'auto-mode-alist '("\\.p?html?\\'" . web-mode))))

    ;; ((leaf web-mode
    ;;    :mode (("\\.phtml?\\'" "\\.html?\\'" . web-html-mode) "\\.js\\'" . web-mode))
    ;;  (progn
    ;;    (autoload #'web-html-mode "web-mode" nil t)
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'auto-mode-alist '("\\.phtml?\\'" . web-html-mode))
    ;;    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-html-mode))
    ;;    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))))
    ))

(cort-deftest-with-macroexpand leaf/interpreter
  '(((leaf ruby-mode
       :mode "\\.rb\\'" "\\.rb2\\'" ("\\.rbg\\'" . rb-mode)
       :interpreter "ruby")
     (prog1 'ruby-mode
       (autoload #'ruby-mode "ruby-mode" nil t)
       (autoload #'rb-mode "ruby-mode" nil t)
       (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
       (add-to-list 'auto-mode-alist '("\\.rb2\\'" . ruby-mode))
       (add-to-list 'auto-mode-alist '("\\.rbg\\'" . rb-mode))
       (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))))

    ((leaf web-mode
       :interpreter "js" "p?html?")
     (prog1 'web-mode
       (autoload #'web-mode "web-mode" nil t)
       (add-to-list 'interpreter-mode-alist '("js" . web-mode))
       (add-to-list 'interpreter-mode-alist '("p?html?" . web-mode))))

    ((leaf web-mode
       :interpreter ("js" "p?html?"))
     (prog1 'web-mode
       (autoload #'web-mode "web-mode" nil t)
       (add-to-list 'interpreter-mode-alist '("js" . web-mode))
       (add-to-list 'interpreter-mode-alist '("p?html?" . web-mode))))

    ;; ((leaf web-mode
    ;;    :interpreter ("js" ("p?html?")))
    ;;  (progn
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'interpreter-mode-alist '("js" . web-mode))
    ;;    (add-to-list 'interpreter-mode-alist '("p?html?" . web-mode))))

    ;; ((leaf web-mode
    ;;    :interpreter (("js" "p?html?") . web-mode))
    ;;  (progn
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'interpreter-mode-alist '("js" . web-mode))
    ;;    (add-to-list 'interpreter-mode-alist '("p?html?" . web-mode))))

    ;; ((leaf web-mode
    ;;    :interpreter (("phtml?" "html?" . web-html-mode) "js" . web-mode))
    ;;  (progn
    ;;    (autoload #'web-html-mode "web-mode" nil t)
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'interpreter-mode-alist '("phtml?" . web-html-mode))
    ;;    (add-to-list 'interpreter-mode-alist '("html?" . web-html-mode))
    ;;    (add-to-list 'interpreter-mode-alist '("js" . web-mode))))
    ))

(cort-deftest-with-macroexpand leaf/magic
  '(((leaf pdf-tools
       :magic ("%PDF" . pdf-view-mode)
       :config
       (pdf-tools-install))
     (prog1 'pdf-tools
       (autoload #'pdf-view-mode "pdf-tools" nil t)
       (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
       (eval-after-load 'pdf-tools
         '(progn
            (pdf-tools-install)))))

    ((leaf web-mode
       :magic "js" "p?html?")
     (prog1 'web-mode
       (autoload #'web-mode "web-mode" nil t)
       (add-to-list 'magic-mode-alist '("js" . web-mode))
       (add-to-list 'magic-mode-alist '("p?html?" . web-mode))))

    ((leaf web-mode
       :magic ("js" "p?html?"))
     (prog1 'web-mode
       (autoload #'web-mode "web-mode" nil t)
       (add-to-list 'magic-mode-alist '("js" . web-mode))
       (add-to-list 'magic-mode-alist '("p?html?" . web-mode))))

    ;; ((leaf web-mode
    ;;    :magic ("js" ("p?html?")))
    ;;  (progn
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'magic-mode-alist '("js" . web-mode))
    ;;    (add-to-list 'magic-mode-alist '("p?html?" . web-mode))))

    ;; ((leaf web-mode
    ;;    :magic (("js" "p?html?") . web-mode))
    ;;  (progn
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'magic-mode-alist '("js" . web-mode))
    ;;    (add-to-list 'magic-mode-alist '("p?html?" . web-mode))))

    ;; ((leaf web-mode
    ;;    :magic (("phtml?" "html?" . web-html-mode) "js" . web-mode))
    ;;  (progn
    ;;    (autoload #'web-html-mode "web-mode" nil t)
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'magic-mode-alist '("phtml?" . web-html-mode))
    ;;    (add-to-list 'magic-mode-alist '("html?" . web-html-mode))
    ;;    (add-to-list 'magic-mode-alist '("js" . web-mode))))
    ))

(cort-deftest-with-macroexpand leaf/magic-fallback
  '(((leaf pdf-tools
       :magic-fallback ("%PDF" . pdf-view-mode)
       :config
       (pdf-tools-install))
     (prog1 'pdf-tools
       (autoload #'pdf-view-mode "pdf-tools" nil t)
       (add-to-list 'magic-fallback-mode-alist '("%PDF" . pdf-view-mode))
       (eval-after-load 'pdf-tools
         '(progn
            (pdf-tools-install)))))

    ((leaf web-mode
       :magic-fallback "js" "p?html?")
     (prog1 'web-mode
       (autoload #'web-mode "web-mode" nil t)
       (add-to-list 'magic-fallback-mode-alist '("js" . web-mode))
       (add-to-list 'magic-fallback-mode-alist '("p?html?" . web-mode))))

    ((leaf web-mode
       :magic-fallback ("js" "p?html?"))
     (prog1 'web-mode
       (autoload #'web-mode "web-mode" nil t)
       (add-to-list 'magic-fallback-mode-alist '("js" . web-mode))
       (add-to-list 'magic-fallback-mode-alist '("p?html?" . web-mode))))

    ;; ((leaf web-mode
    ;;    :magic-fallback ("js" ("p?html?")))
    ;;  (progn
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'magic-fallback-mode-alist '("js" . web-mode))
    ;;    (add-to-list 'magic-fallback-mode-alist '("p?html?" . web-mode))))

    ;; ((leaf web-mode
    ;;    :magic-fallback (("js" "p?html?") . web-mode))
    ;;  (progn
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'magic-fallback-mode-alist '("js" . web-mode))
    ;;    (add-to-list 'magic-fallback-mode-alist '("p?html?" . web-mode))))

    ;; ((leaf web-mode
    ;;    :magic-fallback (("phtml?" "html?" . web-html-mode) "js" . web-mode))
    ;;  (progn
    ;;    (autoload #'web-html-mode "web-mode" nil t)
    ;;    (autoload #'web-mode "web-mode" nil t)
    ;;    (add-to-list 'magic-fallback-mode-alist '("phtml?" . web-html-mode))
    ;;    (add-to-list 'magic-fallback-mode-alist '("html?" . web-html-mode))
    ;;    (add-to-list 'magic-fallback-mode-alist '("js" . web-mode))))
    ))

(cort-deftest-with-macroexpand leaf/hook
  '(((leaf ace-jump-mode
       :hook cc-mode-hook
       :config (ace-jump-mode))
     (prog1 'ace-jump-mode
       (autoload #'ace-jump-mode "ace-jump-mode" nil t)
       (add-hook 'cc-mode-hook #'ace-jump-mode)
       (eval-after-load 'ace-jump-mode
         '(progn
            (ace-jump-mode)))))

    ((leaf ace-jump-mode
       :hook cc-mode-hook)
     (prog1 'ace-jump-mode
       (autoload #'ace-jump-mode "ace-jump-mode" nil t)
       (add-hook 'cc-mode-hook #'ace-jump-mode)))

    ((leaf ace-jump-mode
       :hook cc-mode-hook prog-mode-hook)
     (prog1 'ace-jump-mode
       (autoload #'ace-jump-mode "ace-jump-mode" nil t)
       (add-hook 'cc-mode-hook #'ace-jump-mode)
       (add-hook 'prog-mode-hook #'ace-jump-mode)))

    ;; ((leaf ace-jump-mode
    ;;    :hook (cc-mode-hook (prog-mode-hook)))
    ;;  (progn
    ;;    (autoload #'ace-jump-mode "ace-jump-mode" nil t)
    ;;    (add-hook 'cc-mode-hook #'ace-jump-mode)
    ;;    (add-hook 'prog-mode-hook #'ace-jump-mode)))

    ((leaf ace-jump-mode
       :hook cc-mode-hook (prog-mode-hook . my-ace-jump-mode))
     (prog1 'ace-jump-mode
       (autoload #'ace-jump-mode "ace-jump-mode" nil t)
       (autoload #'my-ace-jump-mode "ace-jump-mode" nil t)
       (add-hook 'cc-mode-hook #'ace-jump-mode)
       (add-hook 'prog-mode-hook #'my-ace-jump-mode)))

    ;; ((leaf ace-jump-mode
    ;;    :hook ((cc-mode-hook prog-mode-hook) . my-ace-jump-mode))
    ;;  (progn
    ;;    (autoload #'my-ace-jump-mode "ace-jump-mode" nil t)
    ;;    (add-hook 'cc-mode-hook #'my-ace-jump-mode)
    ;;    (add-hook 'prog-mode-hook #'my-ace-jump-mode)))

    ;; ((leaf ace-jump-mode
    ;;    :hook ((cc-mode-hook prog-mode-hook . ace-jump-mode) isearch-mode . my-ace-jump-mode))
    ;;  (progn
    ;;    (autoload #'ace-jump-mode "ace-jump-mode" nil t)
    ;;    (autoload #'my-ace-jump-mode "ace-jump-mode" nil t)
    ;;    (add-hook 'cc-mode-hook #'ace-jump-mode)
    ;;    (add-hook 'prog-mode-hook #'ace-jump-mode)
    ;;    (add-hook 'isearch-mode #'my-ace-jump-mode)))
    ))

(cort-deftest-with-macroexpand leaf/advice
  '(((leaf leaf
       :preface
       (defun matu (x)
         (princ (format ">>%s<<" x))
         nil)
       (defun matu-around0 (f &rest args)
         (prog2
             (princ "around0 ==>")
             (apply f args)
           (princ "around0 <==")))
       (defun matu-before0 (&rest args)
         (princ "before0:"))
       :advice
       (:around matu matu-around0)
       (:before matu matu-before0))
     (prog1 'leaf
       (autoload #'matu-around0 "leaf" nil t)
       (autoload #'matu-before0 "leaf" nil t)
       (defun matu (x)
         (princ
          (format ">>%s<<" x))
         nil)
       (defun matu-around0
           (f &rest args)
         (prog2
             (princ "around0 ==>")
             (apply f args)
           (princ "around0 <==")))
       (defun matu-before0
           (&rest args)
         (princ "before0:"))
       (advice-add 'matu :around #'matu-around0)
       (advice-add 'matu :before #'matu-before0)))

    ((leaf leaf
       :preface
       (defun matu (x)
         (princ (format ">>%s<<" x))
         nil)
       (defun matu-around0 (f &rest args)
         (prog2
             (princ "around0 ==>")
             (apply f args)
           (princ "around0 <==")))
       (defun matu-before0 (&rest args)
         (princ "before0:"))
       :advice ((:around matu matu-around0)
                (:before matu matu-before0)))
     (prog1 'leaf
       (autoload #'matu-around0 "leaf" nil t)
       (autoload #'matu-before0 "leaf" nil t)
       (defun matu (x)
         (princ
          (format ">>%s<<" x))
         nil)
       (defun matu-around0
           (f &rest args)
         (prog2
             (princ "around0 ==>")
             (apply f args)
           (princ "around0 <==")))
       (defun matu-before0
           (&rest args)
         (princ "before0:"))
       (advice-add 'matu :around #'matu-around0)
       (advice-add 'matu :before #'matu-before0)))

    ((leaf leaf
       :preface
       (defun matu (x)
         (princ (format ">>%s<<" x))
         nil)
       (defun matu-around0 (f &rest args)
         (prog2
             (princ "around0 ==>")
             (apply f args)
           (princ "around0 <==")))
       (defun matu-before0 (&rest args)
         (princ "before0:"))
       :advice ((:around matu matu-around0)
                (:before matu matu-before0)
                (:around matu (lambda (f &rest args)
                                (prog2
                                    (princ "around1 ==>")
                                    (apply f args)
                                  (princ "around1 <=="))))))
     (prog1 'leaf
       (autoload #'matu-around0 "leaf" nil t)
       (autoload #'matu-before0 "leaf" nil t)
       (defun matu
           (x)
         (princ
          (format ">>%s<<" x))
         nil)
       (defun matu-around0
           (f &rest args)
         (prog2
             (princ "around0 ==>")
             (apply f args)
           (princ "around0 <==")))
       (defun matu-before0
           (&rest args)
         (princ "before0:"))
       (advice-add 'matu :around #'matu-around0)
       (advice-add 'matu :before #'matu-before0)
       (advice-add 'matu :around (function
                                  (lambda
                                    (f &rest args)
                                    (prog2
                                        (princ "around1 ==>")
                                        (apply f args)
                                      (princ "around1 <==")))))))))

(cort-deftest-with-macroexpand leaf/advice-remove
  '(((leaf leaf
       :advice-remove
       (matu matu-around0)
       (matu matu-before0))
     (prog1 'leaf
       (autoload (function matu-before0) "leaf" nil t)
       (autoload #'matu-around0 "leaf" nil t)
       (advice-remove 'matu #'matu-around0)
       (advice-remove 'matu #'matu-before0)))

    ((leaf leaf
       :advice-remove ((:around matu matu-around0)
                       (:before matu matu-before0)))
     (prog1 'leaf
       (autoload #'matu "leaf" nil t)
       (advice-remove ':around #'matu)
       (advice-remove ':before #'matu)))))

(cort-deftest-with-macroexpand leaf/commands
  '(((leaf leaf
       :commands leaf
       :config (leaf-init))
     (prog1 'leaf
       (autoload #'leaf "leaf" nil t)
       (eval-after-load 'leaf
         '(progn
            (leaf-init)))))

    ((leaf leaf
       :commands leaf leaf-pairp leaf-plist-get)
     (prog1 'leaf
       (autoload #'leaf "leaf" nil t)
       (autoload #'leaf-pairp "leaf" nil t)
       (autoload #'leaf-plist-get "leaf" nil t)))

    ((leaf leaf
       :commands leaf (leaf-pairp leaf-plist-get))
     (prog1 'leaf
       (autoload #'leaf "leaf" nil t)
       (autoload #'leaf-pairp "leaf" nil t)
       (autoload #'leaf-plist-get "leaf" nil t)))

    ((leaf leaf
       :commands leaf (leaf-pairp leaf-plist-get (leaf
                                                   (leaf-pairp
                                                    (leaf-pairp
                                                     (leaf-insert-after))))))
     (prog1 'leaf
       (autoload #'leaf "leaf" nil t)
       (autoload #'leaf-pairp "leaf" nil t)
       (autoload #'leaf-plist-get "leaf" nil t)
       (autoload #'leaf-insert-after "leaf" nil t)))))

(cort-deftest-with-macroexpand leaf/pre-setq
  '(((leaf alloc
       :pre-setq `((gc-cons-threshold . ,(* 512 1024 1024))
                   (garbage-collection-messages . t))
       :require t)
     (prog1 'alloc
       (setq gc-cons-threshold 536870912)
       (setq garbage-collection-messages t)
       (require 'alloc)))

    ((leaf alloc
       :pre-setq ((gc-cons-threshold . 536870912)
                  (garbage-collection-messages . t))
       :require t)
     (prog1 'alloc
       (setq gc-cons-threshold 536870912)
       (setq garbage-collection-messages t)
       (require 'alloc)))

    ((leaf leaf
       :pre-setq
       (leaf-backend-bind . 'bind-key)
       (leaf-backend-bind* . 'bind-key)
       :require t)
     (prog1 'leaf
       (setq leaf-backend-bind 'bind-key)
       (setq leaf-backend-bind* 'bind-key)
       (require 'leaf)))

    ;; ((leaf leaf
    ;;    :pre-setq (leaf-backend-bind leaf-backend-bind* . 'bind-key)
    ;;    :require t)
    ;;  (progn
    ;;    (setq leaf-backend-bind 'bind-key)
    ;;    (setq leaf-backend-bind* 'bind-key)
    ;;    (require 'leaf)))

    ;; ((leaf leaf
    ;;    :pre-setq ((leaf-backend-bind) leaf-backend-bind* . 'bind-key)
    ;;    :require t)
    ;;  (progn
    ;;    (setq leaf-backend-bind 'bind-key)
    ;;    (setq leaf-backend-bind* 'bind-key)
    ;;    (require 'leaf)))

    ;; ((leaf leaf
    ;;    :pre-setq ((leaf-backend-bind leaf-backend-bind*) . 'bind-key)
    ;;    :require t)
    ;;  (progn
    ;;    (setq leaf-backend-bind 'bind-key)
    ;;    (setq leaf-backend-bind* 'bind-key)
    ;;    (require 'leaf)))
    ))

(cort-deftest-with-macroexpand leaf/init
  '(((leaf leaf
       :init (leaf-pre-init)
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :init (progn
               (leaf-pre-init)
               (leaf-pre-init-after))
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (progn
         (leaf-pre-init)
         (leaf-pre-init-after))
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :init
       (leaf-pre-init)
       (leaf-pre-init-after)
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (leaf-pre-init-after)
       (require 'leaf)
       (leaf-init)))))

(cort-deftest-with-macroexpand leaf/require
  '(((leaf leaf
       :init (leaf-pre-init)
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :require nil
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :require leaf leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (require 'leaf)
       (require 'leaf-polyfill)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :require t
       :require leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (require 'leaf)
       (require 'leaf-polyfill)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :require t leaf-polyfill
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (require 'leaf)
       (require 'leaf-polyfill)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-pre-init)
       :require (leaf leaf-polyfill leaf-sub leaf-subsub)
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (require 'leaf)
       (require 'leaf-polyfill)
       (require 'leaf-sub)
       (require 'leaf-subsub)
       (leaf-init)))))

(cort-deftest-with-macroexpand leaf/setq
  '(((leaf alloc
       :setq `((gc-cons-threshold . ,(* 512 1024 1024))
               (garbage-collection-messages . t))
       :require t)
     (prog1 'alloc
       (require 'alloc)
       (setq gc-cons-threshold 536870912)
       (setq garbage-collection-messages t)))

    ((leaf alloc
       :setq ((gc-cons-threshold . 536870912)
              (garbage-collection-messages . t))
       :require t)
     (prog1 'alloc
       (require 'alloc)
       (setq gc-cons-threshold 536870912)
       (setq garbage-collection-messages t)))

    ((leaf leaf
       :setq
       (leaf-backend-bind . 'bind-key)
       (leaf-backend-bind* . 'bind-key)
       :require t)
     (prog1 'leaf
       (require 'leaf)
       (setq leaf-backend-bind 'bind-key)
       (setq leaf-backend-bind* 'bind-key)))

    ;; ((leaf leaf
    ;;    :setq (leaf-backend-bind leaf-backend-bind* . 'bind-key)
    ;;    :require t)
    ;;  (progn
    ;;    (require 'leaf)
    ;;    (setq leaf-backend-bind 'bind-key)
    ;;    (setq leaf-backend-bind* 'bind-key)))

    ;; ((leaf leaf
    ;;    :setq ((leaf-backend-bind) leaf-backend-bind* . 'bind-key)
    ;;    :require t)
    ;;  (progn
    ;;    (require 'leaf)
    ;;    (setq leaf-backend-bind 'bind-key)
    ;;    (setq leaf-backend-bind* 'bind-key)))

    ;; ((leaf leaf
    ;;    :setq ((leaf-backend-bind leaf-backend-bind*) . 'bind-key)
    ;;    :require t)
    ;;  (progn
    ;;    (require 'leaf)
    ;;    (setq leaf-backend-bind 'bind-key)
    ;;    (setq leaf-backend-bind* 'bind-key)))
    ))

(cort-deftest-with-macroexpand leaf/setq-default
  '(((leaf alloc
       :setq-default `((gc-cons-threshold . ,(* 512 1024 1024))
                       (garbage-collection-messages . t))
       :require t)
     (prog1 'alloc
       (require 'alloc)
       (setq-default gc-cons-threshold 536870912)
       (setq-default garbage-collection-messages t)))

    ((leaf alloc
       :setq-default ((gc-cons-threshold . 536870912)
                      (garbage-collection-messages . t))
       :require t)
     (prog1 'alloc
       (require 'alloc)
       (setq-default gc-cons-threshold 536870912)
       (setq-default garbage-collection-messages t)))

    ((leaf leaf
       :setq-default
       (leaf-backend-bind . 'bind-key)
       (leaf-backend-bind* . 'bind-key)
       :require t)
     (prog1 'leaf
       (require 'leaf)
       (setq-default leaf-backend-bind 'bind-key)
       (setq-default leaf-backend-bind* 'bind-key)))

    ;; ((leaf leaf
    ;;    :setq-default (leaf-backend-bind leaf-backend-bind* . 'bind-key)
    ;;    :require t)
    ;;  (progn
    ;;    (require 'leaf)
    ;;    (setq-default leaf-backend-bind 'bind-key)
    ;;    (setq-default leaf-backend-bind* 'bind-key)))

    ;; ((leaf leaf
    ;;    :setq-default ((leaf-backend-bind) leaf-backend-bind* . 'bind-key)
    ;;    :require t)
    ;;  (progn
    ;;    (require 'leaf)
    ;;    (setq-default leaf-backend-bind 'bind-key)
    ;;    (setq-default leaf-backend-bind* 'bind-key)))

    ;; ((leaf leaf
    ;;    :setq-default ((leaf-backend-bind leaf-backend-bind*) . 'bind-key)
    ;;    :require t)
    ;;  (progn
    ;;    (require 'leaf)
    ;;    (setq-default leaf-backend-bind 'bind-key)
    ;;    (setq-default leaf-backend-bind* 'bind-key)))
    ))

(cort-deftest-with-macroexpand leaf/config
  '(((leaf leaf
       :init (leaf-pre-init)
       :require t
       :config (leaf-init))
     (prog1 'leaf
       (leaf-pre-init)
       (require 'leaf)
       (leaf-init)))

    ((leaf leaf
       :init (leaf-init)
       :require t
       :config (progn
                 (leaf-pre-init)
                 (leaf-pre-init-after)))
     (prog1 'leaf
       (leaf-init)
       (require 'leaf)
       (progn
         (leaf-pre-init)
         (leaf-pre-init-after))))

    ((leaf leaf
       :init (leaf-init)
       :require t
       :config
       (leaf-pre-init)
       (leaf-pre-init-after))
     (prog1 'leaf
       (leaf-init)
       (require 'leaf)
       (leaf-pre-init)
       (leaf-pre-init-after)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  System keywords
;;

(cort-deftest-with-macroexpand leaf/leaf-autoload
  '(((leaf leaf
       :commands leaf
       :config (leaf-init))
     (prog1 'leaf
       (autoload #'leaf "leaf" nil t)
       (eval-after-load 'leaf
         '(progn
            (leaf-init)))))

    ((leaf leaf
       :leaf-autoload nil
       :commands leaf
       :config (leaf-init))
     (prog1 'leaf
       (eval-after-load 'leaf
         '(progn
            (leaf-init)))))))

(cort-deftest-with-macroexpand leaf/leaf-defer
  '(((leaf leaf
       :commands leaf
       :config (leaf-init))
     (prog1 'leaf
       (autoload #'leaf "leaf" nil t)
       (eval-after-load 'leaf
         '(progn
            (leaf-init)))))

    ((leaf leaf
       :leaf-defer nil
       :commands leaf
       :config (leaf-init))
     (prog1 'leaf
       (autoload #'leaf "leaf" nil t)
       (leaf-init)))))

(cort-deftest-with-macroexpand-let leaf/leaf-protect
    ((leaf-expand-leaf-protect t))
  '(((leaf leaf
       :config (leaf-init))
     (prog1 'leaf
       (leaf-handler-leaf-protect leaf
         (leaf-init))))

    ((leaf leaf
       :leaf-protect nil
       :config (leaf-init))
     (prog1 'leaf
       (leaf-init)))

    ((leaf leaf
       :leaf-protect t nil
       :config (leaf-init))
     (prog1 'leaf
       (leaf-handler-leaf-protect leaf
         (leaf-init))))

    ((leaf-handler-leaf-protect leaf
       (leaf-load)
       (leaf-init))
     (condition-case err
         (progn
           (leaf-load)
           (leaf-init))
       (error
        (leaf-error "Error in `leaf' block.  Error msg: %s"
                    (error-message-string err)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support leaf macros
;;

(cort-deftest-with-macroexpand leaf/handler-package
  '(((leaf macrostep :ensure t)
     (prog1 'macrostep
       (leaf-handler-package macrostep macrostep nil))

     ((leaf-handler-package macrostep macrostep nil)
      (unless
          (package-installed-p 'macrostep)
        (condition-case err
            (progn
              (unless (assoc 'macrostep package-archive-contents)
                (package-refresh-contents))
              (package-install 'macrostep))
          (error
           (condition-case err
               (progn
                 (package-refresh-contents)
                 (package-install 'macrostep))
             (error
              (leaf-error "In `macrostep' block, failed to :package of macrostep.  Error msg: %s"
                          (error-message-string err)))))))))))

(when (version< "24.0" emacs-version)
  (cort-deftest-with-macroexpand leaf/leaf-key
    '(((leaf-key "C-M-i" 'flyspell-correct-wrapper)
       (let* ((old (lookup-key global-map (kbd "C-M-i")))
              (value `(("C-M-i" . global-map) flyspell-correct-wrapper ,(and old (not (numberp old)) old))))
         (push value leaf-key-bindlist)
         (define-key global-map (kbd "C-M-i") 'flyspell-correct-wrapper)))

      ((leaf-key [remap backward-sentence] 'sh-beginning-of-command)
       (let* ((old (lookup-key global-map [remap backward-sentence]))
              (value `(("<remap> <backward-sentence>" . global-map) sh-beginning-of-command ,(and old (not (numberp old)) old))))
         (push value leaf-key-bindlist)
         (define-key global-map [remap backward-sentence] 'sh-beginning-of-command)))

      ((leaf-key "C-M-i" 'flyspell-correct-wrapper 'c-mode-map)
       (let* ((old (lookup-key c-mode-map (kbd "C-M-i")))
              (value `(("C-M-i" . c-mode-map) flyspell-correct-wrapper ,(and old (not (numberp old)) old))))
         (push value leaf-key-bindlist)
         (define-key c-mode-map (kbd "C-M-i") 'flyspell-correct-wrapper)))

      ((leaf-key [remap backward-sentence] 'sh-beginning-of-command)
       (let* ((old (lookup-key global-map [remap backward-sentence]))
              (value `(("<remap> <backward-sentence>" . global-map) sh-beginning-of-command ,(and old (not (numberp old)) old))))
         (push value leaf-key-bindlist)
         (define-key global-map [remap backward-sentence] 'sh-beginning-of-command)))

      ((leaf-key (vector 'key-chord ?i ?j) 'undo nil)
       (let* ((old (lookup-key global-map (vector 'key-chord 105 106)))
              (value `(("<key-chord> i j" . global-map) undo ,(and old (not (numberp old)) old))))
         (push value leaf-key-bindlist)
         (define-key global-map (vector 'key-chord 105 106) 'undo))))))

(cort-deftest-with-macroexpand leaf/leaf-key*
  '(((leaf-key* "C-M-i" 'flyspell-correct-wrapper)
     (leaf-key "C-M-i" 'flyspell-correct-wrapper 'leaf-key-override-global-map))

    ((leaf-key* [remap backward-sentence] 'sh-beginning-of-command)
     (leaf-key [remap backward-sentence] 'sh-beginning-of-command 'leaf-key-override-global-map))))

(cort-deftest-with-macroexpand leaf/leaf-keys
  '(((leaf-keys ("C-M-i" . flyspell-correct-wrapper))
     (leaf-key "C-M-i"
               #'flyspell-correct-wrapper))

    ((leaf-keys (("C-M-i" . flyspell-correct-wrapper)))
     (leaf-key "C-M-i"
               #'flyspell-correct-wrapper))

    ((leaf-keys (("C-c C-n" . go-run)
                 ("C-c ."   . go-test-current-test)))
     (progn
       (leaf-key "C-c C-n" #'go-run)
       (leaf-key "C-c ." #'go-test-current-test)))

    ((leaf-keys (:go-mode-map ("C-M-i" . flyspell-correct-wrapper)))
     (progn
       (leaf-key "C-M-i" #'flyspell-correct-wrapper 'go-mode-map)))

    ((leaf-keys (:go-mode-map
                 ("C-c C-n" . go-run)
                 ("C-c ."   . go-test-current-test)))
     (progn
       (leaf-key "C-c C-n" #'go-run 'go-mode-map)
       (leaf-key "C-c ." #'go-test-current-test 'go-mode-map)))

    ((leaf-keys (:go-mode-map (("C-c C-n" . go-run)
                               ("C-c ."   . go-test-current-test))))
     (progn
       (leaf-key "C-c C-n" #'go-run 'go-mode-map)
       (leaf-key "C-c ." #'go-test-current-test 'go-mode-map)))

    ((leaf-keys (:go-mode-map
                 :package go-mode
                 ("C-M-i" . flyspell-correct-wrapper)))
     (eval-after-load 'go-mode
       '(progn
          (leaf-key "C-M-i" #'flyspell-correct-wrapper 'go-mode-map))))

    ((leaf-keys (:go-mode-map
                 :package go-mode
                 (("C-c C-n" . go-run)
                  ("C-c ."   . go-test-current-test))))
     (eval-after-load 'go-mode
       '(progn
          (leaf-key "C-c C-n" #'go-run 'go-mode-map)
          (leaf-key "C-c ." #'go-test-current-test 'go-mode-map))))

    ((leaf-keys (:go-mode-map
                 :package (cc-mode go-mode)
                 (("C-c C-n" . go-run)
                  ("C-c ."   . go-test-current-test))))
     (eval-after-load 'go-mode
       '(eval-after-load 'cc-mode
          '(progn
             (leaf-key "C-c C-n" #'go-run 'go-mode-map)
             (leaf-key "C-c ." #'go-test-current-test 'go-mode-map)))))

    ((leaf-keys (:go-mode-map
                 :package (cc-mode go-mode)
                 (("C-c C-n" . go-run)
                  ("C-c ."   . go-test-current-test))))
     (eval-after-load 'go-mode
       '(eval-after-load 'cc-mode
          '(progn
             (leaf-key "C-c C-n" #'go-run 'go-mode-map)
             (leaf-key "C-c ." #'go-test-current-test 'go-mode-map)))))

    ((leaf-keys ((:isearch-mode-map
                  ("M-o" . isearch-moccur)
                  ("M-O" . isearch-moccur-all))
                 (:go-mode-map
                  :package (cc-mode go-mode)
                  (("C-c C-n" . go-run)
                   ("C-c ."   . go-test-current-test)))))
     (progn
       (progn
         (leaf-key "M-o"
                   #'isearch-moccur
                   'isearch-mode-map)
         (leaf-key "M-O"
                   #'isearch-moccur-all
                   'isearch-mode-map))
       (eval-after-load 'go-mode
         '(eval-after-load 'cc-mode
            '(progn
               (leaf-key "C-c C-n"
                         #'go-run
                         'go-mode-map)
               (leaf-key "C-c ."
                         #'go-test-current-test
                         'go-mode-map))))))

    ((leaf-keys (("C-c C-n" . go-run)
                 ("C-c ."   . go-test-current-test)
                 (:isearch-mode-map
                  ("M-o" . isearch-moccur)
                  ("M-O" . isearch-moccur-all))
                 (:go-mode-map
                  :package (cc-mode go-mode)
                  (("C-c C-n" . go-run)
                   ("C-c ."   . go-test-current-test)))))
     (progn
       (leaf-key "C-c C-n" #'go-run)
       (leaf-key "C-c ." #'go-test-current-test)
       (progn
         (leaf-key "M-o" #'isearch-moccur 'isearch-mode-map)
         (leaf-key "M-O" #'isearch-moccur-all 'isearch-mode-map))
       (eval-after-load 'go-mode
         '(eval-after-load 'cc-mode
            '(progn
               (leaf-key "C-c C-n" #'go-run 'go-mode-map)
               (leaf-key "C-c ." #'go-test-current-test 'go-mode-map))))))

    ((leaf-keys (([remap compile] . go-run)
                 ("C-c ."   . go-test-current-test)))
     (progn
       (leaf-key [remap compile] #'go-run)
       (leaf-key "C-c ." #'go-test-current-test)))

    ((leaf-keys (((vector 'key-chord ?i ?j) . undo)
                 ([remap compile] . go-run)
                 ("C-c ."   . go-test-current-test)))
     (progn
       (leaf-key (vector 'key-chord 105 106) #'undo)
       (leaf-key [remap compile] #'go-run)
       (leaf-key "C-c ." #'go-test-current-test)))

    ((leaf-keys (:go-mode-map
                 :package go-mode
                 (((vector 'key-chord ?i ?j) . undo)
                  ("C-c C-n" . go-run)
                  ("C-c ."   . go-test-current-test))))
     (eval-after-load 'go-mode
       '(progn
          (leaf-key (vector 'key-chord 105 106) #'undo 'go-mode-map)
          (leaf-key "C-c C-n" #'go-run 'go-mode-map)
          (leaf-key "C-c ." #'go-test-current-test 'go-mode-map))))))

(cort-deftest-with-macroexpand leaf/leaf-keys-dryrun
  '(((leaf-keys ("C-M-i" . flyspell-correct-wrapper) flyspell)
     '((("C-M-i" . flyspell-correct-wrapper))
       (flyspell-correct-wrapper)))

    ((leaf-keys (("C-M-i" . flyspell-correct-wrapper)) flyspell)
     '((("C-M-i" . flyspell-correct-wrapper))
       (flyspell-correct-wrapper)))

    ((leaf-keys (("C-c C-n" . go-run)
                 ("C-c ."   . go-test-current-test))
                go-mode)
     '((("C-c C-n" . go-run)
        ("C-c ." . go-test-current-test))
       (go-run go-test-current-test)))

    ((leaf-keys (:go-mode-map ("C-M-i" . flyspell-correct-wrapper)) go-mode)
     '(((:go-mode-map :package go-mode
                      ("C-M-i" . flyspell-correct-wrapper)))
       (flyspell-correct-wrapper)))

    ((leaf-keys (:go-mode-map
                 ("C-c C-n" . go-run)
                 ("C-c ."   . go-test-current-test))
                go-mode)
     '(((:go-mode-map :package go-mode
                      ("C-c C-n" . go-run)
                      ("C-c ." . go-test-current-test)))
       (go-run go-test-current-test)))

    ((leaf-keys (:go-mode-map
                 :package go-mode
                 ("C-M-i" . flyspell-correct-wrapper))
                go-mode)
     '(((:go-mode-map :package go-mode
                      ("C-M-i" . flyspell-correct-wrapper)))
       (flyspell-correct-wrapper)))

    ((leaf-keys (:go-mode-map
                 :package go-mode
                 (("C-c C-n" . go-run)
                  ("C-c ."   . go-test-current-test)))
                go-mode)
     '(((:go-mode-map :package go-mode
                      (("C-c C-n" . go-run)
                       ("C-c ." . go-test-current-test))))
       (go-run go-test-current-test)))

    ((leaf-keys (:go-mode-map
                 :package (cc-mode go-mode)
                 (("C-c C-n" . go-run)
                  ("C-c ."   . go-test-current-test)))
                go-mode)
     '(((:go-mode-map :package
                      (cc-mode go-mode)
                      (("C-c C-n" . go-run)
                       ("C-c ." . go-test-current-test))))
       (go-run go-test-current-test)))

    ((leaf-keys (:go-mode-map
                 :package (cc-mode go-mode)
                 (("C-c C-n" . go-run)
                  ("C-c ."   . go-test-current-test)))
                go-mode)
     '(((:go-mode-map :package
                      (cc-mode go-mode)
                      (("C-c C-n" . go-run)
                       ("C-c ." . go-test-current-test))))
       (go-run go-test-current-test)))

    ((leaf-keys ((:isearch-mode-map
                  ("M-o" . isearch-moccur)
                  ("M-O" . isearch-moccur-all))
                 (:go-mode-map
                  :package (cc-mode go-mode)
                  (("C-c C-n" . go-run)
                   ("C-c ."   . go-test-current-test))))
                go-mode)
     '(((:isearch-mode-map :package go-mode
                           ("M-o" . isearch-moccur)
                           ("M-O" . isearch-moccur-all))
        (:go-mode-map :package
                      (cc-mode go-mode)
                      (("C-c C-n" . go-run)
                       ("C-c ." . go-test-current-test))))
       (isearch-moccur isearch-moccur-all go-run go-test-current-test)))

    ((leaf-keys (("C-c C-n" . go-run)
                 ("C-c ."   . go-test-current-test)
                 (:isearch-mode-map
                  ("M-o" . isearch-moccur)
                  ("M-O" . isearch-moccur-all))
                 (:go-mode-map
                  :package (cc-mode go-mode)
                  (("C-c C-n" . go-run)
                   ("C-c ."   . go-test-current-test))))
                go-mode)
     '((("C-c C-n" . go-run)
        ("C-c ." . go-test-current-test)
        (:isearch-mode-map :package go-mode
                           ("M-o" . isearch-moccur)
                           ("M-O" . isearch-moccur-all))
        (:go-mode-map :package
                      (cc-mode go-mode)
                      (("C-c C-n" . go-run)
                       ("C-c ." . go-test-current-test))))
       (go-run go-test-current-test isearch-moccur isearch-moccur-all go-run go-test-current-test)))

    ((leaf-keys (([remap compile] . go-run)
                 ("C-c ."   . go-test-current-test))
                go-mode)
     '((([remap compile] . go-run)
        ("C-c ." . go-test-current-test))
       (go-run go-test-current-test)))

    ((leaf-keys (((vector 'key-chord ?i ?j) . undo)
                 ([remap compile] . go-run)
                 ("C-c ."   . go-test-current-test))
                go-mode)
     '((((vector 'key-chord 105 106) . undo)
        ([remap compile] . go-run)
        ("C-c ." . go-test-current-test))
       (undo go-run go-test-current-test)))

    ((leaf-keys (:go-mode-map
                 :package go-mode
                 (((vector 'key-chord ?i ?j) . undo)
                  ("C-c C-n" . go-run)
                  ("C-c ."   . go-test-current-test)))
                go-mode)
     '(((:go-mode-map :package go-mode
                      (((vector 'key-chord 105 106) . undo)
                       ("C-c C-n" . go-run)
                       ("C-c ." . go-test-current-test))))
       (undo go-run go-test-current-test)))

    ((leaf-keys (isearch-mode-map
                 :package isearch
                 ("M-o" . isearch-moccur)
                 ("M-O" . isearch-moccur-all)))
     (eval-after-load 'isearch
       '(progn
          (leaf-key "M-o" #'isearch-moccur 'isearch-mode-map)
          (leaf-key "M-O" #'isearch-moccur-all 'isearch-mode-map))))))

(cort-deftest-with-macroexpand leaf/leaf-keys*
  '(((leaf-keys* ("C-M-i" . flyspell-correct-wrapper))
     (leaf-keys
      (:leaf-key-override-global-map
       ("C-M-i" . flyspell-correct-wrapper))))

    ((leaf-keys* (("C-c C-n" . go-run)
                  ("C-c ."   . go-test-current-test)))
     (leaf-keys
      (:leaf-key-override-global-map
       ("C-c C-n" . go-run)
       ("C-c ." . go-test-current-test))))))

(provide 'leaf-tests)
;;; leaf-tests.el ends here
