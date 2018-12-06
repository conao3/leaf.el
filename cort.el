;;; cort.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
;; Keywords: test

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

(eval-when-compile
  (require 'cl))

(defgroup cort nil
  "Simplify elisp test framework."
  :group 'lisp)

(defconst cort-version "3.0.1"
  "cort.el version")

(defconst cort-env-symbols '(:cort-emacs<
                             :cort-emacs<=
                             :cort-emacs=
                             :cort-emacs>
                             :cort-emacs>=
                             :cort-if)
  "Test case environment symbols.")

(defvar cort-test-cases nil
  "Test list such as ((TEST-NAME VALUE) (TEST-NAME VALUE))")

(defcustom cort-debug nil
  "If non nil, turn on debug mode.

- don't throw annoying error when test fail, just output message."
  :type 'boolean
  :group 'cort)

(defcustom cort-show-backtrace nil
  "If non nil, show backtrace when fail test case."
  :type 'boolean
  :group 'cort)

(defcustom cort-enable-color (null window-system)
  "If non nil, enable color message to output with meta character.
Default, enable color if run test on CUI.
`window-system' returns nil on CUI"
  :type 'boolean
  :group 'cort)

(defcustom cort-header-message
  (if cort-enable-color
      "\n\e[33mRunning %d tests...\e[m\n"
    "\nRunning %d tests...\n")
  "Header message"
  :type 'string
  :group 'cort)

(defcustom cort-passed-label
  (if cort-enable-color
      "\e[36m[PASSED] \e[m"
    "[PASSED] ")
  "Passed label."
  :type 'string
  :group 'cort)

(defcustom cort-fail-label
  (if cort-enable-color
      "\e[31m[FAILED] \e[m"
    "[FAILED] ")
  "Fail label."
  :type 'string
  :group 'cort)

(defcustom cort-error-label
  (if cort-enable-color
      "\e[31m<ERROR>  \e[m"
    "<<ERROR>>")
  "Fail label."
  :type 'string
  :group 'cort)

(defcustom cort-error-message
  (if cort-enable-color
      "\e[31m===== Run %d Tests, %d Expected, %d Failed, %d Errored on Emacs-%s =====\n\e[m"
    "===== Run %d Tests, %d Expected, %d Failed, %d Errored on Emacs-%s =====\n")
  "Error message"
  :type 'string
  :group 'cort)

(defcustom cort-passed-message
  (if cort-enable-color
      "\e[34m===== Run %d Tests, %d Expected, %d Failed, %d Errored on Emacs-%s =====\n\n\e[m"
    "===== Run %d Tests, %d Expected, %d Failed, %d Errored on Emacs-%s =====\n\n")
  "Error message"
  :type 'string
  :group 'cort)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  for old Emacs
;;

(defmacro cort-inc (var &optional step)
  "increment VAR. If given STEP, increment VAR by STEP.
Emacs-22 doesn't support `incf'."
  (declare (indent 1) (debug t))
  `(setq ,var (+ ,var ,(if step step 1))))

;; defalias cl-symbols for old Emacs.
(when (version< emacs-version "24.0")
  (mapc (lambda (x)
          (defalias (intern (format "cl-%s" x)) x))
        '(multiple-value-bind)))

(defmacro cort-case (fn var &rest conds)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  small functions
;;

(defmacro cort-aif (test-form* then-form &rest else-form)
  "Anaphoric if macro.
This macro expansion is implemented carefully so that sexp is not 
evaluated multiple times.

\(fn (ASYM TEST-FORM) THEN-FORM [ELSE-FORM...])"
  (declare (indent 2) (debug t))
  `(let ((,(car test-form*) ,(cadr test-form*)))
     (if ,(car test-form*) ,then-form ,@else-form)))

(defmacro cort-asetq (sym* &optional body)
  "Anaphoric setq macro.

\(fn (ASYM SYM) &optional BODY)"
  (declare (indent 1))
  `(let ((,(car sym*) ,(cadr sym*)))
     (setq ,(cadr sym*) ,body)))

(defmacro cort-alet (varlist* &rest body)
  "Anaphoric let macro. Return first arg value.
CAUTION:
`it' has first var value, it is NOT updated if var value changed.

(macroexpand
 '(cort-alet (it ((result t)))
  (princ it)))
=> (let* ((result t)
          (it result))
     (progn (princ it))
     result)

\(fn (ASYM (VARLIST...)) &rest BODY)"
  (declare (debug t) (indent 1))
  `(let* (,@(cadr varlist*)
          (,(car varlist*) ,(caar (cadr varlist*))))
     (progn ,@body)
     ,(caar (cadr varlist*))))

(defmacro cort-with-gensyms (syms &rest body)
  "Create `let' block with `gensym'ed variables.

\(fn (SYM...) &rest body)"
  (declare (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (gensym)))
                 syms)
     ,@body))

(defsubst cort-truep (var)
  "Return t if var is non-nil."
  (not (not var)))

(defsubst cort-pp (sexp)
  "Return pretty printed SEXP string."
  (replace-regexp-in-string "\n+$" "" (pp-to-string sexp)))

(defsubst cort-list-digest (fn list)
  "Make digest from LIST using FN (using 2 args).
Example:
(list-digest (lambda (a b) (or a b))
  '(nil nil t))
=> nil

(list-digest (lambda (a b) (or a b))
  '(nil nil t))
=> nil"
  (declare (indent 1))
  (let ((result))
    (mapc (lambda (x) (setq result (funcall fn x result))) list)
    result))

(defsubst cort-list-memq (symlist list)
  "Return t if LIST contained element of SYMLIST."
  (cort-truep
   (cort-list-digest (lambda (a b) (or a b))
                     (mapcar (lambda (x) (memq x list)) symlist))))

(defsubst cort-get-funcsym (method)
  "Return function symbol from symbol such as :eq"
  (intern
   (replace-regexp-in-string "^:+" "" (symbol-name method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  support functions
;;

(defun cort-get-value-fn (env)
  "Recursive search function for `cort-get-value'."
  (cort-aif (it (plist-get env :cort-if))
            (if (eval (car it))
                (cadr it)
              (funcall #'cort-get-value-fn (member :cort-if (cddr env))))))

(defun cort-get-value (plist symbol)
  "Get reasonable value from PLIST.
Take SYMBOL value from PLIST and return the value by interpreting cort-if etc.

Example:
;; (cort-get-value
;;  '(x (:default 'a :cort-if (t 'b)))
;; 'x)
;; => 'b
;;
;; (cort-get-value
;;  '(x (:default 'a :cort-if (nil 'b)))
;;  'x)
;; => 'a"

  ;;   (let ((element (plist-get plist symbol))
  ;;    (fn (lambda (env)
  ;;               (cort-aif (it (plist-get env :cort-if))
  ;;                      (if (eval (car it))
  ;;                          (cadr it)
  ;;                        (funcall fn (member :cort-if (cddr env))))))))
  ;;     (cort-aif (it (funcall fn element))
  ;;    it
  ;;       (plist-get element :default)))
  (let ((element (plist-get plist symbol)))
    (cort-aif (it (funcall #'cort-get-value-fn element))
              it
              (plist-get element :default))))

(defun cort-test (plist)
  "Actually execute GIVEN to check it matches EXPECT.
If match, return t, otherwise return nil."

  (let ((method   (cort-get-value plist :method))
        (given    (cort-get-value plist :given))
        (expect   (cort-get-value plist :expect))
        (err-type (cort-get-value plist :err-type)))
    (cort-case #'eq method
               (:cort-error
                (eval
                 `(condition-case err
                      (eval ,given)
                    (,err-type t))))
               (_
                (let* ((funcsym (cort-get-funcsym method)))
                  (funcall funcsym (eval given) (eval expect)))))))

(defun cort-testpass (name plist)
  "Output messages for test passed."

  (let ((mesheader (format "%s %s\n" cort-passed-label name)))
    (princ (concat mesheader))))

(defun cort-testfail (name plist &optional err)
  "Output messages for test failed."

  (let ((method   (cort-get-value plist :method))
        (given    (cort-get-value plist :given))
        (expect   (cort-get-value plist :expect))
        (err-type (cort-get-value plist :err-type)))
    (let* ((failp           (not err))
           (errorp          (not failp))
           (method-errorp   (eq method :cort-error))
           (method-defaultp (not (or method-errorp))))
      (let ((mesheader) (mesmethod) (mesgiven) (mesreturned) (mesexpect)
            (meserror) (mesbacktrace))
        (setq mesgiven  (format "Given:\n%s\n" (cort-pp given)))
        (setq mesbacktrace (format "Backtrace:\n%s\n"
                                   (with-output-to-string
                                     (backtrace))))
        (progn
          (when errorp
            (setq mesheader (format "%s %s\n" cort-error-label name))
            (setq meserror  (format "Unexpected-error: %s\n" (cort-pp err))))
          (when failp
            (setq mesheader (format "%s %s\n" cort-fail-label name))))

        (progn
          (when method-defaultp
            (setq mesmethod (format "< Tested with %s >\n" method))
            (setq mesexpect (format "Expected:\n%s\n" (cort-pp expect)))
            (when failp
              (setq mesreturned (format "Returned:\n%s\n" (cort-pp (eval given))))))
          (when method-errorp
            (setq meserror  (format "Unexpected-error: %s\n" (cort-pp err)))
            (setq mesexpect (format "Expected-error:   %s\n" (cort-pp err-type)))))

        (princ (concat mesheader
                       (cort-aif (it mesmethod)   it)
                       (cort-aif (it mesgiven)    it)
                       (cort-aif (it mesreturned) it)
                       (cort-aif (it mesexpect)   it)
                       (cort-aif (it meserror)    it)
                       (if cort-show-backtrace
                           (cort-aif (it mesbacktrace) it))
                       "\n"
                       ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Define test phase
;;

(defun cort-interpret-env-keyword (env)
  "Interpret a single keyword and return sexp.
ENV is list such as (KEYWORD VALUE)"
  (let ((symbol (car env))
        (value  (cadr env)))
    (let ((keyname (prin1-to-string symbol)))
      (if (string-match (rx (group ":cort-")
                            (group (or "emacs" "if"))
                            (? (group (or "<" "<=" "=" ">=" ">"))))
                        keyname)
          (cort-case #'string= (match-string 2 keyname)
                     ("emacs"
                      (let ((condver  (car value))
                            (expected (cadr value))
                            (sign     (match-string 3 keyname)))
                        (if (string-match "^>=?$" sign)
                            (progn
                              (setq sign (replace-regexp-in-string "^>" "<" sign))
                              (list 2 `(:cort-if
                                        ((not
                                          (funcall
                                           (intern ,(concat "version" sign))
                                           emacs-version ,(prin1-to-string condver)))
                                         ,expected))))
                          (list 2 `(:cort-if
                                    ((funcall
                                      (intern ,(concat "version" sign))
                                      emacs-version ,(prin1-to-string condver))
                                     ,expected))))))
                     
                     ("if"
                      (list 2 `(:cort-if ,value))))

        (list 1 `(:default ,symbol))))))

(defun cort-normalize-env (env)
  "Return normalize test environment list.

Example:
(cort-normalize-env :eq)
=> (:default :eq)

(cort-normalize-env '('b
                     :cort-if (t 'a)))
=> (:default 'b
    :cort-if (t 'a))
"
  (cort-alet (it ((result)))
             (if (and (listp env) (cort-list-memq cort-env-symbols env))
                 (let ((i 0) (envc (length env)))
                   (while (< i envc)
                     (cl-multiple-value-bind (step value)
                         (cort-interpret-env-keyword (nthcdr i env))
                       (cort-asetq (it result)
                                   (append it value))
                       (cort-inc i step))))
               (cort-asetq (it result)
                           (append it `(:default ,env))))))

(defmacro cort-deftest (name keys)
  "Define a test case with the name A.
KEYS supported below form.

basic: (:COMPFUN FORM EXPECT)
error: (:cort-error EXPECTED-ERROR-TYPE FORM)"
  (declare (indent 1))

  (let ((symbol (car keys)))
    (if (fboundp symbol)
        (if (eq (car (symbol-function symbol)) 'macro)
            (setq keys (macroexpand keys))
          (setq keys (eval keys)))))
  
  (let ((fn #'cort-normalize-env))
    (cort-case #'eq (nth 0 keys)
               (:cort-error
                (let ((method   (funcall fn (nth 0 keys)))
                      (err-type (funcall fn (nth 1 keys)))
                      (given    (funcall fn (nth 2 keys))))
                  `(add-to-list 'cort-test-cases
                                '(,name (:cort-testcase
                                         :method   ,method
                                         :err-type ,err-type
                                         :given    ,given))
                                t)))
               (_
                (let ((method (funcall fn (nth 0 keys)))
                      (given  (funcall fn (nth 1 keys)))
                      (expect (funcall fn (nth 2 keys))))
                  (if t ;; (fboundp (cort-get-funcsym (car method)))
                      `(add-to-list 'cort-test-cases
                                    '(,name (:cort-testcase
                                             :method ,method
                                             :given  ,given
                                             :expect ,expect))
                                    t)
                    `(progn
                       (cort-testfail ',name (cdr
                                              '(:cort-testcase
                                                :method ,method
                                                :given  ,given
                                                :expect ,expect)))
                       (error "invalid test case"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Run test phase
;;

(defun cort-prune-tests ()
  "Prune all the tests."
  (interactive)
  (setq cort-test-cases nil)
  (message "prune tests completed."))

(defun cort-run-tests ()
  "Run all the tests."
  (let ((testc  (length cort-test-cases))
        (failc  0)
        (errorc 0))
    (princ (format cort-header-message (length cort-test-cases)))
    (princ (format "%s\n" (emacs-version)))

    (dolist (test cort-test-cases)
      (let* ((name  (car  test))
             (keys  (cadr test))
             (plist (cdr  keys)))       ; remove :cort-testcase symbol
        (condition-case err
            (if (cort-test plist)
                (cort-testpass name plist)
              (cort-testfail name plist)
              (cort-inc failc))
          (error
           (cort-testfail name plist err)
           (cort-inc errorc)))))

    (princ "\n\n")
    (if (or (< 0 failc) (< 0 errorc))
        (if cort-debug
            (princ "Test failed!!\n")
          (error (format cort-error-message
                         testc (- testc failc errorc) failc errorc emacs-version)))
      (princ (format cort-passed-message
                     testc (- testc failc errorc) failc errorc emacs-version)))))

(provide 'cort)
;;; cort.el ends here
