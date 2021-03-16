#+title: leaf.el
#+author: Naoya Yamashita
#+date: <2018-10-25 Thu>
#+exclude_tags: noexport

* Badges                                                           :noexport:

[[https://github.com/conao3/leaf.el][https://raw.githubusercontent.com/conao3/files/master/blob/headers/png/leaf.el.png]]
[[https://github.com/conao3/leaf.el/blob/master/LICENSE][https://img.shields.io/github/license/conao3/leaf.el.svg?style=flat-square]]
[[https://github.com/conao3/leaf.el/releases][https://img.shields.io/github/tag/conao3/leaf.el.svg?style=flat-square]]
[[https://travis-ci.org/conao3/leaf.el][https://img.shields.io/travis/conao3/leaf.el/master.svg?style=flat-square]]
[[https://app.codacy.com/project/conao3/leaf.el/dashboard][https://img.shields.io/codacy/grade/3ad51a9fc08e48f98b6eb75f8571f0b0.svg?logo=codacy&style=flat-square]]
[[https://www.patreon.com/conao3][https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon&style=flat-square]]
[[https://twitter.com/conao_3][https://img.shields.io/badge/twitter-@conao__3-blue.svg?logo=twitter&style=flat-square]]
[[https://join.slack.com/t/conao3-support/shared_invite/enQtNjUzMDMxODcyMjE1LTA4ZGRmOWYwZWE3NmE5NTkyZjk3M2JhYzU2ZmRkMzdiMDdlYTQ0ODMyM2ExOGY0OTkzMzZiMTNmZjJjY2I5NTM][https://img.shields.io/badge/chat-on_slack-blue.svg?logo=slack&style=flat-square]]
[[https://melpa.org/#/leaf][https://melpa.org/packages/leaf-badge.svg]]
[[https://stable.melpa.org/#/leaf][https://stable.melpa.org/packages/leaf-badge.svg]]

* Table of Contents                                                :noexport:

- [[Description]]
- [[Install]]
- [[Usage]]
- [[Customize]]
- [[Syntax]]
- [[Basic keywords]]
  - [[none (keyword)]]
  - [[:require keyword]]
  - [[#ensure][:ensure, :package keywords]]
  - [[:preface, :init, :config keywords]]
  - [[:defer-config keyword]]
  - [[:commands keyword]]
  - [[:after keyword]]
  - [[:bind, :bind* keywords]]
  - [[:bind-keymap, :bind-keymap* keywords]]
- [[Configure variables keywords]]
  - [[:custom, :custom*, :custom-face keywords]]
  - [[:pre-setq, :setq, :setq-default keywords]]
  - [[:setf, :push, :pre-setf, :pre-push keywords]]
- [[Configure list keywords]]
  - [[:mode, :interpreter keywords]]
  - [[:magic, :magic-fallback keywords]]
  - [[:hook keyword]]
  - [[:load-path, :load-path* keywords]]
- [[Condition keywords]]
  - [[:disabled keyword]]
  - [[:if, :when, :unless keywords]]
  - [[:emacs<, :emacs<=, :emacs=, :emacs>, :emacs>= keywords]]
- [[Byte compile keywords]]
  - [[:defun, :defvar keywords]]
- [[Documentation keywords]]
  - [[:doc, :req, :tag, :file, :url keywords]]
- [[Misc keywords]]
  - [[:global-minor-mode keyword]]
  - [[:advice, :advice-remove keywords]]
  - [[:pl-pre-setq, :pl-setq, :pl-setq-default, :pl-custom keywords]]
- [[System keywords]]
  - [[:leaf-protect keyword]]
  - [[:leaf-defer keyword]]
  - [[:leaf-autoload keyword]]
- [[Tips]]
  - [[leaf-find feature]]
- [[Information]]
  - [[Donation]]
  - [[Community]]
  - [[Contribution]]
    - [[leaf.el mechanism]]
    - [[Adding new keywords]]
  - [[Migration]]
    - [[leaf v3.0 to v4.0]]
    - [[leaf v2.0 to v3.0]]
    - [[leaf v1.0 to v2.0]]
  - [[License]]
  - [[Author]]
  - [[Contributors]]
  - [[Special Thanks]]

* Description

~leaf.el~ is yet another [[https://github.com/jwiegley/use-package][use-package]].

~leaf~ solves the stress that I feel while using the ~use-package~ for
2.5 years.  By developing from scratch, we have a cleaner and more
predictable implementation than ~use-package~.

This makes it easy to maintain and add new keywords. (see [[https://github.com/conao3/leaf-keywords.el][leaf-keywords.el]])

~leaf~ has keywords almost identical to ~use-package~, but some of
usage of the keywords is different.

The quickest way to solve problem is using ~macroexpand-1~ to see the
unfolded result if it is not what you intended.  And also there are
also a number of samples in this README and more in the [[https://github.com/conao3/leaf.el/blob/master/leaf-tests.el][test file]].

Currently, ~leaf.el~ and ~leaf-keywords.el~ has below rich keywords.

#+begin_src emacs-lisp
  (leaf-available-keywords)
  ;;=> (:disabled
  ;;    :leaf-protect
  ;;    :load-path :load-path*
  ;;    :leaf-autoload
  ;;    :doc :req :tag :file :url
  ;;    :defun :defvar :leaf-defun :leaf-defvar
  ;;    :preface
  ;;    :when :unless :if
  ;;    :emacs< :emacs<= :emacs= :emacs> :emacs>=
  ;;    :ensure :package :feather :straight :el-get
  ;;    :after
  ;;    :commands
  ;;    :bind :bind*
  ;;    :mode :interpreter :magic :magic-fallback
  ;;    :hook
  ;;    :advice :advice-remove
  ;;    :init
  ;;    :pre-setq :pl-pre-setq :auth-pre-setq
  ;;    :custom :custom* :pl-custom :auth-custom :custom-face
  ;;    :require
  ;;    :hydra :transient :combo :combo*
  ;;    :smartrep :smartrep* :chord :chord*
  ;;    :mode-hook
  ;;    :leaf-defer
  ;;    :config
  ;;    :diminish :delight
  ;;    :global-minor-mode
  ;;    :setq :setq-default
  ;;    :pl-setq :auth-setq :pl-setq-default :auth-setq-default)
#+end_src

* Install

~leaf.el~ and ~leaf-keywords.el~ can install with package.el from MELPA.

Please put the following code (~<leaf-install-code>~ to
~</leaf-install-code>~) to the top of your ~init.el~.

Package to be developed

  - [[https://github.com/conao3/feather.el][feather.el]] instead of ~package.el~ -> (Achieved! Now available but
    it is just a pakcage.el wraper)

  - leaf-key.el instead of ~bind-key~ -> (Achieved! Now ~leaf~ builtin)

#+begin_src emacs-lisp
  ;; <leaf-install-code>
  (eval-and-compile
    (customize-set-variable
     'package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
    (package-initialize)
    (unless (package-installed-p 'leaf)
      (package-refresh-contents)
      (package-install 'leaf))

    (leaf leaf-keywords
      :ensure t
      :init
      ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
      (leaf hydra :ensure t)
      (leaf el-get :ensure t)
      (leaf blackout :ensure t)

      :config
      ;; initialize leaf-keywords.el
      (leaf-keywords-init)))
  ;; </leaf-install-code>

  ;; Now you can use leaf!
  (leaf leaf-tree :ensure t)
  (leaf leaf-convert :ensure t)
  (leaf transient-dwim
    :ensure t
    :bind (("M-=" . transient-dwim-dispatch)))

  ;; You can also configure builtin package via leaf!
  (leaf cus-start
    :doc "define customization properties of builtins"
    :tag "builtin" "internal"
    :custom ((user-full-name . "Naoya Yamashita")
             (user-mail-address . "conao3@gmail.com")
             (user-login-name . "conao3")
             (truncate-lines . t)
             (menu-bar-mode . t)
             (tool-bar-mode . nil)
             (scroll-bar-mode . nil)
             (indent-tabs-mode . nil)))

  (leaf autorevert
    :doc "revert buffers when files on disk change"
    :tag "builtin"
    :custom ((auto-revert-interval . 0.1))
    :global-minor-mode global-auto-revert-mode)

  ;; Nest package configurations
  (leaf flycheck
    :doc "On-the-fly syntax checking"
    :emacs>= 24.3
    :ensure t
    :bind (("M-n" . flycheck-next-error)
           ("M-p" . flycheck-previous-error))
    :custom ((flycheck-emacs-lisp-initialize-packages . t))
    :hook (emacs-lisp-mode-hook lisp-interaction-mode-hook)
    :config
    (leaf flycheck-package
      :doc "A Flycheck checker for elisp package authors"
      :ensure t
      :config
      (flycheck-package-setup))

    (leaf flycheck-elsa
      :doc "Flycheck for Elsa."
      :emacs>= 25
      :ensure t
      :config
      (flycheck-elsa-setup))

    ;; ...
    )

  ;; ...
#+end_src

* Usage

Use ~leaf~ in your init.el like ~use-package~.

You declaratively tell the ~leaf~ to configure the package using special keywords.

~leaf~ converts your declaration into Elisp for Emacs to understand,
and Emacs executes it to configure the package.

* Customize

- ~leaf-defaults~: Default arguments for all leaf-block.
- ~leaf-expand-{{keyword}}~: If nil, not to expand that keyword.
- ~leaf-expand-minimally~: If nil, disable keywords that are not needed for debugging.
- ~leaf-default-plstore~: default ~plstore~ stored variable
- ~leaf-alias-keyword-alist~: Alist represents keyword alias.  Handle KEY is alias of VALUE.

#+begin_src emacs-lisp
    (defcustom leaf-alias-keyword-alist '((:ensure . :package))
      "The alias keyword.  KEY is treated as an alias for VALUE."
      :type 'sexp
      :group 'leaf)
#+end_src

  This default value means ~:ensure~ is alias ~:package~.

  If you want to use ~:ensure~ as ~:feather~, please set this value as
  ~((:ensure . :feather))~.  Please more info related feather is [[https://github.com/conao3/feather.el][here]].

* Syntax

All below examples are excerpts from [[https://github.com/conao3/leaf.el/blob/master/leaf-tests.el][leaf-tests.el]].

These examples are defined in the following format.
We expect ~FORM~ will be expanded to ~EXPECT~.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand TESTCASE-NAME
    '((FORM             ; will be expand by `macroexpand-1'
       EXPECT)          ; expect FORM's expansion will be EXPECT (test by `equal')

      (FORM
       EXPECT)

      ...))

  (cort-deftest-with-macroexpand-let TESTCASE-NAME
      LETFORM
    '((FORM             ; will be expand by `macroexpand-1' in LETFORM
       EXPECT)          ; expect FORM's expansion will be EXPECT (test by `equal')

      (FORM
       EXPECT)

      ...))
#+end_src

* Basic keywords

** none (keyword)

*Unlike use-package*, ~leaf~ will convert to ~nil~ when used without any keywords.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/none
    '(((leaf leaf)
       (prog1 'leaf))))
#+end_src

** :require keyword

If you want to ~require~, you must use the ~:require~ keyword explicitly.

This is ideally the exact opposite of using the ~:no-require~ keyword
in the ~use-package~ if you does not want to ~require~ it.

The leaf's ~:require~ keyword is powerful, specify ~t~ to require the
package, and specify multi symbols to ~require~ all of them.

Since the priority is lower than that of the conditional branch
keyword described later, it is possible to assign whether to ~require~
or not by the conditional branch keyword.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/require
    '(
      ;; 't will be converted leaf--name
      ((leaf leaf
         :init (leaf-pre-init)
         :when leaf-workable-p
         :require t
         :config (leaf-init))
       (prog1 'leaf
         (when leaf-workable-p
           (leaf-pre-init)
           (require 'leaf)
           (leaf-init))))

      ;; 'nil will be just ignored it
      ((leaf leaf
         :init (leaf-pre-init)
         :require nil
         :config (leaf-init))
       (prog1 'leaf
         (leaf-pre-init)
         (leaf-init)))

      ;; multi symbols will be accepted
      ((leaf leaf
         :init (leaf-pre-init)
         :require leaf leaf-polyfill
         :config (leaf-init))
       (prog1 'leaf
         (leaf-pre-init)
         (require 'leaf)
         (require 'leaf-polyfill)
         (leaf-init)))

      ;; multi keywords will be accepted
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

      ;; multi symbols in list will be accepted
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
#+end_src

** :package, :ensure keywords
   :PROPERTIES:
   :CUSTOM_ID: ensure
   :END:

~:package~ provide ~package.el~ frontend.

Because [[https://github.com/conao3/leaf-keywords.el][leaf-keywords.el]] has ~:el-get~ keyword, ~:package~ provide ~package.el~ frontend.

Since ~:ensure~ is to use ~package.el~ by default, ~:ensure~ and
~:package~ produce the same result.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/ensure
    '(
      ;; 't will be converted leaf--name
      ((leaf leaf
         :ensure t
         :config (leaf-init))
       (prog1 'leaf
         (leaf-handler-package leaf leaf nil)
         (leaf-init)))

      ;; multi symbols will be accepted
      ((leaf leaf
         :ensure t leaf-browser
         :config (leaf-init))
       (prog1 'leaf
         (leaf-handler-package leaf leaf nil)
         (leaf-handler-package leaf leaf-browser nil)
         (leaf-init)))

      ;; multi symbols in list will be accepted
      ((leaf leaf
         :ensure (feather leaf-key leaf-browser)
         :config (leaf-init))
       (prog1 'leaf
         (leaf-handler-package leaf feather nil)
         (leaf-handler-package leaf leaf-key nil)
         (leaf-handler-package leaf leaf-browser nil)
         (leaf-init)))))

  (cort-deftest-with-macroexpand leaf/handler-package
    '(
      ;; simple :ensure expandion example
      ((leaf macrostep :ensure t)
       (prog1 'macrostep
         (leaf-handler-package macrostep macrostep nil)))

      ;; `leaf-handler-package' expandion example.
      ;; If `macrostep' isn't installed, try to install.
      ;; If fail install, update local cache and retry to install.
      ((leaf-handler-package macrostep macrostep nil)
       (unless (package-installed-p 'macrostep)
         (unless (assoc 'macrostep package-archive-contents)
           (package-refresh-contents))
         (condition-case err
             (package-install 'macrostep)
           (error
            (condition-case err
                (progn
                  (package-refresh-contents)
                  (package-install 'macrostep))
              (error
               (display-warning 'leaf
                                (format "In `macrostep' block, failed to :package of macrostep.  Error msg: %s"
                                        (error-message-string err)))))))))))
#+end_src

** :preface, :init, :config keywords

These keywords are provided to directly describe elisp with various
settings that ~leaf~ does not support.

These keywords are provided to control where the arguments expand,

- ~:preface~ expands before the conditional branch keywords; ~:if~, ~when~ and ~unless~.

- ~:init~ expands after the conditional branch keyword before ~:require~.

- ~:config~ expands after ~:require~.

You don't need to put ~progn~ because ~leaf~ can receive multiple
S-expressions, but you can do so if you prefer it.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/preface
    '(
      ;; sexp will be expanded in order of :preface, :when, :require, :init, :config.
      ((leaf leaf
         :require t
         :preface (preface-init)
         :when (some-condition)
         :init (package-preconfig)
         :config (package-init))
       (prog1 'leaf
         (preface-init)
         (when (some-condition)
           (package-preconfig)
           (require 'leaf)
           (package-init))))

      ;; multi sexp will be accepted
      ((leaf leaf
         :preface
         (leaf-pre-init)
         (leaf-pre-init-after)
         :when (some-condition)
         :require t
         :init (package-preconfig)
         :config (package-init))
       (prog1 'leaf
         (leaf-pre-init)
         (leaf-pre-init-after)
         (when
             (some-condition)
           (package-preconfig)
           (require 'leaf)
           (package-init))))

      ;; you can use `progn' if you prefer it
      ((leaf leaf
         :preface (progn
                    (leaf-pre-init)
                    (leaf-pre-init-after))
         :when (some-condition)
         :require t
         :init (package-preconfig)
         :config (package-init))
       (prog1 'leaf
         (progn
           (leaf-pre-init)
           (leaf-pre-init-after))
         (when
             (some-condition)
           (package-preconfig)
           (require 'leaf)
           (package-init))))))
#+end_src

** :defer-config keyword

=:defer-config= is similar to =:config=, but with =eval-after-load= as the argument.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/defer-config
    '(((leaf leaf
         :init (leaf-pre-init)
         :defer-config (leaf-init))
       (prog1 'leaf
         (leaf-pre-init)
         (eval-after-load 'leaf
           (progn
             (leaf-init)))))

      ((leaf leaf
         :init (leaf-init)
         :defer-config
         (leaf-pre-init)
         (leaf-pre-init-after))
       (prog1 'leaf
         (leaf-init)
         (eval-after-load 'leaf
           (progn
             (leaf-pre-init)
             (leaf-pre-init-after)))))))
#+end_src

** :commands keyword

~commands~ keyword configures ~autoload~ for its leaf-block name.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/commands
    '(
      ;; specify a symbol to set to autoload function
      ((leaf leaf
         :commands leaf
         :config (leaf-init))
       (prog1 'leaf
         (autoload #'leaf "leaf" nil t)
         (eval-after-load 'leaf
           '(progn
              (leaf-init)))))

      ;; multi symbols will be accepted
      ((leaf leaf
         :commands leaf leaf-pairp leaf-plist-get)
       (prog1 'leaf
         (autoload #'leaf "leaf" nil t)
         (autoload #'leaf-pairp "leaf" nil t)
         (autoload #'leaf-plist-get "leaf" nil t)))

      ;; multi symbols in list will be accepted
      ((leaf leaf
         :commands (leaf leaf-pairp leaf-plist-get))
       (prog1 'leaf
         (autoload #'leaf "leaf" nil t)
         (autoload #'leaf-pairp "leaf" nil t)
         (autoload #'leaf-plist-get "leaf" nil t)))

      ;; It is accepted even if you specify symbol and list at the same time
      ((leaf leaf
         :commands leaf (leaf-pairp leaf-plist-get (leaf-insert-list-after)))
       (prog1 'leaf
         (autoload #'leaf "leaf" nil t)
         (autoload #'leaf-pairp "leaf" nil t)
         (autoload #'leaf-plist-get "leaf" nil t)
         (autoload #'leaf-insert-list-after "leaf" nil t)))))
#+end_src

** :after keyword

~:after~ keyword configure loading order.

*Currently it does not support :or in :after like use-package.*

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/after
    '(
      ;; 't will be converted leaf--name
      ((leaf leaf-browser
         :after leaf
         :require t
         :config (leaf-browser-init))
       (prog1 'leaf-browser
         (eval-after-load 'leaf
           '(progn
              (require 'leaf-browser)
              (leaf-browser-init)))))

      ;; multi symbols will be accepted
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

      ;; multi symbols in list will be accepted
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

      ;; duplicated symbol will be ignored
      ((leaf leaf-browser
         :after leaf (org orglyth) org org
         :require t
         :config (leaf-browser-init))
       (prog1 'leaf-browser
         (eval-after-load 'orglyth
           '(eval-after-load 'org
              '(eval-after-load 'leaf
                 '(progn
                    (require 'leaf-browser)
                    (leaf-browser-init)))))))

      ;; if specified t, assume leaf--name specified
      ((leaf leaf-browser
         :after t
         :require t
         :config (leaf-browser-init))
       (prog1 'leaf-browser
         (eval-after-load 'leaf-browser
           '(progn
              (require 'leaf-browser)
              (leaf-browser-init)))))))
#+end_src

** :bind, :bind* keywords

~:bind~ and ~:bind*~ provide frontend for keybind manager.

When defined globally, key bindings and their corresponding functions
are specified in dotted pairs.

To set it to a specific map, *place the map name as a keyword or
symbol* at the top of the list.

If you omit ~:package~, use leaf--name as ~:package~ to lazy load.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/bind
    '(
      ;; cons-cell will be accepted
      ((leaf macrostep
         :ensure t
         :bind ("C-c e" . macrostep-expand))
       (prog1 'macrostep
         (unless (fboundp 'macrostep-expand) (autoload #'macrostep-expand "macrostep" nil t))
         (declare-function macrostep-expand "macrostep")
         (leaf-handler-package macrostep macrostep nil)
         (leaf-keys (("C-c e" . macrostep-expand)))))

      ;; multi cons-cell will be accepted
      ((leaf color-moccur
         :bind
         ("M-s O" . moccur)
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (leaf-keys (("M-s O" . moccur)
                     ("M-o" . isearch-moccur)
                     ("M-O" . isearch-moccur-all)))))

      ;; multi cons-cell in list will be accepted
      ((leaf color-moccur
         :bind (("M-s O" . moccur)
                ("M-o" . isearch-moccur)
                ("M-O" . isearch-moccur-all)))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (leaf-keys (("M-s O" . moccur)
                     ("M-o" . isearch-moccur)
                     ("M-O" . isearch-moccur-all)))))

      ;; bind to nil to unbind shortcut
      ((leaf color-moccur
         :bind (("M-s" . nil)
                ("M-s o" . isearch-moccur)
                ("M-s i" . isearch-moccur-all)))
       (prog1 'color-moccur
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (leaf-keys (("M-s")
                     ("M-s o" . isearch-moccur)
                     ("M-s i" . isearch-moccur-all)))))

      ;; nested cons-cell list will be accepted
      ((leaf color-moccur
         :bind (("M-s O" . moccur)
                (("M-o" . isearch-moccur)
                 (("M-O" . isearch-moccur-all))
                 ("M-s" . isearch-moccur-some))))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-some) (autoload #'isearch-moccur-some "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (declare-function isearch-moccur-some "color-moccur")
         (leaf-keys (("M-s O" . moccur)
                     ("M-o" . isearch-moccur)
                     ("M-O" . isearch-moccur-all)
                     ("M-s" . isearch-moccur-some)))))

      ;; use keyword at first element to bind specific map
      ((leaf color-moccur
         :bind (("M-s O" . moccur)
                (:isearch-mode-map
                 ("M-o" . isearch-moccur)
                 ("M-O" . isearch-moccur-all))))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (defvar isearch-mode-map)
         (leaf-keys (("M-s O" . moccur)
                     (:isearch-mode-map
                      :package color-moccur
                      ("M-o" . isearch-moccur)
                      ("M-O" . isearch-moccur-all))))))

      ;; specific map at top-level will be accepted
      ((leaf color-moccur
         :bind
         ("M-s O" . moccur)
         (:isearch-mode-map
          ("M-o" . isearch-moccur)
          ("M-O" . isearch-moccur-all)))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (defvar isearch-mode-map)
         (leaf-keys (("M-s O" . moccur)
                     (:isearch-mode-map
                      :package color-moccur
                      ("M-o" . isearch-moccur)
                      ("M-O" . isearch-moccur-all))))))

      ;; use :package to deffering :iserch-mode-map declared
      ((leaf color-moccur
         :bind (("M-s O" . moccur)
                (:isearch-mode-map
                 :package isearch
                 ("M-o" . isearch-moccur)
                 ("M-O" . isearch-moccur-all))))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (defvar isearch-mode-map)
         (leaf-keys (("M-s O" . moccur)
                     (:isearch-mode-map
                      :package isearch
                      ("M-o" . isearch-moccur)
                      ("M-O" . isearch-moccur-all))))))

      ;; you can use symbol instead of keyword to specify map
      ((leaf color-moccur
         :bind (("M-s O" . moccur)
                (isearch-mode-map
                 :package isearch
                 ("M-o" . isearch-moccur)
                 ("M-O" . isearch-moccur-all))))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (defvar isearch-mode-map)
         (leaf-keys (("M-s O" . moccur)
                     (isearch-mode-map
                      :package isearch
                      ("M-o" . isearch-moccur)
                      ("M-O" . isearch-moccur-all))))))

      ;; you can use vectors to remap etc
      ((leaf swiper
          :ensure t
          :bind (([remap isearch-forward] . swiper)))
       (prog1 'swiper
         (unless (fboundp 'swiper) (autoload #'swiper "swiper" nil t))
         (declare-function swiper "swiper")

         (leaf-handler-package swiper swiper nil)
         (leaf-keys (([remap isearch-forward] . swiper)))))

      ((leaf files
          :bind (([(control ?x) (control ?f)] . find-file)))
       (prog1 'files
         (unless (fboundp 'find-file) (autoload #'find-file "files" nil t))
         (declare-function find-file "files")
         (leaf-keys (([(control ?x) (control ?f)] . find-file)))))))

  (cort-deftest-with-macroexpand leaf/bind*
    '(
      ;; bind* to bind override any key-bind map
      ((leaf color-moccur
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
                      ("M-O" . isearch-moccur-all)))))))
#+end_src

** :bind-keymap, :bind-keymap* keywords

~:bind-keymap~ and ~:bind-keymap*~ provide frontend for keybind manager for binding keymap.

Basic usage is same as ~:bind~ and ~:bind*~

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/bind
    '(
      ;; cons-cell will be accepted
      ((leaf macrostep
         :ensure t
         :bind ("C-c e" . macrostep-expand))
       (prog1 'macrostep
         (unless (fboundp 'macrostep-expand) (autoload #'macrostep-expand "macrostep" nil t))
         (declare-function macrostep-expand "macrostep")
         (leaf-handler-package macrostep macrostep nil)
         (leaf-keys (("C-c e" . macrostep-expand)))))

      ;; multi cons-cell will be accepted
      ((leaf color-moccur
         :bind
         ("M-s O" . moccur)
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (leaf-keys (("M-s O" . moccur)
                     ("M-o" . isearch-moccur)
                     ("M-O" . isearch-moccur-all)))))

      ;; multi cons-cell in list will be accepted
      ((leaf color-moccur
         :bind (("M-s O" . moccur)
                ("M-o" . isearch-moccur)
                ("M-O" . isearch-moccur-all)))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (leaf-keys (("M-s O" . moccur)
                     ("M-o" . isearch-moccur)
                     ("M-O" . isearch-moccur-all)))))

      ;; bind to nil to unbind shortcut
      ((leaf color-moccur
         :bind (("M-s" . nil)
                ("M-s o" . isearch-moccur)
                ("M-s i" . isearch-moccur-all)))
       (prog1 'color-moccur
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (leaf-keys (("M-s")
                     ("M-s o" . isearch-moccur)
                     ("M-s i" . isearch-moccur-all)))))

      ;; nested cons-cell list will be accepted
      ((leaf color-moccur
         :bind (("M-s O" . moccur)
                (("M-o" . isearch-moccur)
                 (("M-O" . isearch-moccur-all))
                 ("M-s" . isearch-moccur-some))))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-some) (autoload #'isearch-moccur-some "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (declare-function isearch-moccur-some "color-moccur")
         (leaf-keys (("M-s O" . moccur)
                     ("M-o" . isearch-moccur)
                     ("M-O" . isearch-moccur-all)
                     ("M-s" . isearch-moccur-some)))))

      ;; use keyword at first element to bind specific map
      ((leaf color-moccur
         :bind (("M-s O" . moccur)
                (:isearch-mode-map
                 ("M-o" . isearch-moccur)
                 ("M-O" . isearch-moccur-all))))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (defvar isearch-mode-map)
         (leaf-keys (("M-s O" . moccur)
                     (:isearch-mode-map
                      :package color-moccur
                      ("M-o" . isearch-moccur)
                      ("M-O" . isearch-moccur-all))))))

      ;; specific map at top-level will be accepted
      ((leaf color-moccur
         :bind
         ("M-s O" . moccur)
         (:isearch-mode-map
          ("M-o" . isearch-moccur)
          ("M-O" . isearch-moccur-all)))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (defvar isearch-mode-map)
         (leaf-keys (("M-s O" . moccur)
                     (:isearch-mode-map
                      :package color-moccur
                      ("M-o" . isearch-moccur)
                      ("M-O" . isearch-moccur-all))))))

      ;; use :package to deffering :iserch-mode-map declared
      ((leaf color-moccur
         :bind (("M-s O" . moccur)
                (:isearch-mode-map
                 :package isearch
                 ("M-o" . isearch-moccur)
                 ("M-O" . isearch-moccur-all))))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (defvar isearch-mode-map)
         (leaf-keys (("M-s O" . moccur)
                     (:isearch-mode-map
                      :package isearch
                      ("M-o" . isearch-moccur)
                      ("M-O" . isearch-moccur-all))))))

      ;; you can use symbol instead of keyword to specify map
      ((leaf color-moccur
         :bind (("M-s O" . moccur)
                (isearch-mode-map
                 :package isearch
                 ("M-o" . isearch-moccur)
                 ("M-O" . isearch-moccur-all))))
       (prog1 'color-moccur
         (unless (fboundp 'moccur) (autoload #'moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur) (autoload #'isearch-moccur "color-moccur" nil t))
         (unless (fboundp 'isearch-moccur-all) (autoload #'isearch-moccur-all "color-moccur" nil t))
         (declare-function moccur "color-moccur")
         (declare-function isearch-moccur "color-moccur")
         (declare-function isearch-moccur-all "color-moccur")
         (defvar isearch-mode-map)
         (leaf-keys (("M-s O" . moccur)
                     (isearch-mode-map
                      :package isearch
                      ("M-o" . isearch-moccur)
                      ("M-O" . isearch-moccur-all))))))

      ;; you can use vectors to remap etc
      ((leaf swiper
          :ensure t
          :bind (([remap isearch-forward] . swiper)))
       (prog1 'swiper
         (unless (fboundp 'swiper) (autoload #'swiper "swiper" nil t))
         (declare-function swiper "swiper")

         (leaf-handler-package swiper swiper nil)
         (leaf-keys (([remap isearch-forward] . swiper)))))

      ((leaf files
          :bind (([(control ?x) (control ?f)] . find-file)))
       (prog1 'files
         (unless (fboundp 'find-file) (autoload #'find-file "files" nil t))
         (declare-function find-file "files")
         (leaf-keys (([(control ?x) (control ?f)] . find-file)))))))

  (cort-deftest-with-macroexpand leaf/bind*
    '(
      ;; bind* to bind override any key-bind map
      ((leaf color-moccur
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
                      ("M-O" . isearch-moccur-all)))))))
#+end_src

** COMMENT :defaults keyword

~:defalts~ provide to download recommended settings for specified package.
For more detail, see [[https://github.com/conao3/leaf-defaults.git][leaf-defaults]].

#+BEGIN_SRC emacs-lisp
  (cort-deftest leaf-test/:simple-defaults
    (match-expansion-let ((leaf-backend/:ensure 'package))
     (leaf foo :ensure t :defaults t)
     '(progn
        (funcall #'leaf-backend/:ensure-package 'foo 'foo)
        (feather-install-defaults 'foo)
        (progn))))
#+END_SRC

* Configure variables keywords

** :custom, :custom*, :custom-face keywords

Now that the proper Elisp packaging practices have become widely known,
it is a best practice to use ~custom-set-variables~ to customize packages.

*Unlike use-package*, you must specify a dot pair.

You can of course set multiple variables and set the evaluation result
of the S expression to a variable.

The value set to ~custom-face~ should also be quoed to emphasize uniformity as ~leaf~.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/custom
    '(
      ;; multi cons-cell will be accepted
      ((leaf foo-package
         :custom
         (foo-package-to-enable   . t)
         (foo-package-to-disable  . nil)
         (foo-package-to-symbol   . 'symbol)
         (foo-package-to-function . #'ignore)
         (foo-package-to-lambda   . (lambda (elm) (message elm))))
       (prog1 'foo-package
         (custom-set-variables
          '(foo-package-to-enable t "Customized with leaf in foo-package block")
          '(foo-package-to-disable nil "Customized with leaf in foo-package block")
          '(foo-package-to-symbol 'symbol "Customized with leaf in foo-package block")
          '(foo-package-to-function #'ignore "Customized with leaf in foo-package block")
          '(foo-package-to-lambda (lambda (elm) (message elm)) "Customized with leaf in foo-package block"))))

      ;; multi cons-cell in list will be accepted
      ((leaf foo-package
         :custom ((foo-package-to-enable   . t)
                  (foo-package-to-disable  . nil)
                  (foo-package-to-symbol   . 'symbol)
                  (foo-package-to-function . #'ignore)
                  (foo-package-to-lambda   . (lambda (elm) (message elm)))))
       (prog1 'foo-package
         (custom-set-variables
          '(foo-package-to-enable t "Customized with leaf in foo-package block")
          '(foo-package-to-disable nil "Customized with leaf in foo-package block")
          '(foo-package-to-symbol 'symbol "Customized with leaf in foo-package block")
          '(foo-package-to-function #'ignore "Customized with leaf in foo-package block")
          '(foo-package-to-lambda (lambda (elm) (message elm)) "Customized with leaf in foo-package block"))))

      ;; distribution feature is supported
      ((leaf foo-package
         :custom (((to-enable1 to-enable2 to-enable3) . t)
                  ((to-disable1 to-disable2 to-disable3) . nil)))
       (prog1 'foo-package
         (custom-set-variables
          '(to-enable1 t "Customized with leaf in foo-package block")
          '(to-enable2 t "Customized with leaf in foo-package block")
          '(to-enable3 t "Customized with leaf in foo-package block")
          '(to-disable1 nil "Customized with leaf in foo-package block")
          '(to-disable2 nil "Customized with leaf in foo-package block")
          '(to-disable3 nil "Customized with leaf in foo-package block"))))

      ;; and mix specification is accepted
      ((leaf foo-package
         :custom
         (foo-package-to-function . #'ignore)
         ((to-symbol1 to-symbol2) . 'baz)
         (((to-enable1 to-enable2 to-enable3) . t)
          ((to-disable1 to-disable2 to-disable3) . nil)))
       (prog1 'foo-package
         (custom-set-variables
          '(foo-package-to-function #'ignore "Customized with leaf in foo-package block")
          '(to-symbol1 'baz "Customized with leaf in foo-package block")
          '(to-symbol2 'baz "Customized with leaf in foo-package block")
          '(to-enable1 t "Customized with leaf in foo-package block")
          '(to-enable2 t "Customized with leaf in foo-package block")
          '(to-enable3 t "Customized with leaf in foo-package block")
          '(to-disable1 nil "Customized with leaf in foo-package block")
          '(to-disable2 nil "Customized with leaf in foo-package block")
          '(to-disable3 nil "Customized with leaf in foo-package block"))))))

  (cort-deftest-with-macroexpand leaf/custom-face
  '(
    ;; cons-cell will be accepted
    ((leaf eruby-mode
       :custom-face
       (eruby-standard-face . '((t (:slant italic)))))
     (prog1 'eruby-mode
       (custom-set-faces
        '(eruby-standard-face ((t (:slant italic)))))))

    ;; distribution feature is supported
    ((leaf eruby-mode
       :custom-face
       ((default eruby-standard-face) . '((t (:slant italic)))))
     (prog1 'eruby-mode
       (custom-set-faces
        '(default ((t (:slant italic))))
        '(eruby-standard-face ((t (:slant italic)))))))))

  (cort-deftest-with-macroexpand leaf/custom*
    '(
      ;; multi cons-cell in list will be accepted
      ((leaf foo-package
         :custom* ((foo-package-to-enable   t)
                   (foo-package-to-disable  nil)
                   (foo-package-to-symbol   'symbol)
                   (foo-package-to-function #'ignore)
                   (foo-package-to-lambda   (lambda (elm) (message elm)))))
       (prog1 'foo-package
         (custom-set-variables
          '(foo-package-to-enable t "Customized with leaf in foo-package block")
          '(foo-package-to-disable nil "Customized with leaf in foo-package block")
          '(foo-package-to-symbol 'symbol "Customized with leaf in foo-package block")
          '(foo-package-to-function #'ignore "Customized with leaf in foo-package block")
          '(foo-package-to-lambda (lambda (elm) (message elm)) "Customized with leaf in foo-package block"))))))
#+end_src

** :pre-setq, :setq, :setq-default keywords

These keywords provide a front end to ~setq~ and ~setq-default~.

Because there are packages in the world that must be ~setq~ before doing ~require~ them,
the ~:pre-setq~ keyword is also provided to accommodate them.

The argument specified for ~:pre-setq~ is expanded before ~:require~.

You can of course configure multiple variables and set the evaluation
result of some S expression to variable.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/setq
    '(
      ;; cons-cell will be accepted
      ((leaf alloc
         :setq (gc-cons-threshold . 536870912)
         :require t)
       (prog1 'alloc
         (require 'alloc)
         (setq gc-cons-threshold 536870912)))

      ;; multi cons-cell will be accepted
      ((leaf alloc
         :setq
         (gc-cons-threshold . 536870912)
         (garbage-collection-messages . t)
         :require t)
       (prog1 'alloc
         (require 'alloc)
         (setq gc-cons-threshold 536870912)
         (setq garbage-collection-messages t)))

      ;; multi cons-cell in list will be accepted
      ((leaf alloc
         :setq ((gc-cons-threshold . 536870912)
                (garbage-collection-messages . t))
         :require t)
       (prog1 'alloc
         (require 'alloc)
         (setq gc-cons-threshold 536870912)
         (setq garbage-collection-messages t)))

      ;; use backquote and comma to set result of sexp
      ((leaf alloc
         :setq `((gc-cons-threshold . ,(* 512 1024 1024))
                 (garbage-collection-messages . t))
         :require t)
       (prog1 'alloc
         (require 'alloc)
         (setq gc-cons-threshold 536870912)
         (setq garbage-collection-messages t)))

      ;; distribution feature is supported
      ((leaf leaf
         :setq ((leaf-backend-bind leaf-backend-bind*) . 'bind-key)
         :require t)
       (prog1 'leaf
         (require 'leaf)
         (setq leaf-backend-bind 'bind-key)
         (setq leaf-backend-bind* 'bind-key)))))

  (cort-deftest-with-macroexpand leaf/pre-setq
    '(
      ;; :pre-setq setq before `require'
      ((leaf alloc
       :pre-setq `((gc-cons-threshold . ,(* 512 1024 1024))
                   (garbage-collection-messages . t))
       :require t)
     (prog1 'alloc
       (setq gc-cons-threshold 536870912)
       (setq garbage-collection-messages t)
       (require 'alloc)))))

  (cort-deftest-with-macroexpand leaf/setq-default
    '(
      ;; :setq-default to `setq-default'
      ((leaf alloc
         :setq-default `((gc-cons-threshold . ,(* 512 1024 1024))
                         (garbage-collection-messages . t))
         :require t)
       (prog1 'alloc
         (require 'alloc)
         (setq-default gc-cons-threshold 536870912)
         (setq-default garbage-collection-messages t)))))
#+end_src

** :setf, :push, :pre-setf, :pre-push keywords

These keywords provide a front end to ~setf~ and ~push~.

Note that, *unlike :setq*, it always requires a list of cons cell.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/setf
    '(
      ;; :setf require cons-cell list ONLY.
      ((leaf alloc
         :setf ((gc-cons-threshold . 536870912)
                (garbage-collection-messages . t))
         :require t)
       (prog1 'alloc
         (require 'alloc)
         (setf gc-cons-threshold 536870912)
         (setf garbage-collection-messages t)))

      ;; left value could generalized variable (alist-get, plist-get...)
      ;; note that it is specified as the car of the cons list.
      ((leaf emacs
         :setf
         (((alist-get "gnu" package-archives) . "http://elpa.gnu.org/packages/")
          ((alist-get 'vertical-scroll-bars default-frame-alist) . nil)))
       (prog1 'emacs
         (setf (alist-get "gnu" package-archives) "http://elpa.gnu.org/packages/")
         (setf (alist-get 'vertical-scroll-bars default-frame-alist) nil)))))

  (cort-deftest-with-macroexpand leaf/push
    '(
      ;; :setf require cons-cell list ONLY.
      ((leaf emacs
         :push ((package-archives . '("melpa" . "https://melpa.org/packages/"))
                (auto-mode-alist . '("\\.jpe?g\\'" . image-mode))))
       (prog1 'emacs
         (push '("melpa" . "https://melpa.org/packages/") package-archives)
         (push '("\\.jpe?g\\'" . image-mode) auto-mode-alist)))))
#+end_src

* Configure list keywords

** :mode, :interpreter keywords

~:mode~ keyword define ~auto-mode-alist~. Specifies the major-mode to enable by file extension.

~:interpreter~ keyword define ~interpreter-mode-alist~. Specifies the
major-mode to enable by file shebang.

If you pass symbol to these keyword, use leaf block name as major-mode.
If you want to specify major-mode, pass dotted pair value.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/mode
    '(
      ;; string will be accepted and use leaf--name
      ((leaf web-mode
         :mode "\\.js\\'")
       (prog1 'web-mode
         (autoload #'web-mode "web-mode" nil t)
         (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))))

      ;; multi strings will be accepted
      ((leaf web-mode
         :mode "\\.js\\'" "\\.p?html?\\'")
       (prog1 'web-mode
         (autoload #'web-mode "web-mode" nil t)
         (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.p?html?\\'" . web-mode))))

      ;; multi strings in list will be accepted
      ((leaf web-mode
         :mode ("\\.js\\'" "\\.p?html?\\'"))
       (prog1 'web-mode
         (autoload #'web-mode "web-mode" nil t)
         (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.p?html?\\'" . web-mode))))

      ;; cons-cell will be accepted
      ((leaf web-mode
         :mode ("\\.js\\'" . web-strict-mode))
       (prog1 'web-mode
         (autoload #'web-strict-mode "web-mode" nil t)
         (add-to-list 'auto-mode-alist '("\\.js\\'" . web-strict-mode))))

      ;; distribution feature is supported
      ((leaf web-mode
         :mode (("\\.js\\'" "\\.p?html?\\'") . web-strict-mode))
       (prog1 'web-mode
         (autoload #'web-strict-mode "web-mode" nil t)
         (add-to-list 'auto-mode-alist '("\\.js\\'" . web-strict-mode))
         (add-to-list 'auto-mode-alist '("\\.p?html?\\'" . web-strict-mode))))

      ;; mix specification will be accepted
      ;; open .html with `web-mode' and .js and .phtml with `web-strict-mode'
      ((leaf web-mode
         :mode ("\\.html\\'"
                (("\\.js\\'" "\\.p?html?\\'") . web-strict-mode)))
       (prog1 'web-mode
         (autoload #'web-mode "web-mode" nil t)
         (autoload #'web-strict-mode "web-mode" nil t)
         (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.js\\'" . web-strict-mode))
         (add-to-list 'auto-mode-alist '("\\.p?html?\\'" . web-strict-mode))))


      ;; if package symbol suffix doesn't suffix '-mode', add '-mode'
      ((leaf gnuplot
         :mode "\\.gp$")
       (prog1 'gnuplot
         (unless (fboundp 'gnuplot-mode)
           (autoload #'gnuplot-mode "gnuplot" nil t))
         (declare-function gnuplot-mode "gnuplot")
         (add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))))))

  (cort-deftest-with-macroexpand leaf/interpreter
    '(
      ;; same as :mode
      ((leaf ruby-mode
         :mode "\\.rb\\'" "\\.rb2\\'" ("\\.rbg\\'" . rb-mode)
         :interpreter "ruby")
       (prog1 'ruby-mode
         (autoload #'ruby-mode "ruby-mode" nil t)
         (autoload #'rb-mode "ruby-mode" nil t)
         (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
         (add-to-list 'auto-mode-alist '("\\.rb2\\'" . ruby-mode))
         (add-to-list 'auto-mode-alist '("\\.rbg\\'" . rb-mode))
         (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))))))
#+end_src

** :magic, :magic-fallback keywords

~:magic~ keyword define ~magic-mode-alist~. It is used to determine
major-mode in binary header byte.

~:magic-fallback~ keyward also define ~magic-fallback-alist~.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/magic
    '(
      ;; same as :mode
      ((leaf pdf-tools
         :magic ("%PDF" . pdf-view-mode)
         :config
         (pdf-tools-install))
       (prog1 'pdf-tools
         (autoload #'pdf-view-mode "pdf-tools" nil t)
         (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))
         (eval-after-load 'pdf-tools
           '(progn
              (pdf-tools-install)))))))

  (cort-deftest-with-macroexpand leaf/magic-fallback
    '(
      ;; same as :mode
      ((leaf pdf-tools
         :magic-fallback ("%PDF" . pdf-view-mode)
         :config
         (pdf-tools-install))
       (prog1 'pdf-tools
         (autoload #'pdf-view-mode "pdf-tools" nil t)
         (add-to-list 'magic-fallback-mode-alist '("%PDF" . pdf-view-mode))
         (eval-after-load 'pdf-tools
           '(progn
              (pdf-tools-install)))))))
#+end_src

** :hook keyword

~:hook~ keyword define ~add-hook~ via ~(add-to-list *-hook)~.

*Unlike use-package*, you must spesify the full hook name.
It makes easy to jump definition.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/hook
    '(
      ;; symbol will be accepted
      ((leaf ace-jump-mode
         :hook cc-mode-hook
         :config (ace-jump-mode))
       (prog1 'ace-jump-mode
         (unless (fboundp 'ace-jump-mode) (autoload #'ace-jump-mode "ace-jump-mode" nil t))
         (declare-function ace-jump-mode "ace-jump-mode")
         (add-hook 'cc-mode-hook #'ace-jump-mode)
         (eval-after-load 'ace-jump-mode
           '(progn
              (ace-jump-mode)))))

      ;; multi symbols will be accepted
      ((leaf ace-jump-mode
         :hook cc-mode-hook prog-mode-hook)
       (prog1 'ace-jump-mode
         (unless (fboundp 'ace-jump-mode) (autoload #'ace-jump-mode "ace-jump-mode" nil t))
         (declare-function ace-jump-mode "ace-jump-mode")
         (add-hook 'cc-mode-hook #'ace-jump-mode)
         (add-hook 'prog-mode-hook #'ace-jump-mode)))

      ;; cons-cell will be accepted
      ((leaf ace-jump-mode
         :hook (prog-mode-hook . my-ace-jump-mode))
       (prog1 'ace-jump-mode
         (unless (fboundp 'my-ace-jump-mode) (autoload #'my-ace-jump-mode "ace-jump-mode" nil t))
         (declare-function my-ace-jump-mode "ace-jump-mode")
         (add-hook 'prog-mode-hook #'my-ace-jump-mode)))

      ;; distribution feature is supported
      ((leaf ace-jump-mode
         :hook ((cc-mode-hook prog-mode-hook) . my-ace-jump-mode))
       (prog1 'ace-jump-mode
         (unless (fboundp 'my-ace-jump-mode) (autoload #'my-ace-jump-mode "ace-jump-mode" nil t))
         (declare-function my-ace-jump-mode "ace-jump-mode")
         (add-hook 'cc-mode-hook #'my-ace-jump-mode)
         (add-hook 'prog-mode-hook #'my-ace-jump-mode)))

      ;; guess leaf--name is mode
      ((leaf dired-filter
         :hook dired-mode-hook)
       (prog1 'dired-filter
         (unless (fboundp 'dired-filter-mode)
           (autoload #'dired-filter-mode "dired-filter" nil t))
         (declare-function dired-filter-mode "dired-filter")
         (add-hook 'dired-mode-hook #'dired-filter-mode)))

      ;; lambda sexp is supported
      ((leaf hook
         :hook (foo-hook . (lambda () (foo))))
       (prog1 'hook
         (add-hook 'foo-hook #'(lambda nil (foo)))))

      ;; lambda sexp with many sexps
      ((leaf hook
         :hook (foo-hook . (lambda () (foo) (bar) (baz))))
       (prog1 'hook
         (add-hook 'foo-hook #'(lambda nil (foo) (bar) (baz)))))))
#+end_src

** :load-path, :load-path* keywords

~:load-path~ is *unlike use-package*, you must specify the full path.

Use backquotes if you want the path to be relative to the current
~.emacs.d~, such as use-package.

Or use ~:load-path*~ keyword if you want to dynamic path at
~user-emacs-directory~ using ~locate-user-emacs-file~ like use-package.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/load-path
    '(
      ;; string will be accepted
      ((leaf leaf
         :load-path "~/.emacs.d/elpa-archive/leaf.el/"
         :require t
         :config (leaf-init))
       (prog1 'leaf
         (add-to-list 'load-path "~/.emacs.d/elpa-archive/leaf.el/")
         (require 'leaf)
         (leaf-init)))

      ;; multi strings will be accepted
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

      ;; multi strings in list will be accepted
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

      ;; nested strings is supported
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

      ;; duplicated value is ignored
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

      ;; use backquote and comma to configure with result of sexp
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

  (cort-deftest-with-macroexpand-let leaf/load-path*
      ((user-emacs-directory "~/.emacs.d"))
    '(
      ;; string will be accepted
      ((leaf leaf
         :load-path* "leaf.el"
         :require t
         :config (leaf-init))
       (prog1 'leaf
         (add-to-list 'load-path (locate-user-emacs-file "leaf.el"))
         (require 'leaf)
         (leaf-init)))

      ;; multi strings will be accepted
      ((leaf leaf
         :load-path*
         "leaf.el"
         "leaf-browser.el"
         :require t
         :config (leaf-init))
       (prog1 'leaf
         (add-to-list 'load-path (locate-user-emacs-file "leaf.el"))
         (add-to-list 'load-path (locate-user-emacs-file "leaf-browser.el"))
         (require 'leaf)
         (leaf-init)))

      ;; multi strings in list will be accepted
      ((leaf leaf
         :load-path* ("leaf.el" "leaf-browser.el")
         :require t
         :config (leaf-init))
       (prog1 'leaf
         (add-to-list 'load-path (locate-user-emacs-file "leaf.el"))
         (add-to-list 'load-path (locate-user-emacs-file "leaf-browser.el"))
         (require 'leaf)
         (leaf-init)))))
#+end_src

* Condition keywords

** :disabled keyword

The ~:disabled~ keyword provides the ability to temporarily ~nil~ the
output of that ~leaf~ block.

You can use multiple values for the ~:disabled~ keyword, or multiple ~:disabled~ keyword,
but ~:disabled~ only respects the value specified at the top.

It can also be said that old values can be overridden by described above.

As you can see from the internal structure of ~:disabled~, you do not
need to pass an exact ~t~ to convert it to ~nil~ because it is
comparing it by ~unless~.

#+begin_src emacs-lisp
  (defvar leaf-keywords
    (cdt
     '(:dummy
       :disabled (unless (eval (car leaf--value)) `(,@leaf--body))
       ...)))
#+end_src

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/disabled
    '(
      ;; :disabled activated by 't
      ((leaf leaf :disabled t       :config (leaf-init))
       nil)

      ;; :disabled deactivated by 'nil
      ((leaf leaf :disabled nil     :config (leaf-init))
       (prog1 'leaf
         (leaf-init)))

      ;; 't is overriden with 'nil, so :disabled deactivated
      ((leaf leaf :disabled nil t   :config (leaf-init))
       (prog1 'leaf
         (leaf-init)))

      ;; 'nil is overriden with 't, so :disabled activated
      ((leaf leaf :disabled t :disabled nil     :config (leaf-init))
       nil)))
#+end_src

** :if, :when, :unless keywords

~:if~, ~:when~, ~:unless~ keywords expect sexp return boolean or just boolean value
and wrap converted sexp specified function.

If specified multiple those keywords, evaluate sexp in ~and~.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/if
    '(
      ;; single xexp will accepted
      ((leaf leaf
         :if leafp
         :require t
         :config (leaf-init))
       (prog1 'leaf
         (if leafp
             (progn
               (require 'leaf)
               (leaf-init)))))

      ;; multi sexp will accepted and eval them in `and'
      ((leaf leaf
         :if leafp leaf-avairablep (window-system)
         :require t
         :config (leaf-init))
       (prog1 'leaf
         (if (and leafp leaf-avairablep (window-system))
             (progn
               (require 'leaf)
               (leaf-init)))))

      ;; you can use other condition keywords same time
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

      ;; you want eval sexp before any conditions, you can use :preface
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
    '(
      ;; same as :if
      ((leaf leaf
         :when leafp
         :require t
         :config (leaf-init))
       (prog1 'leaf
         (when leafp
           (require 'leaf)
           (leaf-init))))))

  (cort-deftest-with-macroexpand leaf/unless
    '(
      ;; same as :if
      ((leaf leaf
         :unless leafp
         :require t
         :config (leaf-init))
       (prog1 'leaf
         (unless leafp
           (require 'leaf)
           (leaf-init))))))
#+end_src

** :emacs<, :emacs<=, :emacs=, :emacs>, :emacs>= keywords

You can activate the leaf block depending on version of Emacs.
The keyword is in the form of ~:emacs< {{version}}~,
which is expanded as a conditional expression by analogy with the
inequality of ~Emacs < {{version}}~.  It can be a string, a number, a
quoted string or a number, and only one can be specified.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/emacs<
    '(
      ;; string will be accepted
      ((leaf leaf-keywords
         :emacs> "24.4"
         :config (leaf-keywords-init))
       (prog1 'leaf-keywords
         (when (version< "24.4" emacs-version)
           (leaf-keywords-init))))

      ;; number will be accepted
      ((leaf leaf-keywords
         :emacs> 24.4
         :config (leaf-keywords-init))
       (prog1 'leaf-keywords
         (when (version< "24.4" emacs-version)
           (leaf-keywords-init))))

      ;; quoted number will be accepted
      ((leaf leaf-keywords
         :emacs> '24.4
         :config (leaf-keywords-init))
       (prog1 'leaf-keywords
         (when (version< "24.4" emacs-version)
           (leaf-keywords-init))))

      ;; quoted string will be accepted
      ((leaf leaf-keywords
         :emacs> '"24.4"
         :config (leaf-keywords-init))
       (prog1 'leaf-keywords
         (when (version< "24.4" emacs-version)
           (leaf-keywords-init))))

      ;; one argument list will be accepted
      ((leaf leaf-keywords
         :emacs> ("24.4")
         :config (leaf-keywords-init))
       (prog1 'leaf-keywords
         (when (version< "24.4" emacs-version)
           (leaf-keywords-init))))

      ;; one argument list will be accepted
      ((leaf leaf-keywords
         :emacs> ('"24.4")
         :config (leaf-keywords-init))
       (prog1 'leaf-keywords
         (when (version< "24.4" emacs-version)
           (leaf-keywords-init))))))
#+end_src


* Byte compile keywords

** :defun, :defvar keywords

To suppress byte compilation warnings, you must make the appropriate
declarations in Elisp to tell Emacs that you are making the
appropriate calls.

This is usually done by a ~declare-function~ and an empty ~defvar~,
and ~leaf~ provides a frontend of it.

#+BEGIN_SRC emacs-lisp
  (cort-deftest-with-macroexpand leaf/defun
    '(
      ;; symbol will be accepted and use leaf--name
      ((leaf leaf
         :defun leaf)
       (prog1 'leaf
         (declare-function leaf "leaf")))

      ;; multi symbols will be accepted
      ((leaf leaf
         :defun leaf leaf-normalize-plist leaf-merge-dupkey-values-plist)
       (prog1 'leaf
         (declare-function leaf "leaf")
         (declare-function leaf-normalize-plist "leaf")
         (declare-function leaf-merge-dupkey-values-plist "leaf")))

      ;; multi symbols in list will be accepted
      ((leaf leaf
         :defun (leaf leaf-normalize-plist leaf-merge-dupkey-values-plist))
       (prog1 'leaf
         (declare-function leaf "leaf")
         (declare-function leaf-normalize-plist "leaf")
         (declare-function leaf-merge-dupkey-values-plist "leaf")))

      ;; cons-cell will be accepted
      ((leaf leaf
         :defun (lbrowser-open . leaf-browser))
       (prog1 'leaf
         (declare-function lbrowser-open "leaf-browser")))

      ;; distribution feature is supported
      ((leaf leaf
         :defun ((lbrowser-open lbrowser-close) . leaf-browser))
       (prog1 'leaf
         (declare-function lbrowser-open "leaf-browser")
         (declare-function lbrowser-close "leaf-browser")))))

  (cort-deftest-with-macroexpand leaf/defvar
    '(
      ;; symbol will be accepted
      ((leaf leaf
         :defvar leaf-var)
       (prog1 'leaf
         (defvar leaf-var)))

      ;; multi symbols will be accepted
      ((leaf leaf
         :defvar leaf-var1 leaf-var2 leaf-var3)
       (prog1 'leaf
         (defvar leaf-var1)
         (defvar leaf-var2)
         (defvar leaf-var3)))

      ;; multi symbols in list will be accepted
      ((leaf leaf
         :defvar (leaf-var1 leaf-var2 leaf-var3))
       (prog1 'leaf
         (defvar leaf-var1)
         (defvar leaf-var2)
         (defvar leaf-var3)))

      ;; nested list will be accepted
      ;; duplicated values will be ignored
      ((leaf leaf
         :defvar (leaf-var1 (leaf-var1 leaf-var2 leaf-var3)))
       (prog1 'leaf
         (defvar leaf-var1)
         (defvar leaf-var2)
         (defvar leaf-var3)))))
#+END_SRC

* Documentation keywords

** :doc, :req, :tag, :file, :url keywords

The leaf can describe the document systematically.

It should be possible to develop additional packages
that use the value specified for the document keyword, which is not currently used.

The arguments specified for this keyword have no effect on the result of the conversion.

#+BEGIN_SRC emacs-lisp
  (cort-deftest-with-macroexpand leaf/doc
    '(
      ;; any sexp will be ignored
      ((leaf leaf
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
#+END_SRC
* Misc keywords

** :global-minor-mode keyword

~:global-minor-mode~ keyword provides a front end to easily enable
minor mode.  global-minor-mode is followed by ~-mode~ and has the
custom of being activated by passing a ~1~, so make the possible
guesses accordingly.

The former guess, given ~t~, the mode in which you want ~leaf--name~
to be valid.  If ~-mode~ is not appended to the package name, add
~-mode~.  For this reason, a minor mode without ~-mode~ can not use
this keyword. Please use ~:config~.

Minor-mode function is automatically autoload as a ~leaf--name~
function, but if you want customize this, you can use cons-cell.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/global-minor-mode
    '(
      ;; symbol will be accepted
      ((leaf autorevert
         :global-minor-mode global-auto-revert-mode)
       (prog1 'autorevert
         (unless (fboundp 'global-auto-revert-mode) (autoload #'global-auto-revert-mode "autorevert" nil t))
         (declare-function global-auto-revert-mode "autorevert")
         (global-auto-revert-mode 1)))

      ;; multi strings will be accepted
      ((leaf autorevert
         :global-minor-mode global-auto-revert-mode show-paren-mode)
       (prog1 'autorevert
         (unless (fboundp 'global-auto-revert-mode) (autoload #'global-auto-revert-mode "autorevert" nil t))
         (unless (fboundp 'show-paren-mode) (autoload #'show-paren-mode "autorevert" nil t))
         (declare-function global-auto-revert-mode "autorevert")
         (declare-function show-paren-mode "autorevert")
         (global-auto-revert-mode 1)
         (show-paren-mode 1)))

      ;; multi strings in list will be accepted
      ((leaf autorevert
         :global-minor-mode (global-auto-revert-mode show-paren-mode))
       (prog1 'autorevert
         (unless (fboundp 'global-auto-revert-mode) (autoload #'global-auto-revert-mode "autorevert" nil t))
         (unless (fboundp 'show-paren-mode) (autoload #'show-paren-mode "autorevert" nil t))
         (declare-function global-auto-revert-mode "autorevert")
         (declare-function show-paren-mode "autorevert")
         (global-auto-revert-mode 1)
         (show-paren-mode 1)))

      ;; cons-cell used to controll autoload package
      ((leaf autorevert
         :global-minor-mode ((global-auto-revert-mode . autorevert)
                             (show-paren-mode . paren)))
       (prog1 'autorevert
         (unless (fboundp 'global-auto-revert-mode) (autoload #'global-auto-revert-mode "autorevert" nil t))
         (unless (fboundp 'show-paren-mode) (autoload #'show-paren-mode "paren" nil t))
         (declare-function global-auto-revert-mode "autorevert")
         (declare-function show-paren-mode "paren")
         (global-auto-revert-mode 1)
         (show-paren-mode 1)))

      ;; distribution feature is supported
      ((leaf simple
         :global-minor-mode ((line-number-mode column-number-mode) . simple))
       (prog1 'simple
         (unless (fboundp 'line-number-mode) (autoload #'line-number-mode "simple" nil t))
         (unless (fboundp 'column-number-mode) (autoload #'column-number-mode "simple" nil t))
         (declare-function line-number-mode "simple")
         (declare-function column-number-mode "simple")
         (line-number-mode 1)
         (column-number-mode 1)))

      ;; mix specification will be accepted
      ((leaf autoinsert
         :global-minor-mode (auto-insert-mode
                             ((line-number-mode column-number-mode) . simple)))
       (prog1 'autoinsert
         (unless (fboundp 'auto-insert-mode) (autoload #'auto-insert-mode "autoinsert" nil t))
         (unless (fboundp 'line-number-mode) (autoload #'line-number-mode "simple" nil t))
         (unless (fboundp 'column-number-mode) (autoload #'column-number-mode "simple" nil t))
         (declare-function auto-insert-mode "autoinsert")
         (declare-function line-number-mode "simple")
         (declare-function column-number-mode "simple")
         (auto-insert-mode 1)
         (line-number-mode 1)
         (column-number-mode 1)))

      ;; t will convert leaf--name, and suffix 'mode'
      ((leaf autorevert
         :global-minor-mode t)
       (prog1 'autorevert
         (unless (fboundp 'autorevert-mode) (autoload #'autorevert-mode "autorevert" nil t))
         (declare-function autorevert-mode "autorevert")
         (autorevert-mode 1)))

      ;; symbol not suffix 'mode', add 'mode' suffix
      ((leaf autorevert
         :global-minor-mode autorevert)
       (prog1 'autorevert
         (unless (fboundp 'autorevert-mode) (autoload #'autorevert-mode "autorevert" nil t))
         (declare-function autorevert-mode "autorevert")
         (autorevert-mode 1)))))
#+end_src

** :advice, :advice-remove keywords

~:advice~ provide frontend of ~advice-add~, and ~:advice-remove~
provide frontend of ~advice-remove~.

~:advice~ keyword accept list of ~(WHERE SYMBOL FUNCTION)~ or nested it.

You can use all ~WHERE~ symbol such as (~:around~ ~:before~ ~:after~
~:override~ ~:after-until~ ~:after-while~ ~:before-until~
~:before-while~ ~:filter-args~ ~:filter-return~)

~SYMBOL~ is the adviced function symbol, ~FUNCTION~ is advice function symbol or lambda form.

~:advice-remove~ must not specify ~WHERE~ keyword.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/advice
    '(
      ;; define advice function(s) in :preface
      ;; list like ({{place}} {{adviced-function}} {{advice-function}}) will be accepted
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

      ;; multi lists like ({{place}} {{adviced-function}} {{advice-function}}) in list is accepted
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

      ;; you can use `lambda' in {{function}} place
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
    '(
      ;; list like ({{adviced-function}} {{advice-function}}) will be accepted
      ((leaf leaf
         :advice-remove
         (matu matu-around0)
         (matu matu-before0))
       (prog1 'leaf
         (autoload #'matu-before0 "leaf" nil t)
         (autoload #'matu-around0 "leaf" nil t)
         (advice-remove 'matu #'matu-around0)
         (advice-remove 'matu #'matu-before0)))

      ;; multi lists like ({{adviced-function}} {{advice-function}}) in list will be accepted
      ((leaf leaf
         :advice-remove ((matu matu-around0)
                         (matu matu-before0)))
       (prog1 'leaf
         (autoload #'matu-before0 "leaf" nil t)
         (autoload #'matu-around0 "leaf" nil t)
         (advice-remove 'matu #'matu-around0)
         (advice-remove 'matu #'matu-before0)))))
#+end_src

** :pl-pre-setq, :pl-setq, :pl-setq-default, :pl-custom keywords

Those keywords provide configure variables with [[https://github.com/emacs-mirror/emacs/blob/master/lisp/plstore.el][plstore.el]].
~plstore~ provide plist based data managing and encryption.

The keywords for plstore corresponding to ~:pre-setq~, ~:setq~,
~:setq-default~ and ~:custom~ are ~:pl-pre-setq~, ~:pl-setq~,
~:pl-setq-default~ and ~:pl-custom~.

Before those keyword using, prepare below plstore data and store it.
If you omit right value, ~leaf~ uses plstore file at =~/.emacs.d/leaf-plstore.plist=.

#+begin_src emacs-lisp
  (("leaf-sql"
    :secret-sql-connection-alist (("Postgres/d125q"
                                   (sql-product 'postgres)
                                   (sql-user "d125q")
                                   (sql-password "password")
                                   (sql-server "server")
                                   (sql-port 5432)
                                   (sql-database "database"))
                                  ("MySQL/d125q"
                                   (sql-product 'mysql)
                                   (sql-user "d125q")
                                   (sql-password "password")
                                   (sql-server "server")
                                   (sql-port 3306)
                                   (sql-database "database"))))
   ("leaf-erc"
    :secret-erc-password           "password"
    :secret-erc-nickserv-passwords ((freenode (("nick-one" . "password")
                                               ("nick-two" . "password")))
                                    (DALnet   (("nickname" . "password"))))
    :secret-erc-user-full-name     "Naoya Yamashita"
    :secret-erc-nick               "conao3")))
#+end_src

If you save plist file named as =~/.emacs.d/plstore.plist=, open
plstore file and decription if needed (then type password).

#+begin_src emacs-lisp
  (leaf plstore
    :setq `(some-plstore . ,(plstore-open (expand-file-name "~/.emacs.d/plstore.plist"))))
#+end_src

~leaf~ expand ~plstore~ related keywords as below.

Before using those keywords, we recommended that you check how
~plstore~ works in ~*scratch*~ and not through ~leaf~.

#+begin_src emacs-lisp
  (defcustom leaf-default-plstore
    (let ((path (locate-user-emacs-file "leaf-plstore.plist")))
      (when (file-readable-p path)
        (plstore-open path)))
    "Default value if omit store variable in plsore related keywords.
  This variable must be result of `plstore-open'."
    :type 'sexp
    :group 'leaf)

  (cort-deftest-with-macroexpand leaf/pl-custom
    '(
      ;; Emulate customizing `sql-connection-alist' with value taken from `some-plstore'.
      ((leaf sql
         :pl-custom
         (sql-connection-alist . some-plstore))
       (prog1 'sql
         (custom-set-variables
          '(sql-connection-alist (leaf-handler-auth sql sql-connection-alist some-plstore) "Customized in leaf `sql' from plstore `some-plstore'"))))

      ;; Emulate customizing `erc-password' and `erc-nickserv-passwords'
      ;; with values taken from `some-plstore', and `erc-user-full-name'
      ;; and `erc-nick' with values taken from `another-plstore'.
      ((leaf erc
         :pl-custom
         ((erc-password erc-nickserv-passwords) . some-plstore)
         ((erc-user-full-name erc-nick) . another-plstore))
       (prog1 'erc
         (custom-set-variables
          '(erc-password           (leaf-handler-auth erc erc-password some-plstore) "Customized in leaf `erc' from plstore `some-plstore'")
          '(erc-nickserv-passwords (leaf-handler-auth erc erc-nickserv-passwords some-plstore) "Customized in leaf `erc' from plstore `some-plstore'")
          '(erc-user-full-name     (leaf-handler-auth erc erc-user-full-name another-plstore) "Customized in leaf `erc' from plstore `another-plstore'")
          '(erc-nick               (leaf-handler-auth erc erc-nick another-plstore) "Customized in leaf `erc' from plstore `another-plstore'"))))

      ;; you can use symbol to configure with `leaf-default-plstore'.
      ((leaf erc
         :pl-custom erc-nick erc-password)
       (prog1 'erc
         (custom-set-variables
          '(erc-nick     (leaf-handler-auth erc erc-nick leaf-default-plstore) "Customized in leaf `erc' from plstore `leaf-default-plstore'")
          '(erc-password (leaf-handler-auth erc erc-password leaf-default-plstore) "Customized in leaf `erc' from plstore `leaf-default-plstore'"))))))

  (cort-deftest-with-macroexpand leaf/pl-setq
    '(
      ;; Emulate setting `sql-connection-alist' with value taken from `some-plstore'.
      ((leaf sql
         :pl-setq
         (sql-connection-alist . some-plstore))
       (prog1 'sql
         (setq sql-connection-alist
               (leaf-handler-auth sql sql-connection-alist some-plstore))))
      ;; Emulate setting `erc-password' and `erc-nickserv-passwords'
      ;; with values taken from `some-plstore', and `erc-user-full-name'
      ;; and `erc-nick' with values taken from `another-plstore'.
      ((leaf erc
         :pl-setq
         ((erc-password erc-nickserv-passwords) . some-plstore)
         ((erc-user-full-name erc-nick) . another-plstore))
       (prog1 'erc
         (setq erc-password           (leaf-handler-auth erc erc-password some-plstore))
         (setq erc-nickserv-passwords (leaf-handler-auth erc erc-nickserv-passwords some-plstore))
         (setq erc-user-full-name     (leaf-handler-auth erc erc-user-full-name another-plstore))
         (setq erc-nick               (leaf-handler-auth erc erc-nick another-plstore))))))
#+end_src

* System keywords

System keywords enabled by defalts on all leaf-block.

If you disable temporary, pass these keyword to ~nil~,
or add ~nil~ to ~leaf-defaults~ to disable all leaf-block
or set ~leaf-expand-leaf-protect~ to nil.

** :leaf-protect keyword

If the leaf fails at the top of the configuration file,
most of the configuration file will not be read.

Therefore, it simply reports an error and expands the error-handling block
that moves execution to the next leaf-block.

#+begin_src emacs-lisp
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
#+end_src

** :leaf-defer keyword

leaf-blocks with ~:bind~ or ~:mode~ can often delay loading or configuration evaluation.

The keywords that enable this feature are defined below and expand as follows

It seems ~:leaf-defer nil~ same as ~:demand t~ in ~use-package~.

#+begin_src emacs-lisp
  (defcustom leaf-defer-keywords (cdr '(:dummy
                                        :bind :bind*
                                        :mode :interpreter :magic :magic-fallback
                                        :hook :commands))
    "Specifies a keyword to perform a deferred load.
  `leaf' blocks are lazily loaded by their package name
  with values for these keywords."
    :type 'sexp
    :group 'leaf)

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
#+end_src

** :leaf-autoload keyword

For keywords that set functions, ~leaf~ can auto-expand the autoload expression
enable lazy loading without relying on magic comments, ~;;;Autoload~.

In some cases, you may want to disable this auto-expansion.
(I can't think of that case, but it's provided as a function.)

#+begin_src emacs-lisp
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
#+end_src

** :leaf-defun keyword

Depending on the context, you may find that the function is declared
in the package that is currently being set. Previously, the
~declare-function~ statement was explicitly generated for these
functions using the: defun keyword, but this keyword no longer
requires that work.

#+begin_src emacs-lisp
  (cort-deftest-with-macroexpand leaf/leaf-defun
    '(((leaf annotate
        :commands annotate-mode
        :bind (("C-c a a" . annotate-annotate)
               ("C-c a ]" . annotate-next-annotation)
               ("C-c a [" . annotate-previous-anotation)))
       (prog1 'annotate
         (unless (fboundp 'annotate-annotate) (autoload #'annotate-annotate "annotate" nil t))
         (unless (fboundp 'annotate-next-annotation) (autoload #'annotate-next-annotation "annotate" nil t))
         (unless (fboundp 'annotate-previous-anotation) (autoload #'annotate-previous-anotation "annotate" nil t))
         (unless (fboundp 'annotate-mode) (autoload #'annotate-mode "annotate" nil t))
         (declare-function annotate-annotate "annotate")
         (declare-function annotate-next-annotation "annotate")
         (declare-function annotate-previous-anotation "annotate")
         (declare-function annotate-mode "annotate")
         (leaf-keys
          (("C-c a a" . annotate-annotate)
           ("C-c a ]" . annotate-next-annotation)
           ("C-c a [" . annotate-previous-anotation)))))

      ((leaf annotate
         :leaf-autoload nil
         :leaf-defun nil
         :commands annotate-mode
         :bind (("C-c a a" . annotate-annotate)
                ("C-c a ]" . annotate-next-annotation)
                ("C-c a [" . annotate-previous-anotation)))
       (prog1 'annotate
         (leaf-keys
          (("C-c a a" . annotate-annotate)
           ("C-c a ]" . annotate-next-annotation)
           ("C-c a [" . annotate-previous-anotation)))))))
#+end_src

** leaf-defvar

When binding to a specific map with: bind, the map variable must be
explicitly declared with: defvar. This keyword suppresses byte
compiler warnings by automatically declaring the variable.

* Tips
** leaf-find feature
Use ~leaf-find~ to search a leaf block in the configuration file. like ~find-library~.

* Information
** Donation

I love OSS and I am dreaming of working on it as *full-time* job.

*With your support*, I will be able to spend more time at OSS!

[[https://www.patreon.com/conao3][https://c5.patreon.com/external/logo/become_a_patron_button.png]]

** Community

All feedback and suggestions are welcome!

You can use github issues, but you can also use [[https://join.slack.com/t/conao3-support/shared_invite/enQtNjUzMDMxODcyMjE1LTA4ZGRmOWYwZWE3NmE5NTkyZjk3M2JhYzU2ZmRkMzdiMDdlYTQ0ODMyM2ExOGY0OTkzMzZiMTNmZjJjY2I5NTM][Slack]]
if you want a more casual conversation.

** Contribution

We welcome PR! But It is need sign to FSF.

Travis Cl test ~leaf-test.el~ with all Emacs version 23 or above.

I think that it is difficult to prepare the environment locally,
so I think that it is good to throw PR and test Travis for the time being!
Feel free throw PR!

~leaf.el~ creates the intended elisp code from DSL with a simple mechanism.

It is clear what internal conversion is done and it is also easy to customize it.

*** leaf.el mechanism

1. Append ~leaf-defaults~ and ~leaf-system-defaults~ to ~leaf~ arguments.
2. Because ~leaf~ receives arguments too many format, normalize as plist.
   1. Normalize plist by ~leaf-normalize-plist~.
   2. Sort plist by ~leaf-keyword~.

      #+begin_src emacs-lisp
        (:bind
         ("M-s O" . moccur)
         (:isearch-mode-map
          :package isearch
          ("M-o" . isearch-moccur)
          ("M-O" . isearch-moccur-all)))

        ;; => (:leaf-protect (t)
        ;;     :leaf-autoload (t)
        ;;     :bind (("M-s O" . moccur)
        ;;            (:isearch-mode-map
        ;;             :package isearch
        ;;             ("M-o" . isearch-moccur)
        ;;             ("M-O" . isearch-moccur-all)))
        ;;     :leaf-defer (t))
      #+end_src
3. Run normalizer, and process keyword using below variables
   | Variable Name    | Description                                        |
   |------------------+----------------------------------------------------|
   | ~leaf--raw~      | The all leaf arguments                             |
   | ~leaf--name~     | The name of leaf-block                             |
   | ~leaf--key~      | The :keyword of current processing                 |
   | ~leaf--keyname~  | The :keyword name as string of current processing  |
   | ~leaf--value~    | The arguments which is current processed           |
   | ~leaf--body~     | The result of the following keywords and arguments |
   | ~leaf--rest~     | The following keywords and arguments               |
   | ~leaf--autoload~ | The list of pair ~(fn . pkg)~                      |
4. Apply the normalized values to the keyword specific normalizer.

   The definition is ~leaf-normalize~, overwrite ~leaf--value~.
5. Run conversion process keyword.

   The conversion definition is ~leaf-keywords~, overridden ~leaf--body~
6. Wrap finaly ~leaf--body~ with ~prog1~.

*** Adding new keywords

~leaf~ normalize argument with ~leaf-normalize~, and conversion with ~leaf-keywords~.

So, pushing new element these variable, ~leaf~ can recognize new keywords.

In [[https://github.com/conao3/leaf-keywords.el][leaf-keywords.el]], you can see practical example, and you can PR it.

Note that leaf only contains keywords for packages that come with the Emacs standard,
and that keywords that depend on external packages are added to its repository.

** Migration

*** leaf v3.0 to v4.0
**** Drop support Emacs versions lower than Emacs-24.4
#+begin_quote
I used Emacs 23 bundled with MacOS as a reference when
determining the minimum Emacs support for leaf.el.  However,
MacOS stopped bundling Emacs 23, and by supporting Emacs 23, the
inability to take advantage of most of the Elisp ecosystem like
~package.el~, ~cask~, and ~ert~ made development inefficient.

I plan to release leaf.el v3.8 as the last version to support
Emacs 23, and leaf.el as leaf.el v4.0, with Emacs 24 as the
smallest version of Emacs.

If you're using leaf.el with Emacs 23, you probably don't use
package.el, but rather the traditional site-lisp method of using
leaf.el. (package.el dropped Emacs 23 a long time ago) You should
be able to continue using leaf.el as before by downloading
version 3.8 from the [[https://github.com/conao3/leaf.el/releases][releases]] page.
#+end_quote

We have posted above announcements for more than 1 month and have
dropped support for Emacs versions below 24.4 because of a bug in
a global variable that prevented it from passing tests under
Emacs 24.2.  https://github.com/conao3/leaf.el/runs/515675886

Emacs-24.4 is a version with ~lexical-binding~ support and should
be considered the minimum version supported by the current Emacs
packages.

**** Distribute under GPLv3 instead of AGPLv3

Now ~leaf~ project distribute under GPLv3 instead of AGPLv3.

*** leaf v2.0 to v3.0
**** Drop bind-key.el support for :bind and feather.el support for :ensure

To make ~leaf~ dependent only on packages that are itself and packages attached to and Emacs,
we have removed the back-end selection for ~bind-key~ and ~leaf-key~ for ~:bind~
and the back-end selection for ~package.el~, ~feather.el~, and ~el-get~ for ~:ensure~.

You should now use the external package specific keywords, such as
~:bind-key~ and ~:el-get~, ~:feather~, defined in [[https://github.com/conao3/leaf-keywords.el][leaf-keywords.el]].

Therefore, the keyword ~:ensure~ has been changed to ~:package~.
This has no effect because we have defined alias.

*** leaf v1.0 to v2.0
**** Change not to ~require~ by default

In order to realize the philosophy of "Leaf of setting",
we changed it so as not to ~require~ by default.

If you want to request explicitly use the ~:require t~ flag.
#+begin_src emacs-lisp
  ;; behavior of leaf v2.0
  (leaf foo)
  => (progn)

  (leaf foo :require t)
  => (progn
       (require 'foo))

  ;; behavior of leaf v1.0
  (leaf foo)
  => (progn
       (require 'foo))

  (leaf foo :require t)
  => (progn
       (require 'foo))
#+end_src

** License

#+begin_example
  General Public License Version 3 (GPLv3)
  Copyright (c) Naoya Yamashita - https://conao3.com
  https://github.com/conao3/leaf.el/blob/master/LICENSE
#+end_example

** Author

- Naoya Yamashita ([[https://github.com/conao3][conao3]])

** Contributors

- Kzflute ([[https://github.com/Kzflute][Kzflute]])
- KeenS ([[https://github.com/KeenS][κeen]])
- Dario Gjorgjevski ([[https://github.com/d125q][d125q]])
- Masanori Mano ([[https://github.com/grugrut][grugrut]])
- Thomas Padioleau ([[https://github.com/tpadioleau][tpadioleau]])

** Special Thanks

Advice and comments given by [[http://emacs-jp.github.io/][Emacs-JP]]'s forum member has been a great
help in developing ~leaf.el~.

Thank you very much!!
