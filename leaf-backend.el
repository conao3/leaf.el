;;; leaf-backend.el ---                              -*- lexical-binding: t; -*-

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

;; `leaf-core'
(defvar leaf-backend/:ensure)
(defvar leaf-backend/:bind)
(defvar leaf-backend/:bind*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  leaf meta handler
;;

(defun leaf-meta-backend/:ensure (name value)
  ":ensure meta handler."
  (let ((fn (intern (format "leaf-backend/:ensure-%s" leaf-backend/:ensure))))
    (when leaf-backend/:ensure
      (funcall fn name value))))
      
(defun leaf-meta-backend/:bind (name value)
  ":bind meta handler."
  (let ((fn (intern (format "leaf-backend/:bind-%s" leaf-backend/:bind))))
    (when leaf-backend/:bind
      (funcall fn name value))))

(defun leaf-meta-backend/:bind* (name value)
  ":bind* meta handler."
  (let ((fn (intern (format "leaf-backend/:bind*-%s" leaf-backend/:bind*))))
    (when leaf-backend/:bind*
      (funcall fn name value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  leaf prepared backend
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  :ensure backend
;;

;; `package'
(defvar package-archive-contents)
(eval-when-compile
  (autoload 'package-install "package")
  (autoload 'package-installed-p "package")
  (autoload 'package-refresh-contents "package"))

(defun leaf-backend/:ensure-package (name value)
  ":ensure package.el backend."
  (mapc (lambda (package)
          (let ((package* (if (eq package t) name package)))
            (unless (package-installed-p package*)
              (condition-case err
                  (if (assoc package* package-archive-contents)
                      (package-install package*)
                    (package-refresh-contents)
                    (package-install package*))
                (error
                 (display-warning
                  'leaf (format "Failed to install %s: %s"
                                package* (error-message-string err))))))))
        value))

(defun leaf-backend/:bind-bind-key (name value)
  ":bind bind-key.el backend."
  (mapc (lambda (bind)
          (eval `(bind-keys :package ,name ,@bind)))
        value))

(defun leaf-backend/:bind*-bind-key (name value)
  ":bind* bind-key.el backend."
  (mapc (lambda (bind)
          (eval `(bind-keys* :package ,name ,@bind)))
        value))

(provide 'leaf-backend)
;;; leaf-keyword-backend.el ends here
