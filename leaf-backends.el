;;; leaf-backends.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
;; Keywords: settings

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  leaf prepared backends
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

(defun leaf-backend/:ensure-package (name package)
  ":ensure package.el backend."
  (unless (package-installed-p package)
    (condition-case err
        (if (assoc package package-archive-contents)
            (package-install package)
          (package-refresh-contents)
          (package-install package))
      (error
       (display-warning
        'leaf (format "Failed to install %s: %s"
                      package (error-message-string err)))))))

(defun leaf-backend/:bind-bind-key (name bind)
  ":bind bind-key.el backend."
  (eval `(bind-keys :package ,name ,@bind)))

(defun leaf-backend/:bind*-bind-key (name bind)
  ":bind* bind-key.el backend."
  (eval `(bind-keys* :package ,name ,@bind)))

(provide 'leaf-backends)
;;; leaf-keyword-backends.el ends here
