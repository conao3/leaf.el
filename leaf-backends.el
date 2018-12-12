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
;;  for legacy Emacs
;;

(unless (fboundp 'declare-function)
  (defmacro declare-function (_fn _file &rest _args)
    "Tell the byte-compiler that function FN is defined, in FILE."
    nil))

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
(declare-function package-install "package")
(declare-function package-installed-p "package")
(declare-function package-refresh-contents "package")

(defun leaf-backend/:ensure-package (package)
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

(provide 'leaf-backends)
;;; leaf-keyword-backends.el ends here
