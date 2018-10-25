;;; leaf-tests-noert.el ---                          -*- lexical-binding: t; -*-

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

;; (defun ert-run-tests-batch-and-exit (&optional selector)
;;   "Like `ert-run-tests-batch', but exits Emacs when done.
;;
;; The exit status will be 0 if all test results were as expected, 1
;; on unexpected results, or 2 if the tool detected an error outside
;; of the tests (e.g. invalid SELECTOR or bug in the code that runs
;; the tests)."
;;   (or noninteractive
;;       (user-error "This function is only for use in batch mode"))
;;   ;; Better crash loudly than attempting to recover from undefined
;;   ;; behavior.
;;   (setq attempt-stack-overflow-recovery nil
;;         attempt-orderly-shutdown-on-fatal-signal nil)
;;   (unwind-protect
;;       (let ((stats (ert-run-tests-batch selector)))
;;         (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))
;;     (unwind-protect
;;         (progn
;;           (message "Error running tests")
;;           (backtrace))
;;       (kill-emacs 2))))

(defun run-tests-batch-and-exit ()
  (kill-emacs 0))

(provide 'leaf-tests-noert)
;;; leaf-tests-noert.el ends here
