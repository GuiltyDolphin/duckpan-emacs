;;; duckpan-core.el --- base functionality for duckpan-emacs

;; Copyright (C) 2016 Ben Moon
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Ben Moon <guiltydolphin@gmail.com>
;; Version: 0.1.1
;; Keywords: duckpan, DuckDuckHack, DuckDuckGo
;; URL: https://github.com/GuiltyDolphin/duckpan-emacs


;;; Commentary:
;;;
;;; Defines the base functionality for duckpan-emacs.

;;; Code:

(defcustom duckpan-instant-answer-projects
  '(("Goodie" . "zeroclickinfo-goodies")
    ("Spice" . "zeroclickinfo-spice"))
  "Instant Answer projects and their associated repositories."
  :group 'duckpan
  :type 'list)

(defun duckpan-project-repos ()
  "Available project repositories."
  (mapcar 'cdr duckpan-instant-answer-projects))


(defun duckpan-project-p (path)
  "T if PATH is in a duckpan-configured project."
  (duckpan-project-root path))

(defun duckpan-get-ddg-project-type (path)
  "Get the project type for PATH.

Return NIL if no project is found."
  (let ((path (buffer-file-name)))
    (catch 'pval
      (dolist (project (duckpan-project-repos))
        (when (string-match-p project path)
          (throw 'pval (car (rassoc project duckpan-instant-answer-projects))))))))

(defun duckpan-project-root (path)
  "Get the directory in PATH in which to execute duckpan commands."
  (let ((project (assoc-default (duckpan-get-ddg-project-type path) duckpan-instant-answer-projects)))
    (when project
      (let* ((full-path (buffer-file-name))
             (start (string-match project full-path)))
        (when start
          (file-name-as-directory
           (concat (substring full-path 0 start) project)))))))

(defun duckpan-get-install ()
  "Retrieve duckpan installation script."
  (with-temp-buffer
    (call-process "curl" nil t nil "http://duckpan.org/install.pl")
    (buffer-string)))

(defun duckpan-install ()
  "Install duckpan."
  (with-temp-message "Installing duckpan..."
    (let ((install-script (duckpan-get-install)))
      (call-process "perl" nil nil nil install-script))))

(defun duckpan-setup ()
  "Setup duckpan."
  (if (executable-find "duckpan")
      (message "duckpan is already installed on the system.")
    (duckpan-install)))

(defun upper-case-p (str)
  "Return non-NIL if STR is entirely upper-case."
  (let ((case-fold-search nil))
    (string-match-p "^[^a-z]*$" str)))

(defmacro with-duckpan-project-root (&rest body)
  "Execute BODY in the root of the project."
  (declare (indent 0) (debug t))
  `(if (duckpan-project-p default-directory)
       (with-temp-buffer
         (let ((path (duckpan-project-root default-directory)))
           (cd path)
           (progn ,@body)))
     (message "Not in a valid duckpan project directory.")))



(provide 'duckpan-core)
;;; duckpan-core.el ends here
