;;; duckpan-ia.el --- instant answer support for duckpan-emacs

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
;;; Provides instant answer support for duckpan-emacs.

;;; Code:

(require 'duckpan-core)

(defun upper-case-p (str)
  "Return non-NIL if STR is entirely upper-case."
  (let ((case-fold-search nil))
    (string-match-p "^[^a-z]*$" str)))

(defun duckpan-ia-name-to-share (name)
  "Return a valid share directory version of NAME."
  (downcase
   (let ((case-fold-search nil))
     (if (upper-case-p name)
         name
       (substring (replace-regexp-in-string "[A-Z]" (lambda (c) (format "_%s" c)) name) 1)))))

(defun duckpan-ia-path-to-perl (type name)
  "Get the expected relative perl path for a TYPE instant-answer called NAME.

TYPE should be one of Spice or Goodie."
  (concat (duckpan-ia-lib-directory type) (format "%s.pm" name)))

(defun duckpan-ia-path-to-js (type name)
  "Get the expected relative javascript path for a TYPE instant-answer called NAME."
  (concat (duckpan-ia-share-directory type name)
          (format "%s.js" (duckpan-ia-name-to-share name))))

(defun duckpan-ia-lib-directory (type)
  "Return the lib directory for a project type TYPE."
  (format "lib/DDG/%s/" type))


(defun duckpan-ia-share-directory (type name)
  "Return the share directory for a project type TYPE for NAME."
  (format "share/%s/%s/" (downcase type) (duckpan-ia-name-to-share name)))

(defun duckpan-ia-paths-for-type (type name)
  "Return standard paths for a TYPE project called NAME."
  (cond
   ((equal type "Goodie") (list (duckpan-ia-path-to-perl type name)))
   ((equal type "Spice") (list (duckpan-ia-path-to-perl type name)
                               (duckpan-ia-path-to-js type name)))))


(defun duckpan-instant-answers (path type)
  "Get the instant answers for the project under PATH.

TYPE should be one of Spice or Goodie."
  (let* ((ia-path (concat path (duckpan-ia-lib-directory type)))
         (ias (directory-files ia-path nil "\.pm$")))
    (mapcar 'file-name-base ias)))

(defun duckpan-full-ia-paths (name)
  "Get the full instant-answer paths for NAME."
  (let* ((project-type (duckpan-get-ddg-project-type default-directory))
        (project-path (duckpan-project-root default-directory))
        (ia-paths (duckpan-ia-paths-for-type project-type name)))
    (mapcar (lambda (path) (concat project-path path)) ia-paths)))

(defun duckpan-ia-choose-instant-answer ()
  "Get the user to choose from the available instant answers."
  (completing-read "Choose an instant answer: " (duckpan-instant-answers)))

;;;###autoload
(defun duckpan-ia-goto-instant-answer (&optional name)
  "Goto to the instant answer file for NAME."
  (interactive)
  (let* ((name (or name (duckpan-ia-choose-instant-answer)))
         (paths (duckpan-full-ia-paths name)))
    (dolist (path paths) (find-file-other-window path))))


(provide 'duckpan-ia)
;;; duckpan-ia.el ends here
