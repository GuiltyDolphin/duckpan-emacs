;;; duckpan-github.el --- github support for duckpan-emacs

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
;;; Provides GitHub support for duckpan-emacs.

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
  (let ((project (duckpan-project-root path)))
    (when project
      (= 0 (with-temp-buffer
             (cd project)
             (duckpan-git "status"))))))

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

(defun duckpan-github-url (user repo)
  "Get the GitHub URL in the form https://github.com/USER/REPO."
  (format "https://github.com/%s/%s" user repo))

(defun duckpan-github-clone (url)
  "Clone the repository of URL into the current directory."
  (with-temp-buffer
    (call-process "git" nil t nil "clone" url)))

(defun duckpan-git (&rest args)
  "Call git with ARGS."
  (apply 'call-process "git" nil nil nil args))

(defun duckpan-setup-upstream (repo)
  "Initialize an upstream remote for REPO."
  (duckpan-git "remote" "add" "upstream" (duckpan-github-url "duckduckgo" repo)))

(defun duckpan-fetch-upstream ()
  "Pull and merge from the upstream repository."
  (duckpan-git "fetch" "upstream")
  (duckpan-git "merge" "upstream/master" "master"))

(defun duckpan-github-choose-repo ()
  "Get the user to choose from the repositories specified in DUCKPAN-REPOS."
  (completing-read "Which repository to configure?: " (duckpan-project-repos)))

;;;###autoload
(defun duckpan-github-initialize-repo (user)
  "Initialize USER's fork of a DuckDuckGo repository."
  (interactive "MEnter your GitHub username: ")
  (let* ((repo (duckpan-github-choose-repo))
         (fork (duckpan-github-url user repo)))
    (if (file-exists-p repo)
        (message "There is already a directory with the name '%s'" repo)
      (if (duckpan-project-p default-directory)
          (message "Already in a duckpan-configured directory")
        (with-temp-message (format "Cloning into %s" fork)
          (duckpan-github-clone fork))
        (with-temp-buffer
          (cd repo)
          (duckpan-setup-upstream repo)
          default-directory)))))

(provide 'duckpan-github)
;;; duckpan-github.el ends here
