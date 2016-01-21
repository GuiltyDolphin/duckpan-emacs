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

(require 'duckpan-core)

(defcustom duckpan-instant-answer-projects
  '(("Goodie" . "zeroclickinfo-goodies")
    ("Spice" . "zeroclickinfo-spice"))
  "Instant Answer projects and their associated repositories."
  :group 'duckpan
  :type 'list)

(defun duckpan-project-repos ()
  "Available project repositories."
  (mapcar 'cdr duckpan-instant-answer-projects))


(defun duckpan-github-project-p (path)
  "T if PATH is in a duckpan-configured project and a git repository."
  (let ((project (duckpan-project-root path)))
    (when project
      (= 0 (with-temp-buffer
             (cd project)
             (duckpan-git "status"))))))

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

(defun duckpan-github-setup-upstream (repo)
  "Initialize an upstream remote for REPO."
  (duckpan-git "remote" "add" "upstream" (duckpan-github-url "duckduckgo" repo)))

(defun duckpan-github-fetch-upstream ()
  "Pull and merge from the upstream repository."
  (duckpan-git "fetch" "upstream")
  (duckpan-git "merge" "upstream/master" "master"))

(defun duckpan-github-choose-repo ()
  "Get the user to choose from the repositories specified in DUCKPAN-REPOS."
  (completing-read "Which repository to configure?: " (duckpan-project-repos)))

;;;###autoload
(defun duckpan-github-initialize-repo-interactive (user repo)
  "Initialize USER's fork of the DuckDuckGo repository REPO."
  (interactive (let ((user (read-string "Enter your GitHub username: "))
                     (repo (duckpan-github-choose-repo)))
                 (list user repo)))
  (duckpan-github-initialize-repo user repo))

(defun duckpan-github-initialize-repo (user repo)
  "Initialize USER's fork of the DuckDuckGo repository REPO."
  (let ((fork (duckpan-github-url user repo)))
    (if (file-exists-p repo)
        (message "There is already a directory with the name '%s'" repo)
      (if (duckpan-github-project-p default-directory)
          (message "Already in a duckpan-configured directory")
        (with-temp-message (format "Cloning into %s" fork)
          (duckpan-github-clone fork))
        (with-temp-buffer
          (cd repo)
          (duckpan-github-setup-upstream repo)
          default-directory)))))

(provide 'duckpan-github)
;;; duckpan-github.el ends here
