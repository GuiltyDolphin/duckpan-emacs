;;; duckpan.el --- tools for working with DuckDuckHack development.

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
;; Version: 0.1
;; Package-requires: ((magit "2.4.0"))
;; Keywords: duckpan, DuckDuckHack, DuckDuckGo
;; URL: https://github.com/GuiltyDolphin/duckpan-emacs


;;; Commentary:
;;;
;;; Provides commands for working with DuckDuckHack's *duckpan* tool.

;;; Code:

(defcustom duckpan-repos
  '("zeroclickinfo-goodies" "zeroclickinfo-spice")
  "Repositories duckpan is configured for."
  :group 'duckpan
  :type 'list)

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
      (dolist (project duckpan-repos)
        (when (string-match-p project path) (throw 'pval project))))))

(defun duckpan-project-root (path)
  "Get the directory in PATH in which to execute duckpan commands."
  (let ((project (duckpan-get-ddg-project-type path)))
    (when project
      (let* ((full-path (buffer-file-name))
             (start (string-match project full-path)))
        (when start
          (file-name-as-directory
           (concat (substring full-path 0 start) project)))))))

(defun duckpan-github-url (user repo)
  "Get the GitHub URL in the form https://github.com/USER/REPO."
  (format "https://github.com/%s/%s" user repo))

(defun duckpan-clone (url)
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

(defun duckpan-choose-repo ()
  "Get the user to choose from the repositories specified in DUCKPAN-REPOS."
  (completing-read "Which repository to configure?: " duckpan-repos))

(defun duckpan-initialize-repo (user)
  "Initialize USER's fork of a DuckDuckGo repository."
  (interactive "MEnter your GitHub username: ")
  (let* ((repo (duckpan-choose-repo))
         (fork (duckpan-github-url user repo)))
    (if (file-exists-p repo)
        (message "There is already a directory with the name '%s'" repo)
      (if (duckpan-project-p default-directory)
          (message "Already in a duckpan-configured directory")
        (duckpan-clone fork)
        (with-temp-buffer
          (cd repo)
          (duckpan-setup-upstream repo))))))

(defun duckpan-get-install ()
  "Retrieve duckpan installation script."
  (with-temp-buffer
    (call-process "curl" nil t nil "http://duckpan.org/install.pl")
    (buffer-string)))

(defun duckpan-install ()
  "Install duckpan."
  (let ((install-script (duckpan-get-install)))
    (message "Installing duckpan...")
    (call-process "perl" nil nil nil install-script)
    (message "duckpan successfully installed")))

(defun duckpan-setup ()
  "Setup duckpan."
  (if (executable-find "duckpan")
      (message "duckpan is already installed on the system.")
    (duckpan-install)))


(provide 'duckpan)
;;; duckpan.el ends here
