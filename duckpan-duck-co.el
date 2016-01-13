;;; duckpan-duck-co.el --- duck.co integration for duckpan-emacs

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
;;; Provides duck.co support for duckpan-emacs

;;; Code:

(require 'duckpan-core)

(defmacro duckpan-defcustom-url-part (symbol standard doc)
  "Define a custom duck-co url variable with name SYMBOL.

Default value is STANDARD and the documentation string is DOC."
  (declare (indent defun) (doc-string 3))
  `(defvar ,(intern (concat "duckpan-duck-co-url-" (symbol-name symbol)))
     ,standard
     ,doc))
  ;   :group 'duckpan
  ;   :type 'string))

(duckpan-defcustom-url-part ia-base
 "https://duck.co/ia"
 "Base duck.co URL for instant answers.")

(duckpan-defcustom-url-part view-url-part
  "view"
  "Component of URL for viewing instant answers.")

(duckpan-defcustom-url-part pipeline-part
  "dev/pipeline"
  "Component of URL for viewing the pipeline.")

(defun duckpan-join-url (&rest url-components)
  "Join each of URL-COMPONENTS with a url delimiter."
  (mapconcat 'identity url-components "/"))

(defun duckpan-url-query (url query)
  "Get a query url for URL and QUERY."
  (format "%s?q=%s" url query))

(defun upper-case-p (str)
  "Return non-NIL if STR is entirely upper-case."
  (let ((case-fold-search nil))
    (string-match-p "^[^a-z]*$" str)))

(defun duckpan-duck-co-ia-url (name)
  "Get the url for viewing the instant answer NAME."
  (duckpan-join-url duckpan-duck-co-url-ia-base
                    duckpan-duck-co-url-view-url-part
                    (duckpan-ia-name-to-share name)))

;;;###autoload
(defun duckpan-navigate-to-instant-answer-page (&optional name)
  "Open the instant answer page for NAME in a browser."
  (interactive)
  (let ((name (or name (duckpan-choose-instant-answer))))
    (browse-url (duckpan-duck-co-ia-url name))))

;;;###autoload
(defun duckpan-duck-co-search-ia (&optional name)
  "Open an instant answer search for NAME in a browser."
  (interactive "MSearch for what?: ")
  (browse-url (duckpan-url-query
               duckpan-duck-co-url-ia-base
               name)))

(defun duckpan-duck-co-pipeline-query (&optional name)
  "Generate a url querying the developers' pipeline for NAME."
  (duckpan-url-query
   (duckpan-join-url duckpan-duck-co-url-ia-base
                     duckpan-duck-co-url-pipeline-part)
   name))

;;;###autoload
(defun duckpan-duck-co-search-dev-pipeline (&optional name)
  "Open an instant answer development pipeline search for NAME in a browser."
  (interactive "MSearch for what?: ")
  (browse-url (duckpan-duck-co-pipeline-query name)))

(provide 'duckpan-duck-co)
;;; duckpan-duck-co.el ends here
