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

(require 'magit)

(defun initialize-spice-repo (username)
  "Initialize a zeroclickinfo-spice repository from USERNAME's fork."
  (interactive "MEnter your GitHub username: ")
  (magit clone (format "https://github.com/%s/zeroclickinfo-spice" username) "zeroclickinfo-spice"))


(provide 'duckpan)
;;; duckpan.el ends here
