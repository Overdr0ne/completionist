;;; completionize.el --- completionize widgets built on completionist  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Overdr0ne

;; Author: Overdr0ne <scmorris.dev@gmail.com>
;; Keywords: tools, completion

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; completionize is to completionist as consult is to vertico: a collection
;; of interactive commands and UI widgets built on top of the completionist
;; persistent buffer API.

;;; Code:

(require 'completionize-buffer)
(require 'completionize-recentf)
(require 'completionize-process)
(require 'completionize-project)
(require 'completionize-persp)

(provide 'completionize)
;;; completionize.el ends here
