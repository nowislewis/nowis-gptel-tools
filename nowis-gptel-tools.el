;;; nowis-gptel-tools.el --- Load all gptel agent tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: 
;; Keywords: tools, gptel, agent

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

;; This file loads all gptel agent tools automatically.
;; Simply require this file in your Emacs configuration to load all available tools.

;; Available tools:
;; - gptel-agent-denote-tools.el: Tools for creating and managing notes with denote

;;; Code:

(defvar nowis-gptel-tools-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where gptel tools are located.")

;; Load all tool files
(when (and nowis-gptel-tools-directory
           (file-directory-p nowis-gptel-tools-directory))
  (let ((tool-files (directory-files nowis-gptel-tools-directory
                                    t
                                    "\\.el\\'")))
    (dolist (file tool-files)
      (when (and (not (string-match-p "nowis-gptel-tools\\.el\\'" file))
                 (string-match-p "-tools\\.el\\'" file))
        (load file nil t)
        (message "Loaded gptel tool: %s" (file-name-nondirectory file))))))

(provide 'nowis-gptel-tools)
;;; nowis-gptel-tools.el ends here
