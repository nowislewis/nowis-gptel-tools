;;; gptel-agent-denote-tools.el --- Denote tools for gptel-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:
;; Keywords: tools, denote, gptel

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

;; Denote tools for gptel-agent to create and manage notes using the denote package.
;; This file should be loaded in your Emacs configuration.

;;; Code:

(require 'gptel)
(require 'denote)

(defun gptel-agent--denote-create-note (title &optional signature keywords directory file-type date template identifier)
  "Create a new note using denote with the appropriate metadata and file name.

TITLE is the title of the note (required).
SIGNATURE is an optional signature for special labels, priorities, or contexts.
KEYWORDS is an optional comma-separated list of keywords.
DIRECTORY is the directory to create the note in (defaults to denote-directory).
FILE-TYPE is the file type (org, text, markdown-yaml, markdown-toml).
DATE is an optional date string (format: 2022-06-30 or 2022-06-16 14:30).
TEMPLATE is an optional template symbol from denote-templates.
IDENTIFIER is an optional identifier string (format: 20220630T1430000).

Returns the path to the created note file."
  (let ((note-path (denote title
                          (if (and keywords (not (string-empty-p keywords)))
                              (split-string keywords ",")
                            nil)
                          (pcase file-type
                            ("org" :org)
                            ("text" :text)
                            ("markdown-yaml" :markdown-yaml)
                            ("markdown-toml" :markdown-toml)
                            (_ :org))
                          (or directory denote-directory)
                          date
                          template
                          signature
                          identifier)))
    (format "Created note: %s\nPath: %s" title note-path)))

(gptel-make-tool
 :name "DenoteCreateNote"
 :description "Create a new note using the denote package with the appropriate metadata and file name.

This tool creates notes following the denote file naming scheme: `ID==SIGNATURE--TITLE__KEYWORDS.EXTENSION`

- TITLE: The title of the note (required)
- SIGNATURE: Optional signature for special labels, priorities, or contexts
- KEYWORDS: Optional comma-separated list of keywords describing the note
- DIRECTORY: Directory to create the note in (defaults to denote-directory)
- FILE-TYPE: File type symbol (org, text, markdown-yaml, markdown-toml)
- DATE: Optional date string (format: 2022-06-30 or 2022-06-16 14:30)
- IDENTIFIER: Optional identifier string (format: 20220630T1430000)
- TEMPLATE: Optional template symbol from denote-templates

Returns the path to the created note file."
 :function #'gptel-agent--denote-create-note
 :args '(( :name "title"
           :type string
           :description "The title of the note (required)")
         ( :name "signature"
           :type string
           :description "Optional signature for special labels, priorities, or contexts"
           :optional t)
         ( :name "keywords"
           :type string
           :description "Optional comma-separated list of keywords describing the note"
           :optional t)
         ( :name "directory"
           :type string
           :description "Directory to create the note in (defaults to denote-directory)"
           :optional t)
         ( :name "file-type"
           :type string
           :enum ["org" "text" "markdown-yaml" "markdown-toml"]
           :description "File type for the note (defaults to org)"
           :optional t)
         ( :name "date"
           :type string
           :description "Optional date string (format: 2022-06-30 or 2022-06-16 14:30)"
           :optional t)
         ( :name "template"
           :type string
           :description "Optional template symbol from denote-templates"
           :optional t)
         ( :name "identifier"
           :type string
           :description "Optional identifier string (format: 20220630T1430000)"
           :optional t))
 :category "gptel-agent"
 :confirm t
 :include t)

(provide 'gptel-agent-denote-tools)
;;; gptel-agent-denote-tools.el ends here