;;; helm-clojars.el --- Helm interface to clojars.org

;; Copyright (C) 2015 Julien Fantin

;; Author: Julien Fantin <julienfantin@gmail.com>
;; Maintainer: Julien Fantin <julienfantin@gmail.com>
;; URL: https://github.com/julienfantin/helm-clojars
;; Created: 29th December 201t
;; Version: 0.0.1
;; Keywords: helm, clojars, clojure, maven, leiningen
;; Package-Requires: ((emacs "24") (helm "1.6.3") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Helm interface to the clojars.org search API.
;;
;; Usage:
;;
;; M-x helm-clojars
;;
;; Customization:
;;
;; See the `helm-clojars' customize group.
;;
;;; Code:

(require 'helm)
(require 'helm-net)
(require 'json)
(require 'url)
(eval-when-compile
  (require 'cl))


;; * Customizations

(defgroup helm-clojars nil
  "Helm interface to clojars.org"
  :group 'helm)

(defcustom helm-clojars-search-url
  "https://clojars.org/search?q=%s&format=json"
  "URL format for helm-clojars search"
  :group 'helm-clojars
  :type 'string)

(defcustom helm-clojars-actions
  '(("Copy to kill-ring" . helm-clojars-kill-artifact)
    ("Insert at point" . helm-clojars-insert-artifact))
  "List of actions for helm-clojars source"
  :group 'helm-clojars
  :type '(alist :key-type string :value-type function))

(defcustom helm-clojars-wildcard-search
  t
  "Turn search terms into wildcard lucene queries, e.g. \"foo
  bar\" will result in a query for \"foo* bar*\". This variable
  has no effect if `helm-clojars-fuzzy-search' is set"
  :group 'helm-clojars
  :type 'boolean)

(defcustom helm-clojars-fuzzy-search
  nil
  "Turn search terms into fuzzy lucene queries, e.g. \"foo bar\" will
  result in a query for \"foo~ bar~\""
  :group 'helm-clojars
  :type 'boolean)

(defcustom helm-clojars-fuzzy-match
  t
  "Enable candidates fuzzy matching"
  :group 'helm-clojars
  :type 'boolean)

(defcustom helm-clojars-requires-pattern
  3
  "Minimum search pattern length"
  :group 'helm-clojars
  :type 'int)

(defcustom helm-clojars-group-width
  30
  "Maximum group name width"
  :group 'helm-clojars
  :type 'int)

(defcustom helm-clojars-jar-width
  20
  "Maximum jar name width"
  :group 'helm-clojars
  :type 'int)

(defcustom helm-clojars-version-width
  18
  "Maximum jar name width"
  :group 'helm-clojars
  :type 'int)

(defcustom helm-clojars-candidate-width
  nil
  "Maximum candidate width"
  :group 'helm-clojars
  :type 'int)

(defcustom helm-clojars-padding-width
  2
  "Padding width between columns"
  :group 'helm-clojars
  :type 'int)


;; * Search API

(defvar url-http-end-of-headers)

(defun helm-clojars-format-artifact-leiningen (artifact)
  (let* ((group (cdr (assoc 'group_name artifact)))
         (jar (cdr (assoc 'jar_name artifact)))
         (version (cdr (assoc 'version artifact)))
         (s (if group (format "%s/%s" group jar))))
    (format "[%s \"%s\"]" s version)))

(defun helm-clojars-search-result-read-results ()
  ;; vector -> seq
  (mapcar 'identity (cdar (json-read))))

(defun helm-clojars-trim (s len)
  (truncate-string-to-width s len 0 ?  " "))

(defun helm-clojars-search-result-parser ()
  (goto-char (+ 1 url-http-end-of-headers))
  (let* ((pad (make-string helm-clojars-padding-width ? ))
         (max-width (or helm-clojars-candidate-width
                        (window-width (helm-window))
                        (window-width (selected-window)))))
    (loop
     for artifact   in (helm-clojars-search-result-read-results)
     for group       = (cdr (assoc 'group_name artifact))
     for jar         = (cdr (assoc 'jar_name artifact))
     for version     = (cdr (assoc 'version artifact))
     for description = (cdr (assoc 'description artifact))
     collect (cons
              (helm-clojars-trim
               (concat
                (helm-clojars-trim group helm-clojars-group-width)
                pad
                (helm-clojars-trim jar helm-clojars-jar-width)
                pad
                (helm-clojars-trim version helm-clojars-version-width)
                pad
                description)
               max-width)
              artifact))))

(defun helm-clojars-search-synatx-p (input)
  (string-match-p "[\\*\\?\\~]" input))

(defun helm-clojars-search-format-input-1 (suffix input)
  (mapconcat
   (lambda (term) (format "%s%s" term suffix))
   (split-string input)
   " "))

(defun helm-clojars-search-format-input (input)
  (url-hexify-string
   (cond
    ((helm-clojars-search-synatx-p input) input)

    (helm-clojars-fuzzy-search
     (helm-clojars-search-format-input-1 "~" input))

    (helm-clojars-wildcard-search
     (helm-clojars-search-format-input-1 "*" input))

    (t input))))

(defun helm-clojars-fetch-search (input)
  "Fetch suggestions for INPUT from JSON buffer."
  (let* ((debug-on-error t)
         (query (helm-clojars-search-format-input input))
         (url (format helm-clojars-search-url query)))
    (helm-net--url-retrieve-sync url #'helm-clojars-search-result-parser)))


;; * Actions

(defun helm-clojars-kill-artifact (artifact)
  (kill-new (helm-clojars-format-artifact-leiningen artifact)))

(defun helm-clojars-insert-artifact (artifact)
  (insert (helm-clojars-format-artifact-leiningen artifact)))


;; * Helm source

(defun helm-clojars-search-match-part (candidate)
  (car (split-string candidate "[0-9]")))


(defun helm-clojars-search-candidates (&optional request-prefix)
  (helm-clojars-fetch-search
   (or (and request-prefix (concat request-prefix " " helm-pattern))
       helm-pattern)))

(defvar helm-source-clojars
  (helm-build-sync-source "Search clojars.org"
    :candidates #'helm-clojars-search-candidates
    :action helm-clojars-actions
    :volatile t
    :match-part #'helm-clojars-search-match-part
    :fuzzy-match helm-clojars-fuzzy-match
    :keymap helm-map
    :requires-pattern helm-clojars-requires-pattern))


;; * Interactive commands

;;;###autoload
(defun helm-clojars ()
  "`helm' interface to clojars.org search api"
  (interactive)
  (helm-other-buffer 'helm-source-clojars "*helm clojars*"))

(provide 'helm-clojars)
;;; helm-clojars.el ends here
