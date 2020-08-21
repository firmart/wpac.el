;;; wpac.el --- Wikipedia autocompletion -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Firmin Martin

;; Author: Firmin Martin
;; Maintainer: Firmin Martin
;; Version: 0.1
;; Keywords: convenience, autocompletion
;; URL: https://www.github.com/firmart/wpac
;; Package-Requires: ((emacs "26"))


;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; wpac.el is a package providing autocompletion of Wikipedia articles, templates, etc.
;; Expected to use along with emacs-server.


;; Example:
;;
;; (wpac--results-count
;;  (wpac--get-response
;;   '(:action "query"
;;             :list "search"
;;             :format "json"
;;             :srnamespace 10
;;             :srsearch "Wikipedia"
;;             :srinfo "totalhits")))

;; TODO: POST with json instead of url parameters.

;; built-in Emacs lib
(require 'json)

;;; Code:
;;; Custom group
(defcustom wpac-project-base-url "https://en.wikipedia.org"
  "Wikimedia project url on which completion is done."
  :type  'string
  :group 'wpac
  :package-version '(wpac . "0.1"))

;;; Internal variables
(defvar wpac--api-path "/w/api.php?")

;;; API
;;;; General

(defun wpac--get-response (plist)
  "GET url query's response from PLIST query."
  (let* ((url (concat wpac-project-base-url wpac--api-path (wpac--plist-to-url-params plist)))
         (buffer (url-retrieve-synchronously url))
         (code (url-http-symbol-value-in-buffer 'url-http-response-status buffer))
         (json-object-type 'plist)
         (results-count 0)
         (json))

    (message "%s" url)

    (when (= code 200)
      (save-excursion
        (switch-to-buffer buffer)
        (setq json
              (json-read-from-string
               (buffer-substring url-http-end-of-headers (point-max))))))))

(defun wpac--error-p (plist)
  "Return error message if there is error, otherwise return nil."
  (when (plist-get plist :errors)
    (plist-get plist :*)))

;;;; Search
;; See https://www.mediawiki.org/wiki/API:Search

(defun wpac--results-count (plist)
  "Return search results count."
  (unless (wpac--error-p plist)
    (wpac--plist-get-rec
     plist
     :query :searchinfo :totalhits)))

;;; Misc.
;;;; plist

;; from https://emacs.stackexchange.com/a/10630/23697
(defun wpac--map-plist (fn plist)
  "Map PLIST values with function FN."
  (let ((pl    plist)
        (vals  ()))
    (while pl
      (push (funcall fn (car pl) (cadr pl)) vals)
      (setq pl (cddr pl)))
    (nreverse vals)))

(defun wpac--plist-to-url-params (plist)
  "Convert a plist of form (:key . value) to a key1=value1&key2=value2 string."
  (string-join
   (mapcar
    (lambda (kv)
      (format "%s=%s"
              (substring (symbol-name (car kv)) 1)
              (cdr kv)))
    (wpac--map-plist #'cons plist))
   "&"))

(defun wpac--plist-get-rec (plist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq plist (plist-get plist (pop keys))))
  plist)

(provide 'wpac)
;;; wpac.el ends here
