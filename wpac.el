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
(require 'url-http)

;;; Code:
;;; Custom group
(defcustom wpac-project-base-url "https://en.wikipedia.org"
  "Wikimedia project url on which completion is done."
  :type  'string
  :group 'wpac
  :package-version '(wpac . "0.1"))

;;; Internal variables
(defvar wpac--api-path "/w/api.php?")
(defvar url-http-response-status nil)

;;; API
;;;; General

(defun wpac--get-response (plist &optional callback args)
  "Retrieve results from PLIST query, and apply CALLBACK with ARGS if succeed."
  (let* ((url (concat wpac-project-base-url wpac--api-path (wpac--plist-to-url-params plist)))
         (buffer (url-retrieve-synchronously url))
         (code (url-http-symbol-value-in-buffer 'url-http-response-status buffer))
         (json-object-type 'plist)
         (results-count 0)
         (json))

    (when (= code 200)
      (with-temp-buffer buffer
                        (goto-char (point-min))
                        (re-search-forward "^\r?\n" nil t)
                        (backward-char 1)
                        ;; Saw the end of the headers
                        (setq url-http-end-of-headers (set-marker (make-marker) (point)))
                        (setq json
                              (json-read-from-string
                               (buffer-substring url-http-end-of-headers (point-max)))))
      (unless (wpac--error-p json)
        (if callback
            (apply callback (list json args))
          json)))))

(defun wpac--error-p (plist)
  "Return error string if PLIST is an error, otherwise return nil."
  (when (plist-get plist :errors)
    (plist-get plist :*)))

(defmacro wpac--query-wrapper (method query)
  `(defun ,(intern (format "wpac-query-%s" method)) ,query
     ,(format "Retrieve results of `%s' with parameters `%s'." query (format "wpac--query-%s" method))
     (wpac--get-response
      ,@query
      #'wpac--plist-get-rec
      ,(intern (format "wpac--query-%s" method)))))

(defmacro wpac--interp-wrapper (method result)
  `(defun ,(intern (format "wpac-interp-%s" method)) ,result
     ,(format "Interpret results of `%s' with parameters `%s'." result (format "wpac--query-%s" method))
     (wpac--get-plist-get-rec
      ,@result
      ,(intern (format "wpac--query-%s" method)))))

;;;; Search
;; See https://www.mediawiki.org/wiki/API:Search
;;;;; Results count

(defvar wpac--query-results-count '(:query :searchinfo :totalhits))
(wpac--interp-wrapper "results-count" (result))
(wpac--query-wrapper "results-count" (query))

;;; Autocomplete
(defun wpac--ac-prefix-template ()
  (let ((query `(:action "query"
                         :list "prefixsearch"
                         :format "json"
                         :pslimit "10"
                         :psnamespace 10
                         :pssearch ,ac-prefix)))
    (mapcar
     (lambda (e) (string-remove-prefix "Template:" (plist-get e :title)))
     (wpac--plist-get-rec
      (wpac--get-response query)
      '(:query :prefixsearch)))))

(ac-define-source wp-template
  '((candidates . wpac--ac-template)
    (prefix . "{{\\(.*\\)")
    (requires . 0)
    (symbol . "t")
    (cache)))
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
  "Convert a PLIST of form (:key . value) to a key1=value1&key2=value2 string."
  (string-join
   (mapcar
    (lambda (kv)
      (format "%s=%s"
              (substring (symbol-name (car kv)) 1)
              (cdr kv)))
    (wpac--map-plist #'cons plist))
   "&"))

(defun wpac--plist-get-rec (plist keys)
  "Recursively find KEYS in PLIST."
  (while keys
    (setq plist (plist-get plist (pop keys))))
  plist)

(provide 'wpac)
;;; wpac.el ends here
