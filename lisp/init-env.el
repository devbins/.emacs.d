;;; init-env.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-env.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Thu Feb 13 11:24:24 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 12
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar env-vars-file
  (concat user-emacs-directory ".env")
  "Absolute path to the env file where environment variables are set.")

(defvar devbins-ignored-environment-variables
  '(
    "DBUS_SESSION_BUS_ADDRESS"
    "GPG_AGENT_INFO"
    "SSH_AGENT_PID"
    "SSH_AUTH_SOCK"
    )
  "Ignored environments variables. This env. vars are not import in the
`.devbins.env' file.")

(defun init-emacs-env (&optional force)
  "Attempt to fetch the environment variables from the users shell.
This solution is far from perfect and we should not rely on this function
a lot. We use it only to initialize the env file when it does not exist
yet.
If FORCE is non-nil then force the initialization of the file, note that the
current contents of the file will be overwritten."
  (when (or force (not (file-exists-p env-vars-file)))
    (with-temp-file env-vars-file
      (let ((shell-command-switches (cond
                                     ((or(eq system-type 'darwin)
                                         (eq system-type 'cygwin)
                                         (eq system-type 'gnu/linux))
                                      ;; execute env twice, once with a
                                      ;; non-interactive login shell and
                                      ;; once with an interactive shell
                                      ;; in order to capture all the init
                                      ;; files possible.
                                      '("-lc" "-ic"))
                                     ((eq system-type 'windows-nt) '("-c"))))
             (tmpfile (make-temp-file env-vars-file))
             (executable (cond ((or(eq system-type 'darwin)
                                  (eq system-type 'cygwin)
                                  (eq system-type 'gnu/linux)) "env")
                              ((eq system-type 'windows-nt) "set"))))
        (insert
         (concat
          "# ---------------------------------------------------------------------------\n"
          "#                    emacs environment variables\n"
          "# ---------------------------------------------------------------------------\n"))
        (let ((env-point (point)))
          (dolist (shell-command-switch shell-command-switches)
            (call-process-shell-command
             (concat executable " > " (shell-quote-argument tmpfile)))
            (insert-file-contents tmpfile))
          (delete-file tmpfile)
          ;; sort the environment variables
          (sort-regexp-fields nil "^.*$" ".*?=" env-point (point-max))
          ;; remove adjacent duplicated lines
          (delete-duplicate-lines env-point (point-max) nil t)
          ;; remove adjacent duplicated lines
          (delete-duplicate-lines env-point (point-max) nil t)
          ;; remove ignored environment variables
          (dolist (v devbins-ignored-environment-variables)
            (flush-lines v env-point (point-max))))))
    (message
     (concat "emacs has imported your environment variables from "
             "your shell and saved them to `%s'.\n"
             "Open this file for more info")
     env-vars-file)))


(defvar load-env-vars-env-var-regexp
  (rx
   line-start
   (0+ space)
   (optional "export" (0+ space)) ;; optional export
   (group (1+ (in "_" alnum))) ;; key
   (or
    (and (0+ space) "=" (0+ space))
    (and ":" (1+ space))) ;; separator
   (or
    line-start
    (and "'" (group (0+ (or "\\'" (not (any "'"))))) "'") ;; single quoted value
    (and ?\" (group (0+ (or "\\\"" (not (any "\""))))) ?\") ;; double quoted value
    (group (1+ (not (in "#" "\n")))) ;; unquoted value
    )
   (0+ space)
   (optional "#" (0+ any))
   )
  "Regexp to match env vars in file."
  )

(defun load-env-vars-re-seq (regexp)
  "Get a list of all REGEXP matches in a buffer."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let (matches)
        (while (re-search-forward regexp nil t)
          (push (list (match-string-no-properties 1) (or (match-string-no-properties 2) (match-string-no-properties 3) (match-string-no-properties 4))) matches))
        matches))))

(defun load-env-vars-extract-env-vars ()
  "Extract environment variable name and value from STRING."
  (load-env-vars-re-seq load-env-vars-env-var-regexp))
(defun load-env-vars-set-env (env-vars)
  "Set environment variables from key value lists from ENV-VARS."
  (setq exec-path (cl-remove-duplicates (mapcar #'directory-file-name exec-path)
                                        :test #'string-equal :from-end t))
  (let ((convert-to-os-path (if (memq system-type '(windows-nt ms-dos))
                                (apply-partially #'subst-char-in-string ?/ ?\\)
                              ;; Assume that we start with forward slashes.
                              #'identity)))
    (dolist (element env-vars)
      (let ((key (car element)) (value (cadr element)))
        (if (string-equal "PATH" key)
            (let ((paths (split-string value path-separator)))
              (setq exec-path (cl-remove-duplicates
                               (append (mapcar (lambda (path) (directory-file-name (subst-char-in-string ?\\ ?/ path))) paths) exec-path)
                               :test #'string-equal :from-end t)
                    )
              (setenv "PATH" (mapconcat convert-to-os-path exec-path path-separator)))
          (setenv key value))))))

;;;###autoload
(defun load-env-vars (file-path)
  "Load environment variables found in FILE-PATH."
  (interactive "fEnvironment variables file: ")
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((env-vars (load-env-vars-extract-env-vars)))
      (load-env-vars-set-env env-vars))))

(if (not (file-exists-p env-vars-file))
    (progn
      (init-emacs-env t)
      (load-env-vars env-vars-file))
  (load-env-vars env-vars-file))


(provide 'init-env)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-env.el ends here
