;;; init-w3m.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-w3m.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Thu Mar 26 08:59:40 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 3
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

(use-package w3m
  :defer t
  :init (setq w3m-coding-system 'utf-8
              w3m-file-coding-system 'utf-8
              w3m-file-name-coding-system 'utf-8
              w3m-input-coding-system 'utf-8
              w3m-output-coding-system 'utf-8
              ;; emacs-w3m will test the ImageMagick support for png32
              ;; and create files named "png32:-" everywhere
              w3m-imagick-convert-program nil
              w3m-terminal-coding-system 'utf-8
              w3m-use-cookies t
              w3m-cookie-accept-bad-cookies t
              w3m-home-page "http://www.google.com.au"
              w3m-command-arguments       '("-F" "-cookie")
              w3m-mailto-url-function     'compose-mail
              ;; use shr to view html mail which is dependent on libxml
              ;; I prefer w3m. That's emacs 24.3+ default setup.
              ;; If you prefer colored mail body and other advanced features,
              ;; you can either comment out below line and let Emacs decide the
              ;; best html mail rendering engine, or "(setq mm-text-html-renderer 'shr)"
              ;; in "~/.gnus.el"
              ;; mm-text-html-renderer 'w3m ; I prefer w3m
              w3m-use-toolbar t
              ;; show images in the browser
              ;; setq w3m-default-display-inline-images t
              ;; w3m-use-tab     nil
              w3m-confirm-leaving-secure-page nil
              w3m-search-default-engine "g"
              w3m-key-binding 'info)
  :hook (w3m-mode . w3m-lnum-mode)
  :config
  (defun w3m-get-url-from-search-engine-alist (k l)
    (let (rlt)
      (if (listp l)
          (if (string= k (caar l))
              (setq rlt (nth 1 (car l)))
            (setq rlt (w3m-get-url-from-search-engine-alist k (cdr l)))))
      rlt))

  ;; C-u S g RET <search term> RET in w3m
  (setq w3m-search-engine-alist
        '(("g" "http://www.google.com.au/search?q=%s" utf-8)
          ;; stackoverflow search
          ("q" "http://www.google.com.au/search?q=%s+site:stackoverflow.com" utf-8)
          ;; wikipedia
          ("w" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
          ;; online dictionary
          ("d" "http://dictionary.reference.com/search?q=%s" utf-8)
          ;; financial dictionary
          ("f" "http://financial-dictionary.thefreedictionary.com/%s" utf-8)))

  (defun w3m-set-url-from-search-engine-alist (k l url)
    (if (listp l)
        (if (string= k (caar l))
            (setcdr (car l) (list url))
          (w3m-set-url-from-search-engine-alist k (cdr l) url))))

  (defvar w3m-global-keyword nil
    "`w3m-display-hook' must search current buffer with this keyword twice if not nil")

  (defun w3m-guess-keyword (&optional encode-space-with-plus)
    (let* ((keyword (my-use-selected-string-or-ask "Enter keyword:"))
           (encoded-keyword (w3m-url-encode-string (setq w3m-global-keyword keyword))))
      ;; some search requires plus sign to replace space
      (if encode-space-with-plus
          (replace-regexp-in-string "%20" " " encoded-keyword)
        encoded-keyword)))

  (defun w3m-customized-search-api (search-engine &optional encode-space-with-plus)
    (w3m-search search-engine (w3m-guess-keyword encode-space-with-plus)))

  (defun w3m-stackoverflow-search ()
    (interactive)
    (w3m-customized-search-api "q"))

  (defun w3m-google-search ()
    "Google search keyword"
    (interactive)
    (w3m-customized-search-api "g"))

  (defun w3m-search-financial-dictionary ()
    "Search financial dictionary"
    (interactive)
    (w3m-customized-search-api "f" t))


  ;; {{ Search using external browser
  (setq browse-url-generic-program
        (cond
         (sys/macp ; mac
          "open")
         (sys/linuxp ; linux or unix
          ;; prefer Chrome than Firefox
          (or (executable-find "google-chrome")
             (executable-find "firefox")))
         (t
          ;; Windows: you need add "firefox.exe" to environment variable PATH
          ;; @see https://en.wikipedia.org/wiki/PATH_(variable)
          (executable-find "firefox")
          ;; if you prefer chrome
          ;; (executable-find "chrome")
          )))

  (setq browse-url-browser-function 'browse-url-generic)

  ;; use external browser to search programming stuff
  (defun w3mext-hacker-search ()
    "Search on all programming related sites in external browser"
    (interactive)
    (let ((keyword (w3m-guess-keyword)))
      ;; google
      (browse-url-generic (concat "http://www.google.com.au/search?hl=en&q=%22"
                                  keyword
                                  "%22"
                                  (if buffer-file-name
                                      (concat "+filetype%3A" (file-name-extension buffer-file-name))
									"")))
      ;; stackoverflow.com
      (browse-url-generic (concat "http://www.google.com.au/search?hl=en&q="
                                  keyword
                                  "+site:stackoverflow.com" ))))
  ;; }}

  (defun w3mext-open-link-or-image-or-url ()
    "Opens the current link or image or current page's uri or any url-like text under cursor in firefox."
    (interactive)
    (let* (url)
      (when (or (string= major-mode "w3m-mode") (string= major-mode "gnus-article-mode"))
        (setq url (w3m-anchor))
        (if (or (not url) (string= url "buffer://"))
            (setq url (or (w3m-image) w3m-current-url))))
      (browse-url-generic (if url url (car (browse-url-interactive-arg "URL: "))))))

  (defun w3mext-encode-specials (str)
    (setq str (replace-regexp-in-string "(" "%28" str))
    (setq str (replace-regexp-in-string ")" "%29" str))
    (setq str (replace-regexp-in-string ")" "%20" str))))

(provide 'init-w3m)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-w3m.el ends here
