;;; init-misc.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-misc.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Thu Feb 13 11:38:36 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 15
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

(use-package aria2
  :defer t
  :commands aria2-downloads-list
  :quelpa (aria2 :fetcher github :repo "LdBeth/aria2.el")
  :config
  (setq aria2-download-directory (expand-file-name "~/Downloads"))
  (evil-define-key 'normal aria2-mode-map
    "f" 'aria2-add-file
    "u" 'aria2-add-uris
    "D" 'aria2-remove-download
    "C" 'aria2-clean-removed-download
    "q" 'quit-window
    "Q" 'aria2-terminate))

;; Header2Pac
(use-package header2
  :hook (emacs-lisp-mode . auto-make-header)
  :load-path (lambda () (expand-file-name "site-lisp/header2" user-emacs-directory))
  :custom
  (header-copyright-notice (concat "Copyright (C) 2019 " (user-full-name) "\n"))
  :config
  (autoload 'auto-make-header "header2")
  (autoload 'auto-update-file-header "header2")
  (add-hook 'write-file-hooks 'auto-update-file-header))


;; broadway isometric1 starwars doom banner
;; sudo pacman -S figlet
(use-package figlet
  :config (setq figlet-default-font "banner"))

(use-package carbon-now-sh)

(use-package speed-type)

(use-package telega
  :commands (telega)
  :defer t
  :init (setq telega-use-images (if (display-graphic-p) t nil)
              telega-proxies (list
                              '(:server "127.0.0.1" :port 1086 :enable t
                                :type (:@type "proxyTypeSocks5"))
                              )))

(use-package password-store)

(provide 'init-misc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-misc.el ends here
