;;; init-company.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-company.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:24:31 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 64
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


(use-package company-tabnine
  :disabled
  :after company
  :config
  (defun tabnine//merge-company-tabnine-to-company-lsp ()
    (when (memq 'company-lsp company-backends)
      (setq-local company-backends (remove 'company-lsp company-backends))
      (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))))

  (defun tabnine//company-box-icons--tabnine (candidate)
    (when (eq (get-text-property 0 'company-backend candidate)
              'company-tabnine)
      'Reference))

  (defun tabnine//sort-by-tabnine (candidates)
    "The first two candidates will be from company-lsp, the following two
candidates will be from company-tabnine, others keeping their own origin order."
    (if (or (functionp company-backend)
           (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-1
            candidates-2)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-2))
            (push candidate candidates-1)
            (puthash candidate t candidates-table)))
        (setq candidates-1 (nreverse candidates-1))
        (setq candidates-2 (nreverse candidates-2))
        (nconc (seq-take candidates-1 2)
               (seq-take candidates-2 2)
               (seq-drop candidates-1 2)
               (seq-drop candidates-2 2)))))

  (add-to-list 'company-backends #'company-tabnine)

  (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
    (let ((company-message-func (ad-get-arg 0)))
      (when (and company-message-func
               (stringp (funcall company-message-func)))
        (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
          ad-do-it))))

  (with-eval-after-load 'lsp-mode
    (advice-add 'lsp :after #'tabnine//merge-company-tabnine-to-company-lsp)))

(use-package insert-translated-name
  :quelpa (insert-translated-name :fetcher github :repo "manateelazycat/insert-translated-name")
  :bind ("C-c t t" . 'insert-translated-name-insert)
  :commands (insert-translated-name-insert)
  :init (setq insert-translated-name-translate-engine 'youdao)
  :config
  (defvar insert-translated-name-camel-style-mode-list
    '(go-mode)))

(use-package corfu-english-helper
  :quelpa (corfu-english-helper :fetcher github :repo "manateelazycat/corfu-english-helper")
  :after corfu
  :commands (toggle-corfu-english-helper corfu-english-helper-search)
  :bind ("C-c t e" . 'toggle-corfu-english-helper))


(provide 'init-company)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
