;;; init-ivy.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ivy.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:23:51 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 134
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

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind
  (:map vertico-map
   ("C-j" . vertico-next)
   ("C-k" . vertico-previous)
   ("C-f" . vertico-exit)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  (:map vertico-map
   ("RET"   . 'vertico-directory-enter)
   ("/"     . 'vertico-directory-enter)
   ("DEL"   . 'vertico-directory-delete-char)
   ("M-DEL" . 'vertico-directory-delete-word)))

(use-package vertico-posframe
  :after vertico
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (setq vertico-posframe-parameters '((left-fringe . 8)
                                      (right-fringe . 8)
                                      (weight . 50))))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         ([remap apropos]                       . consult-apropos)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap evil-show-marks]               . consult-mark)
         ([remap evil-show-registers]           . consult-register)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         :map isearch-mode-map
         ("M-e" . consult-isearch)
         ("M-s e" . consult-isearch)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :init
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'multi-occur :override #'consult-multi-occur)
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (setq consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :preface
  (defun recenter-on-top ()
    "`recenter' on top"
    (interactive)
    (recenter 0))
  (defun reveal-entry ()
    "Reveal Org or Outline entry and recenter on top."
    (cond
     ((and (eq major-mode 'org-mode)
           (org-at-heading-p))
      (org-show-entry))
     ((and (or (eq major-mode 'outline-mode)
               (bound-and-true-p outline-minor-mode))
           (outline-on-heading-p))
      (outline-show-entry)))))

(use-package consult-yasnippet
  :after (consult yasnippet)
  :bind ("M-s y" . consult-yasnippet))

(use-package marginalia
   :hook (after-init . marginalia-mode))

(use-package all-the-icons-completion
  :after marginalia
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :bind
  (:map corfu-map
   ([tab] . smarter-tab-to-complete)
   ("TAB" . smarter-tab-to-complete)
   ("M-/" . smarter-tab-to-complete)
   ("C-j" . corfu-next)
   ("C-k" . corfu-previous)
   ("C-f" . corfu-insert)
   ("C-d" . corfu-info-documentation)
   ("M-." . corfu-info-location))
  :preface
  (defun smarter-tab-to-complete ()
    "Try to `org-cycle', `yas-expand', and `yas-next-field' at current cursor position.
If all failed, try to complete the common part with `corfu-complete'"
    (interactive)
    (when yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (func-list
             (if (equal major-mode 'org-mode) '(org-cycle yas-expand yas-next-field)
               '(yas-expand yas-next-field))))
        (catch 'func-suceed
          (dolist (func func-list)
            (ignore-errors (call-interactively func))
            (unless (and (eq old-point (point))
                         (eq old-tick (buffer-chars-modified-tick)))
              (throw 'func-suceed t)))
          (corfu-complete)))))
  :init
  (setq corfu-auto t
        corfu-cycle t
        corfu-auto-delay 0.0
        corfu-auto-prefix 1
        corfu-on-exact-match nil
        corfu-preview-current nil
        corfu-echo-documentation t)
  :config
  (with-eval-after-load 'all-the-icons
    (defvar kind-all-the-icons--cache nil
      "The cache of styled and padded label (text or icon).
An alist.")

    (defun kind-all-the-icons-reset-cache ()
      "Remove all cached icons from `kind-all-the-icons-mapping'."
      (interactive)
      (setq kind-all-the-icons--cache nil))

    (defun kind-all-the-icons--set-default-clear-cache (&rest args)
      (kind-all-the-icons-reset-cache)
      (apply #'set-default args))

    (defvar kind-all-the-icons--icons
      `((unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
        (text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
        (method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (fun . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (ctor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (var . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (i/f . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (mod . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
        (prop . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
        (unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
        (value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
        (k/w . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
        (snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
        (sn . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
        (color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
        (file . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
        (reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
        (ref . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
        (folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
        (dir . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
        (enum-member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (enummember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
        (const . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
        (struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
        (operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
        (op . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
        (type-parameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
        (param . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
        (template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15))
        (t . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))))


    (defsubst kind-all-the-icons--metadata-get (metadata type-name)
      (or
       (plist-get completion-extra-properties (intern (format ":%s" type-name)))
       (cdr (assq (intern type-name) metadata))))

    (defun kind-all-the-icons-formatted (kind)
      "Format icon kind with all-the-icons"
      (or (alist-get kind kind-all-the-icons--cache)
          (let ((map (assq kind kind-all-the-icons--icons)))
            (let*  ((icon (if map
                              (cdr map)
                            (cdr (assq t kind-all-the-icons--icons))))
                    (half (/ (default-font-width) 2))
                    (pad (propertize " " 'display `(space :width (,half))))
                    (disp (concat pad icon pad)))
              (setf (alist-get kind kind-all-the-icons--cache) disp)
              disp))))

    (defun kind-all-the-icons-margin-formatter (metadata)
      "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
      (if-let ((kind-func (kind-all-the-icons--metadata-get metadata "company-kind")))
          (lambda (cand)
	          (if-let ((kind (funcall kind-func cand)))
	              (kind-all-the-icons-formatted kind)
	            (kind-all-the-icons-formatted t))))) ;; as a backup
    (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter)
    ))

(use-package corfu-doc
  :after corfu
  :bind (:map corfu-map
         ("M-p" . corfu-doc-scroll-down)
         ("M-n" . corfu-doc-scroll-up)
         ("M-d" . corfu-doc-toggle))
  :hook (corfu-mode . corfu-doc-mode))

(use-package corfu-history
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-history-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package copilot
  :quelpa (copilot :fetcher github :repo "zerolfx/copilot.el"
                   :files ("*.el" "dist"))
  :init
  ;; accept completion from copilot and fallback to company
  (defun my-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (corfu-complete))))

(provide 'init-ivy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
