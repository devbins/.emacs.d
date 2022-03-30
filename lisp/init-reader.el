;;; reader.el --- -*- lexical-binding: t -*-
;;
;; Filename: reader.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Tue May  4 18:01:10 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 31
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

;; PDF reader
(use-package pdf-view
  :if (display-graphic-p)
  :ensure pdf-tools
  :diminish (pdf-view-themed-minor-mode pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
  :defines pdf-annot-activate-created-annotations
  :hook ((pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
         (pdf-tools-enabled . pdf-isearch-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
         ("C-s" . isearch-forward)
         ("j" . pdf-view-next-line-or-next-page)
         ("k" . pdf-view-previous-line-or-previous-page)
         ("J" . pdf-view-next-page)
         ("K" . pdf-view-previous-page)
         ("G" . pdf-view-last-page))
  :init (setq pdf-view-use-scaling t
              pdf-view-use-imagemagick nil
              pdf-annot-activate-created-annotations t)
  :config
  ;; Activate the package
  (pdf-tools-install t nil t nil)

  (with-no-warnings
    (defun my-pdf-isearch-hl-matches (current matches &optional occur-hack-p)
      "Highlighting edges CURRENT and MATCHES."
      (cl-destructuring-bind (fg1 bg1 fg2 bg2)
          (pdf-isearch-current-colors)
        (let* ((width (car (pdf-view-image-size)))
               (page (pdf-view-current-page))
               (window (selected-window))
               (buffer (current-buffer))
               (tick (cl-incf pdf-isearch--hl-matches-tick))
               (pdf-info-asynchronous
                (lambda (status data)
                  (when (and (null status)
                           (eq tick pdf-isearch--hl-matches-tick)
                           (buffer-live-p buffer)
                           (window-live-p window)
                           (eq (window-buffer window)
                               buffer))
                    (with-selected-window window
                      (when (and (derived-mode-p 'pdf-view-mode)
                               (or isearch-mode
                                  occur-hack-p)
                               (eq page (pdf-view-current-page)))
                        (pdf-view-display-image
                         (pdf-view-create-image data :width width))))))))
          (pdf-info-renderpage-text-regions
           page width t nil
           `(,fg1 ,bg1 ,@(pdf-util-scale-pixel-to-relative
                          current))
           `(,fg2 ,bg2 ,@(pdf-util-scale-pixel-to-relative
                          (apply 'append
                                 (remove current matches))))))))
    (advice-add #'pdf-isearch-hl-matches :override #'my-pdf-isearch-hl-matches)

    (defun my-pdf-annot-show-annotation (a &optional highlight-p window)
      "Make annotation A visible."
      (save-selected-window
        (when window (select-window window))
        (pdf-util-assert-pdf-window)
        (let ((page (pdf-annot-get a 'page))
              (size (pdf-view-image-size)))
          (unless (= page (pdf-view-current-page))
            (pdf-view-goto-page page))
          (let ((edges (pdf-annot-get-display-edges a)))
            (when highlight-p
              (pdf-view-display-image
               (pdf-view-create-image
                   (pdf-cache-renderpage-highlight
                    page (car size)
                    `("white" "steel blue" 0.35 ,@edges))
                 :map (pdf-view-apply-hotspot-functions
                       window page size)
                 :width (car size))))
            (pdf-util-scroll-to-edges
             (pdf-util-scale-relative-to-pixel (car edges)))))))
    (advice-add #'pdf-annot-show-annotation :override #'my-pdf-annot-show-annotation))

  ;; Recover last viewed position
  (use-package saveplace-pdf-view
    :commands (saveplace-pdf-view-find-file-advice saveplace-pdf-view-to-alist-advice)
    :init
    (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
    (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice))

  (evil-leader/set-key-for-mode 'pdf-view-mode
    ;; Slicing image
    "msm" 'pdf-view-set-slice-using-mouse
    "msb" 'pdf-view-set-slice-from-bounding-box
    "msr" 'pdf-view-reset-slice
    ;; Annotations
    "maD" 	'pdf-annot-delete
    "mat" 	'pdf-annot-attachment-dired
    "mah" 	'pdf-annot-add-highlight-markup-annotation
    "mal" 	'pdf-annot-list-annotations
    "mam" 	'pdf-annot-add-markup-annotation
    "mao" 	'pdf-annot-add-strikeout-markup-annotation
    "mas" 	'pdf-annot-add-squiggly-markup-annotation
    "mat" 	'pdf-annot-add-text-annotation
    "mau" 	'pdf-annot-add-underline-markup-annotation
    ;; Fit image to window
    "mfw" 'pdf-view-fit-width-to-window
    "mfh" 'pdf-view-fit-height-to-window
    "mfp" 'pdf-view-fit-page-to-window
    ;; Other
    "mss" 'pdf-occur
    "mp" 'pdf-misc-print-document
    "mO" 'pdf-outline
    "mn" 'pdf-view-midnight-minor-mode)

  (evil-define-key 'normal pdf-view-mode-map
    ;; Navigation
    "+"  'pdf-view-enlarge
    "-"  'pdf-view-shrink
    "0"  'image-bol
    "$"  'image-eol
    "j"  'pdf-view-next-line-or-next-page
    "k"  'pdf-view-previous-line-or-previous-page
    "l"  'image-forward-hscroll
    "h"  'image-backward-hscroll
    "J"  'pdf-view-next-page
    "K"  'pdf-view-previous-page
    "gg"  'pdf-view-first-page
    "G"  'pdf-view-last-page
    "gt"  'pdf-view-goto-page
    "gl"  'pdf-view-goto-label
    "u" 'pdf-view-scroll-down-or-previous-page
    "d" 'pdf-view-scroll-up-or-next-page
    (kbd "C-u") 'pdf-view-scroll-down-or-previous-page
    (kbd "C-d") 'pdf-view-scroll-up-or-next-page
    (kbd "``")  'pdf-history-backward
    ;; Search
    "/" 'isearch-forward
    "?" 'isearch-backward
    ;; Actions
    "r"   'pdf-view-revert-buffer
    "o"   'pdf-links-action-perform
    "O"   'pdf-outline
    "zr"  'pdf-view-scale-reset)

  (evil-define-key 'visual pdf-view-mode-map "y" 'pdf-view-kill-ring-save)

  (evil-define-key 'normal pdf-outline-buffer-mode-map
    "-"                'negative-argument
    "j"                'next-line
    "k"                'previous-line
    "gk"               'outline-backward-same-level
    "gj"               'outline-forward-same-level
    (kbd "<backtab>")  'show-all
    "gh"               'pdf-outline-up-heading
    "gg"               'beginning-of-buffer
    "G"                'pdf-outline-end-of-buffer
    (kbd "TAB")              'outline-toggle-children
    (kbd "RET")              'pdf-outline-follow-link
    (kbd "M-RET")      'pdf-outline-follow-link-and-quit
    "f"                'pdf-outline-display-link
    [mouse-1]          'pdf-outline-mouse-display-link
    "o"                'pdf-outline-select-pdf-window
    "``"               'pdf-outline-move-to-current-page
    "''"               'pdf-outline-move-to-current-page
    "Q"                'pdf-outline-quit-and-kill
    "q"                'quit-window
    "F"                'pdf-outline-follow-mode)

  (evil-define-key 'normal pdf-annot-list-mode-map
    "f"                'pdf-annot-list-display-annotation-from-id
    "d"                'tablist-flag-forward
    "x"                'tablist-do-flagged-delete
    "u"                'tablist-unmark-forward
    "q"                'tablist-quit)


  (evil-define-key 'normal pdf-occur-buffer-mode-map
    "q"              'tablist-quit
    "g"              'pdf-occur-revert-buffer-with-args
    "r"              'pdf-occur-revert-buffer-with-args
    "?"              'evil-search-backward))

;; Epub reader
(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :functions read-mode
  :hook (nov-mode . (lambda ()
                      (my-nov-setup)
                      (shrface-mode)))
  :init
  (defun my-nov-setup ()
    "Setup `nov-mode' for better reading experience."
    (visual-line-mode 1)
    (read-mode)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5))
  :config
  ;; FIXME: errors while opening `nov' files with Unicode characters
  ;; @see https://github.com/wasamasa/nov.el/issues/63
  (with-no-warnings
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (when-let* ((name (nov-content-unique-identifier-name content))
                  (selector (format "package>metadata>identifier[id='%s']"
                                    (regexp-quote name)))
                  (id (car (esxml-node-children (esxml-query selector content)))))
        (intern id)))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))

  ;; Fix encoding issue on Windows
  (when sys/win32p
    (setq process-coding-system-alist
          (cons `(,nov-unzip-program . (gbk . gbk))
                process-coding-system-alist)))
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions)))

;; Nice writing
(use-package olivetti
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width .67))

(provide 'init-reader)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reader.el ends here
