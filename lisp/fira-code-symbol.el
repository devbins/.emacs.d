;;; init-fira-code-symbol.el ---
;;
;; Filename: init-fira-code-symbol.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Jul 28 07:48:33 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 5
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

(when (package-installed-p 'prettify-greek)
  (require 'prettify-greek))

;;; inspired from https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; This works when using emacs without server/client
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

(defconst fira-code-symbol-font-lock-keywords-alist
        '(
          ("www" . #Xe100)
          ("**" . #Xe101)
          ("***" . #Xe102)
          ("**/" . #Xe103)
          ("*>" . #Xe104)
          ("*/" . #Xe105)
          ("\\\\" . #Xe106)
          ("\\\\\\" . #Xe107)
          ("{-" . #Xe108)
          ;; ("[]" . #Xe109)
          ("::" . #Xe10a)
          (":::" . #Xe10b)
          (":=" . #Xe10c)
          ("!!" . #Xe10d)
          ("!=" . #Xe10e)
          ("!==" . #Xe10f)
          ("-}" . #Xe110)
          ("--" . #Xe111)
          ("---" . #Xe112)
          ("-->" . #Xe113)
          ("->" . #Xe114)
          ("->>" . #Xe115)
          ("-<" . #Xe116)
          ("-<<" . #Xe117)
          ("-~" . #Xe118)
          ("#{" .  #Xe119)
          ("#[" . #Xe11a)
          ("##" . #Xe11b)
          ("###" . #Xe11c)
          ("####" . #Xe11d)
          ("#(" . #Xe11e)
          ("#?" . #Xe11f)
          ("#_" . #Xe120)
          ("#_(" . #Xe121)
          (".-" . #Xe122)
          (".=" . #Xe123)
          (".." . #Xe124)
          ("..<" . #Xe125)
          ("..." . #Xe126)
          ("?=" . #Xe127)
          ("??" . #Xe128)
          (";;" . #Xe129)
          ("/*" . #Xe12a)
          ("/**" . #Xe12b)
          ("/=" . #Xe12c)
          ("/==" . #Xe12d)
          ("/>" . #Xe12e)
          ("//" . #Xe12f)
          ("///" . #Xe130)
          ("&&" . #Xe131)
          ("||" . #Xe132)
          ("||=" . #Xe133)
          ("|=" . #Xe134)
          ("|>" . #Xe135)
          ("^=" . #Xe136)
          ("$>" . #Xe137)
          ("++" . #Xe138)
          ("+++" . #Xe139)
          ("+>" . #Xe13a)
          ("+>" . #Xe13a)
          ("=:=" . #Xe13b)
          ("==" .  #Xe13c)
          ("===" . #Xe13d)
          ("==>" . #Xe13e)
          ("=>" . #Xe13f)
          ("=>>" . #Xe140)
          ("<=" . #Xe141)
          ("=<<" . #Xe142)
          ("=/=" . #Xe143)
          (">-" .  #Xe144)
          (">=" . #Xe145)
          (">=>" . #Xe146)
          (">>" . #Xe147)
          (">>-" . #Xe148)
          (">>=" . #Xe149)
          (">>>" . #Xe14a)
          ("<*" . #Xe14b)
          ("<*>" . #Xe14c)
          ("<|" . #Xe14d)
          ("<|>" . #Xe14e)
          ("<$" . #Xe14f)
          ("<$>" . #Xe150)
          ("<!--" . #Xe151)
          ("<-" . #Xe152)
          ("<--" . #Xe153)
          ("<->" . #Xe154)
          ("<+" . #Xe155)
          ("<+>" . #Xe156)
          ("<=" . #Xe157)
          ("<==" . #Xe158)
          ("<=>" . #Xe159)
          ("<=<" . #Xe15a)
          ("<>" . #Xe15b)
          ("<<" . #Xe15c)
          ("<<-" . #Xe15d)
          ("<<=" . #Xe15e)
          ("<<<" . #Xe15f)
          ("<~" . #Xe160)
          ("<~~" . #Xe161)
          ("</" . #Xe162)
          ("</>" . #Xe163)
          ("~@" .  #Xe164)
          ("~-" . #Xe165)
          ("~=" . #Xe166)
          ("~>" . #Xe167)
          ("~~" . #Xe168)
          ("~~>" . #Xe169)
          ("%%" . #Xe16a)
          ("x" . #Xe16b)
          (":" . #Xe16c)
          ("+" . #Xe16d)
          ("*" . #Xe16f)))

(defvar fira-code-symbol-mode--old-prettify-alist)

(defun turn-on-fira-code-symbol-mode ()
  "Enable Fira Code ligatures in current buffer."
  (if (featurep 'prettify-greek)
      (setq-local fira-code-symbol-mode--old-prettify-alist (append prettify-greek-lower prettify-greek-upper prettify-symbols-alist))
    (setq-local fira-code-symbol-mode--old-prettify-alist prettify-symbols-alist))
  (setq-local prettify-symbols-alist (append fira-code-symbol-font-lock-keywords-alist fira-code-symbol-mode--old-prettify-alist))
  (prettify-symbols-mode 1))

(defun turn-off-fira-code-symbol-mode ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist fira-code-symbol-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode fira-code-symbol-mode
  "Fira Code ligatures minor mode"
  :lighter " Fira Code Symbol"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-symbol-mode
      (turn-on-fira-code-symbol-mode)
    (turn-off-fira-code-symbol-mode)))

(define-globalized-minor-mode global-fira-code-symbol-mode fira-code-symbol-mode
  (lambda () (fira-code-symbol-mode 1)))

(defun fira-code-symbol-hook ()
  "enable fira code symbol in major mode"
  (fira-code-symbol-mode 1))

(defun remove-fira-code-symbol-from-major-mode ()
  "disable fira code symbol in current major mode & remove hook in major-mode-hook"
  (remove-hook (intern (format "%s-hook" major-mode)) 'fira-code-symbol-hook))

(defun add-fira-code-symbol-from-major-mode ()
  "enable fira code symbol in current major mode & remove hook in major-mode-hook"
  (add-hook (intern (format "%s-hook" major-mode)) 'fira-code-symbol-hook))

(defun disable-fira-code-symbol-in-major-mode ()
  "disable in all buffers opened in current major mode & remove hook from current major mode "
  (interactive)
  (let ((current-buffer-major-mode major-mode))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (if (and (eq major-mode current-buffer-major-mode) fira-code-symbol-mode)
            (fira-code-symbol-mode -1)))))
  (remove-fira-code-symbol-from-major-mode)
  (remove-hook 'prog-mode-hook 'fira-code-symbol-hook))

(defun enable-fira-code-symbol-in-major-mode ()
  "enable in all buffers opened in current major mode & add hook to current major mode "
  (interactive)
  (let ((current-buffer-major-mode major-mode))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (if (and (eq major-mode current-buffer-major-mode) (not (eq fira-code-symbol-mode 1)))
            (fira-code-symbol-mode 1)))))
  (add-fira-code-symbol-from-major-mode))


(provide 'fira-code-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-fira-code-symbol.el ends here
