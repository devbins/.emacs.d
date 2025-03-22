;;; init-custom.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-custom.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:16:30 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 65
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

(defgroup devbins nil
  "Emacs customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/devbins/.emacs.d"))

;; ELPA: refer to https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom package-archives-alist
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    `(,(cons 'melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
      ,(cons 'melpa-mirror
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
      ,(cons 'emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
      ,(cons 'netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
      ,(cons 'ustc
             `(,(cons "gnu"   (concat proto "://mirrors.ustc.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.ustc.edu.cn/elpa/melpa/"))))
      ,(cons 'tencent
             `(,(cons "gnu"   (concat proto "://mirrors.cloud.tencent.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.cloud.tencent.com/elpa/melpa/"))))
      ,(cons 'tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))))
  "The package archives group list."
  :group 'devbins
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom devbins-package-archives 'melpa
  "Set package archives from which to fetch."
  :group 'devbins
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value package-archives-alist)
                   (error "Unknown package archives: `%s'" value)))
         (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    package-archives-alist)))

(setq devbins-package-archives 'melpa)

(defcustom prettify-prog-symbols-alist
  '(("lambda" . ?λ)
    ("<-" . ?←)
    ("->" . ?→)
    ("->>" . ?↠)
    ("=>" . ?⇒)
    ("map" . ?↦)
    ("/=" . ?≠)
    ("!=" . ?≠)
    ("==" . ?≡)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("<=<" . ?↢)
    (">=>" . ?↣)
    ("&&" . ?∧)
    ("||" . ?∨)
    ("not" . ?¬)
    ("alpha" . ?α)
    ("beta" . ?β)
    ("gamma" . ?γ)
    ("delta" . ?Δ)
    ("epsilon" . ?ε)
    ("zeta" . ?ζ)
    ("eta" . ?η)
    ("theta" . ?θ)
    ("micro" . ?μ)
    ("pi" . ?π)
    ("rho" . ?ρ)
    ("sigma" . ?σ)
    ("phi" . ?φ)
    ("omega" . ?Ω)
    ("sqrt" . ?√)
    ("sum" . ?∑)
    ("infinity" . ?∞)
    ("Infinity" . ?∞)

    ("False"  . ?𝔽)
    ("True"   . ?𝕋)

    ("None"   . ?∅)
    ("none"   . ?∅)

    ("in"     . ?⊆)
    ("not in"    . ?⊈)
    ("`notElem`" . ?⊈)
    ("return" . ?⇒)
    ("def"    . ?𝒇))
  "Alist of symbol prettifications for `prog-mode'."
  :group 'devbins
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom prettify-org-symbols-alist
  '(("[ ]" . ?☐)
    ("[X]" . ?☑)
    ("[-]" . ?⛝)

    (":PROPERTIES:"   . ?)
    (":ID:"           . ?🪪)
    (":END:"          . ?🔚)

    ("#+ARCHIVE:" . ?📦)
    ("#+AUTHOR:" . ?👤)
    (":AUTHOR:" . ?👤)
    ("#+CREATOR:" . ?💁)
    (":CREATOR:" . ?💁)
    ("#+DATE:" . ?📆)
    (":DATE:" . ?📆)
    ("#+DESCRIPTION:" . ?⸙)
    (":DESCRIPTION:" . ?⸙)
    ("#+EMAIL:" . ?📧)
    ("#+OPTIONS:" . ?⚙)
    ("#+SETUPFILE:" . ?⚒)
    ("#+TAGS:" . ?🏷)
    ("#+TITLE:" . ?📓)

    ("#+BEGIN_SRC" . ?✎)
    ("#+END_SRC" . ?□)
    ("#+begin_src" . ?✎)
    ("#+end_src" . ?□)
    ("#+BEGIN_QUOTE" . ?»)
    ("#+END_QUOTE" . ?«)
    ("#+HEADERS" . ?☰)
    ("#+RESULTS:" . ?💻)
    ("#+attr_latex:"    . "🄛")
	("#+attr_html:"     . "🄗")
	("#+attr_org:"      . "🄞")
	("#+name:"          . "🄝")
	("#+caption:"       . "🄒")
    ("[#A]" . ?🅐)
    ("[#B]" . ?🅑)
    ("[#C]" . ?🅒))
  "Alist of symbol prettifications for `org-mode'."
  :group 'devbins
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom prettify-magit-symbols-alist
  '(("Feature:" . ?🌟)
    ("Add:" . ?)
    ("Fix:" . ?🐛)
    ("Clean:" . ?)
    ("Perf:" . ?🚀)
    ("Style:" . ?🎨)
    ("Test:" . ?🔧)
    ("Refactor:" . ?🔨)
    ("Chore:" . ?🏠)
    ("Docs:" . ?📝))
  "Alist of symbol prettifications for `org-mode'."
  :group 'devbins
  :type '(alist :key-type string :value-type (choice character sexp)))


(setq org-agenda-dir "~/.org/.agenda/"
      org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir)
      org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir)
      org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir)
      org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir)
      org-agenda-file-punch (expand-file-name "punch.org" org-agenda-dir))

(provide 'init-custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
