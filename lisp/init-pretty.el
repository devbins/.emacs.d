;;; init-pretty.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-pretty.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Thu Feb 13 11:38:36 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 71
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

(use-package pretty-mode
  :commands (turn-on-pretty-mode global-prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist '(("lambda" . ?λ)
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
                                         ("Infinity" . ?∞)))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  :hook (((text-mode
           org-mode)  . turn-on-pretty-mode)
         (after-init  . global-prettify-symbols-mode)
         (prog-mode . (lambda () (mapc (lambda (pair) (push pair prettify-symbols-alist))
                                  '(;; Data Type             P N
                                    ("Float"  . #x211d)  ;; ℝxxxx
                                    ("float"  . #x211d)  ;; ℝxxx
                                    ("Int"    . #x2124)  ;; ℤxxx
                                    ("int"    . #x2124)  ;; 𝕫xxx
                                    ("String" . (#x1d54a (Br . Bl) #x2006))  ;; 𝕊 xxxxxx
                                    ("string" . (#x1d564 (Br . Bl) #x2006))  ;; 𝕤 xxxxxx
                                    ("str"    . (#x1d564 (Br . Bl) #x2006))  ;; 𝕤 xxxx
                                    ("Char"   . #x2102)   ;; ℂx
                                    ("char"   . (#x1d554 (Br . Bl) #x2006))  ;; 𝕔 x

                                    ("False"  . #x1d53d)  ;; 𝕱 𝔽
                                    ("True"   . #x1d54b)  ;; 𝕿 𝕋

                                    ("Any"    . #x2203)  ;; ∃
                                    ("any"    . #x2203)  ;; ∃
                                    ("any_"   . #x2203)  ;; ∃
                                    ("And"    . #x22c0)  ;; ⋀
                                    ("and"    . #x22cf)  ;; ⋏
                                    ("Or"     . #x22c1)  ;; ⋁
                                    ("or"     . #x22cE)  ;; ⋎
                                    ("not_"   . #x00ac)  ;; ¬

                                    ("All"    . #x2200)  ;; ∀
                                    ("all"    . #x2200)  ;; ∀
                                    ("all_"   . #x2200)  ;; ∀
                                    ("for"    . #x2200)  ;; ∀
                                    ("forall" . #x2200)  ;; ∀
                                    ("forM"   . #x2200)  ;; ∀


                                    ("Sum"    . #x2211)  ;; ∑
                                    ("Product" . #x220F) ;; ∏
                                    ("product" . #x220F) ;; ∏

                                    ("None"   . #x2205)  ;; ∅
                                    ("none"   . #x2205)  ;; ∅

                                    ("in"     . #x2286)  ;; ⊆
                                    ("`elem`" . #x2286)  ;; ⊆
                                    ("not in"    . #x2288)  ;; ⊈
                                    ("`notElem`" . #x2288)  ;; ⊈

                                    ("return" . (#x21d2 (Br . Bl) #x2006 (Br . Bl) #x2004))  ;; ⇒  x
                                    ("yield"  . (#x21d4 (Br . Bl) #x2004))  ;; ⇔ x
                                    ("pure"   . (#x21f0 (Br . Bl)))))))          ;; ⇰ x

         (python-mode . (lambda ()
                          (mapc (lambda (pair) (push pair prettify-symbols-alist))
                                '(;; Syntax
                                  ;;("def"    . (#x1d521 (Br . Bl) #x1d522 (Br . Bl) #x1d523))
                                  ("def"    . #x1D487)  ;; 𝒇 1 111
                                  ("List"   . #x1d543)  ;; 𝕃 𝕷
                                  ("list"   . (#x1d55d (Br . Bl) #x2006 (Br . Bl) #x2005))  ;; 𝕝   𝖑
                                  ("Dict"   . #x1d53B)  ;; 𝔻 𝕯
                                  ("dict"   . #x1d555)  ;; 𝕕 𝖉
                                  ("Set"    . #x1d61a)  ;; 𝔖 𝘚
                                  ("set"    . #x1d634)  ;; 𝔰 𝘴
                                  ("Tuple"  . #x1d61b)  ;; 𝕋 𝕿 𝘛
                                  ("tuple"  . #x1d635)  ;; 𝕥 𝖙 𝘵

                                  ("Union"  . #x22c3)  ;; ⋃
                                  ("union"  . #x22c3))))))  ;; ⋃

  :config
  (pretty-activate-groups
   '(:sub-and-superscripts :greek :arithmetic))

  (pretty-deactivate-groups
   '(:equality :ordering :ordering-double :ordering-triple
     :arrows :arrows-twoheaded :punctuation
     :logic :sets :arithmetic-double :arithmetic-triple)))

(require 'fira-code-symbol)
(global-fira-code-symbol-mode 1)

(use-package pretty-magit
  :defer t
  :commands(pretty-magit-setup)
  :load-path (lambda () (expand-file-name "site-lisp/pretty-magit" user-emacs-directory))
  :hook(magit-mode . pretty-magit-setup)
  :config
  (pretty-magit-add-leaders
   '(("Feature" ?🌟 (:foreground "slate gray" :height 1.2))
     ("Add"     ? (:foreground "#375E97" :height 1.2))
     ("Fix"     ?🐛 (:foreground "#FB6542" :height 1.2))
     ("Clean"   ? (:foreground "#FFBB00" :height 1.2))
     ("Perf"    ?🚀 (:foreground "#66bb6a" :height 1.2))
     ("Style"   ?🎨 (:foreground "#4fc3f7" :height 1.2))
     ("Test"    ?🔧 (:foreground "#8d6e63" :height 1.2))
     ("Refactor" ?🔨 (:foreground "#ef6c00" :height 1.2))
     ("Chore"   ?🏠 (:foreground "#9c27b0" :height 1.2))
     ("Docs"    ?📝 (:foreground "#3F681C" :height 1.2)))))

(provide 'init-pretty)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-pretty.el ends here
