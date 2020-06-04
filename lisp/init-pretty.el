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
;;     Update #: 35
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

         ((prog-mode
           emacs-lisp-mode) . (lambda () (mapc (lambda (pair) (push pair prettify-symbols-alist))
                                          '(;; Global
                                            ;; Pipes
                                            ("<|"  . (?\s (Br . Bl) #Xe14d))
                                            ("<>"  . (?\s (Br . Bl) #Xe15b))
                                            ("<|>" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe14e))
                                            ("|>"  . (?\s (Br . Bl) #Xe135))

                                            ;; Brackets
                                            ("<*"  . (?\s (Br . Bl) #Xe14b))
                                            ("<*>" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe14c))
                                            ("*>"  . (?\s (Br . Bl) #Xe104))
                                            ("<$"  . (?\s (Br . Bl) #Xe14f))
                                            ("<$>" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe150))
                                            ("$>"  . (?\s (Br . Bl) #Xe137))
                                            ("<+"  . (?\s (Br . Bl) #Xe155))
                                            ("<+>" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe156))
                                            ("+>"  . (?\s (Br . Bl) #Xe13a))
                                            ("[]"  . (#x2005 (Br . Bl) #x1d731 (Br . Bl) #x2005))

                                            ;; Equality
                                            ("=/="  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe143))
                                            ("/=="  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe12d))
                                            ("/==>" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) #Xe13c))
                                            ("!==>" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) #Xe13c))
                                            ;; Special
                                            ("||="  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe133))
                                            ("|="   . (?\s (Br . Bl) #Xe134))
                                            ("~="   . (?\s (Br . Bl) #Xe166))
                                            ("^="   . (?\s (Br . Bl) #Xe136))
                                            ("=:="  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe13b))

                                            ;; Comparisons
                                            ("</"   . (?\s (Br . Bl) #Xe162))
                                            ("</>"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe163))

                                            ;; Shifts
                                            ("=>>"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe147))
                                            (">>>"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe14a))
                                            (">>>"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe14a))
                                            ("-<<"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe15c))
                                            ("<<<"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe15f))

                                            ;; Dots
                                            (".-"   . (?\s (Br . Bl) #Xe122))
                                            (".="   . (?\s (Br . Bl) #Xe123))
                                            ("..<"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe125))

                                            ;; Hashes
                                            ("#{"   . (?\s (Br . Bl) #Xe119))
                                            ("#("   . (?\s (Br . Bl) #Xe11e))
                                            ("#_"   . (?\s (Br . Bl) #Xe120))
                                            ("#_("  . (?\s (Br . Bl) #Xe121))
                                            ("#?"   . (?\s (Br . Bl) #Xe11f))
                                            ("#["   . (?\s (Br . Bl) #Xe11a))

                                            ;; REPEATED CHARACTERS
                                            ;; 2-Repeats
                                            ("!!"   . (?\s (Br . Bl) #Xe10d))
                                            ("%%"   . (?\s (Br . Bl) #Xe16a))

                                            ;; 2+3-Repeats
                                            ("##"   . (?\s (Br . Bl) #Xe11b))
                                            ("###"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe11c))
                                            ("####" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe11d))
                                            ;; ("---"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe112))
                                            ("{-"   . (?\s (Br . Bl) #Xe108))
                                            ("-}"   . (?\s (Br . Bl) #Xe110))
                                            ("\\\\" . (?\s (Br . Bl) #Xe106))
                                            ("\\\\\\" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe107))
                                            (".."   . (?\s (Br . Bl) #Xe124))
                                            ("..."  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe126))
                                            ("+++"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe139))
                                            ("//"   . (?\s (Br . Bl) #Xe12f))
                                            ("///"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe130))
                                            ("::"   . (?\s (Br . Bl) #Xe10a))  ;; 
                                            (":::"  . (?\s (Br . Bl) ?\s (Br . Bl) #Xe10b))

                                            ;; Arrows
                                            ;; Direct
                                            ;; ("->"  . (?\s (Br . Bl) #Xe114))  ;; 
                                            ;; ("=>"  . (?\s (Br . Bl) #Xe13f))
                                            ("=>>" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe140))
                                            ("<<-" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe15d))
                                            ("<<=" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe15e))
                                            ("<->" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe154))
                                            ("<=>" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe159))
                                            ;; Branches
                                            ("-<"  . (?\s (Br . Bl) #Xe116))
                                            ("-<<" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe117))
                                            (">-"  . (?\s (Br . Bl) #Xe144))
                                            (">>-" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe148))
                                            ("=<<" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe142))
                                            ;; Squiggly
                                            ("<~"  . (?\s (Br . Bl) #Xe160))
                                            ("<~~" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe161))
                                            ("~>"  . (?\s (Br . Bl) #Xe167))
                                            ("~~>" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe169))
                                            ("-~"  . (?\s (Br . Bl) #Xe118))
                                            ("~-"  . (?\s (Br . Bl) #Xe165))

                                            ;; MISC
                                            ("www" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe100))
                                            ("~@"  . (?\s (Br . Bl) #Xe164))
                                            ("~~"  . (?\s (Br . Bl) #Xe168))
                                            ("?="  . (?\s (Br . Bl) #Xe127))
                                            (":="  . (?\s (Br . Bl) #Xe10c))
                                            ("/>"  . (?\s (Br . Bl) #Xe12e))
                                            ("+"   . #Xe16d)
                                            ("(:"  . (?\s (Br . Bl) #Xe16c))))))


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

(use-package ipretty
  :defer t
  :hook (after-init . ipretty-mode))

;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; This works when using emacs without server/client
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now


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
