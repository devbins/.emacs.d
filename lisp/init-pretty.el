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
;;     Update #: 13
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
                                    ;; ("String" . #x1d57e)  ;; 𝕊 𝕾
                                    ;; ("string" . #x1d598)  ;; 𝕤 𝖘
                                    ;; ("str"    . #x1d598)  ;; 𝕤 𝖘
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
                                    ("not"    . #x00ac)  ;; ¬
                                    ("not_"   . #x00ac)  ;; ¬

                                    ("All"    . #x2200)  ;; ∀
                                    ("all"    . #x2200)  ;; ∀
                                    ("all_"   . #x2200)  ;; ∀
                                    ("for"    . #x2200)  ;; ∀
                                    ("forall" . #x2200)  ;; ∀
                                    ("forM"   . #x2200)  ;; ∀

                                    ("pi"     . #x03c0)  ;; π

                                    ("sum"    . #x2211)  ;; ∑
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
                                  ("union"  . #x22c3)))))  ;; ⋃

         (haskell-mode . (lambda ()
                           (mapc (lambda (pair) (push pair prettify-symbols-alist))
                                 '(;; Syntax
                                   ("pure" . (#x21f0 (Br . Bl) #x2006))))))) ;; ⇰  x
  ;; (" . "  . (?\s (Br . Bl) #x2218 (Br . Bl) ?\s (Br . Bl) #x2006)) ;; ∘

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

(defun pretty-fonts-set-fontsets (CODE-FONT-ALIST)
  "Utility to associate many unicode points with specified `CODE-FONT-ALIST'."
  (--each CODE-FONT-ALIST
    (-let (((font . codes) it))
      (--each codes
        (set-fontset-font nil `(,it . ,it) font)
        (set-fontset-font t `(,it . ,it) font)))))

(defun pretty-fonts--add-kwds (FONT-LOCK-ALIST)
  "Exploits `font-lock-add-keywords'(`FONT-LOCK-ALIST') to apply regex-unicode replacements."
  (font-lock-add-keywords
   nil (--map (-let (((rgx uni-point) it))
                `(,rgx (0 (progn
                            (compose-region
                             (match-beginning 1) (match-end 1)
                             ,(concat "\t" (list uni-point)))
                            nil))))
              FONT-LOCK-ALIST)))

(defmacro pretty-fonts-set-kwds (FONT-LOCK-HOOKS-ALIST)
  "Set regex-unicode replacements to many modes(`FONT-LOCK-HOOKS-ALIST')."
  `(--each ,FONT-LOCK-HOOKS-ALIST
     (-let (((font-locks . mode-hooks) it))
       (--each mode-hooks
         (add-hook it (-partial 'pretty-fonts--add-kwds
                                (symbol-value font-locks)))))))

(defconst pretty-fonts-fira-font
  '(;; OPERATORS
    ;; Pipes
    ("\\(<|\\)" #Xe14d) ("\\(<>\\)" #Xe15b) ("\\(<|>\\)" #Xe14e) ("\\(|>\\)" #Xe135)

    ;; Brackets
    ("\\(<\\*\\)" #Xe14b) ("\\(<\\*>\\)" #Xe14c) ("\\(\\*>\\)" #Xe104)
    ("\\(<\\$\\)" #Xe14f) ("\\(<\\$>\\)" #Xe150) ("\\(\\$>\\)" #Xe137)
    ("\\(<\\+\\)" #Xe155) ("\\(<\\+>\\)" #Xe156) ("\\(\\+>\\)" #Xe13a)

    ;; Equality
    ("\\(!=\\)" #Xe10e) ("\\(!==\\)"         #Xe10f) ("\\(=/=\\)" #Xe143)
    ("\\(/=\\)" #Xe12c) ("\\(/==\\)"         #Xe12d)
    ("\\(===\\)" #Xe13d) ("[^!/]\\(==\\)[^>]" #Xe13c)

    ;; Equality Special
    ("\\(||=\\)"  #Xe133) ("[^|]\\(|=\\)" #Xe134)
    ("\\(~=\\)"   #Xe166)
    ("\\(\\^=\\)" #Xe136)
    ("\\(=:=\\)"  #Xe13b)

    ;; Comparisons
    ("\\(<=\\)" #Xe141) ("\\(>=\\)" #Xe145)
    ("\\(</\\)" #Xe162) ("\\(</>\\)" #Xe163)

    ;; Shifts
    ("[^-=]\\(>>\\)" #Xe147) ("\\(>>>\\)" #Xe14a)
    ("[^-=]\\(<<\\)" #Xe15c) ("\\(<<<\\)" #Xe15f)

    ;; Dots
    ("\\(\\.-\\)"    #Xe122) ("\\(\\.=\\)" #Xe123)
    ("\\(\\.\\.<\\)" #Xe125)

    ;; Hashes
    ("\\(#{\\)"  #Xe119) ("\\(#(\\)"   #Xe11e) ("\\(#_\\)"   #Xe120)
    ("\\(#_(\\)" #Xe121) ("\\(#\\?\\)" #Xe11f) ("\\(#\\[\\)" #Xe11a)

    ;; REPEATED CHARACTERS
    ;; 2-Repeats
    ("\\(||\\)" #Xe132)
    ("\\(!!\\)" #Xe10d)
    ("\\(%%\\)" #Xe16a)
    ("\\(&&\\)" #Xe131)

    ;; 2+3-Repeats
    ("\\(##\\)"       #Xe11b) ("\\(###\\)"          #Xe11c) ("\\(####\\)" #Xe11d)
    ("\\(--\\)"       #Xe111) ("\\(---\\)"          #Xe112)
    ("\\({-\\)"       #Xe108) ("\\(-}\\)"           #Xe110)
    ("\\(\\\\\\\\\\)" #Xe106) ("\\(\\\\\\\\\\\\\\)" #Xe107)
    ("\\(\\.\\.\\)"   #Xe124) ("\\(\\.\\.\\.\\)"    #Xe126)
    ("\\(\\+\\+\\)"   #Xe138) ("\\(\\+\\+\\+\\)"    #Xe139)
    ("\\(//\\)"       #Xe12f) ("\\(///\\)"          #Xe130)
    ("\\(::\\)"       #Xe10a) ("\\(:::\\)"          #Xe10b)

    ;; ARROWS
    ;; Direct
    ("[^-]\\(->\\)" #Xe114) ("[^=]\\(=>\\)" #Xe13f)
    ("\\(<-\\)"     #Xe152)
    ("\\(-->\\)"    #Xe113) ("\\(->>\\)"    #Xe115)
    ("\\(==>\\)"    #Xe13e) ("\\(=>>\\)"    #Xe140)
    ("\\(<--\\)"    #Xe153) ("\\(<<-\\)"    #Xe15d)
    ("\\(<==\\)"    #Xe158) ("\\(<<=\\)"    #Xe15e)
    ("\\(<->\\)"    #Xe154) ("\\(<=>\\)"    #Xe159)

    ;; Branches
    ("\\(-<\\)"  #Xe116) ("\\(-<<\\)" #Xe117)
    ("\\(>-\\)"  #Xe144) ("\\(>>-\\)" #Xe148)
    ("\\(=<<\\)" #Xe142) ("\\(>>=\\)" #Xe149)
    ("\\(>=>\\)" #Xe146) ("\\(<=<\\)" #Xe15a)

    ;; Squiggly
    ("\\(<~\\)" #Xe160) ("\\(<~~\\)" #Xe161)
    ("\\(~>\\)" #Xe167) ("\\(~~>\\)" #Xe169)
    ("\\(-~\\)" #Xe118) ("\\(~-\\)"  #Xe165)

    ;; MISC
    ("\\(www\\)"                   #Xe100)
    ("\\(<!--\\)"                  #Xe151)
    ("\\(~@\\)"                    #Xe164)
    ("[^<]\\(~~\\)"                #Xe168)
    ("\\(\\?=\\)"                  #Xe127)
    ("[^=]\\(:=\\)"                #Xe10c)
    ("\\(/>\\)"                    #Xe12e)
    ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
    ("[^:=]\\(:\\)[^:=]"           #Xe16c)
    ("\\(<=\\)"                    #Xe157))
  "Fira font ligatures and their regexes.")

(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)
  (pretty-fonts-set-kwds
   '((pretty-fonts-fira-font prog-mode-hook org-mode-hook))))


(use-package pretty-magit
  :defer t
  :commands(pretty-magit-setup)
  :load-path "~/.emacs.d/site-lisp/pretty-magit"
  :hook(magit-mode . pretty-magit-setup)
  :config
  (pretty-magit-add-leaders
   '(("Feature" ? (:foreground "slate gray" :height 1.2))
     ("Add"     ? (:foreground "#375E97" :height 1.2))
     ("Fix"     ? (:foreground "#FB6542" :height 1.2))
     ("Clean"   ? (:foreground "#FFBB00" :height 1.2))
     ("Docs"    ? (:foreground "#3F681C" :height 1.2)))))

(provide 'init-pretty)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-pretty.el ends here
