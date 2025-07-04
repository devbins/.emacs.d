;;; init-org.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-org.el
;; Description:
;; Author: devbins
;; Maintainer:
;; Copyright (C) 2019 devbins
;; Created: Wed Feb 12 19:32:07 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 982
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

(use-package org
  :ensure nil
  :preface
  (defun hot-expand (str &optional mod)
    "Expand org template.
STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org")
    :color blue :quit-key "ESC")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<C") "comment")
     ("C" (hot-expand "<c") "center")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("q" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "rust :crates '() :features '(())") "rust")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang")
     ("+" (hot-expand "<s" "cpp :includes <iostream>") "cpp"))
    "Misc"
    (("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :hook ((org-mode . (lambda ()
                       "Beautify org symbols."
                       (setq prettify-symbols-alist prettify-org-symbols-alist)
                       (prettify-symbols-mode 1)))
         (org-indent-mode . (lambda()
                              (diminish 'org-indent-mode)
                              ;; WORKAROUND: Prevent text moving around while using brackets
                              ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                              (make-variable-buffer-local 'show-paren-mode)
                              (setq show-paren-mode nil))))
  :init (setq org-directory "~/.org"
              org-todo-keywords '((sequence "TODO(t!)" "DOING(d!)" "|" "DONE(o!)" "ABORT(a@/!)")
                                  (sequence "❍(!)" "⥁(!)" "❓(!)" "⤽(!)" "|" "✔(!)" "✘(@/!)" "♱(@/!)"))
              org-priority-faces '((?A . error)
                                   (?B . warning)
                                   (?C . success))
              org-imenu-depth 5
              ;; define the refile targets
              org-refile-targets '((org-agenda-files :maxlevel . 3))
              org-refile-allow-creating-parent-nodes 'confirm
              org-refile-use-outline-path 'file
              org-outline-path-complete-in-steps nil
              org-auto-align-tags nil
              org-tags-column 0
              org-catch-invisible-edits 'show-and-error
              org-log-done 'notevery
              org-html-checkbox-type 'unicode

              prettify-symbols-unprettify-at-point t
              org-log-into-drawer t
              org-drawers (quote ("PROPERTIES" "LOGBOOK"))
              org-clock-into-drawer t
              org-clock-idle-time 30
              org-clock-out-when-done t
              org-clock-out-remove-zero-time-clocks t
              org-clock-in-switch-to-state "DOING"
              org-duration-format '((special . h:mm))
              org-columns-default-format "%50ITEM(Task) %8PRIORITY(Priority) %6TODO(Status) %6Effort(Effort){:} %8CLOCKSUM %16SCHEDULED %16DEADLINE"
              org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                            ("STYLE_ALL" . "habit")))



              ;; #+CAPTION: 设定图片宽度为100
              ;; #+ATTR_HTML: :width 100
              ;; file:data/2013/pict/test.png
              org-image-actual-width 300
              org-startup-with-inline-images t
              org-display-remote-inline-images 'download

              org-hide-leading-stars t
              org-fontify-whole-heading-line t
              org-fontify-done-headline t
              org-fontify-quote-and-verse-blocks t
              org-hide-emphasis-markers t
              org-pretty-entities t ;; 显示 _ 下标 ^ 上标，通过下面的配置，当需要显示的时候放到 {} 中。SPC m T e 来切换显示
              org-use-sub-superscripts '{}

              org-ditaa-jar-path (concat user-emacs-directory "ditaa.jar")
              ;; org-startup-indented t
              org-startup-folded    'show2levels
              org-adapt-indentation t
              org-startup-truncated nil
              org-ellipsis (if (char-displayable-p ?⤵) "  ⤵" "  ▼")
              org-tag-alist (quote ((:startgroup)
                                    ("@OFFICE" . ?o)
                                    ("@HOME" . ?h)
                                    (:endgroup)
                                    ("READING" . ?r)
                                    ("WORK" . ?W)
                                    ("BLOG" . ?b)
                                    ("TOC" . ?t)
                                    ("THOUGHT". ?T)
                                    ("crypt" . ?e)
                                    ("NOTE" . ?n)
                                    ("MUSIC" . ?m)
                                    ("MOVIE" . ?f))))
  :config
  (add-to-list 'org-modules 'org-protocol)
  (defface org-bold
    '((t :foreground "#d2268b"
         :background unspecified
         :weight bold
         :underline t
         :overline t))
    "Face for org-mode bold."
    :group 'org-faces)

  (defface org-doing
    '((t :foreground "red"
         :background "white"))
    "Face for org-mode bold."
    :group 'org-faces)

  (setq org-emphasis-alist
        '(("*" org-bold)
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim verbatim (:background "#d2268b"))
          ("~" org-code verbatim)
          ("+" (:strike-through t))))

  ;; 完成任务时, 将其划线勾掉
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (set-face-attribute 'org-level-1 nil :height 2.2 :bold t)
  (set-face-attribute 'org-level-2 nil :height 1.8 :bold t)
  (set-face-attribute 'org-level-3 nil :height 1.4 :bold t)
  (set-face-attribute 'org-level-4 nil :height 1.2 :bold t)
  (set-face-attribute 'org-level-5 nil :height 1.2 :bold t)
  (set-face-attribute 'org-level-6 nil :height 1.2 :bold t)
  (set-face-attribute 'org-level-7 nil :height 1.2 :bold t)

  (set-face-attribute 'org-block-begin-line nil :underline t)
  (set-face-attribute 'org-block-end-line nil :overline t :underline nil)

  ;; Use embedded webkit browser if possible
  (when (featurep 'xwidget-internal)
    (push '("\\.\\(x?html?\\|pdf\\)\\'"
            .
            (lambda (file _link)
              (xwidget-webkit-browse-url (concat "file://" file))
              (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
                (when (buffer-live-p buf)
                  (and (eq buf (current-buffer)) (quit-window))
                  (pop-to-buffer buf)))))
          org-file-apps))

  (use-package ox
    :ensure nil
    :init (setq org-export-with-broken-links 'mark
                org-export-with-sub-superscripts '{}
                org-export-use-babel nil ;; 导出的时候不执行代码，会导致设置的 header-arg 无效 do not evaluate again during export.
                org-export-with-toc t
                org-export-with-section-numbers t
                org-export-with-entities t ;; 导出时是否进行转义
                org-export-with-smart-quotes t
                org-export-coding-system 'utf-8
                org-export-with-footnotes t
                org-export-with-priority t
                org-export-with-drawers nil))

  ;; Add gfm/md backends
  (use-package ox-gfm
    :init
    (add-to-list 'org-export-backends 'md)
    (add-to-list 'org-export-backends 'gfm))
  (use-package ox-hugo
    :init
    (add-to-list 'org-export-backends 'hugo))

  (advice-add 'org-todo :after 'org-save-all-org-buffers)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)


  ;; Thank https://emacs-china.org/t/org-link-echo-area-link/19927/2
  (defun org-show-link-when-idle()
    ;; 在echo area中显示链接详情
    (require 'help-at-pt)
    (setq help-at-pt-display-when-idle t) ;; 不会立即生效
    (setq help-at-pt-timer-delay 0.5)
    (help-at-pt-set-timer)) ;; 调用才会生效
  (add-hook 'org-mode-hook #'org-show-link-when-idle)

  ;; Prettify UI
  (use-package org-superstar
    :defer t
    :hook (org-mode . org-superstar-mode)
    :init
    (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "☢" "❀" "✿" "☯" "☭" "♥" "✜" "♠" "★")
          org-superstar-item-bullet-alist
          '((?* . ?◈)
            (?+ . ?✚)
            (?- . ?▶))))

  (use-package org-pretty-tags
    :hook (org-mode . org-pretty-tags-mode)
    :config
    (setq org-pretty-tags-surrogate-strings
          (quote
           (("@HOME" . "🏠")
            ("@OFFICE" . "🏢")
            ("ARCHIVE". "📦")
            ("READING" . "📚")
            ("WORK" . "💼")
            ("BLOG" . "✍️")
            ("TOC" . "☰")
            ("THOUGHT". "💡")
            ("crypt" . "🔐")
            ("MUSIC" . "♬")
            ("NOTE" . "📝")
            ("MOVIE" . "🎬")
            ("verb" . "☸️")))))

  (use-package valign
    :hook ((org-mode markdown-mode) . valign-mode)
    :init (setq valign-fancy-bar t))

  ;; Show hidden emphasis markers
  (use-package org-appear
    :commands (org-appear-manual-start org-appear-manual-stop)
    :hook ((org-mode . (lambda ()
                        (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
                        (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t)))
           (org-mode . org-appear-mode))
    :config
    (setq org-appear-trigger 'manual
          org-appear-autolinks t
          org-appear-autosubmarkers t
          org-appear-autoentities t
          org-appear-autokeywords t
          org-appear-inside-latex t))

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
           ("C-M-y" . org-rich-yank)))

  ;; Table of contents，添加 toc 标签即可
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Export text/html MIME emails
  (use-package org-mime
    :bind (:map message-mode-map
           ("C-c M-o" . org-mime-htmlize)
           :map org-mode-map
           ("C-c M-o" . org-mime-org-buffer-htmlize)))


  (use-package htmlize
    :init (setq htmlize-pre-style t
                htmlize-output-type 'inline-css))

  (use-package ox-html
    :ensure nil
    :init
    (defun org-audio-link-export (path desc backend)
      (let ((ext (file-name-extension path)))
        (cond
         ((memq backend '(html gfm md hugo))
          (format "<audio preload='metadata' controls='controls'><source type='audio/%s' src='%s' /></audio>" ext path))
         ;; fall-through case for everything else
         (t
          path))))
    (defun org-video-link-export (path desc backend)
      (let ((ext (file-name-extension path)))
        (cond
         ((memq backend '(html gfm md hugo))
          (format "<video preload='metadata' controls='controls'><source type='video/%s' src='%s' /></video>" ext path))
         ;; fall-through case for everything else
         (t
          path))))

    (org-link-set-parameters "audio" :export 'org-audio-link-export) ;;    [[audio:test.mp3]]
    (org-link-set-parameters "video" :export 'org-video-link-export) ;;    [[video:test.mp4]]
    :config
    ;; Org export code style
    (setq org-html-htmlize-output-type 'inline-css
          org-html-validation-link nil
          org-html-checkbox-type 'unicode)
    (setq-default org-html-doctype "html5")
    (setq-default org-html-html5-fancy t))

  (use-package tex
    :ensure auctex
    :custom
    (TeX-parse-self t) ; 自动解析 tex 文件
    (TeX-PDF-mode t)
    (TeX-DVI-via-PDFTeX t)
    (TeX-engine 'xetex) ;; 支持中文
    :hook (latex-mode . (lambda () (turn-on-cdlatex)
                          (turn-on-reftex))))

  (use-package cdlatex
    :hook ((org-mode . (lambda () (turn-on-org-cdlatex)
                        ;; (org-latex-preview-auto-mode)
                        ))
           (LaTeX-mode . turn-on-cdlatex)))

  ;; Default LaTeX preview image directory
  (setq org-preview-latex-image-directory "imgs/"
        org-preview-latex-default-process 'dvisvgm ;; tlmgr install dvisvgm
        org-highlight-latex-and-related '(native)) ;; Highlight inline LaTeX code

  ;; 避免图片太小
  (plist-put org-format-latex-options :scale 2.5)
  ;; Setup for `org-latex-preview'
  (setq org-latex-packages-alist '(("T1" "fontenc" t)
                                   ("" "amsmath"   t)
                                   ("" "amssymb"   t)
                                   ("" "siunitx"   t)

                                   ;; Font packages
                                   ("libertinus" "newtx" t)

                                   ;; Load this after all math to give access to bold math
                                   ;; See https://ctan.org/pkg/newtx
                                   ("" "bm" t)

                                   ;; Package physics2 requires to be loaded after font
                                   ;; packages. See https://ctan.org/pkg/physics2
                                   ("" "physics2" t)

                                   ;; Differentiations
                                   ("normal" "fixdif" t)))

    (use-package ox-latex
    :ensure nil
    :defer t
    :config
    (add-to-list 'org-latex-classes
                 '("cn-article"
                   "\\documentclass[UTF8,a4paper]{article}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                 '("cn-report"
                   "\\documentclass[11pt,a4paper]{report}"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
    (setq org-latex-default-class "cn-article")
    (setq org-latex-image-default-height "0.9\\textheight"
          org-latex-image-default-width "\\linewidth")
    (setq org-latex-pdf-process
	      '("xelatex -interaction nonstopmode -output-directory %o %f"
	        "bibtex %b"
	        "xelatex -interaction nonstopmode -output-directory %o %f"
	        "xelatex -interaction nonstopmode -output-directory %o %f"
	        "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl auto"
	        ))
    ;; 使用 Listings 宏包格式化源代码(只是把代码框用 listing 环境框起来，还需要额外的设置)
    (setq org-latex-listings t)
    ;; mapping jupyter-python to Python
    (add-to-list 'org-latex-listings-langs '(jupyter-python "Python"))
    ;; Options for \lset command（reference to listing Manual)
    (setq org-latex-listings-options
          '(
            ("basicstyle" "\\small\\ttfamily")       ; 源代码字体样式
            ("keywordstyle" "\\color{eminence}\\small")                 ; 关键词字体样式
            ;; ("identifierstyle" "\\color{doc}\\small")
            ("commentstyle" "\\color{commentgreen}\\small\\itshape")    ; 批注样式
            ("stringstyle" "\\color{red}\\small")                       ; 字符串样式
            ("showstringspaces" "false")                                ; 字符串空格显示
            ("numbers" "left")                                          ; 行号显示
            ("numberstyle" "\\color{preprocess}")                       ; 行号样式
            ("stepnumber" "1")                                          ; 行号递增
            ("xleftmargin" "2em")                                       ;
            ;; ("backgroundcolor" "\\color{background}")                   ; 代码框背景色
            ("tabsize" "4")                                             ; TAB 等效空格数
            ("captionpos" "t")                                          ; 标题位置 top or buttom(t|b)
            ("breaklines" "true")                                       ; 自动断行
            ("breakatwhitespace" "true")                                ; 只在空格分行
            ("showspaces" "false")                                      ; 显示空格
            ("columns" "flexible")                                      ; 列样式
            ("frame" "tb")                                              ; 代码框：single, or tb 上下线
            ("frameleftmargin" "1.5em")                                 ; frame 向右偏移
            ;; ("frameround" "tttt")                                       ; 代码框： 圆角
            ;; ("framesep" "0pt")
            ;; ("framerule" "1pt")                                         ; 框的线宽
            ;; ("rulecolor" "\\color{background}")                         ; 框颜色
            ;; ("fillcolor" "\\color{white}")
            ;; ("rulesepcolor" "\\color{comdil}")
            ("framexleftmargin" "5mm")                                  ; let line numer inside frame
            )))

  (use-package org-ref
    :after org)

  (when emacs/>=27p
    ;; Auto-toggle Org LaTeX fragments
    (use-package org-fragtog
      :diminish
      :hook (org-mode . org-fragtog-mode)))

  ;; Preview
  (use-package org-preview-html
    :diminish)

  ;; brew install pdf2svg
  (use-package org-inline-pdf
    :hook (org-mode . org-inline-pdf-mode))
  (use-package org-inline-anim
    :hook (org-mode . org-inline-anim-mode))
  ;; 在网址后面加上 ?print-pdf 可以生成pdf格式
  (use-package ox-reveal
    :hook (org-mode . (lambda() (require 'ox-reveal)))
    :config
    (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
          org-reveal-theme "black"
          org-reveal-width 1200
          org-reveal-height 1000
          org-reveal-margin "0.1"
          org-reveal-min-scale "0.5"
          org-reveal-max-scale "2.5"
          org-reveal-transition "cube"
          org-reveal-plugins '(classList markdown zoom notes)
          org-reveal-control t
          org-reveal-center t
          org-reveal-progress t
          org-reveal-mathjax t
          org-reveal-klipsify-src t
          org-reveal-history nil))

  (use-package ox-pandoc
    :after org)

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
           ("C-<f7>" . org-tree-slide-mode)
           :map org-tree-slide-mode-map
           ("<left>" . org-tree-slide-move-previous-tree)
           ("<right>" . org-tree-slide-move-next-tree)
           ("S-SPC" . org-tree-slide-move-previous-tree)
           ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 5))


  (evil-leader/set-key-for-mode 'org-mode
    "mou"                       'org-update-all-dblocks
    "moT"                       'org-set-tags
    "mov"                       'org-columns
    "moq"                       'org-columns-quit

    "mc"                        'org-capture

    ;; Clock
    ;; These keybindings should match those under the "aoC" prefix (below)
    "mCc"                     'org-clock-cancel
    "mCd"                     'org-clock-display
    "mCe"                     'org-evaluate-time-range
    "mCg"                     'org-clock-goto
    "mCi"                     'org-clock-in
    "mCI"                     'org-clock-in-last
    "mCo"                     'org-clock-out
    "mCp"                     'org-pomodoro
    "mCR"                     'org-clock-report
    "mCr"                     'org-resolve-clocks

    "mdd"                     'org-deadline
    "mds"                     'org-schedule
    "mdt"                     'org-time-stamp
    "mdT"                     'org-time-stamp-inactive
    "mee"                     'org-export-dispatch
    "mfi"                     'org-feed-goto-inbox
    "mfu"                     'org-feed-update-all

    "ma"                      'org-agenda

    "mp"                      'org-priority

    "mTc"                     'org-toggle-checkbox
    "mTe"                     'org-toggle-pretty-entities
    "mTi"                     'org-toggle-inline-images
    "mTl"                     'org-toggle-link-display
    "mTt"                     'org-show-todo-tree
    "mTT"                     'org-todo
    "mTx"                     'org-toggle-latex-fragment

    ;; More cycling options (timestamps, headlines, items, properties)
    "mL"                      'org-shiftright
    "mH"                      'org-shiftleft
    "mJ"                      'org-shiftdown
    "mK"                      'org-shiftup

    ;; Change between TODO sets
    "mC-S-l"                  'org-shiftcontrolright
    "mC-S-h"                  'org-shiftcontrolleft
    "mC-S-j"                  'org-shiftcontroldown
    "mC-S-k"                  'org-shiftcontrolup

    ;; Subtree editing
    "msa"                     'org-toggle-archive-tag
    "msA"                     'org-archive-subtree
    "msb"                     'org-tree-to-indirect-buffer
    "msd"                     'org-cut-subtree
    "msh"                     'org-promote-subtree
    "msj"                     'org-move-subtree-down
    "msk"                     'org-move-subtree-up
    "msl"                     'org-demote-subtree
    "msn"                     'org-narrow-to-subtree
    "msN"                     'widen
    "msr"                     'org-refile
    "mss"                     'org-sparse-tree
    "msS"                     'org-sort

    ;; tables
    "mta"                     'org-table-align
    "mtb"                     'org-table-blank-field
    "mtc"                     'org-table-convert
    "mtdc"                    'org-table-delete-column
    "mtdr"                    'org-table-kill-row
    "mte"                     'org-table-eval-formula
    "mtE"                     'org-table-export
    "mth"                     'org-table-previous-field
    "mtH"                     'org-table-move-column-left
    "mtic"                    'org-table-insert-column
    "mtih"                    'org-table-insert-hline
    "mtiH"                    'org-table-hline-and-move
    "mtir"                    'org-table-insert-row
    "mtI"                     'org-table-import
    "mtj"                     'org-table-next-row
    "mtJ"                     'org-table-move-row-down
    "mtK"                     'org-table-move-row-up
    "mtl"                     'org-table-next-field
    "mtL"                     'org-table-move-column-right
    "mtn"                     'org-table-create
    "mtN"                     'org-table-create-with-table.el
    "mtr"                     'org-table-recalculate
    "mts"                     'org-table-sort-lines
    "mttf"                    'org-table-toggle-formula-debugger
    "mtto"                    'org-table-toggle-coordinate-overlays
    "mtw"                     'org-table-wrap-region

    ;; Source blocks / org-babel
    "mbp"                     'org-babel-previous-src-block
    "mbn"                     'org-babel-next-src-block
    "mbe"                     'org-babel-execute-maybe
    "mbo"                     'org-babel-open-src-block-result
    "mbv"                     'org-babel-expand-src-block
    "mbu"                     'org-babel-goto-src-block-head
    "mbg"                     'org-babel-goto-named-src-block
    "mbr"                     'org-babel-goto-named-result
    "mbb"                     'org-babel-execute-buffer
    "mbs"                     'org-babel-execute-subtree
    "mbd"                     'org-babel-demarcate-block
    "mbt"                     'org-babel-tangle
    "mbf"                     'org-babel-tangle-file
    "mbc"                     'org-babel-check-src-block
    "mbj"                     'org-babel-insert-header-arg
    "mbl"                     'org-babel-load-in-session
    "mbi"                     'org-babel-lob-ingest
    "mbI"                     'org-babel-view-src-block-info
    "mbz"                     'org-babel-switch-to-session
    "mbZ"                     'org-babel-switch-to-session-with-code
    "mba"                     'org-babel-sha1-hash
    "mbx"                     'org-babel-do-key-sequence-in-edit-buffer
    ;; Multi-purpose keys
    "m*"                      'org-ctrl-c-star
    "m-"                      'org-ctrl-c-minus
    "m#"                      'org-update-statistics-cookies
    ;; attachments
    "mA"                      'org-attach
    ;; insertion
    "mib"                     'org-insert-structure-template
    "mid"                     'org-insert-drawer
    "mie"                     'org-set-effort
    "mif"                     'org-footnote-new
    "mih"                     'org-insert-heading
    "miH"                     'org-insert-heading-after-current
    "mii"                     'org-id-get-create
    "mil"                     'org-insert-link
    "min"                     'org-add-note
    "mip"                     'org-set-property
    "mis"                     'org-insert-subheading
    "mit"                     'org-set-tags-command)

  (which-key-add-major-mode-key-based-replacements 'org-mode
    "SPC m C" "clock"
    "SPC m t" "table"
    "SPC m T" "toggle"
    "SPC m b" "babel"
    "SPC m d" "time"
    "SPC m i" "insert"
    "SPC m s" "subtree")
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

  ;; Pomodoro
  (use-package org-pomodoro
    :preface
    (defun pomodoro-notify (title msg)
      (notify title msg))
    :bind (:map org-agenda-mode-map
           ("P" . org-pomodoro))
    :hook ((org-pomodoro-started . (lambda () (when (and sys/macp (file-exists-p "/Applications/JustFocus.app")) (do-applescript "tell application \"JustFocus\"\nlaunch\n start pomodoro\nend tell"))))
           (org-pomodoro-finished . (lambda () (pomodoro-notify "Pomodoro Completed!" "Time for a break.")))
           (org-pomodoro-break-finished . (lambda () (pomodoro-notify "Pomodoro Short Break Finished" "Ready for Another?")))
           (org-pomodoro-long-break-finished . (lambda () (pomodoro-notify "Pomodoro Long Break Finished" "Ready for Another?")))
           (org-pomodoro-killed . (lambda () (progn (pomodoro-notify "Pomodoro Killed" "One does not simply kill a pomodoro!")
                                               (when (and sys/macp (file-exists-p "/Applications/JustFocus.app")) (do-applescript "tell application \"JustFocus\"\n stop\nend tell")))))))

  ;;change download dir -*- mode: Org; org-download-image-dir: "images"; -*-
  (use-package org-download
    :commands (org-download-enable
               org-download-yank
               org-download-screenshot)
    :hook (org-mode . org-download-enable)
    :config
    (defun my-org-download-method (link)
      (let ((filename
             (file-name-nondirectory
              (car (url-path-and-query
                    (url-generic-parse-url link)))))
            (dirname (concat "imgs/" (file-name-sans-extension (buffer-name)))))
        (unless (file-exists-p dirname)
          (make-directory dirname))
        (expand-file-name filename dirname)))
    (setq org-download-method 'my-org-download-method)
    (setq org-download-display-inline-images 'posframe
          org-download-image-attr-list '("#+ATTR_HTML: :width 80% :align center"))
    (when (eq system-type 'windows-nt)
      (setq org-download-screenshot-method "convert clipboard: %s")))

  ;; 加密文章
  ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
  (use-package org-crypt
    :ensure nil
    :demand
    :init
    ;; 设置要加密的 tag 标记为 secret
    (setq org-crypt-tag-matcher "crypt"
          ;; 避免 secret 这个 tag 被子項目继承造成重复加密
          org-tags-exclude-from-inheritance (quote ("crypt"))
          ;; 设置用于加密的 GPG ID 设置为 nil 使用对称加密 (symmetric encryption)
          org-crypt-key user-mail-address)
    :config
    (setenv "GPG_AGENT_INFO" nil)
    ;; 当被加密的部份被保存时，自動加密回去
    (org-crypt-use-before-save-magic))

  ;; pinentry-start 要使用的时候 Mac 下需要
  ;; 把allow-emacs-pinentry 加入 .gnupg/gpg-agent.conf
  ;; http://elpa.gnu.org/packages/pinentry.html
  ;; This will force Emacs to use its own internal password prompt instead of an external pin entry program.
  (use-package pinentry
    :if sys/macp
    :hook (org-mode . pinentry-start))

  (use-package orgit
    :disabled)

  (use-package evil-org
    :defer t
    :after org
    :diminish
    :init
    (setq evil-org-use-additional-insert t
          evil-org-key-theme `(textobjects
                               navigation
                               additional
                               ,@(when t '(todo))))
    :hook (org-mode . (lambda ()
                        (evil-org-mode)
                        (evil-normalize-keymaps))))

  (use-package auto-space-mode
    :ensure nil
    :hook (org-mode . auto-space-mode))

  (use-package org-capture
    :ensure nil
    :config
    (setq org-capture-templates '(("i" "inbox" entry (file+headline org-agenda-file-inbox "inbox")
                                   "* %?\n  %i\n %U"
                                   :empty-lines 1)
                                  ("t" "Todo" entry (file+headline org-agenda-file-inbox "Workspace")
                                   "* TODO [#B] %?\n  %i\n"
                                   :empty-lines 1)
                                  ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
                                   "* %?\n  %i\n %U"
                                   :empty-lines 1)
                                  ("a" "Anki basic"
                                   entry
                                   (file+headline org-agenda-file-note "ANKI")
                                   "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n")
                                  ("N" "notes" entry (file+headlie org-agenda-file-note "Browser notes")
                                   "* %U - %:annotation %^g\n\n  %?"
                                   :empty-lines 1 :kill-buffer t)
                                  ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
                                   "* TODO [#B] %?\n  %i\n %U"
                                   :empty-lines 1)
                                  ("B" "Protocol Bookmarks" entry (file+headline org-agenda-file-inbox "Bookmarks")
                                   "* %U - %:annotation"
                                   :kill-buffer t :empty-lines 1)
                                  ("s" "Code Snippet" entry
                                   (file org-agenda-file-code-snippet)
                                   "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
                                  ("w" "work" entry (file+headline org-agenda-file-gtd "work")
                                   "* TODO [#A] %?\n  %i\n %U"
                                   :empty-lines 1[[zsh:1: command not found: osascript]])
                                  ;; org-mac-chrome-get-frontmost-url org-mac-chrome-insert-frontmost-url
                                  ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
                                   "* TODO [#C] %?\n %(grab-mac-link)\n %i\n %U"
                                   :empty-lines 1)
                                  ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
                                   "* TODO [#C] %?\n  %i\n %a \n %U"
                                   :empty-lines 1)
                                  ("p" "punch" entry (file+olp+datetree org-agenda-file-punch)
                                   "* %^{想法}%? %U")
                                  ("j" "Journal Entry" entry (file+olp+datetree org-agenda-file-journal)
                                   "* %?"
                                   :empty-lines 1))))

  (use-package org-journal
    :commands (org-journal-new-entry org-journal-search-forever)
    :config
    (setq org-journal-dir "~/.org/journal/"
          org-journal-file-format "%Y-%m.org"
          org-journal-file-type 'monthly
          org-journal-date-format "%Y %B %d, %A"
          org-journal-start-on-weekday 'Sunday
          org-journal-enable-agenda-integration t)
    (evil-leader/set-key-for-mode 'org-journal-mode
      "mj" 'org-journal-new-entry
      "mn" 'org-journal-next-entry
      "mp" 'org-journal-previous-entry)
    (evil-leader/set-key-for-mode 'calendar-mode
      "mr" 'org-journal-read-entry
      "mi" 'org-journal-new-date-entry
      "mn" 'org-journal-next-entry
      "mp" 'org-journal-previous-entry
      "ms" 'org-journal-search-forever
      "mw" 'org-journal-search-calendar-week
      "mm" 'org-journal-search-calendar-month
      "my" 'org-journal-search-calendar-year)))

(use-package org-src
  :ensure nil
  :after org
  :config
  ;; =================================================
  ;; 自动给结果的图片加上相关属性
  ;; =================================================
  (setq original-image-width-before-del "400") ; 设置图片的默认宽度为400
  (setq original-caption-before-del "")        ; 设置默认的图示文本为空

  (defun insert-attr-decls ()
    "insert string before babel execution results"
    (insert (concat "\n#+CAPTION:"
                    original-caption-before-del
                    "\n#+ATTR_ORG: :width "
                    original-image-width-before-del
                    "\n#+ATTR_LATEX: :width "
                    (if (>= (/ (string-to-number original-image-width-before-del) 800.0) 1)
                        "1.0"
                      (number-to-string (/ (string-to-number original-image-width-before-del) 800.0)))
                    "\\linewidth :float nil"
                    "\n#+ATTR_HTML: :width "
                    original-image-width-before-del
                    )))

  (defun insert-attr-decls-at (s)
    "insert string right after specific string"
    (let ((case-fold-search t))
      (if (search-forward s nil t)
          (progn
            ;; (search-backward s nil t)
            (insert-attr-decls)))))

  (defun insert-attr-decls-at-results (orig-fun &optional arg info param)
    "insert extra image attributes after babel execution"
    (interactive)
    (progn
      (when (member (car (org-babel-get-src-block-info)) '("mermaid" "ditaa" "dot" "lilypond" "plantuml" "gnuplot" "d2"))
        (setq original-image-width-before-del (number-to-string (if-let* ((babel-width (alist-get :width (nth 2 (org-babel-get-src-block-info))))) babel-width (string-to-number original-image-width-before-del))))
        (save-excursion
          ;; `#+begin_results' for :wrap results, `#+RESULTS:' for non :wrap results
          (insert-attr-decls-at "#+begin_results")))
      (org-redisplay-inline-images)))
  (advice-add 'org-babel-execute-src-block :after #'insert-attr-decls-at-results)

  ;; 再次执行时需要将旧的图片相关参数行删除，并从中头参数中获得宽度参数，参考
  ;; https://emacs.stackexchange.com/questions/57710/how-to-set-image-size-in-result-of-src-block-in-org-mode
  (defun get-attributes-from-src-block-result (&rest args)
    "get information via last babel execution"
    (let ((location (org-babel-where-is-src-block-result))
          ;; 主要获取的是图示文字和宽度信息，下面这个正则就是为了捕获这两个信息
          (attr-regexp "[:blank:]*#\\+\\(ATTR_ORG: :width \\([0-9]\\{3\\}\\)\\|CAPTION:\\(.*\\)\\)"))
      (setq original-caption-before-del "") ; 重置为空
      (when location
        (save-excursion
          (goto-char location)
          (when (looking-at (concat org-babel-result-regexp ".*$"))
            (next-line 2)               ; 因为有个begin_result的抽屉，所以往下2行
            ;; 通过正则表达式来捕获需要的信息
            (while (looking-at attr-regexp)
              (when (match-string 2)
                (setq original-image-width-before-del (match-string 2)))
              (when (match-string 3)
                (setq original-caption-before-del (match-string 3)))
              (next-line)               ; 因为设置了:wrap，所以这里不需要删除这一行
              )
            )))))
  (advice-add 'org-babel-execute-src-block :before #'get-attributes-from-src-block-result)

  ;; limit the babel result length
  (defvar org-babel-result-lines-limit 40)
  (defvar org-babel-result-length-limit 6000)

  (defun org-babel-insert-result@limit (orig-fn result &rest args)
    (if (not (member (car (org-babel-get-src-block-info)) '("jupyter-python"))) ; not for jupyter-python etc.
        (if (and result (or org-babel-result-lines-limit org-babel-result-length-limit))
            (let (new-result plines plenght limit)
              (with-temp-buffer
                (insert result)
                (setq plines (if org-babel-result-lines-limit
                                 (goto-line org-babel-result-lines-limit)
                               (point-max)))
                (setq plenght (if org-babel-result-length-limit
                                  (min org-babel-result-length-limit (point-max))
                                (point-max)))
                (setq limit (min plines plenght))
                (setq new-result (concat (buffer-substring (point-min) limit)
                                         (if (< limit (point-max)) "..."))))
              (apply orig-fn new-result args))
          (apply orig-fn result args))
      (apply orig-fn result args)))

  (advice-add 'org-babel-insert-result :around #'org-babel-insert-result@limit)

  ;; 代码块语法高亮
  (setq org-src-fontify-natively t
        ;; 使用编程语言的TAB绑定设置
        org-src-tab-acts-natively t
        ;; 代码块编辑窗口的打开方式：当前窗口+代码块编辑窗口
        org-src-window-setup 'reorganize-frame
        ;; 执行前是否需要确认
        org-confirm-babel-evaluate nil
        ;; 代码块默认前置多少空格
        org-edit-src-content-indentation 0
        org-babel-C-compiler "gcc -std=c17"
        org-babel-C++-compiler "g++ -std=c++17")

  (defvar load-language-list '((emacs-lisp . t)
                               (perl       . t)
                               (python     . t)
                               (sql        . t)
                               (sqlite     . t)
                               (ruby       . t)
                               (js         . t)
                               (css        . t)
                               (sass       . t)
                               (C          . t) ;; #+begin_src cpp :includes <iostream> :flags "-std=c++11"
                               (calc       . t)
                               (java       . t)
                               (dot        . t)
                               (ditaa      . t)
                               (calc       . t)
                               (eshell     . t)
                               (shell      . t)
                               (plantuml   . t)
                               (jupyter    . t)
                               (latex      . t)))

  ;; 代码块的语言模式设置，设置之后才能正确语法高亮
  (setq org-src-lang-modes '(("bash"   . sh)
                             ("C"      . c)
                             ("C++"    . c++)
                             ("cpp"    . c++)
                             ("elisp"  . emacs-lisp)
                             ("go"     . go)
                             ("rust"   . rustic)
                             ("shell"  . sh)
                             ("ts"     . typescript)
                             ("mysql"  . sql)))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  ;; REST
  ;; Org babel extensions
  ;; HTTP client
  ;; not in org babel do not use indent
  (use-package verb
    :init (cl-pushnew '(verb . t) load-language-list)
    :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

  ;; walkman-setup
  (use-package walkman)

  ;; usage: BEGIN_SRC hurl
  (use-package hurl-mode
    :load-path "site-lisp/hurl-mode"
    :mode "\\.hurl\\'")

  ;; Async src_block execution
  ;; usage: begin_src sh :async
  (use-package ob-async
    :config
    (setq ob-async-no-async-languages-alist '("ipython"
                                              "jupyter-python"
                                              "jupyter-julia"
                                              "jupyter-R"
                                              "jupyter-javascript"))

    (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                         (:session . "py"))))

  (use-package ob-kotlin
    :init (cl-pushnew '(kotlin . t) load-language-list))

  (use-package gnuplot
    :init (cl-pushnew '(gnuplot . t) load-language-list))

  ;;brew install mermaid-cli
  (use-package mermaid-mode
    :if (executable-find "mmdc"))
  (use-package ob-mermaid
    :if (executable-find "mmdc")
    :init (cl-pushnew '(mermaid . t) load-language-list))

  (use-package ob-dart
    :if (executable-find "dart")
    :init (cl-pushnew '(dart . t) load-language-list))

  (use-package ob-typescript
    :init (cl-pushnew '(typescript . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list))


(use-package svg-tag-mode
  :hook ((org-mode org-agenda-mode) . svg-tag-mode)
  :init
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
  (setq svg-tag-action-at-point 'edit)

  (defun svg-progress-percent (value)
    (save-match-data
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar  (/ (string-to-number value) 100.0)
                                         nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag (concat value "%")
                               nil :stroke 0 :margin 0)) :ascent 'center)))

  (defun svg-progress-count (value)
    (save-match-data
      (let* ((seq (split-string value "/"))
             (count (if (stringp (car seq))
                        (float (string-to-number (car seq)))
                      0))
             (total (if (stringp (cadr seq))
                        (float (string-to-number (cadr seq)))
                      1000)))
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar (/ count total) nil
                                          :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                    (svg-lib-tag value nil
                                 :stroke 0 :margin 0)) :ascent 'center))))

  (setq svg-tag-tags
        `(
          ;; Org tags
          ;; (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          ;; (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

          ;; Task priority
          ("\\[#[D-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :margin 0))))

          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; TODO / DONE
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0 :radius 15))))
          ("DOING" . ((lambda (tag) (svg-tag-make "DOING" :face 'org-doing :inverse t :margin 0 :radius 15))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0 :radius 15))))
          ("ABORT" . ((lambda (tag) (svg-tag-make "ABORT" :face 'org-done :margin 0 :radius 15))))


          ;; Citation of the form [cite:@Knuth:1984]
          ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                            (svg-tag-make tag
                                                          :inverse ;TODO:
                                                          :beg 7 :end -1
                                                          :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                     (svg-tag-make tag
                                                                   :end -1
                                                                   :crop-left t))))


          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))

  (defun org-agenda-show-svg ()
    (let* ((case-fold-search nil)
           (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
           (keyword (car keywords)))
      (while keyword
        (save-excursion
          (while (re-search-forward (nth 0 keyword) nil t)
            (overlay-put (make-overlay
                          (match-beginning 0) (match-end 0))
                         'display  (nth 3 (eval (nth 2 keyword)))) ))
        (pop keywords)
        (setq keyword (car keywords)))))
  (add-hook 'org-agenda-finalize-hook #'org-agenda-show-svg))

;; use excalidraw in org mode
;; npm install --global excalidraw_export canvas
;; use iimage-mode can display svg image in org mode
(use-package org-excalidraw
  :commands (org-excalidraw-initialize)
  :load-path "site-lisp/org-excalidraw"
  :hook (org-mode . (lambda ()
                      (org-excalidraw-initialize)))
  :config
  (setq org-excalidraw-directory "~/.org/excalidraw"))

(use-package easy-hugo
  :init
  (setq easy-hugo-basedir "~/git/blog/"
        easy-hugo-url "https://devbins.github.io/"
        easy-hugo-preview-url "http://127.0.0.1:1313/"
        easy-hugo-postdir "content/post"
        easy-hugo-default-ext ".org"
        easy-hugo-server-flags "-D"
        easy-hugo-org-header t)
  :config
  (easy-hugo-enable-menu))

(use-package deft
  :defer t
  :init (setq deft-directory "~/Nextcloud/"
              deft-extensions '("md" "org" "txt")
              deft-recursive t
              deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
              deft-use-filename-as-title t))

(defun org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-directory (buffer-file-name))
                  "imgs/" (file-name-sans-extension (buffer-name)) "/" (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename) t))
  ;; take screenshot
  (if (eq system-type 'darwin)
      (progn
        (call-process-shell-command "screencapture" nil nil nil nil " -s " (concat "\"" filename "\"" ))
        (call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" ))))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ;; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:./imgs/" (file-name-sans-extension (file-relative-name (buffer-file-name))) "/" (file-name-nondirectory filename) "]]")))
  (org-display-inline-images))

;; https://github.com/jjasghar/alfred-org-capture
(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "Source Code Pro")))
  (select-frame-by-name "remember")
  (org-capture))


(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

;; https://www.zmonster.me/2020/06/27/org-roam-introduction.html
(use-package org-roam
  :commands (org-roam-alias-add)
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory (expand-file-name "~/.org"))
  (org-roam-mute-cache-build t)
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-graph-viewer (if (featurep 'xwidget-internal)
                             #'xwidget-webkit-browse-url
                           #'browse-url))
  :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ("C-c n j" . org-roam-dailies-capture-today)
           ("C-c n I" . org-roam-insert-immediate))
  :init
  (setq org-roam-node-display-template (concat "${hierarchy:*} ${backlinkscount:3} ${directories:20}" (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
  (add-to-list 'org-modules 'org-roam-protocol)

  ;; https://github.com/org-roam/org-roam/wiki/User-contributed-Tricks#showing-node-hierarchy
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    (let ((level (org-roam-node-level node)))
      (concat
       (when (> level 0) (concat (org-roam-node-file-title node) " > "))
       (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
       (org-roam-node-title node))))

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (directory-file-name dirs))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (defun org-roam-open-refs ()
    "Open REFs of the node at point."
    (interactive)
    (save-excursion
      (goto-char (org-roam-node-point (org-roam-node-at-point 'assert)))
      (when-let* ((p (org-entry-get (point) "ROAM_REFS"))
                  (refs (when p (split-string-and-unquote p)))
                  (refs (if (length> refs 1)
                            (completing-read-multiple "Open: " refs)
                          refs))
                  (user-error "No ROAM_REFS found"))

        (when-let* ((oc-cites (seq-map
                              (lambda (ref) (substring ref 1))
                              (seq-filter (apply-partially #'string-prefix-p "@") refs))))
          (citar-run-default-action oc-cites))

        (dolist (ref refs)
          (unless (string-prefix-p "@" ref)
            (browse-url ref))))))

  (with-eval-after-load 'embark
    (defun org-roam-backlinks-query (node)
      "Gets the backlinks of NODE with `org-roam-db-query'."
      (org-roam-db-query
       [:select [source dest]
	            :from links
	            :where (= dest $s1)
	            :and (= type "id")]
       (org-roam-node-id node)))

    (defun org-roam-backlinks-p (source node)
      "Predicate function that checks if NODE is a backlink of SOURCE."
      (let* ((source-id (org-roam-node-id source))
	         (backlinks (org-roam-backlinks-query source))
	         (id (org-roam-node-id node))
	         (id-list (list id source-id)))
        (member id-list backlinks)))

    (defun org-roam-backlinks--read-node-backlinks (source)
      "Runs `org-roam-node-read' on the backlinks of SOURCE.
 The predicate used as `org-roam-node-read''s filter-fn is
 `org-roam-backlinks-p'."
      (org-roam-node-read nil (apply-partially #'org-roam-backlinks-p source)))

    (defun org-roam-backlinks-node-read (entry)
      "Read a NODE and run `org-roam-backlinks--read-node-backlinks'."
      (let* ((node (get-text-property 0 'node entry))
             (backlink (org-roam-backlinks--read-node-backlinks node)))
        (find-file (org-roam-node-file backlink))))

    (defvar-keymap embark-org-roam-map
      :doc "Keymap for Embark org roam node actions."
      :parent embark-general-map
      "i" #'org-roam-node-insert
      "b" #'org-roam-backlinks-node-read
      "r" #'org-roam-node-random)
    (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-map)))

  (with-eval-after-load 'shackle
    (add-to-list 'shackle-rules '("*org-roam*" :align right)))

  (with-eval-after-load 'popper
    (add-to-list 'popper-reference-buffers '(org-roam-mode)))

  (evil-leader/set-key-for-mode 'org-roam-mode
    "mrl" 'org-roam
    "mrt" 'org-roam-dailies-today
    "mrb" 'org-roam-switch-to-buffer
    "mrf" 'org-roam-find-file
    "mri" 'org-roam-insert
    "mrg" 'org-roam-graph)

  (use-package org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)
    (when (featurep 'xwidget-internal)
      (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)))

  (use-package org-roam-bibtex
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :bind (:map org-mode-map (("C-c n a" . orb-note-actions)))))

(use-package org-remark
  :bind (;; :bind keyword also implicitly defers org-remark itself.
         ;; Keybindings before :map is set for global-map.
         ("C-c n m" . org-remark-mark)
         ("C-c n l" . org-remark-mark-line) ; new in v1.3
         :map org-remark-mode-map
         ("C-c n o" . org-remark-open)
         ("C-c n ]" . org-remark-view-next)
         ("C-c n [" . org-remark-view-prev)
         ("C-c n r" . org-remark-remove)
         ("C-c n d" . org-remark-delete))
  ;; Alternative way to enable `org-remark-global-tracking-mode' in
  ;; `after-init-hook'.
  ;; :hook (after-init . org-remark-global-tracking-mode)
  :init
  ;; It is recommended that `org-remark-global-tracking-mode' be
  ;; enabled when Emacs initializes. Alternatively, you can put it to
  ;; `after-init-hook' as in the comment above
  (org-remark-global-tracking-mode +1)
  :config
  (use-package org-remark-info
    :ensure nil
    :after info
    :config (org-remark-info-mode +1))
  (use-package org-remark-eww
    :ensure nil
    :after eww
    :config (org-remark-eww-mode +1))
  (use-package org-remark-nov
    :ensure nil
    :after nov
    :config (org-remark-nov-mode +1)))

(use-package org-analyzer
  :commands (org-analyzer-start))

;; https://gitlab.com/phillord/org-drill
(use-package org-drill
  :config (add-to-list 'org-modules 'org-drill))

(use-package org-board)

(use-package org-noter
  :init (setq org-noter-notes-search-path '((expand-file-name "~/.org/book"))
              org-noter-auto-save-last-location t
              org-noter-highlight-selected-text t))

(use-package anki-helper
  :load-path "site-lisp/emacs-anki-helper"
  :config
  (defun anki-helper--entry-locate (filename entry-name)
    (find-file filename)
    (let* ((data (org-element-parse-buffer))
           (pos (org-element-map data '(headline)
                                 (lambda (elt)
                                   (when (string= (org-element-property :raw-value elt)
                                                  entry-name)
                                     (org-element-property :begin elt)))
                                 nil t)))
      (goto-char pos)
      (org-reveal)))

  (defun anki-helper-fields-get-with-backlink ()
    "Get filed info of the current entry with backlink."
    (let* ((front-and-back (anki-helper-fields-get-default))
           (filename (file-name-nondirectory (buffer-file-name)))
           (elt (plist-get (org-element-at-point) 'headline))
           (entry (plist-get elt :raw-value)))
      `(,@front-and-back ,filename ,entry)))

  (setq anki-helper-note-types '(("Basic (with backlink)" "Front" "Back" "Source" "Location"))
        anki-helper-fields-get-alist '(("Basic (with backlink)" . anki-helper-fields-get-with-backlink))
        anki-helper-default-note-type "Basic (with backlink)"))

(use-package plantuml
  :commands (plantuml--parse-headlines plantuml-org-to-mindmap plantuml-display-json plantuml-org-to-mindmap-open)
  :load-path "site-lisp/plantuml-emacs"
  :config
  (setq plantuml-jar-path (expand-file-name (concat user-emacs-directory "plantuml.jar"))
        plantuml-output-type "svg"
        plantuml-relative-path "./imgs/"
        plantuml-theme "plain"
        plantuml-add-index-number t
        plantuml-log-command t
        plantuml-org-headline-bold t))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
