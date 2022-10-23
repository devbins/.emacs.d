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
;;     Update #: 654
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
    :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
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
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
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
  :init (setq org-todo-keywords
              '((sequence "TODO(t!)" "DOING(d!)" "|" "DONE(o!)" "ABORT(a@/!)")
                (sequence "❍(!)" "⥁(!)" "❓(!)" "⤽(!)" "|" "✔(!)" "✘(@/!)" "♱(@/!)"))
              org-priority-faces '((?A . error)
                                   (?B . warning)
                                   (?C . success))
              org-imenu-depth 5
              ;; define the refile targets
              org-refile-targets '((org-agenda-files :maxlevel . 3))
              org-refile-use-outline-path 'file
              org-outline-path-complete-in-steps nil
              org-auto-align-tags nil
              org-tags-column 0
              org-catch-invisible-edits 'show-and-error
              org-log-done 'notevery
              org-html-checkbox-type 'unicode

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
              org-image-actual-width (/ (display-pixel-width) 3)
              org-startup-with-inline-images t
              org-display-remote-inline-images 'download

              org-hide-leading-stars t
              org-hide-emphasis-markers t
              org-pretty-entities t ;; 显示 _ 下标 ^ 上标，通过下面的配置，当需要显示的时候放到 {} 中。SPC m T e 来切换显示
              org-export-with-broken-links 'mark
              org-export-with-sub-superscripts '{}
              org-export-use-babel nil ;; 导出的时候不执行代码，会导致设置的 header-arg 无效 do not evaluate again during export.
              org-export-with-toc nil
              org-export-with-section-numbers nil
              org-export-with-entities t ;; 导出时是否进行转义
              org-use-sub-superscripts '{}

              org-ditaa-jar-path (concat user-emacs-directory "ditaa.jar")
              ;; org-startup-indented t
              org-startup-folded    'show2levels
              org-adapt-indentation t
              org-startup-truncated nil
              org-ellipsis (if (char-displayable-p ?⤵) "  ⤵" "  ▼")
              org-tag-alist (quote ((:startgroup)
                                    ("@errand" . ?e)
                                    ("@office" . ?o)
                                    ("@home" . ?H)
                                    (:endgroup)
                                    ("READING" . ?r)
                                    ("PERSONAL" . ?P)
                                    ("WORK" . ?W)
                                    ("ORG" . ?O)
                                    ("BLOG" . ?b)
                                    ("THOUGHT". ?T)
                                    ("crypt" . ?E)
                                    ("NOTE" . ?n)
                                    ("CANCELLED" . ?c)
                                    ("FLAGGED" . ??))))
  :config
  (add-to-list 'org-modules 'org-protocol)
  (defface org-bold
    '((t :foreground "#d2268b"
         :background nil
         :weight bold
         :underline t
         :overline t))
    "Face for org-mode bold."
    :group 'org-faces)

  (setq org-emphasis-alist
        '(("*" org-bold)
          ("/" italic)
          ("_" underline)
          ("=" org-verbatim verbatim)
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

  ;; Add gfm/md backends
  (use-package ox-gfm)
  (use-package ox-hugo)
  (add-to-list 'org-export-backends 'md)
  (add-to-list 'org-export-backends 'gfm)
  (add-to-list 'org-export-backends 'hugo)

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
    (setq org-superstar-special-todo-items t
          org-superstar-headline-bullets-list '("◉" "○" "✸" "☢" "❀" "✿" "☯" "☭" "♥" "✜" "♠" "★")
          org-superstar-item-bullet-alist
          '((?* . ?◈)
            (?+ . ?✚)
            (?- . ?▶))))

  (use-package svg-tag-mode
    :init
    (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
    (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
    (defconst day-re "[A-Za-z]\\{3\\}")
    (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

    (defun svg-progress-percent (value)
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                        nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag (concat value "%")
                               nil :stroke 0 :margin 0)) :ascent 'center))

    (defun svg-progress-count (value)
      (let* ((seq (mapcar #'string-to-number (split-string value "/")))
             (count (float (car seq)))
             (total (float (cadr seq))))
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar (/ count total) nil
                                          :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                    (svg-lib-tag value nil
                                 :stroke 0 :margin 0)) :ascent 'center)))

    (setq svg-tag-tags
          `(
            ;; Org tags
            (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
            (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

            ;; Task priority
            ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                  (svg-tag-make tag :face 'org-priority
                                                :beg 2 :end -1 :margin 0))))

            ;; Progress
            ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                                (svg-progress-percent (substring tag 1 -2)))))
            ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                              (svg-progress-count (substring tag 1 -1)))))

            ;; TODO / DONE
            ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
            ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


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
    :hook (org-mode . svg-tag-mode))

  (use-package valign
    :quelpa (valign :fetcher github :repo "casouri/valign")
    :hook (org-mode . valign-mode)
    :init (setq valign-fancy-bar t))

  ;; Show hidden emphasis markers
  (use-package org-appear
    :hook (org-mode . org-appear-mode))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-src-fontify-natively t
        org-src-window-setup 'other-window
        org-src-tab-acts-natively t)

  (setq org-babel-C-compiler "gcc -std=c++17"
        org-babel-C++-compiler "g++ -std=c++17")

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (sql . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t) ;; #+begin_src cpp :includes <iostream> :flags "-std=c++11"
                               (calc . t)
                               (java . t)
                               (dot . t)
                               (ditaa . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/>=26p
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)
  ;; Org babel extensions
  ;; HTTP client
  ;; usage: BEGIN_SRC restclient
  (use-package ob-restclient
    :init (cl-pushnew '(restclient . t) load-language-list))

  ;; Async src_block execution
  ;; usage: begin_src sh :async
  (use-package ob-async
    :config (setq ob-async-no-async-languages-alist
                  '("ipython"
                    "jupyter-python"
                    "jupyter-julia"
                    "jupyter-R"
                    "jupyter-javascript")))


  (use-package ob-kotlin
    :init (cl-pushnew '(kotlin . t) load-language-list))

  (use-package gnuplot
    :init (cl-pushnew '(gnuplot . t) load-language-list))

  (use-package mermaid-mode
    :if (executable-find "mmdc"))
  (use-package ob-mermaid
    :if (executable-find "mmdc")
    :init (cl-pushnew '(mermaid . t) load-language-list))

  (use-package ob-dart
    :init (cl-pushnew '(dart . t) load-language-list))

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
           ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Export text/html MIME emails
  (use-package org-mime
    :bind (:map message-mode-map
           ("C-c M-o" . org-mime-htmlize)
           :map org-mode-map
           ("C-c M-o" . org-mime-org-buffer-htmlize)))


  (use-package htmlize)

  (use-package ox-html
    :ensure nil
    :config
    ;; Org export code style
    (setq org-html-htmlize-output-type 'css)
    (setq-default org-html-doctype "html5")
    (setq-default org-html-html5-fancy t))

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

  (use-package org-inline-pdf)
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
    "mCc"                       'org-clock-cancel
    "mCd"                       'org-clock-display
    "mCe"                       'org-evaluate-time-range
    "mCg"                       'org-clock-goto
    "mCi"                       'org-clock-in
    "mCI"                       'org-clock-in-last
    "mCo"                       'org-clock-out
    "mCp"                       'org-pomodoro
    "mCR"                       'org-clock-report
    "mCr"                       'org-resolve-clocks

    "mdd"                       'org-deadline
    "mds"                       'org-schedule
    "mdt"                       'org-time-stamp
    "mdT"                       'org-time-stamp-inactive
    "mee"                       'org-export-dispatch
    "mfi"                       'org-feed-goto-inbox
    "mfu"                       'org-feed-update-all

    "ma"                        'org-agenda

    "mp"                        'org-priority

    "mTc"                       'org-toggle-checkbox
    "mTe"                       'org-toggle-pretty-entities
    "mTi"                       'org-toggle-inline-images
    "mTl"                       'org-toggle-link-display
    "mTt"                       'org-show-todo-tree
    "mTT"                       'org-todo
    "mTx"                       'org-toggle-latex-fragment

    ;; More cycling options (timestamps, headlines, items, properties)
    "mL"                        'org-shiftright
    "mH"                        'org-shiftleft
    "mJ"                        'org-shiftdown
    "mK"                        'org-shiftup

    ;; Change between TODO sets
    "mC-S-l"                    'org-shiftcontrolright
    "mC-S-h"                    'org-shiftcontrolleft
    "mC-S-j"                    'org-shiftcontroldown
    "mC-S-k"                    'org-shiftcontrolup

    ;; Subtree editing
    "msa"                       'org-toggle-archive-tag
    "msA"                       'org-archive-subtree
    "msb"                       'org-tree-to-indirect-buffer
    "msd"                       'org-cut-subtree
    "msh"                       'org-promote-subtree
    "msj"                       'org-move-subtree-down
    "msk"                       'org-move-subtree-up
    "msl"                       'org-demote-subtree
    "msn"                       'org-narrow-to-subtree
    "msN"                       'widen
    "msr"                       'org-refile
    "mss"                       'org-sparse-tree
    "msS"                       'org-sort

    ;; tables
    "mta"                       'org-table-align
    "mtb"                       'org-table-blank-field
    "mtc"                       'org-table-convert
    "mtdc"                      'org-table-delete-column
    "mtdr"                      'org-table-kill-row
    "mte"                       'org-table-eval-formula
    "mtE"                       'org-table-export
    "mth"                       'org-table-previous-field
    "mtH"                       'org-table-move-column-left
    "mtic"                      'org-table-insert-column
    "mtih"                      'org-table-insert-hline
    "mtiH"                      'org-table-hline-and-move
    "mtir"                      'org-table-insert-row
    "mtI"                       'org-table-import
    "mtj"                       'org-table-next-row
    "mtJ"                       'org-table-move-row-down
    "mtK"                       'org-table-move-row-up
    "mtl"                       'org-table-next-field
    "mtL"                       'org-table-move-column-right
    "mtn"                       'org-table-create
    "mtN"                       'org-table-create-with-table.el
    "mtr"                       'org-table-recalculate
    "mts"                       'org-table-sort-lines
    "mttf"                      'org-table-toggle-formula-debugger
    "mtto"                      'org-table-toggle-coordinate-overlays
    "mtw"                       'org-table-wrap-region

    ;; Source blocks / org-babel
    "mbp"                       'org-babel-previous-src-block
    "mbn"                       'org-babel-next-src-block
    "mbe"                       'org-babel-execute-maybe
    "mbo"                       'org-babel-open-src-block-result
    "mbv"                       'org-babel-expand-src-block
    "mbu"                       'org-babel-goto-src-block-head
    "mbg"                       'org-babel-goto-named-src-block
    "mbr"                       'org-babel-goto-named-result
    "mbb"                       'org-babel-execute-buffer
    "mbs"                       'org-babel-execute-subtree
    "mbd"                       'org-babel-demarcate-block
    "mbt"                       'org-babel-tangle
    "mbf"                       'org-babel-tangle-file
    "mbc"                       'org-babel-check-src-block
    "mbj"                       'org-babel-insert-header-arg
    "mbl"                       'org-babel-load-in-session
    "mbi"                       'org-babel-lob-ingest
    "mbI"                       'org-babel-view-src-block-info
    "mbz"                       'org-babel-switch-to-session
    "mbZ"                       'org-babel-switch-to-session-with-code
    "mba"                       'org-babel-sha1-hash
    "mbx"                       'org-babel-do-key-sequence-in-edit-buffer
    ;; Multi-purpose keys
    "m*"                        'org-ctrl-c-star
    "m-"                        'org-ctrl-c-minus
    "m#"                        'org-update-statistics-cookies
    ;; attachments
    "mA"                        'org-attach
    ;; insertion
    "mib"                       'org-insert-structure-template
    "mid"                       'org-insert-drawer
    "mie"                       'org-set-effort
    "mif"                       'org-footnote-new
    "mih"                       'org-insert-heading
    "miH"                       'org-insert-heading-after-current
    "mii"                       'org-insert-item
    "mil"                       'org-insert-link
    "min"                       'org-add-note
    "mip"                       'org-set-property
    "mis"                       'org-insert-subheading
    "mit"                       'org-set-tags-command)
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
    :hook ((org-pomodoro-started . (lambda ()(when sys/macp (do-applescript "tell application \"JustFocus\"\n    launch\n    start pomodoro\nend tell"))))
           (org-pomodoro-finished . (lambda () (pomodoro-notify "Pomodoro Completed!" "Time for a break.")))
           (org-pomodoro-break-finished . (lambda () (pomodoro-notify "Pomodoro Short Break Finished" "Ready for Another?")))
           (org-pomodoro-long-break-finished . (lambda () (pomodoro-notify "Pomodoro Long Break Finished" "Ready for Another?")))
           (org-pomodoro-killed . (lambda () (progn (pomodoro-notify "Pomodoro Killed" "One does not simply kill a pomodoro!")
                                               (when sys/macp (do-applescript "tell application \"JustFocus\"\n    stop\nend tell")))))))

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
          ;; 设置用於加密的 GPG ID 设置为 nil 使用对称加密 (symmetric encryption)
          org-crypt-key nil)
    :config
    (setenv "GPG_AGENT_INFO" nil)
    ;; 当被加密的部份被保存时，自動加密回去
    (org-crypt-use-before-save-magic))

  (use-package epa-file
    :ensure nil
    :config
    (epa-file-enable)
    (setq epa-pinentry-mode 'loopback
          epa-file-select-keys 0
          epa-file-cache-passphrase-for-symmetric-encryption t))

  ;; pinentry-start 要使用的时候 Mac 下需要
  ;; 把allow-emacs-pinentry 加入 .gnupg/gpg-agent.conf
  ;; http://elpa.gnu.org/packages/pinentry.html
  ;; This will force Emacs to use its own internal password prompt instead of an external pin entry program.
  (use-package pinentry
    :defer t
    :if sys/macp
    :config
    (with-eval-after-load 'org
      (pinentry-start)))

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


(use-package easy-hugo
  :defer t
  :init
  (setq easy-hugo-basedir "~/git/blog/")
  (setq easy-hugo-url "https://devbins.github.io/")
  (setq easy-hugo-preview-url "http://127.0.0.1:1313/")
  (setq easy-hugo-postdir "content/post")
  (setq easy-hugo-default-ext ".org"))

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
    (make-directory (file-name-directory filename)))
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
  :custom
  (org-roam-v2-ack t)
  (org-roam-directory (expand-file-name "~/.org"))
  (org-roam-mute-cache-build t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ("C-c n j" . org-roam-dailies-capture-today)
           ("C-c n I" . org-roam-insert-immediate))
  :config
  (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory))
  (add-to-list 'org-modules 'org-roam-protocol)
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
        org-roam-ui-open-on-start t)))

  (use-package org-roam-bibtex
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :bind (:map org-mode-map (("C-c n a" . orb-note-actions))))

(use-package org-analyzer
  :commands (org-analyzer-start))

;; https://gitlab.com/phillord/org-drill
(use-package org-drill
  :config (add-to-list 'org-modules 'org-drill))
(use-package org-board)

;; https://github.com/chenyanming/calibredb.el
(use-package calibredb
  :if (executable-find "calibre")
  :commands (calibredb calibredb-list)
  :config
  (setq calibredb-root-dir "~/Calibre Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)))

(use-package org-media-note
  :quelpa (org-media-note :fetcher github :repo "yuchen-lea/org-media-note")
  :hook (org-mode .  org-media-note-mode)
  :bind (("H-v" . org-media-note-hydra/body))  ;; Main entrance
  :config
  (setq org-media-note-screenshot-image-dir "~/notes/imgs/"))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
