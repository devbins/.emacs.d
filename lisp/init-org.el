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
;;     Update #: 387
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
  :custom-face (org-ellipsis ((t (:foreground nil))))
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
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
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
              '((sequence "TODO(!)" "DOING(!)" "|" "DONE(!)" "ABORT(@/!)")
                (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
              org-todo-keyword-faces '(("TODO")
                                       ("DOING" . "orange")
                                       ("DONE" . "green")
                                       ("ABORT" . "grey")
                                       ("❓" . warning))
              org-priority-faces '((?A . error)
                                   (?B . warning)
                                   (?C . success))
              org-imenu-depth 5
              ;; define the refile targets
              org-refile-targets '((org-agenda-files :maxlevel . 3))
              org-refile-use-outline-path 'file
              org-outline-path-complete-in-steps nil
              org-tags-column -80
              org-log-done 'notevery
              org-html-checkbox-type 'unicode

              org-log-into-drawer t
              org-drawers (quote ("PROPERTIES" "LOGBOOK"))
              org-clock-into-drawer t
              org-clock-idle-time 30
              org-clock-out-when-done t
              org-clock-in-switch-to-state "DOING"
              org-columns-default-format "%50ITEM(Task) %8PRIORITY(Priority) %6TODO(Status) %6Effort(Effort){:} %8CLOCKSUM %16SCHEDULED %16DEADLINE"
              org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                            ("STYLE_ALL" . "habit")))



              ;; #+CAPTION: 设定图片宽度为100
              ;; #+ATTR_HTML: :width 100
              ;; file:data/2013/pict/test.png
              ;; org-image-actual-width (/ (display-pixel-width) 3)
              org-startup-with-inline-images t

              org-hide-emphasis-markers t
              org-pretty-entities t ;; 显示 _ 下标 ^ 上标，通过下面的配置，当需要显示的时候放到 {} 中。SPC m T e 来切换显示
              org-export-with-broken-links                  'mark
              org-export-with-sub-superscripts              '{}
              org-export-use-babel nil ;; 导出的时候不执行代码，会导致设置的 header-arg 无效 do not evaluate again during export.
              org-export-with-toc nil
              org-export-with-section-numbers nil
              org-export-with-entities t ;; 导出时是否进行转义
              org-use-sub-superscripts                      '{}

              org-catch-invisible-edits 'smart

              org-ditaa-jar-path (concat user-emacs-directory "ditaa.jar")
              ;; org-startup-indented t
              ;; org-startup-folded    'content
              org-startup-truncated nil
              org-ellipsis (if (char-displayable-p ?⤵) "  ⤵" "  ▼")
              org-tag-alist (quote ((:startgroup)
                                    ("@errand" . ?e)
                                    ("@office" . ?o)
                                    ("@home" . ?H)
                                    (:endgroup)
                                    ("WAITING" . ?w)
                                    ("HOLD" . ?h)
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

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  ;; Prettify UI
  (use-package org-superstar
    :defer t
    :hook (org-mode . org-superstar-mode)
    :init
    (setq org-superstar-special-todo-items t
          org-superstar-headline-bullets-list
          '("◉" "○" "✸" "✿" "☯" "☭" "♥" "✜" "♠" "☢" "❀" "★")
          org-superstar-item-bullet-alist
          '((?* . ?◈)
            (?+ . ?✚)
            (?- . ?▶))))

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (display-graphic-p)
                    '("⚡" "⬆" "⬇" "☕")
                  '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))

  (use-package valign
    :quelpa (valign :fetcher github :repo "casouri/valign")
    :hook (org-mode . valign-mode))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

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

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)
  ;; Org babel extensions
  ;; HTTP client
  ;; usage: BEGIN_SRC http :pretty
  (use-package ob-http
    :init (cl-pushnew '(http . t) load-language-list))

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

  (use-package org-ref
    :after org)

  ;; Preview
  (use-package org-preview-html
    :diminish)

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
          org-reveal-history nil))

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
    (setq org-tree-slide-skip-outline-level 2))


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
    (setq org-download-display-inline-images 'posframe
          org-download-image-attr-list '("#+ATTR_HTML: :width 80% :align center")
          org-download-method 'directory
          org-download-image-dir "images/")
    (when (eq system-type 'windows-nt)
      (setq org-download-screenshot-method "convert clipboard: %s")))

  ;; 加密文章
  ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
  (use-package org-crypt
    :ensure nil
    :demand
    :init
    ;; 设置要加密的 tag 標籤為 secret
    (setq org-crypt-tag-matcher "crypt"
          ;; 避免 secret 这个 tag 被子項目继承造成重复加密
          org-tags-exclude-from-inheritance (quote ("crypt"))
          ;; 设置用於加密的 GPG ID 设置为 nil 使用对称加密 (symmetric encryption)
          org-crypt-key nil)
    :config
    (setenv "GPG_AGENT_INFO" nil)
    ;; 当被加密的部份被保存时，自動加密回去
    (org-crypt-use-before-save-magic))


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
                                  ("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
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
    :defer t
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
                  "imgs/" (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
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
      (insert (concat "[[file:./imgs/" (file-name-nondirectory filename) "]]")))
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
  :custom ((org-roam-directory (expand-file-name "~/.org"))
           (org-roam-mute-cache-build t))
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-show-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert)))
  :config
  (add-to-list 'org-modules 'org-roam-protocol)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d>-${slug}"
           :head "#+title: ${title}\n#+roam_alias: \n#+roam_tags: \n"
           :unnarrowed t)))
  (add-to-list 'org-roam-capture-ref-templates
               '("a" "Annotation" plain (function org-roam-capture--get-point)
                 "%U ${body}\n"
                 :file-name "${slug}"
                 :head "#+title: ${title}\n#+roam_key: ${ref}\n#+roam_alias:\n"
                 :immediate-finish t
                 :unnarrowed t))
  (evil-leader/set-key-for-mode 'org-roam-mode
    "mrl" 'org-roam
    "mrt" 'org-roam-dailies-today
    "mrb" 'org-roam-switch-to-buffer
    "mrf" 'org-roam-find-file
    "mri" 'org-roam-insert
    "mrg" 'org-roam-graph)

  (use-package org-roam-bibtex
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :bind (:map org-mode-map (("C-c n a" . orb-note-actions))))

  (use-package org-roam-server
    :commands (org-roam-server-mode)))

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
