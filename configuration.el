(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'server)
(or (server-running-p)
     (server-start))

(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(set-language-environment "UTF-8")

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(tooltip-mode -1)

(use-package awesome-tab
  :load-path "elisp/awesome-tab"
  :config
  (setq awesome-tab-display-icon nil)
  (setq awesome-tab-height 130)
  (awesome-tab-mode t))

(global-set-key (kbd "s-1") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-2") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-3") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-4") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-5") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-6") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-7") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-8") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-9") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-0") 'awesome-tab-select-visible-tab)

(use-package neotree
  :config
  (setq neo-theme 'ascii
	neo-smart-open t))
(global-set-key (kbd "C-x C-n") 'neotree-toggle)

(use-package ibuffer-sidebar
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font 't
	ibuffer-sidebar-face '(:family "Consolas" :height 120)))

(use-package ibuffer-projectile
  :commands (ibuffer-projectile-set-filter-groups
             ibuffer-projectile-generate-filter-groups)
  :init
  (defun j-ibuffer-projectile-run ()
    "Set up `ibuffer-projectile'."
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  (add-hook 'ibuffer-sidebar-mode-hook #'j-ibuffer-projectile-run)
  (add-hook 'ibuffer-hook #'j-ibuffer-projectile-run)
  :config
  (setq ibuffer-projectile-prefix ""))

(global-set-key (kbd "C-x C-m") 'ibuffer-sidebar-toggle-sidebar)

(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)

(defun powershell6 (&optional buffer)
  "Launches a PowerShell 6 instance in buffer *powershell6* and switches to it."
  (interactive)
  (let ((buffer (or buffer "*powershell6*"))
	(powershell6-prog "C:\\Program Files\\PowerShell\\6\\pwsh.exe"))
    (make-comint-in-buffer "shell" "*powershell6*" powershell6-prog)
    (switch-to-buffer buffer)))

;; make comint-derived modes' like `shell' output and prompt read-only
(setq comint-prompt-read-only t)

(defun my-comint-preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))

(add-hook 'comint-preoutput-filter-functions 'my-comint-preoutput-turn-buffer-read-only)

(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

(setq default-process-coding-system '(utf-8 . utf-8))

(use-package shell-pop)

(global-set-key (kbd "C-`") 'shell-pop)

(use-package magit)

(use-package which-key)
(which-key-mode)

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1))

(use-package elcord)
(elcord-mode)

(use-package latex
  :defer t
  :ensure auctex
  :config
  (setq font-latex-fontify-script nil)
  (setq font-latex-fontify-sectioning 'color)
  (setq TeX-auto-save t))

(set-default 'preview-scale-function 1.3)

(use-package org)

(global-set-key (kbd "C-x C-a") 'org-agenda)

(setq org-startup-indented 't)
(setq org-hide-leading-stars 't)

(defadvice org-archive-subtree (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (afile (org-extract-archive-file (org-get-local-archive-location)))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath (split-string olpath "/")))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (org-narrow-to-subtree)
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading
                            "\n"))
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))))

(add-to-list 'org-modules 'org-tempo t)

(require 'ox-latex)
(add-to-list 'org-latex-classes
           '("ADMQ-scrartcl"
         "\\documentclass[DIV=calc, 11pt]{scrartcl}
\\usepackage{xpatch}
\\makeatletter
    \\xpatchcmd{\\@maketitle}{\\begin{center}}{\\begin{flushleft}}{}{}
    \\xpatchcmd{\\@maketitle}{\\end{center}}{\\end{flushleft}}{}{}
    \\xpatchcmd{\\@maketitle}{\\begin{tabular}[t]{c}}{\\begin{tabular}[t]{@{}l@{}}}{}{}
\\makeatother

\\usepackage[activate=true,
    final,
    babel=true,
    auto=true,
    expansion,
    protrusion=true,
    tracking=true,
    kerning=true,
    spacing=true,
    factor=0,
    stretch=15,
    shrink=30]{microtype}

\\usepackage[utf8]{inputenc}
\\usepackage{polski}
\\usepackage[polish]{babel}
\\usepackage{setspace}
\\usepackage[textsize=scriptsize, colorinlistoftodos, obeyDraft]{todonotes}

\\newcommand{\\todoim}[2][]
{\\todo[color=red, #1]{#2}}

\\newcommand{\\todomed}[2][]
{\\todo[color=yellow, #1]{#2}}

\\usepackage{marginnote}
\\renewcommand*{\\marginfont}{\\color{gray}\\small\\ttfamily}

\\usepackage[hidelinks]{hyperref}

\\setkomafont{date}{%
    \\usekomafont{subtitle}
    }

\\setkomafont{author}{%
    \\usekomafont{subtitle}
    }

[NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]

\\setstretch{1}

\\usepackage{geometry}
\\geometry{a4paper, margin=0.5in, right=1.7in, bottom=0.7in, footskip=0.3in, marginpar=1.2in}

\\usepackage{enumitem}
\\setlist[itemize]{topsep=0.3em, itemsep=0em, label={\\scriptsize\\textbullet}}
\\setlist[enumerate]{topsep=0.3em, leftmargin=2.8em, itemsep=0em, label={\\small\\textbf{\\arabic*.}}}

\\usepackage{amsmath, amsthm}
\\usepackage{natbib}

\\newtheorem{theorem}{Twierdzenie}
\\numberwithin{equation}{section}
\\setlength{\\parindent}{0em}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Copyright (C) 1998-2020  by Seiji Zenitani

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Version: 20200322
;; Keywords: tools, unix
;; Created: 1998-12-27
;; Compatibility: Emacs 21 or later
;; URL(en): https://github.com/zenitani/elisp/blob/master/smart-compile.el
;; URL(jp): http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#smart-compile

;;; Commentary:

;; This package provides `smart-compile' function.
;; You can associate a particular file with a particular compile function,
;; by editing `smart-compile-alist'.
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'smart-compile)
;;
;; Note that it requires emacs 21 or later.

;;; Code:

(defgroup smart-compile nil
  "An interface to `compile'."
  :group 'processes
  :prefix "smart-compile")

(defcustom smart-compile-alist '(
				 ("\\.pas\\'" . "fpc %f && %n.exe")
  (emacs-lisp-mode    . (emacs-lisp-byte-compile))
  (html-mode          . (browse-url-of-buffer))
  (nxhtml-mode        . (browse-url-of-buffer))
  (html-helper-mode   . (browse-url-of-buffer))
  (octave-mode        . (run-octave))
  ("\\.c\\'"          . "gcc -O2 %f -lm -o %n")
;;  ("\\.c\\'"          . "gcc -O2 %f -lm -o %n && ./%n") ;; unix, macOS
;;  ("\\.c\\'"          . "gcc -O2 %f -lm -o %n && %n") ;; win
  ("\\.[Cc]+[Pp]*\\'" . "g++ -O2 %f -lm -o %n")
  ("\\.cron\\(tab\\)?\\'" . "crontab %f")
  ("\\.cu\\'"         . "nvcc %f -o %n")
  ("\\.cuf\\'"        . "pgfortran %f -o %n")
  ("\\.[Ff]\\'"       . "gfortran %f -o %n")
  ("\\.[Ff]90\\'"     . "gfortran %f -o %n")
  ("\\.hs\\'"         . "ghc %f -o %n")
  ("\\.java\\'"       . "javac %f")
  ("\\.jl\\'"         . "julia %f")
  ("\\.m\\'"          . "gcc -O2 %f -lobjc -lpthread -o %n")
  ("\\.mp\\'"         . "mptopdf %f")
  ("\\.php\\'"        . "php -l %f")
  ("\\.pl\\'"         . "perl %f")
  ("\\.py\\'"         . "python3 %f")
  ("\\.rb\\'"         . "ruby %f")
  ("Rakefile\\'"      . "rake")
  ("Gemfile\\'"       . "bundle install")
  ("\\.tex\\'"        . (tex-file))
  ("\\.texi\\'"       . "makeinfo %f")
;;  ("\\.pl\\'"         . "perl -cw %f") ; syntax check
;;  ("\\.rb\\'"         . "ruby -cw %f") ; syntax check
)  "Alist of filename patterns vs corresponding format control strings.
Each element looks like (REGEXP . STRING) or (MAJOR-MODE . STRING).
Visiting a file whose name matches REGEXP specifies STRING as the
format control string.  Instead of REGEXP, MAJOR-MODE can also be used.
The compilation command will be generated from STRING.
The following %-sequences will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extension  ( netscape )
  %e  extension of file name       ( bin )

  %o  value of `smart-compile-option-string'  ( \"user-defined\" ).

If the second item of the alist element is an emacs-lisp FUNCTION,
evaluate FUNCTION instead of running a compilation command.
"
   :type '(repeat
           (cons
            (choice
             (regexp :tag "Filename pattern")
             (function :tag "Major-mode"))
            (choice
             (string :tag "Compilation command")
             (sexp :tag "Lisp expression"))))
   :group 'smart-compile)
(put 'smart-compile-alist 'risky-local-variable t)

(defconst smart-compile-replace-alist '(
  ("%F" . (buffer-file-name))
  ("%f" . (file-name-nondirectory (buffer-file-name)))
  ("%n" . (file-name-sans-extension
           (file-name-nondirectory (buffer-file-name))))
  ("%e" . (or (file-name-extension (buffer-file-name)) ""))
  ("%o" . smart-compile-option-string)
;;   ("%U" . (user-login-name))
  )
  "Alist of %-sequences for format control strings in `smart-compile-alist'.")
(put 'smart-compile-replace-alist 'risky-local-variable t)

(defvar smart-compile-check-makefile t)
(make-variable-buffer-local 'smart-compile-check-makefile)

(defcustom smart-compile-make-program "make "
  "The command by which to invoke the make program."
  :type 'string
  :group 'smart-compile)

(defcustom smart-compile-option-string ""
  "The option string that replaces %o.  The default is empty."
  :type 'string
  :group 'smart-compile)


;;;###autoload
(defun smart-compile (&optional arg)
  "An interface to `compile'.
It calls `compile' or other compile function,
which is defined in `smart-compile-alist'."
  (interactive "p")
  (let ((name (buffer-file-name))
        (not-yet t))
    
    (if (not name)(error "cannot get filename."))
;;     (message (number-to-string arg))

    (cond

     ;; local command
     ;; The prefix 4 (C-u M-x smart-compile) skips this section
     ;; in order to re-generate the compile-command
     ((and (not (= arg 4)) ; C-u M-x smart-compile
           (local-variable-p 'compile-command)
           compile-command)
      (call-interactively 'compile)
      (setq not-yet nil)
      )

     ;; make?
     ((and smart-compile-check-makefile
           (or (file-readable-p "Makefile")
               (file-readable-p "makefile")))
      (if (y-or-n-p "Makefile is found.  Try 'make'? ")
          (progn
            (set (make-local-variable 'compile-command) "make ")
            (call-interactively 'compile)
            (setq not-yet nil)
            )
        (setq smart-compile-check-makefile nil)))

     ) ;; end of (cond ...)

    ;; compile
    (let( (alist smart-compile-alist) 
          (case-fold-search nil)
          (function nil) )
      (while (and alist not-yet)
        (if (or
             (and (symbolp (caar alist))
                  (eq (caar alist) major-mode))
             (and (stringp (caar alist))
                  (string-match (caar alist) name))
             )
            (progn
              (setq function (cdar alist))
              (if (stringp function)
                  (progn
                    (set (make-local-variable 'compile-command)
                         (smart-compile-string function))
                    (call-interactively 'compile)
                    )
                (if (listp function)
                    (eval function)
                    ))
              (setq alist nil)
              (setq not-yet nil)
              )
          (setq alist (cdr alist)) )
        ))

    ;; If compile-command is not defined and the contents begins with "#!",
    ;; set compile-command to filename.
    (if (and not-yet
             (not (memq system-type '(windows-nt ms-dos)))
             (not (string-match "/\\.[^/]+$" name))
             (not
              (and (local-variable-p 'compile-command)
                   compile-command))
             )
        (save-restriction
          (widen)
          (if (equal "#!" (buffer-substring 1 (min 3 (point-max))))
              (set (make-local-variable 'compile-command) name)
            ))
      )
    
    ;; compile
    (if not-yet (call-interactively 'compile) )

    ))

(defun smart-compile-string (format-string)
  "Document forthcoming..."
  (if (and (boundp 'buffer-file-name)
           (stringp buffer-file-name))
      (let ((rlist smart-compile-replace-alist)
            (case-fold-search nil))
        (while rlist
          (while (string-match (caar rlist) format-string)
            (setq format-string
                  (replace-match
                   (eval (cdar rlist)) t nil format-string)))
          (setq rlist (cdr rlist))
          )
        ))
  format-string)

(add-hook 'prog-mode-hook
	  (lambda ()
		 (local-set-key (kbd "C-x c") 'smart-compile)))

(use-package rjsx-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

(use-package flycheck)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

(setq flycheck-check-syntax-automatically '(save mode-enable))

;; Enable eslint checker for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; Enable flycheck globally
(add-hook 'prog-mode-hook #'flycheck-mode)

(use-package add-node-modules-path)

(add-hook 'flycheck-mode-hook 'add-node-modules-path)

(use-package company)
(add-hook 'prog-mode-hook 'company-mode)

(add-hook 'text-mode-hook 'visual-line-mode)

(use-package markdown-mode)

(use-package darkroom)

(defun my-markdown-mode-hook ()
  (visual-line-mode 1)
  (darkroom-tentative-mode 1))
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents  . 10)
  (bookmarks . 5)
  (projects . 5)
  (agenda . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner '1)
  (dashboard-setup-startup-hook))

(delete-selection-mode 1)

(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(defun my-disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my-disable-scroll-bars)

;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines nil)

(blink-cursor-mode +1)

(use-package diminish
  :config
  (diminish 'projectile-mode)
  (diminish 'which-key-mode)
  (diminish 'auto-revert-mode))

(use-package rainbow-mode)

(defvar d-neo-dir-root '"#bb9584")
(defvar d-neo-file-link '"#b8afad")
(defvar d-neo-expand-btn '"#8ab3b5")
(defvar d-org-hide '"#3b3228")
(defvar d-markdown-code-bg '"#40362b")
(defvar l-neo-dir-root '"#d65d0e")
(defvar l-neo-file-link '"#665c54")
(defvar l-neo-expand-btn '"#076678")
(defvar l-org-hide '"#f9f5d7")
(defvar l-markdown-code-bg '"#f2eed0")

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-mocha t))

(set-face-attribute 'neo-dir-link-face nil
		    :foreground d-neo-dir-root)

(set-face-attribute 'neo-file-link-face nil
		    :foreground d-neo-file-link)

(set-face-attribute 'neo-root-dir-face nil
 		    :foreground d-neo-dir-root
 		    :weight 'bold)

(set-face-attribute 'neo-expand-btn-face nil
		    :foreground d-neo-expand-btn
		    :weight 'bold)

(set-face-attribute 'markdown-code-face nil
		    :background d-markdown-code-bg
		    :inherit 'nil)

(set-face-attribute 'org-hide nil
		    :foreground d-org-hide)

(bind-keys ("C-c tl" . (lambda ()
 			 (interactive)
 			 (load-theme 'base16-gruvbox-light-hard)
 			 (set-face-attribute 'neo-dir-link-face nil
					     :foreground l-neo-dir-root)

			 (set-face-attribute 'neo-file-link-face nil
					     :foreground l-neo-file-link)
			 (set-face-attribute 'neo-root-dir-face nil
					     :foreground l-neo-dir-root
					     :weight 'bold)
			 (set-face-attribute 'neo-expand-btn-face nil
					     :foreground l-neo-expand-btn
					     :weight 'bold)
			 (set-face-attribute 'org-hide nil
					     :foreground l-org-hide)
			 (set-face-attribute 'markdown-code-face nil
					     :background l-markdown-code-bg
					     :inherit 'nil)))
           ("C-c td" . (lambda ()
			 (interactive)
			 (load-theme 'base16-mocha)
			 (set-face-attribute 'neo-dir-link-face nil
					     :foreground d-neo-dir-root)
			 
			 (set-face-attribute 'neo-file-link-face nil
					     :foreground d-neo-file-link)
			 
			 (set-face-attribute 'neo-root-dir-face nil
					     :foreground d-neo-dir-root
					     :weight 'bold)
			 (set-face-attribute 'neo-expand-btn-face nil
					     :foreground d-neo-expand-btn
					     :weight 'bold)
			 (set-face-attribute 'org-hide nil
					     :foreground d-org-hide)
			 (set-face-attribute 'markdown-code-face nil
					     :background d-markdown-code-bg
					     :inherit 'nil))))

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'super)

(setq default-directory "c:/git/" )
