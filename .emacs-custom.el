;;; .emacs-custom.el --- Adam Greloch's .emacs-custom.el File for GNU Emacs

;; Copyright (C) 2020 Adam Greloch

;; Author: Adam Greloch <zplhatesbananas@gmail.com>
;; Version: 20200410
;; Keywords: local, convenience
;; URL: https://bitbucket.org/admq/emacs.d/

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This file is a part of my configuration for GNU Emacs.  It has only recently
;; been tested on GNU Emacs 26.3. Since I'm a total beginner in GNU Emacs,
;; beware of newbie moves.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and start")
     (output-dvi "Yap")
     (output-pdf "PDF Tools")
     (output-html "start"))))
 '(custom-safe-themes
   (quote
    ("d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" default)))
 '(dashboard-items (quote ((recents . 10) (bookmarks . 5) (agenda . 5))))
 '(display-battery-mode nil)
 '(display-line-numbers-width nil)
 '(fringe-mode 20 nil (fringe))
 '(line-spacing 0.1)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(neo-theme (quote ascii))
 '(neo-window-fixed-size nil)
 '(neo-window-width 30)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/casual.org" "~/Dropbox/org/school.org" "c:/git/admq_latex_repo/admq_latex.org")))
 '(package-selected-packages
   (quote
    (zenburn-theme pdf-tools awesome-tab rjsx-mode company shell-pop auctex magit add-node-modules-path flycheck neotree spacegray-theme ibuffer-projectile rainbow-delimiters linum-relative spacemacs-theme ibuffer-sidebar ace-window dashboard vscode-icon dired-sidebar darkroom use-package markdown-mode)))
 '(shell-pop-autocd-to-working-dir nil)
 '(shell-pop-shell-type
   (quote
    ("powershell6" "*powershell6*"
     (lambda nil
       (powershell6)))))
 '(shell-pop-window-position "right"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :height 130))))
 '(fringe ((t (:background "nil"))))
 '(neo-button-face ((t (:underline nil))))
 '(neo-dir-link-face ((t (:foreground "#ffffff"))))
 '(neo-file-link-face ((t (:foreground "#a7bca4"))))
 '(neo-root-dir-face ((t (:foreground "#ffffff" :weight bold)))))

;;; .emacs-custom.el ends here
