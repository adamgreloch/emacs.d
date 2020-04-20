
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
 '(LaTeX-indent-environment-list
   (quote
    (("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("filecontents" current-indentation)
     ("filecontents*" current-indentation)
     ("align" LaTeX-indent-tabular)
     ("align*" LaTeX-indent-tabular)
     ("array" LaTeX-indent-tabular)
     ("eqnarray" LaTeX-indent-tabular)
     ("eqnarray*" LaTeX-indent-tabular)
     ("displaymath")
     ("equation")
     ("equation*")
     ("picture")
     ("tabbing"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and start")
     (output-dvi "Yap")
     (output-pdf "SumatraPDF")
     (output-html "start"))))
 '(blink-cursor-mode t)
 '(custom-safe-themes
   (quote
    ("85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "bffb799032a7404b33e431e6a1c46dc0ca62f54fdd20744a35a57c3f78586646" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" default)))
 '(dashboard-items (quote ((recents . 10) (bookmarks . 5) (agenda . 5))))
 '(display-battery-mode nil)
 '(display-line-numbers-width nil)
 '(fringe-mode 20 nil (fringe))
 '(line-spacing 0.1)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/casual.org" "~/Dropbox/org/school.org" "c:/git/admq_latex_repo/admq_latex.org")))
 '(package-selected-packages
   (quote
    (rainbow-mode ido-vertical-mode ivy diminish flx-ido base16-theme which-key awesome-tab rjsx-mode company shell-pop auctex magit add-node-modules-path flycheck neotree ibuffer-projectile rainbow-delimiters ibuffer-sidebar ace-window dashboard dired-sidebar darkroom use-package markdown-mode)))
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
 '(fringe ((t (:background "nil")))))

;;; .emacs-custom.el ends here
