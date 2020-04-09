;;; core-custom.el --- Adam Greloch's core-custom.el File For GNU Emacs

;; Copyright (C) 2020 Adam Greloch

;; Author: Adam Greloch <zplhatesbananas@gmail.com>
;; Version: 20200409
;; Keywords: local, convenience

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

;; This is my personal startup file for GNU Emacs.  It has only recently
;; been tested on GNU Emacs 26.3. Since I'm a total beginner in GNU Emacs,
;; beware of newbie moves.

;;; Code:

;; ===========================================================================
;; Custom set variables, faces, agenda-files
;; ===========================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe-mode 20 nil (fringe))
 '(line-spacing 0.1)
 '(org-agenda-files
   (quote
    ("~/Dysk Google/org/casual.org" "~/Dysk Google/org/school.org")))
 '(package-selected-packages
   (quote
    (company shell-pop auctex magit add-node-modules-path flycheck web-mode neotree spacegray-theme ibuffer-projectile rainbow-delimiters linum-relative spacemacs-theme ibuffer-sidebar ace-window all-the-icons dashboard vscode-icon dired-sidebar darkroom use-package markdown-mode)))
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
 '(neo-dir-link-face ((t (:foreground "#ffffff"))))
 '(neo-file-link-face ((t (:foreground "#a7bca4"))))
 '(neo-root-dir-face ((t (:foreground "#ffffff" :weight bold)))))

(provide 'core-custom)

;;; core-custom.el ends here
