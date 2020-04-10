;;; init.el --- Adam Greloch's init.el File For GNU Emacs

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
;; Simple Settings and Internal Packages
;; ===========================================================================

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))

(package-initialize)

;; ===========================================================================
;; Module loading
;; ===========================================================================

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(defvar emacs-dir (file-name-directory "~/.emacs.d/")
  "The root dir of the Emacs distribution.")

(defvar core-dir (expand-file-name "core" emacs-dir)
  "The home of core functionality.")

(defvar modules-dir (expand-file-name "modules" emacs-dir)
  "This directory houses all of the modules.")

(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)

(require 'core-packages)
(require 'core-settings)
(require 'core-custom)
(require 'core-ui)
(require 'module-smart-compile)
(require 'module-powershell6)
(require 'module-theme)
(require 'module-latex)
(require 'markdown-mode)
(require 'module-web-mode)

;; leaving this here for now until I find out what it did and why is it
;; here

;; (defadvice linum-update-window (around linum-dynamic activate)
;;   (let* ((w (length (number-to-string
;;                      (count-lines (point-min) (point-max)))))
;;          (linum-format (concat " %" (number-to-string w) "d ")))
;;     ad-do-it))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" default)))
 '(fringe-mode 20 nil (fringe))
 '(line-spacing 0.1)
 '(neo-theme (quote ascii))
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
 '(neo-button-face ((t (:underline nil))))
 '(neo-dir-link-face ((t (:foreground "#ffffff"))))
 '(neo-file-link-face ((t (:foreground "#a7bca4"))))
 '(neo-root-dir-face ((t (:foreground "#ffffff" :weight bold)))))
