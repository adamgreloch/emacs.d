;;; module-theme.el --- Adam Greloch's module-theme.el File For GNU Emacs

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

(setq dashboard-startup-banner '1)

;; (use-package almost-mono-themes
;;   :config
;;   ;; (load-theme 'almost-mono-black t)
;;   (load-theme 'almost-mono-gray t))

(set-face-attribute 'neo-dir-link-face nil
		    :foreground "#ffffff")

(set-face-attribute 'neo-file-link-face nil
		    :foreground "#a7bca4")

(set-face-attribute 'neo-root-dir-face nil
 		    :foreground "#ffffff"
 		    :weight 'bold)


;; (bind-keys ("C-c tl" . (lambda ()
;; 			 (interactive)
;; 			 (load-theme 'almost-mono-cream)
;; 			 (set-face-attribute 'neo-dir-link-face nil
;; 					     :foreground "#000000")
			 
;; 			 (set-face-attribute 'neo-file-link-face nil
;; 					     :foreground "#3c5e2b")
			 
;; 			 (set-face-attribute 'neo-root-dir-face nil
;; 					     :foreground "#000000"
;; 					     :weight 'bold)))
;;            ("C-c td" . (lambda ()
;; 			 (interactive)
;; 			 (load-theme 'almost-mono-gray)
;; 			 (set-face-attribute 'neo-dir-link-face nil
;; 					     :foreground "#ffffff")

;; 			 (set-face-attribute 'neo-file-link-face nil
;; 					     :foreground "#a7bca4")

;; 			 (set-face-attribute 'neo-root-dir-face nil
;; 					     :foreground "#ffffff"
;; 					     :weight 'bold))))

;; (use-package base16-theme
;;   :ensure t
;;   :config
;;   (load-theme 'base16-ocean t))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(provide 'module-theme)

;;; module-theme.el ends here
