;;; module-theme.el --- Adam Greloch's module-theme.el File For GNU Emacs

;; Copyright (C) 2020 Adam Greloch

;; Author: Adam Greloch <zplhatesbananas@gmail.com>
;; Version: 20200420
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

(use-package rainbow-mode)

(setq dashboard-startup-banner '1)

;; User-definable colors for light and dark theme.
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
		    :inherit nil)

;; TODO: examine why this is perceived as incorrect face
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
					     :inherit nil)))
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
					     :inherit nil))))

(provide 'module-theme)

;;; module-theme.el ends here
