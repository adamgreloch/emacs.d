;;; core-ui.el --- Adam Greloch's core-ui.el File For GNU Emacs

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
;; Core UI configuration
;; ===========================================================================

(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; went with modifying frame parameters to disable scroll-bars when daemon is
;; running
;; (toggle-scroll-bar -1)

(defun my-disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my-disable-scroll-bars)

(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines nil)

;; ===========================================================================
;; Navigation
;; ===========================================================================

(setq window-divider-mode 1)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;; set *dashboard* as initial buffer when daemon is already running
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package ace-window)

(use-package neotree
  :config
  (setq neo-theme 'arrow
	neo-smart-open t))

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

;; ===========================================================================
;; Shortcuts
;; ===========================================================================

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x C-n") 'neotree-toggle)
(global-set-key (kbd "C-x C-m") 'ibuffer-sidebar-toggle-sidebar)

(provide 'core-ui)

;;; core-ui.el ends here
