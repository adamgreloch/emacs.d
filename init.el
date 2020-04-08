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
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(org-agenda-files
   (quote
    ("~/Dysk Google/org/casual.org" "~/Dysk Google/org/school.org")))
 '(package-selected-packages
   (quote
    (magit add-node-modules-path flycheck web-mode neotree spacegray-theme ibuffer-projectile rainbow-delimiters linum-relative spacemacs-theme ibuffer-sidebar ace-window all-the-icons dashboard vscode-icon dired-sidebar darkroom use-package markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :height 130))))
 '(neo-dir-link-face ((t (:foreground "#ffffff"))))
 '(neo-file-link-face ((t (:foreground "#a7bca4"))))
 '(neo-root-dir-face ((t (:foreground "#ffffff" :weight bold)))))

(setq line-spacing 0.15)

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

(require 'core-ui)
(require 'module-theme)
(require 'module-web-mode)

(use-package magit)
(use-package markdown-mode)
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

(defun my-markdown-mode-hook ()
  (visual-line-mode 1)
  (darkroom-tentative-mode 1))
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

(use-package neotree
  :config
  (setq neo-theme 'arrow
	neo-smart-open t))

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
  (setq ibuffer-projectile-prefix "Project: "))

(use-package ibuffer-sidebar
  :ensure nil
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font 't
	ibuffer-sidebar-face '(:family "Consolas" :height 120)))

;; toggle neotree and ibuffer-sidebar simultaneously
(defun sidebar-toggle ()
  (interactive)
  (neotree-toggle)
  (ibuffer-sidebar-toggle-sidebar))

(global-set-key (kbd "C-x C-n") 'sidebar-toggle)

(setq default-directory "d:/git/" )

(use-package all-the-icons)

(setq window-divider-mode 1)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x C-a") 'org-agenda)

(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
(setq default-process-coding-system '(utf-8 . utf-8))

(use-package shell-pop)

(global-set-key (kbd "C-`") 'shell-pop)

(add-hook 'js-mode-hook #'rainbow-delimiters-mode)

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'super)

;; stop creating those #auto-save# files
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
