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
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(org-agenda-files
   (quote
    ("~/Dysk Google/org/casual.org" "~/Dysk Google/org/school.org")))
 '(package-selected-packages
   (quote
    (spacegray-theme ibuffer-projectile rainbow-delimiters linum-relative spacemacs-theme ibuffer-sidebar ace-window all-the-icons dashboard vscode-icon dired-sidebar darkroom use-package markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :height 120)))))

;; (load-theme 'spacemacs-dark t)
(load-theme 'spacegray t)

(global-hl-line-mode 1)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

(defun my-markdown-mode-hook ()
  (visual-line-mode 1)
  (darkroom-tentative-mode 1))
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq ring-bell-function 'ignore)

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-face `(:family "Consolas" :height 120))
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font  t))

(use-package ibuffer-projectile
  :ensure t
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
  (setq ibuffer-sidebar-use-custom-font t)
  (setq ibuffer-sidebar-face `(:family "Consolas" :height 120)))

(defun sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

(global-set-key (kbd "C-x C-n") 'sidebar-toggle)

(setq default-directory "d:/git/" )

(use-package all-the-icons)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x C-a") 'org-agenda)

(add-hook 'js-mode-hook #'rainbow-delimiters-mode)

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'super)

(toggle-scroll-bar -1)

;; stop creating those #auto-save# files
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
