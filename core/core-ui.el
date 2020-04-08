(use-package rainbow-delimiters)
(use-package vscode-icon)
(add-hook 'prog-mode-hook 'linum-mode)

(setq ring-bell-function 'ignore)
;; (load-theme 'spacegray t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(use-package ace-window)

(provide 'core-ui)
