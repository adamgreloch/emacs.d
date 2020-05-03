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
     (output-pdf "Atril")
     (output-html "start"))))
 '(ansi-term-color-vector
   [unspecified "#061229" "#d07346" "#99bf52" "#fbd461" "#5299bf" "#9989cc" "#5299bf" "#b8bbc2"] t)
 '(blink-cursor-mode t)
 '(custom-safe-themes
   (quote
    ("939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" default)))
 '(display-line-numbers-width nil)
 '(elcord-display-buffer-details nil)
 '(elcord-display-elapsed t)
 '(elcord-use-major-mode-as-main-icon t)
 '(fringe-mode 6 nil (fringe))
 '(inhibit-startup-screen t)
 '(line-spacing 0.15)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(neo-theme (quote ascii))
 '(neo-window-fixed-size nil)
 '(neo-window-width 30)
 '(org-agenda-files
   (quote
    ("~/.emacs.d/configuration.org" "~/Dropbox/org/school.org" "~/Dropbox/org/freetime.org")))
 '(org-journal-file-format "%Y%m%d.org")
 '(package-selected-packages
   (quote
    (zenburn-theme graphql-mode smart-compile org-cliplink org-clip-link smartparens company-quickhelp realgud meghanada google-c-style autodisass-java-bytecode activity-watch-mode esup helm ido-grid-mode vmd-mode org-journal smex org-plus-contrib org-mode elcord rainbow-mode ido-vertical-mode ivy diminish flx-ido base16-theme which-key awesome-tab rjsx-mode company shell-pop auctex magit add-node-modules-path flycheck neotree ibuffer-projectile rainbow-delimiters ibuffer-sidebar ace-window dashboard dired-sidebar darkroom use-package markdown-mode)))
 '(shell-pop-autocd-to-working-dir nil)
 '(shell-pop-window-position "right"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 110 :weight normal))))
 '(fringe ((t (:background "nil")))))

;;; .emacs-custom.el ends here
