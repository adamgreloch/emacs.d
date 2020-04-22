 
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
    ("1dacaddeba04ac1d1a2c6c8100952283b63c4b5279f3d58fb76a4f5dd8936a2c" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "bffb799032a7404b33e431e6a1c46dc0ca62f54fdd20744a35a57c3f78586646" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" default)))
 '(display-battery-mode nil)
 '(display-line-numbers-width nil)
 '(elcord-display-buffer-details nil)
 '(elcord-use-major-mode-as-main-icon t)
 '(fringe-mode 6 nil (fringe))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(neo-theme (quote ascii))
 '(neo-window-fixed-size nil)
 '(neo-window-width 30)
 '(org-agenda-files
   (quote
    ("~/.emacs.d/configuration.org" "~/Dropbox/org/casual.org" "~/Dropbox/org/school.org")))
 '(org-journal-file-format "%Y%m%d.org")
 '(package-selected-packages
   (quote
    (org-journal smex org-plus-contrib org-mode elcord rainbow-mode ido-vertical-mode ivy diminish flx-ido base16-theme which-key awesome-tab rjsx-mode company shell-pop auctex magit add-node-modules-path flycheck neotree ibuffer-projectile rainbow-delimiters ibuffer-sidebar ace-window dashboard dired-sidebar darkroom use-package markdown-mode)))
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
 '(default ((t (:family "Meslo LG S" :height 110 :weight normal :spacing 90))))
 '(fringe ((t (:background "nil")))))

;;; .emacs-custom.el ends here
