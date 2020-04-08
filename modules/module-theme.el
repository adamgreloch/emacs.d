(setq dashboard-startup-banner '1)

(use-package almost-mono-themes
  :config
  ;; (load-theme 'almost-mono-black t)
  (load-theme 'almost-mono-gray t))

(custom-set-faces
 '(neo-root-dir-face ((t (:foreground "#ffffff"
				      :weight bold))))
 '(neo-dir-link-face ((t (:foreground "#ffffff"))))
 '(neo-file-link-face ((t (:foreground "#a7bca4")))))

;; (set-face-attribute 'mode-line nil
;;                     :background "#353236"
;;                     :foreground "#657b83"
;;                     :box '(:line-width 4 :colOr "#353236")
;;                     :overline nil
;;                     :underline nil)

;; (set-face-attribute 'mode-line-inactive nil
;;                     :background "#1f2024"
;;                     :foreground "#727072"
;;                     :box '(:line-width 4 :color "#353236")
;;                     :overline nil
;;                     :underline nil)

;; (set-face-attribute 'fringe nil
;; 		    :background "#282b33")

;; (set-face-attribute 'window-divider nil
;; 		    :foreground "#1f2024")

(provide 'module-theme)

;;; module-theme.el ends here
