;;; module-powershell6.el --- Adam Greloch's module-powershell6.el File For GNU Emacs

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

;; ===========================================================================
;; inferior PowerShell 6 and `shell-pop' configuration
;; ===========================================================================

(defun powershell6 (&optional buffer)
  "Launches a PowerShell 6 instance in buffer *powershell6* and switches to it."
  (interactive)
  (let ((buffer (or buffer "*powershell6*"))
	(powershell6-prog "C:\\Program Files\\PowerShell\\6\\pwsh.exe"))
    (make-comint-in-buffer "shell" "*powershell6*" powershell6-prog)
    (switch-to-buffer buffer)))

;; make comint-derived modes' like `shell' output and prompt read-only
(setq comint-prompt-read-only t)

(defun my-comint-preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))

(add-hook 'comint-preoutput-filter-functions 'my-comint-preoutput-turn-buffer-read-only)

(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
(setq default-process-coding-system '(utf-8 . utf-8))

(use-package shell-pop)

(global-set-key (kbd "C-`") 'shell-pop)

(provide 'module-powershell6)

;;; module-powershell6.el ends here
