;;; init.el --- Adam Greloch's init.el File For GNU Emacs

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

;; ===========================================================================
;; Simple Settings and Internal Packages
;; ===========================================================================

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
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))

(package-initialize)

(require 'server)
(or (server-running-p)
     (server-start))

;; ===========================================================================
;; Module loading
;; ===========================================================================

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

(defvar emacs-dir (file-name-directory "~/.emacs.d/")
  "The root dir of the Emacs distribution.")

(defvar core-dir (expand-file-name "core" emacs-dir)
  "The home of core functionality.")

(defvar modules-dir (expand-file-name "modules" emacs-dir)
  "This directory houses all of the modules.")

(defvar machines-dir (expand-file-name "machines" emacs-dir)
  "This directory stores my individual settings for different OS-es.")

(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path machines-dir)

(require 'core-packages)
(require 'core-settings)
(require 'core-ui)
(require 'module-org)
(require 'module-smart-compile)
(require 'module-powershell6)
(require 'module-latex)
(require 'module-markdown-mode)
(require 'module-web-mode)
(require 'module-theme)
(require 'windows-settings)

;;; init.el ends here
