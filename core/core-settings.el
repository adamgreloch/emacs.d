;;; core-settings.el --- Adam Greloch's core-settings.el File For GNU Emacs

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
;; Miscellaneous but important settings and fixes
;; ===========================================================================

(set-language-environment "UTF-8")

;; stop creating those #auto-save# files
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; turn off graphical tooltips
(tooltip-mode -1)
 
;; rebind menu key as super
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'super)

;; TODO find a way to make this variable happy on seperate machines (on my
;; PC i've got my main git folder located under d:/ drive while on laptop it's
;; under c:/)
(setq default-directory "c:/git/" )

;; replace selected text automatically when typing
(delete-selection-mode 1)

(provide 'core-settings)

;;; core-settings.el ends here
