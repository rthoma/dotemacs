;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-02-emacs.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;; Last edited: June 17, 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic Emacs setup
;;
;; Maximize window on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Font setup
(add-to-list 'default-frame-alist '(font . "Bitstream Vera Sans Mono-12"))
(set-frame-font "Bitstream Vera Sans Mono-12" nil t)

;; Winner mode for switching between window layouts
(when (fboundp 'winner-mode) (winner-mode 1))

;; Show row and column number
(setq line-number-mode t
      column-number-mode t)

;; Bind meta to the Mac command key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super)

;; Configure backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 4
      version-control t)

;; Answer "y" rather than "yes"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Custom visual bell function
(defun rthoma/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

;; Redefine ring-bell-function
(setq visible-bell nil)
(setq ring-bell-function #'rthoma/terminal-visible-bell)

;; Prevent excessive garbage collection in the mini-buffer
(defun rthoma/minibuffer-setup-hook ()
  (setq gc-cons-threshold 10000000)) ;; 10 MB

(defun rthoma/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000)) ;; 800 kB

(add-hook 'minibuffer-setup-hook #'rthoma/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'rthoma/minibuffer-exit-hook)

;; eof
