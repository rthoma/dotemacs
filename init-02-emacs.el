;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-02-emacs.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(setq ns-command-modifier 'control
      ns-option-modifier 'meta)

;; Swap alt and control using SharpKeys on Windows
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-alt-to-system nil)

;; Answer 'y' rather than 'yes'
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous package setup
;;
(use-package time
  :init
  (setq display-time-24hr-format t
        display-time-default-load-average nil)
  :config
  (display-time))

(use-package whitespace
  :diminish whitespace-mode
  :bind ("C-c s w" . whitespace-mode)
  :init
  (progn
    (setq whitespace-line-column 79)
    (add-hook 'before-save-hook #'delete-trailing-whitespace)))

(use-package exec-path-from-shell
  :ensure t
  :pin melpa-stable
  :defer 2
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package server ;; Start Emacs server (i.e., emacs --daemon)
  :defer 3
  :config
  (when (fboundp 'server-running-p)
    (unless (server-running-p)
      (server-start))))

;; eof
