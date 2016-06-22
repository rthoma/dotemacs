;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-07-ivy.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;; Last edited: June 19, 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ivy setup
;;
(use-package ivy
  :ensure t
  :pin gnu
  :init
  (progn
    (setq ivy-height 6
          ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d) ")
    (bind-key "C-c r f" 'ivy-recentf))
  :config
  (when (fboundp 'ivy-mode) (ivy-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Time setup
;;
(use-package time ;; not deferred
  :init
  (setq display-time-24hr-format t
        display-time-default-load-average nil)
  :config
  (display-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Whitespace mode setup
;;
(use-package whitespace ;; deferred
  :bind ("C-c s w" . whitespace-mode)
  :config (setq whitespace-line-column nil)
  :diminish whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; zenburn setup
;;
(use-package zenburn-theme ;; not deferred
  :ensure t
  :pin melpa-stable
  :init
  (progn
    (setq custom-enabled-themes 'zenburn)
    (setq custom-safe-themes
      (quote
    ("afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8" default)
  ))))

;; eof
