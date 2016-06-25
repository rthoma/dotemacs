;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-07-ivy.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ivy setup
;;
(use-package ivy
  ;;
  ;; Package preferences
  ;;
  :ensure t
  :pin gnu
  ;;
  ;; Pre-load initialization
  ;;
  :init
  (progn
    (setq ivy-height 6
          ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d) ")
    (bind-key "C-c C-f" 'ivy-recentf))
  ;;
  ;; After load configuration
  ;;
  :config
  (progn
    (use-package swiper
      :ensure t
      :pin melpa-stable
      :init
      (bind-key "C-s" 'swiper))
      
    (use-package counsel 
      :ensure t
      :pin melpa-stable
      :init
      (progn 
        (bind-key "M-x" 'counsel-M-x)
        (bind-key "C-x C-f" 'counsel-find-file)
        (bind-key "C-c g" 'counsel-git)
        (bind-key "C-c j" 'counsel-git-grep)
        (bind-key "C-c k" 'counsel-ag)
        (bind-key "C-x l" 'counsel-locate)
        (bind-key "C-S-o" 'counsel-rhythmbox)
        (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

    (when (fboundp 'ivy-mode) (ivy-mode 1))))

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
