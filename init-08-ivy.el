;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-08-ivy.el
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
  :pin melpa-stable
  ;;
  ;; Pre-load initialization
  ;;
  :init
  (progn
    (setq ivy-height 6
          ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d) "))
  ;;
  ;; After load configuration
  ;;
  :config
  (progn
    (when (fboundp 'ivy-mode) (ivy-mode 1))

    (bind-keys :map ivy-mode-map
               ("C-c C-f" . ivy-recentf))

    (use-package swiper
      :ensure t
      :pin melpa-stable
      :init
      :config
      (bind-keys :map ivy-mode-map
                 ("C-s" . swiper)))

    (use-package counsel
      :ensure t
      :pin melpa-stable
      :init
      :config
      (progn
        (when (fboundp 'counsel-mode) (counsel-mode 1))

        (bind-keys :map counsel-mode-map
                   ("M-x" . counsel-M-x)
                   ("C-x C-f" . counsel-find-file)
                   ("C-c g" . counsel-git)
                   ("C-c j" . counsel-git-grep)
                   ("C-c k" . counsel-ag)
                   ("C-x l" . counsel-locate)
                   ("C-S-o" . counsel-rhythmbox))

        (bind-keys :map read-expression-map
                   ("C-r" . counsel-expression-history))))))

;; eof
