;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-05-org.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;; Last edited: June 19, 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org setup
;;
(use-package org
  ;;
  ;; Package preferences
  ;;
  :ensure t
  :pin gnu
  :defer t
  ;;
  ;; Preload initialization
  ;;
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb))
  ;;
  :init
  (progn
    (setq org-directory "~/.emacs.d/orgfiles/"
          org-archive-location "~/.emacs.d/orgfiles/archive/")
    (setq org-todo-keywords
          '((sequence "TODO" "DONE" "PROG" "WAIT" "CANC")))
    (setq org-todo-keyword-faces
          '(("TODO" . org-todo)
            ("WAIT" . (:foreground "#F2E1AC" :weight bold))
            ("PROG" . (:foreground "#74d3d5" :weight bold))
            ("CANC" . org-todo)
            ("DONE" . org-done)))
    (setq user-full-name "rthoma")
    (setq org-src-preserve-indentation t))
  ;;
  ;; After load configuration
  ;;
  ;; :config
  ;; (progn ;; config stuff)
  )

;; eof
