;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; init-05-org.el
;; Emacs, Version 25.1.50 (9.0)
;; OS X Yosemite, Version 10.10.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org mode setup
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
  ;;
  ;; After load configuration
  ;;
  :config
  (progn
    (setq org-directory "~/.emacs.d/orgfiles/"
	  org-archive-location "~/.emacs.d/orgfiles/archive/")

    ;; todo keywords
    (setq org-todo-keywords
	  '((sequence "TODO" "PROG" "WAIT" "CANC" "DONE")))
    (setq org-todo-keyword-faces
	  '(("TODO" . org-todo)
	    ("PROG" . (:foreground "#74D3D5" :weight bold)) 
	    ("WAIT" . (:foreground "#F2E1AC" :weight bold))
	    ("CANC" . org-todo)
	    ("DONE" . org-done)))

    (setq user-full-name "rthoma")
    (setq org-src-preserve-indentation t)
    (setq org-log-done 'time)

    ;; key bindings
    (bind-keys :map org-mode-map
               ("C-c l" . org-store-link)
               ("C-c a" . org-agenda)
               ("C-c c" . org-capture)
               ("C-c b" . org-iswitchb))))

;; eof
