(add-to-list 'load-path "~/.emacs.d/vendor/use-package")
(require 'use-package)

;; this needs to be loaded before evil
(use-package undo-tree
  :load-path "vendor/undo-tree")

(use-package evil
  :load-path "vendor/evil"

  :init
  (setf evil-want-C-u-scroll t)
  (setf evil-want-fine-undo t)

  :config
  ;; enable evil mode
  (evil-mode 1)

  ;; make bindings more vim-like
  (defun minibuffer-keyboard-quit ()
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setf deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
  (global-set-key (kbd "RET") 'newline-and-indent)

  ;; key bindings
  (evil-ex-define-cmd "!" 'shell-command)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map ";" 'buffer-menu)

  ;; proof mode bindings
  (evil-define-key 'normal proof-mode-map
    (kbd "<right>") 'proof-goto-point
    (kbd "<up>") 'proof-undo-last-successful-command
    (kbd "<down>") 'proof-assert-next-command-interactive
    (kbd "<left>") 'proof-goto-end-of-locked)
  (evil-define-key 'insert proof-mode-map
    (kbd "<right>") 'proof-goto-point
    (kbd "<up>") 'proof-undo-last-successful-command
    (kbd "<down>") 'proof-assert-next-command-interactive
    (kbd "<left>") 'proof-goto-end-of-locked))

(use-package proof-site
  :load-path "vendor/PG/generic"

  :config
  ;; colors and display
  (if (display-graphic-p)
      ;; GUI
      ; red: #fec1c2, green: #e1fec1, blue: #c1fefc
      (custom-set-faces
       '(proof-locked-face ((t (:background "#e1fec1"))))
       '(proof-queue-face ((t (:background "#c1fefc")))))
      ;; CLI
      (custom-set-faces
       '(proof-locked-face ((t (:background "#262626")))) ; terminal
       '(proof-queue-face ((t (:background "#00005f"))))))

  (add-hook 'proof-mode-hook (lambda ()
                               (setq-local global-hl-line-mode
                                           (null global-hl-line-mode))))
  (add-hook 'coq-mode-hook #'company-coq-mode)

  (setf proof-colour-locked t)
  (setf overlay-arrow-string "")
  (setf proof-splash-enable nil)

  ;; Coq specific
  (setf coq-compile-before-require t)
  (setf coq-one-command-per-line nil)) ; results in better behavior for electric terminator

(use-package editorconfig
  :load-path "vendor/editorconfig-emacs"

  :config
  (editorconfig-mode 1))

;; this needs to be loaded before fiplr
(use-package grizzl
  :load-path "vendor/grizzl")

;; this needs to be loaded after evil
(use-package fiplr
  :load-path "vendor/fiplr"

  :config
  (define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)
  ;; the fiplr-keymap may not be part of the public API, but this seems to work
  ;; for now
  (define-key *fiplr-keymap* (kbd "<f5>") 'fiplr-reload-list))

;; this needs to be loaded after evil
(use-package neotree
  :load-path "vendor/emacs-neotree"

  :config
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter))))

;; this needs to be loaded after evil and neotree
(use-package evil-leader
  :load-path "vendor/evil-leader"

  :config
  (global-evil-leader-mode)
  (evil-leader/set-key
    "e" 'proof-electric-terminator-toggle
    "c" 'proof-interrupt-process
    "m" 'menu-bar-open
    "n" 'neotree-toggle
    "f" 'neotree-find))

(use-package company
  :load-path "vendor/company-mode"

  :config
  (global-company-mode))

(use-package math-symbol-lists
  :load-path "vendor/math-symbol-lists")

;; this needs to be loaded after company and math-symbol-lists
(use-package company-math
  :load-path "vendor/company-math")

(use-package yasnippet
  :load-path "vendor/yasnippet")

;; this needs to be loaded after company-math and yasnippet
(use-package company-coq
  :load-path "vendor/company-coq"

  :config
  (setf company-coq-disabled-features
        '(unicode-math-backend prettify-symbols smart-subscripts)))

(use-package linum-relative
  :load-path "vendor/linum-relative"

  :init
  (global-linum-mode t)
  ;; add padding next to line number
  (setf linum-format
        (lambda (line)
          (propertize
           (format
            (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
              (concat "%" (number-to-string w) "d "))
            line)
           'face
           'linum)))

  :config
  (linum-relative-on)
  (setf linum-relative-format "%3s "
        linum-relative-current-symbol ""))
