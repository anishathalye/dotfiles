(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-color-theme-solarized")
(setf solarized-termcolors 256)
(load-theme 'solarized t)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))
