(defun efs/run-in-background (command) 
  (let ((command-parts (split-string command "[ ]+"))) 
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 4)

  ;; Open eshell by default
  (eshell)

  ;; Show battery status in the mode line
  (display-battery-mode 1)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t) 
  (setq display-time-format "%a %d %b %H:%M") 
  (display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Launch apps that will run in the background
  ;; Network manager
  ;;(efs/run-in-background "nm-applet")
  ;; Pulse Audio
  ;;(efs/run-in-background "pasystray")
  )

(defun efs/exwm-update-class () 
        (exwm-workspace-rename-buffer exwm-class-name))

(defun clover/exwm-update-title () 
        (exwm-workspace-rename-buffer (concat exwm-class-name ": " exwm-title)))

(defun clover/exwm-lock-screen ()
        (interactive)
        (start-process-shell-command "lock" nil "slock"))

(use-package 
        exwm 
        :config
        ;; Set the default number of workspaces
        (setq exwm-workspace-number 10)

        ;; When window "class" updates, use it to set the buffer name
        ;; (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)
        (add-hook 'exwm-update-title-hook #'clover/exwm-update-title)

        ;; When EXWM starts up, do some extra confifuration
        (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

        ;; Rebind CapsLock to Ctrl
        (start-process-shell-command "xmodmap" nil "xmodmap ~/.Xmodmap")

        ;; Set the screen resolution (update this to be the correct resolution for your screen!)
        (require 'exwm-randr) 
        (exwm-randr-enable) 
        (start-process-shell-command "xrandr" nil
                "xrandr --output DVI-D-0 --off --output HDMI-0 --off --output DP-0 --mode 1920x1200 --pos 0x480 --rotate normal --output DP-1 --off --output DP-2 --primary --mode 3840x2160 --pos 1920x0 --rotate normal --output DP-3 --off --output DP-4 --mode 1920x1200 --pos 5760x480 --rotate normal --output DP-5 --off") 
        (setq exwm-randr-workspace-monitor-plist '(1 "DP-0" 2 "DP-0" 3 "DP-0" 7 "DP-4" 8 "DP-4" 9
                                                          "DP-4"))
        ;;exwm-workspace-warp-cursor t)
        ;;mouse-autoselect-window t)


        ;; Load the system tray before exwm-init
        ;; (require 'exwm-systemtray)
        ;; (exwm-systemtray-enable)

        ;; Allow buffers on all workspaces
        (setq exwm-workspace-show-all-buffers 1)
        (setq exwm-layout-show-all-buffers t)

        ;; These keys should always pass through to Emacs
        (setq exwm-input-prefix-keys '(?\C-x ?\C-u ?\C-h ?\M-x ?\M-`?\M-& ?\M-: ?\C-\M-j ;; Buffer list
                                              ?\C-\ ;; Ctrl+Spacep
                                              ?\M-o ;; Other window
                                              ?\s-j ?\s-l))t

        ;; Ctrl+Q will enable the next key to be sent directly
        (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

        ;; Set up global key bindings.  These always work, no matter the input state!
        ;; Keep in mind that changing this list after EXWM initializes has no effect.
        (setq exwm-input-global-keys `(
                                              ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
                                              ([?\s-r] . exwm-reset)

                                              ;; Launch applications via shell command
                                              ([?\s-&] . (lambda (command) 
                                                                 (interactive (list
                                                                                      (read-shell-command
                                                                                              "$
"))) 
                                                                 (start-process-shell-command
                                                                         command nil command)))

                                              ;; Switch workspace
                                              ([?\s-w] . exwm-workspace-switch) 
                                              ([?\s-`] . (lambda () 
                                                                 (interactive) 
                                                                 (exwm-workspace-switch-create 0)))

                                              ;; Toggle char line mode
                                              ([?\s-k] . exwm-input-toggle-keyboard)
                                              ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
                                              ,@(mapcar (lambda (i) 
                                                                `(,(kbd (format "s-%d" i)) . (lambda
                                                                                                     () 
                                                                                                     (interactive) 
                                                                                                     (exwm-workspace-switch-create
                                                                                                             ,i)))) 
                                                        (number-sequence 0 9)))) 
        (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app) 
        (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)

        (exwm-input-set-key (kbd "s-l l") 'clover/exwm-lock-screen)
        (exwm-enable))
