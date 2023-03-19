;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Peng Ye"
      user-mail-address "yemouren@protonmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq! doom-font (font-spec :family "FiraCode Nerd Font" :size 15  :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font" :size 15)
      doom-unicode-font (font-spec :family "FiraCode Nerd Font" :size 15))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; (setq default-frame-alist
;;       (append (list
;; 	       ;; '(font . "Roboto Mono Emacs Regular:size=14")
;; 	       '(min-height . 1)  '(height     . 30)
;; 	       '(min-width  . 1) '(width      . 60)
;;                '(vertical-scroll-bars . nil)
;;                '(internal-border-width . 35)
;;                '(left-fringe    . 0)
;;                '(right-fringe   . 0)
;;                '(tool-bar-lines . 0)
;;                '(menu-bar-lines . 0))))

;; (add-hook! 'solaire-mode-hook (set-face-background 'internal-border (face-background 'fringe)))
;; (set-frame-parameter nil 'internal-border-width 60)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq gc-cons-threshold (* 50 1000 1000))
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(display-time-mode t)
(display-battery-mode 1)
(doom/set-frame-opacity '90)
(setq initial-major-mode 'org-mode)
(setq browse-url-handlers '(("\\`file:" . browse-url-default-browser)))

;; Profile emacs startup
(setq confirm-kill-emacs nil
      confirm-kill-processes nil)

(after! centaur-tabs
  (setq centaur-tabs-style "wave"))


(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(use-package! whitespace
  :hook (after-init . global-whitespace-mode) ;; 注意，这里是全局打开
  :config
  ;; Don't use different background for tabs.
  (face-spec-set 'whitespace-tab
                 '((t :background unspecified)))
  ;; Only use background and underline for long lines, so we can still have
  ;; syntax highlight.

  ;; For some reason use face-defface-spec as spec-type doesn't work.  My guess
  ;; is it's due to the variables with the same name as the faces in
  ;; whitespace.el.  Anyway, we have to manually set some attribute to
  ;; unspecified here.
  (face-spec-set 'whitespace-line
                 '((((background light))
                    :background "#d8d8d8" :foreground unspecified
                    :underline t :weight unspecified)
                   (t
                    :background "#404040" :foreground unspecified
                    :underline t :weight unspecified)))

  ;; Use softer visual cue for space before tabs.
  (face-spec-set 'whitespace-space-before-tab
                 '((((background light))
                    :background "#d8d8d8" :foreground "#de4da1")
                   (t
                    :inherit warning
                    :background "#404040" :foreground "#ee6aa7")))

  (setq
   whitespace-line-column nil
   whitespace-style
   '(face             ; visualize things below:
     empty            ; empty lines at beginning/end of buffer
     lines-tail       ; lines go beyond `fill-column'
     space-before-tab ; spaces before tab
     trailing         ; trailing blanks
     tabs             ; tabs (show by face)
     tab-mark         ; tabs (show by symbol)
     )))

(after! evil
  (defalias 'evil-insert-state 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
  (setq evil-emacs-state-cursor 'bar))

(set-eshell-alias!
 "yt" "youtube-dl $*"
 "yta" "youtube-dl -x -f bestaudio/best $*"
 "gcl" "git clone --depth=1 $*"
 "open" "xdg-open $*"
 "xo" "xdg-open $*"
 "g" "git --no-pager $*"
 "c" "clear-scrollback"
 )

;; EXWM config
(defun shutdown ()
  (interactive)
  (shell-command "sudo shutdown -h now"))

(defun reboot ()
  (interactive)
  (shell-command "sudo reboot"))

(defun logout ()
  (interactive)
  (kill-emacs))

(defun display-off ()
  (interactive)
  (shell-command "xset dpms force off"))

(defun lock-screen ()
  "Lock screen using (zone) and xtrlock
 calls M-x zone on all frames and runs xtrlock"
  (interactive)
  (save-excursion
    (set-process-sentinel
     (start-process "xtrlock" nil "xtrlock")
     '(lambda (process event)
        (zone-leave-me-alone)))
    (zone-when-idle 1)))

(defun screenshot-full ()
  (interactive)
  (shell-command "scrot ~/Pictures/screenshot/pic-$(date '+%y%m%d-%H%M-%S').png"))

(defun screenshot-current-window ()
  (interactive)
  (shell-command "scrot -f ~/Pictures/screenshot/pic-$(date '+%y%m%d-%H%M-%S').png"))

(defun screenshot-select ()
  (interactive)
  (shell-command "scrot --select ~/Pictures/screenshot/pic-$(date '+%y%m%d-%H%M-%S').png"))

(defun screenshot-clip
  (interactive)
  (shell-command "scrot -e 'xclip -selection clipboard -t image/png -i $f' -s")
  )
(use-package! wallpaper
  :hook ((exwm-randr-screen-change . wallpaper-set-wallpaper)
         (after-init . wallpaper-cycle-mode))
  :custom ((wallpaper-cycle-single t)
           (wallpaper-scaling 'scale)
           (wallpaper-cycle-interval 450)
           (wallpaper-cycle-directory "~/Pictures/Wallpaper"))
  :config
  (unless (executable-find "feh")
    (display-warning 'wallpaper "External command `feh' not found!")))
(defvar exwm--toggle-workspace 0
  "Previously selected workspace. Used with `exwm/jump-to-last-exwm'.")

(defun exwm/jump-to-last-exwm ()
  "Jump to last window."
  (interactive)
  (exwm-workspace-switch exwm--toggle-workspace))

(defadvice exwm-workspace-switch
    (before save-toggle-workspace activate)
  (setq exwm--toggle-workspace exwm-workspace-current-index))

(defun yeh/exwm-input-line-mode ()
  "Set exwm window to line-mode and show mode line."
  (call-interactively #'exwm-input-grab-keyboard)
  (exwm-layout-show-mode-line))

(defun yeh/exwm-input-char-mode ()
  "Set exwm window to char-mode and hide mode line."
  (call-interactively #'exwm-input-release-keyboard)
  (exwm-layout-hide-mode-line))

(defun yeh/exwm-input-toggle-mode ()
  "Toggle between line- and char-mode."
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (second (second mode-line-process)) "line")
          (yeh/exwm-input-char-mode)
        (yeh/exwm-input-line-mode)))))

(use-package! xelb
  :if (display-graphic-p))

(use-package! exwm
  :if (display-graphic-p)
  :init
  (setq exwm-workspace-number 5)
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  (setq window-divider-default-right-width 1)
  :config
  (require 'exwm-config)
  (setq exwm-workspace-index-map
        (lambda (index) (number-to-string (1+ index))))
  (progn
    (exwm-input-set-key (kbd "<s-tab>")  #'exwm/jump-to-last-exwm)
    (exwm-input-set-key (kbd "s-w")  #'(lambda ()
                                         (interactive)
                                         (start-process-shell-command
                                          "Brave-browser" nil "braven")))
    (exwm-input-set-key (kbd "s-d") #'(lambda (command)
                                        (interactive (list (read-shell-command "Command: ")))
                                        (start-process-shell-command command nil command)))
    (exwm-input-set-key (kbd "s-=") #'desktop-environment-volume-increment)
    (exwm-input-set-key (kbd "s--") #'desktop-environment-volume-decrement)
    (exwm-input-set-key (kbd "s-i") #'yeh/exwm-input-toggle-mode)
    (mapcar (lambda (i)
              (exwm-input-set-key (kbd (format "s-%d" i))
                                  #'(lambda ()
                                      (interactive)
                                      (exwm-workspace-switch-create (1- i)))))
            (number-sequence 0 9)))

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (add-hook 'exwm-manage-finish-hook
            (lambda () (call-interactively #'exwm-input-release-keyboard)
              (exwm-layout-hide-mode-line)))

  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (setq floating-mode-line nil)))
  ;; Make buffer name more meaningful
  (add-hook 'exwm-update-class-hook
            (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-title)))
  (exwm-enable))

(use-package! exwm-systemtray
  :if (display-graphic-p)
  :after exwm
  :config
  (exwm-systemtray-enable))

(use-package! desktop-environment
  :if (display-graphic-p)
  :after exwm
  :init
  (setq desktop-environment-screenshot-directory "~/Pictures/screenshot"
        desktop-environment-update-exwm-global-keys :global)
  :config
  (desktop-environment-mode))

(use-package! xdg
  :if (display-graphic-p)
  :commands (xdg-config-dirs xdg-config-home xdg-desktop-read-file))

(require 'exwm)
(require 'exwm-config)
(exwm-enable)
(require 'exwm-systemtray)
(exwm-systemtray-enable)
