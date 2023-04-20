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
(setq! doom-font (font-spec :family "MesloLGM Nerd Font"
                            :size 16
                            :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font"
                                          :size 16)
      doom-unicode-font (font-spec :family "FiraCode Nerd Font" :size 16))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq fancy-splash-image "~/.config/doom/banner.jpg")
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/"
      org-ellipsis " ▾ "
      )
(setq org-html-head-extra "<link rel=\"stylesheet\" href=\"https://unpkg.com/marx-css/css/marx.css\" type=\"text/css\">")
(add-hook 'prog-mode-hook #'which-function-mode)
(setq-default mode-line-format (cons '(:eval (which-function-mode)) mode-line-format))
(setq which-func-format (quote (:propertize which-func-current :weight bold)))
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
(setq-default custom-file (expand-file-name ".custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(global-undo-tree-mode t)

(display-time-mode t)
(display-battery-mode 1)
(doom/set-frame-opacity '90)
(setq initial-major-mode 'org-mode)
(setq browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
(setq-default default-input-method "rime")

;; Profile emacs startup
(setq confirm-kill-emacs nil
      confirm-kill-processes nil)

(after! centaur-tabs
  (setq centaur-tabs-style "wave"))


;; set `i' enter emacs-state
(after! evil
  (defalias 'evil-insert-state 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
  (setq evil-emacs-state-cursor 'bar))

(after! ox-hugo
  (setq org-hugo-base-dir "~/Dev/blog")
  (setq org-hugo-auto-set-lastmod t))

(use-package! gptel
 :config
 (setq! gptel-api-key "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")
 (setq! gptel-default-mode 'org-mode))

(map! :leader
      :desc "Zeal at point"
      "d z" #'zeal-at-point)

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up)

  (defun doom-modeline-segment--nov-info ()
    (concat
     " "
     (propertize
      (cdr (assoc 'creator nov-metadata))
      'face 'doom-modeline-project-parent-dir)
     " "
     (cdr (assoc 'title nov-metadata))
     " "
     (propertize
      (format "%d/%d"
              (1+ nov-documents-index)
              (length nov-documents))
      'face 'doom-modeline-info)))

  (advice-add 'nov-render-title :override #'ignore)

  (defun +nov-mode-setup ()
    "Tweak nov-mode to our liking."
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.4
                             :width 'semi-expanded)
    (face-remap-add-relative 'default :height 1.3)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors nil)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 81
                nov-text-width 80)
    (visual-fill-column-mode 1)
    (hl-line-mode -1)
    ;; Re-render with new display settings
    (nov-render-document)
    ;; Look up words with the dictionary.
    (add-to-list '+lookup-definition-functions #'+lookup/dictionary-definition)
    ;; Customise the mode-line to make it more minimal and relevant.
    (setq-local
     mode-line-format
     `((:eval
        (doom-modeline-segment--workspace-name))
       (:eval
        (doom-modeline-segment--window-number))
       (:eval
        (doom-modeline-segment--nov-info))
       ,(propertize
         " %P "
         'face 'doom-modeline-buffer-minor-mode)
       ,(propertize
         " "
         'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
         'display `((space
                     :align-to
                     (- (+ right right-fringe right-margin)
                        ,(* (let ((width (doom-modeline--font-width)))
                              (or (and (= width 1) 1)
                                  (/ width (frame-char-width) 1.0)))
                            (string-width
                             (format-mode-line (cons "" '(:eval (doom-modeline-segment--major-mode))))))))))
       (:eval (doom-modeline-segment--major-mode)))))

  (add-hook 'nov-mode-hook #'+nov-mode-setup))

(setq-default gdb-many-windows t)
(setq-default gdb-show-main t)
(after! dap-mode
  (setq dap-gdb-lldb-command "gdb --interpreter=mi")
  (require 'dap-gdb-lldb)
  ;; (setq dap-gdb-debug-template-path (concat doom-private-dir "templates/gdb-dashboard-debug-template.txt"))
  (setq dap-gdb-auto-create-components nil)
  (setq dap-gdb-debug-program-args-format "-i=mi %e %s")
  (setq dap-gdb-debugged-command #'dap-gdb--spawn))

(defun launch-st-here ()
  "Launch st in the current directory."
  (interactive)
  (let ((default-directory (file-name-directory buffer-file-name)))
    (start-process "st" nil "st")))
(after! company
  (setq company-idle-delay nil))

(setq evil-ex-substitute-global t)

;;; :ui modeline
;; An evil mode indicator is redundant with cursor shape
(advice-add #'doom-modeline-segment--modals :override #'ignore)

(after! org (setq org-hide-emphasis-markers t))
(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t))

;; Enable the auto-change theme feature

;; (use-package! circadian
;;   :config
;;   ;; Set the themes to be used during the day and night
;;   (setq circadian-themes '((:sunrise . doom-one-light)
;;                            (:sunset  . doom-vibrant)))
;;   ;; Activate the auto-change theme feature
;;   (circadian-setup))

;; (use-package! rime
;;   :config
;;   (setq rime-user-data-dir "~/.config/fcitx/rime")
;;   ;; (setq rime-posframe-properties
;;   ;;       (list :background-color "#333333"
;;   ;;             :foreground-color "#dcdccc"
;;   ;;             ;; :font "WenQuanYi Micro Hei Mono-15"
;;   ;;             :internal-border-width 10))
;;   (setq rime-show-candidate 'posframe)
;;   (setq rime-inline-ascii-trigger 'shift-l)
;;   (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
;;   (setq mode-line-mule-info '((:eval (rime-lighter))))
;;   (setq rime-disable-predicates
;;         '(rime-predicate-evil-mode-p
;;           rime-predicate-after-alphabet-char-p
;;           rime-predicate-prog-in-code-p
;;           rime-predicate-after-alphabet-char-p)))
;; (use-package! lsp-ui)

;; (after! vterm
;;   (define-key vterm-mode-map (kbd "<C-backspace>")
;;     (lambda () (interactive) (vterm-send-key (kbd "C-w")))))

;; (use-package! super-save
;;   :config
;;   (super-save-mode +1)
;;   (setq auto-save-default t)
;;   (setq super-save-auto-save-when-idle t)
;;   (add-to-list 'super-save-triggers 'ace-window)
;;   ;; save on find-file
;;   (add-to-list 'super-save-hook-triggers 'find-file-hook))

;; (use-package! nov
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;;   (defun my-nov-font-setup ()
;;   ;; (face-remap-add-relative 'variable-pitch :family ""
;;   ;;                                          :height 1.0))
;; (add-hook 'nov-mode-hook 'my-nov-font-setup)))

;; (use-package! nov-xwidget
;;   :after nov
;;   :config
;;   (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
;;   (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))

;; (use-package! eaf
;;   :load-path "~/.elisp/emacs-application-framework"
;;   :init
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
;;   :config

;;   (defalias 'browse-web #'eaf-open-browser)
;;   (setq eaf-browser-continue-where-left-off t)
;;   ;; (require 'eaf-file-manager)
;;   ;; (require 'eaf-music-player)
;;   ;; (require 'eaf-image-viewer)
;;   ;; (require 'eaf-camera)
;;   ;; (require 'eaf-demo)
;;   ;; (require 'eaf-terminal)
;;   ;; (require 'eaf-video-player)
;;   ;; (require 'eaf-vue-demo)
;;   ;; (require 'eaf-file-sender)
;;   ;; (require 'eaf-mindmap)
;;   ;; (require 'eaf-jupyter)
;;   ;; (require 'eaf-org-previewer)
;;   ;; (require 'eaf-system-monitor)
;;   ;; (require 'eaf-rss-reader)
;;   ;; (require 'eaf-file-browser)
;;   ;; (require 'eaf-mail)
;;   ;; (require 'eaf-git)
;;   (require 'eaf-browser)
;;   (require 'eaf-pdf-viewer)
;;   (require 'eaf-markdown-previewer)
;;   (require 'eaf-org-previewer)
;;   (require 'eaf-airshare)
;;   (require 'eaf-terminal)
;;   (require 'eaf-file-manager)
;;   ;; (require 'eaf-netease-cloud-music)
;;   (require 'eaf-video-player)
;;   (require 'eaf-image-viewer)

;;   (when (display-graphic-p)
;;     (require 'eaf-all-the-icons))

;;   (require 'eaf-evil)
;;   (setq eaf-browser-dark-mode nil)
;;   (setq eaf-terminal-dark-mode nil)
;;   (setq eaf-pdf-dark-mode "ignore") ; see below
;;   (defun adviser-find-file (orig-fn file &rest args)
;;     (let ((fn (if (commandp 'eaf-open) 'eaf-open orig-fn)))
;;     (pcase (file-name-extension file)
;;       ("pdf"  (apply fn file nil))
;;       ("epub" (apply fn file nil))
;;       (_      (apply orig-fn file args))
;;       )))
;;   (advice-add #'find-file :around #'adviser-find-file)
;;   (setq browse-url-browser-function 'eaf-open-browser)
;;   (defalias 'browse-web #'eaf-open-browser)
;;   (setq eaf-browser-enable-adblocker t)
;;   (setq eaf-browser-default-search-engine "google")
;;   (setq eaf-proxy-type "http")
;;   (setq eaf-proxy-host "127.0.0.1")
;;   (setq eaf-proxy-port "7890")
;;   (global-set-key (kbd "C-s") 'eaf-search-it))

;; (set-eshell-alias!
;;  "yt" "youtube-dl $*"
;;  "yta" "youtube-dl -x -f bestaudio/best $*"
;;  "gcl" "git clone --depth=1 $*"
;;  "open" "xdg-open $*"
;;  "xo" "xdg-open $*"
;;  "g" "git --no-pager $*"
;;  "c" "clear-scrollback")

;; ;; EXWM config
;; (defun shutdown ()
;;   (interactive)
;;   (shell-command "shutdown -h now"))

;; (defun reboot ()
;;   (interactive)
;;   (shell-command "reboot"))

;; (defun logout ()
;;   (interactive)
;;   (kill-emacs))

;; (defun display-off ()
;;   (interactive)
;;   (shell-command "xset dpms force off"))

;; (defun lock-screen ()
;;   "Lock screen using (zone) and xtrlock
;;  calls M-x zone on all frames and runs xtrlock"
;;   (interactive)
;;   (save-excursion
;;     (set-process-sentinel
;;      (start-process "slock" nil "slock")
;;      '(lambda (process event)
;;         (zone-leave-me-alone)))
;;     (zone-when-idle 1)))

;; (defun screenshot-full ()
;;   (interactive)
;;   (shell-command
;;    "scrot ~/Pictures/screenshot/pic-$(date '+%y%m%d-%H%M-%S').png"))

;; (defun screenshot-current-window ()
;;   (interactive)
;;   (shell-command
;;    "scrot -f ~/Pictures/screenshot/pic-$(date '+%y%m%d-%H%M-%S').png"))

;; (defun screenshot-select ()
;;   (interactive)
;;   (shell-command
;;    "scrot --select ~/Pictures/screenshot/pic-$(date '+%y%m%d-%H%M-%S').png"))

;; (defun screenshot-clip
;;   (interactive)
;;   (shell-command
;;    "scrot -e 'xclip -selection clipboard -t image/png -i $f' -s")
;;   )
;; (use-package! wallpaper
;;   :hook ((exwm-randr-screen-change . wallpaper-set-wallpaper)
;;          (after-init . wallpaper-cycle-mode))
;;   :custom ((wallpaper-cycle-single t)
;;            (wallpaper-scaling 'scale)
;;            (wallpaper-cycle-interval 450)
;;            (wallpaper-cycle-directory "~/Pictures/Wallpaper"))
;;   :config
;;   (unless (executable-find "feh")
;;     (display-warning 'wallpaper "External command `feh' not found!")))
;; (defvar exwm--toggle-workspace 0
;;   "Previously selected workspace. Used with `exwm/jump-to-last-exwm'.")

;; (defun exwm/jump-to-last-exwm ()
;;   "Jump to last window."
;;   (interactive)
;;   (exwm-workspace-switch exwm--toggle-workspace))

;; (defadvice exwm-workspace-switch
;;     (before save-toggle-workspace activate)
;;   (setq exwm--toggle-workspace exwm-workspace-current-index))

;; (defun yeh/exwm-input-line-mode ()
;;   "Set exwm window to line-mode and show mode line."
;;   (call-interactively #'exwm-input-grab-keyboard)
;;   (exwm-layout-show-mode-line))

;; (defun yeh/exwm-input-char-mode ()
;;   "Set exwm window to char-mode and hide mode line."
;;   (call-interactively #'exwm-input-release-keyboard)
;;   (exwm-layout-hide-mode-line))

;; (defun yeh/exwm-input-toggle-mode ()
;;   "Toggle between line- and char-mode."
;;   (interactive)
;;   (with-current-buffer (window-buffer)
;;     (when (eq major-mode 'exwm-mode)
;;       (if (equal (nth 1 (nth 1 mode-line-process)) "line")
;;           (yeh/exwm-input-char-mode)
;;         (yeh/exwm-input-line-mode)))))

;; (use-package! xelb
;;   :if (display-graphic-p))

;; (use-package! exwm
;;   :if (display-graphic-p)
;;   :init
;;   (setq exwm-workspace-number 5)
;;   (setq mouse-autoselect-window t
;;         focus-follows-mouse t)
;;   (setq window-divider-default-right-width 1)
;;   :config
;;   (require 'exwm-config)
;;   (setq exwm-workspace-index-map
;;         (lambda (index) (number-to-string (1+ index))))
;;   (progn
;;     (exwm-input-set-key (kbd "<s-tab>")  #'exwm/jump-to-last-exwm)
;;     (exwm-input-set-key (kbd "<s-return>")  #'(lambda () (interactive)
;;                                                 (start-process-shell-command "St" nil "st")
;;                                                 ;; (+eshell/here)
;;                                                 ))
;;     (exwm-input-set-key (kbd "s-w")  #'(lambda ()
;;                                          (interactive)
;;                                          (start-process-shell-command
;;                                           "Brave-browser" nil "brave")))
;;     (exwm-input-set-key (kbd "s-d") #'(lambda (command)
;;                                         (interactive (list (read-shell-command
;;                                                             "> ")))
;;                                         (start-process-shell-command
;;                                          command nil command)))
;;     (exwm-input-set-key (kbd "s-=") #'desktop-environment-volume-increment)
;;     (exwm-input-set-key (kbd "s--") #'desktop-environment-volume-decrement)
;;     (exwm-input-set-key (kbd "s-i") #'yeh/exwm-input-toggle-mode)
;;     (mapcar (lambda (i)
;;               (exwm-input-set-key (kbd (format "s-%d" i))
;;                                   #'(lambda ()
;;                                       (interactive)
;;                                       (exwm-workspace-switch-create (1- i)))))
;;             (number-sequence 0 9)))

;;   (add-hook 'exwm-update-class-hook
;;             (lambda ()
;;               (exwm-workspace-rename-buffer exwm-class-name)))

;;   (add-hook 'exwm-manage-finish-hook
;;             (lambda () (call-interactively #'exwm-input-release-keyboard)
;;               (exwm-layout-hide-mode-line)))

;;   (add-hook 'exwm-floating-setup-hook
;;             (lambda ()
;;               (setq floating-mode-line nil)))
;;   ;; Make buffer name more meaningful
;;   (add-hook 'exwm-update-class-hook
;;             (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
;;   (add-hook 'exwm-update-title-hook
;;             (lambda ()
;;               (exwm-workspace-rename-buffer exwm-title)))
;;   (exwm-enable))

;; (use-package! exwm-systemtray
;;   :if (display-graphic-p)
;;   :after exwm
;;   :config
;;   (exwm-systemtray-enable))

;; (use-package! desktop-environment
;;   :if (display-graphic-p)
;;   :after exwm
;;   :init
;;   (desktop-environment-mode)
;;   (setq desktop-environment-screenshot-directory "~/Pictures/screenshot"
;;         desktop-environment-update-exwm-global-keys :global)
;;   :config
;;   (desktop-environment-mode))

;; (use-package! xdg
;;   :if (display-graphic-p)
;;   :commands (xdg-config-dirs xdg-config-home xdg-desktop-read-file))

;; ;; (require 'exwm)
;; ;; (require 'exwm-config)
;; (exwm-enable)
;; (require 'exwm-systemtray)
