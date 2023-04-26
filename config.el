(setq initial-major-mode 'org-mode)

(setq user-full-name "Peng Ye"
      user-mail-address "yemouren@protonmail.com")

(setq-default custom-file (expand-file-name ".custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Profile emacs startup
(setq confirm-kill-emacs nil
      confirm-kill-processes nil)

(setq! doom-font (font-spec :family "Cascadia Code"
                            :size 18
                            :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "Cascadia Code"
                                           :size 18)
       doom-unicode-font (font-spec :family "Cascadia Code" :size 18))

(setq doom-theme 'doom-one)

(setq fancy-splash-image "~/.config/doom/banner.jpg")

(setq display-line-numbers-type 'relative)

(doom/set-frame-opacity '97)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq gc-cons-threshold (* 50 1000 1000)
      read-process-output-max (* 3 1024 1024))

(global-undo-tree-mode t)

(add-hook 'prog-mode-hook #'which-function-mode)
(setq which-func-format (quote (:propertize which-func-current :weight bold)))
(setq-default mode-line-format (cons '(:eval (which-function-mode)) mode-line-format))

(setq browse-url-handlers '(("\\`file:" . browse-url-default-browser)))

(after! centaur-tabs
  (setq centaur-tabs-style "wave"))

(when (display-graphic-p)
  (after! evil
    (defalias 'evil-insert-state 'evil-emacs-state)
    (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
    (setq evil-emacs-state-cursor 'bar)
    (setq evil-ex-substitute-global t)))

(after! ox-hugo
  (setq org-hugo-base-dir "~/Dev/blog")
  (setq org-hugo-auto-set-lastmod t))

(after! org
  (setq org-hide-emphasis-markers t)
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-directory "~/org/"
        org-ellipsis " ▾ ")
  (setq org-html-head-extra "<link rel=\"stylesheet\" href=\"https://unpkg.com/marx-css/css/marx.css\" type=\"text/css\">")
  )

(use-package! org-attach-screenshot
  :bind ("<f6> s" . org-attach-screenshot)
  :config
  (setq org-attach-screenshot-dirfunction
        (lambda ()
          (progn (cl-assert (buffer-file-name))
               (concat (file-name-sans-extension (buffer-file-name))
                "-att")))
        org-attach-screenshot-command-line "maim -u -s %f"))

(after! doom-modeline
  (advice-add #'doom-modeline-segment--modals :override #'ignore))

(after! vertico
   (setq vertico-resize t))

(use-package! gptel
  :config
  (setq! gptel-api-key "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")
  (setq! gptel-mode 'org-mode))

(use-package! rime
  :config
  (setq rime-user-data-dir "~/.config/fcitx/rime")
  ;; (setq rime-posframe-properties
  ;;       (list :background-color "#333333"
  ;;             :foreground-color "#dcdccc"
  ;;             ;; :font "WenQuanYi Micro Hei Mono-15"
  ;;             :internal-border-width 10))
  (setq default-input-method "rime")
  (setq rime-show-candidate 'posframe)
  (setq rime-inline-ascii-trigger 'shift-l)
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (setq mode-line-mule-info '((:eval (rime-lighter))))
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-prog-in-code-p
          rime-predicate-after-alphabet-char-p)))

(use-package! super-save
  :config
  (super-save-mode +1)
  (setq auto-save-default t)
  (setq super-save-auto-save-when-idle t)
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(use-package! eaf
  :load-path "~/.elisp/emacs-application-framework"
  ;; :custom
  ;; (eaf-browser-continue-where-left-off t)
  ;; (eaf-browser-enable-adblocker t)
  ;; (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
  :config
  (defalias 'browse-web #'eaf-open-browser)
  ;; (setq eaf-browser-continue-where-left-off t)
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-video-player)
  (require 'eaf-mindmap)
  (require 'eaf-terminal)
  (require 'eaf-image-viewer)
  (require 'eaf-file-manager)
  (require 'eaf-git)
  (require 'eaf-all-the-icons)
  (require 'eaf-evil)

  (setq eaf-evil-leader-key "C-SPC")
  (setq eaf-browser-dark-mode t)
  (setq browse-url-browser-function 'eaf-open-browser)
  (setq eaf-browser-enable-adblocker t)
  (setq eaf-browser-default-search-engine "duckduckgo")

  (setq eaf-terminal-font-family "FiraCode Nerd Font")
  (setq eaf-webengine-font-family "FiraCode Nerd Font")
  (setq eaf-terminal-font-size '18)

  (defun adviser-find-file (orig-fn file &rest args)
  (let ((fn (if (commandp 'eaf-open) 'eaf-open orig-fn)))
    (pcase (file-name-extension file)
      ("pdf"  (apply fn file nil))
      ("epub" (apply fn file nil))
      (_      (apply orig-fn file args)))))
  (advice-add #'find-file :around #'adviser-find-file)

  (setq eaf-proxy-type "http")
  (setq eaf-proxy-host "127.0.0.1")
  (setq eaf-proxy-port "7890")
  (map! :leader
        :desc "Eaf Search it"
        "o s e" #'eaf-search-it)
  (map! :leader
        :desc "Eaf open terminal"
        "o s t" #'eaf-open-terminal))

(setq +latex-viewers nil)
(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
     (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
     (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))))

(after! go-translate
  ;; 配置多个翻译语言对
  (setq gts-translate-list '(("en" "zh") ("zh" "en")))

  ;; 配置默认的 translator
  ;; 这些配置将被 gts-do-translate 命令使用
  (setq gts-default-translator
        (gts-translator

         :picker ; 用于拾取初始文本、from、to，只能配置一个

         ;;(gts-noprompt-picker)
         ;;(gts-noprompt-picker :texter (gts-whole-buffer-texter))
         (gts-prompt-picker)
         ;;(gts-prompt-picker :single t)
         ;;(gts-prompt-picker :texter (gts-current-or-selection-texter) :single t)

         :engines ; 翻译引擎，可以配置多个。另外可以传入不同的 Parser 从而使用不同样式的输出

         (list
          (gts-bing-engine)
          ;;(gts-google-engine)
          ;;(gts-google-rpc-engine)
          ;;(gts-deepl-engine :auth-key [YOUR_AUTH_KEY] :pro nil)
          ;; (gts-google-engine :parser (gts-google-summary-parser))
          ;;(gts-google-engine :parser (gts-google-parser))
          ;;(gts-google-rpc-engine :parser (gts-google-rpc-summary-parser))
          ;; (gts-google-rpc-engine :parser (gts-google-rpc-parser))
          (gts-youdao-dict-engine)
          ;;(gts-stardict-engine)
          )

         :render ; 渲染器，只能一个，用于输出结果到指定目标。如果使用 childframe 版本的，需自行安装 posframe

         ;; (gts-buffer-render)
         (gts-posframe-pop-render)
         ;;(gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")
         ;;(gts-posframe-pin-render)
         ;;(gts-posframe-pin-render :position (cons 1200 20))
         ;;(gts-posframe-pin-render :width 80 :height 25 :position (cons 1000 20) :forecolor "#ffffff" :backcolor "#111111")
         ;;(gts-kill-ring-render)

         ;; translate
         :splitter ; 分割器，可选。如果设置了，将会分段按照提供的规则分段进行翻译。可以选择定制 Render 混合输出分段翻译的结果

         (gts-paragraph-splitter)))
  (map! :leader
        :desc "Translation at point(en to zh and zh to en)"
        "o s T" #'gts-do-translate))
