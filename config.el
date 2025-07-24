(setq use-package-always-ensure t)
(add-to-list 'load-path "~\\.emacs.d\\scripts\\")

(require 'elpaca-setup)
(require 'buffer-move)

(use-package evil
          :init
          (setq evil-want-integration t)
          (setq evil-want-keybinding nil)
          (setq evil-vsplit-window-right t)
          (setq evil-split-window-below t)
          (evil-mode ))
    (use-package evil-collection
      :after evil
      :config
      (setq evil-collection-mode-list '(dashboard dired ibuffer))
      (evil-collection-init))
    (use-package evil-tutor)

        ;;Turns off elpaca-use-package-mode current declaration
        ;;Note this will cause evaluate the declaration immediately. It is not deferred.
        ;;Useful for configuring built-in emacs features.
        (use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))
;; Don't install anything. Defer execution of BODY

(use-package general
      :config
    (general-evil-setup)
    (general-create-definer ult/leader-keys
      :states '(normal insert visual emacs)
      :keymaps 'override
      :prefix "SPC"
      :global-prefix "M-SPC")
    (ult/leader-keys
      "." '(find-file :wk "Find file")
      "f c" '((lambda () (interactive) (find-file "~/.emacs.d/config.org")) :wk "Edit emacs config")
      "f r" '(counsel-recentf :wk "Find recent files" )
      "TAB TAB" '(comment-line :wk "Comment lines")
	  "p s" '(powershell :wk "Powershell"))
    (ult/leader-keys
      "b" '(:ignore t :wk "buffer")
      "b b" '(switch-to-buffer :wk "Switch buffer")
      "b i" '(ibuffer :wk "Ibuffer")
      "b k" '(kill-current-buffer :wk "Kill this buffer")
      "b n" '(next-buffer :wk "Next buffer")
      "b p" '(previous-buffer :wk "Previous buffer")
      "b r" '(revert-buffer :wk "Reload buffer"))
    (ult/leader-keys
  "d" '(:ignore t :wk "Dired")
  "d d" '(dired :wk "Open dired")
  "d j" '(dired-jump :wk "Dired jump to current")
  "d n" '(neotree-dir :wk "Open directory in neotree")
  "d p" '(peep-dired :wk "Peep-dired"))
(ult/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate an elisp expresssion")
    "e h" '(counsel-esh-history :wk "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e s" '(eshell :wk "Eshell"))
(ult/leader-keys
  "h" '(:ignore t :wk "Help")
  "h a" '(counsel-apropos :wk "Apropos")
  "h b" '(describe-bindings :wk "Describe bindings")
  "h c" '(describe-char :wk "Describe character under cursor")
  "h d" '(:ignore t :wk "Emacs documentation")
  "h d a" '(about-emacs :wk "About Emacs")
  "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
  "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
  "h d m" '(info-emacs-manual :wk "The Emacs manual")
  "h d n" '(view-emacs-news :wk "View Emacs news")
  "h d o" '(describe-distribution :wk "How to obtain Emacs")
  "h d p" '(view-emacs-problems :wk "View Emacs problems")
  "h d t" '(view-emacs-todo :wk "View Emacs todo")
  "h d w" '(describe-no-warranty :wk "Describe no warranty")
  "h e" '(view-echo-area-messages :wk "View echo area messages")
  "h f" '(describe-function :wk "Describe function")
  "h F" '(describe-face :wk "Describe face")
  "h g" '(describe-gnu-project :wk "Describe GNU Project")
  "h i" '(info :wk "Info")
  "h I" '(describe-input-method :wk "Describe input method")
  "h k" '(describe-key :wk "Describe key")
  "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
  "h L" '(describe-language-environment :wk "Describe language environment")
  "h m" '(describe-mode :wk "Describe mode")
  "h r" '(:ignore t :wk "Reload")
  "h r r" '((lambda () (interactive)
              (load-file "~/.config/emacs/init.el")
              (ignore (elpaca-process-queues)))
            :wk "Reload emacs config")
  "h t" '(load-theme :wk "Load theme")
  "h v" '(describe-variable :wk "Describe variable")
  "h w" '(where-is :wk "Prints keybinding for command if set")
  "h x" '(describe-command :wk "Display full documentation for command"))

    (ult/leader-keys
  "m" '(:ignore t :wk "Org")
  "m a" '(org-agenda :wk "Org agenda")
  "m e" '(org-export-dispatch :wk "Org export dispatch")
  "m i" '(org-toggle-item :wk "Org toggle item")
  "m t" '(org-todo :wk "Org todo")
  "m B" '(org-babel-tangle :wk "Org babel tangle")
  "m T" '(org-todo-list :wk "Org todo list"))

(ult/leader-keys
  "m b" '(:ignore t :wk "Tables")
  "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

(ult/leader-keys
  "m d" '(:ignore t :wk "Date/deadline")
  "m d t" '(org-time-stamp :wk "Org time stamp"))

(ult/leader-keys
  "p" '(projectile-command-map :wk "Projectile"))
(ult/leader-keys
  "t" '(:ignore t :wk "Toggle")
  "t e" '(eshell-toggle :wk "Toggle eshell")
  "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
  "t t" '(visual-line-mode :wk "Toggle truncated lines"))
(ult/leader-keys
  "w" '(:ignore t :wk "Windows")
  ;; Window splits
  "w c" '(evil-window-delete :wk "Close window")
  "w n" '(evil-window-new :wk "New window")
  "w s" '(evil-window-split :wk "Horizontal split window")
  "w v" '(evil-window-vsplit :wk "Vertical split window")
  ;; Window motions
  "w h" '(evil-window-left :wk "Window left")
  "w j" '(evil-window-down :wk "Window down")
  "w k" '(evil-window-up :wk "Window up")
  "w l" '(evil-window-right :wk "Window right")
  "w w" '(evil-window-next :wk "Goto next window")
  ;; Move Windows
  "w H" '(buf-move-left :wk "Buffer move left")
  "w J" '(buf-move-down :wk "Buffer move down")
  "w K" '(buf-move-up :wk "Buffer move up")
  "w L" '(buf-move-right :wk "Buffer move right"))
    )

(use-package nerd-icons
    ;; (nerd-icons-font-family "Tinos Nerd Font")
)
  (use-package nerd-icons-dired
    :hook (dired-mode . nerd-icons-dired-mode ))

(setq backup-directory-alist '((".*" . "~\\.backups")))

(use-package company
    :defer 2
    :diminish
    :custom
    (company-begin-commands '(self-insert-command))
    (comany-idle-delay .1)
    (company-minimum-prefix-length 2)
    (company-show-numbers t)
    (company-tooltip-align-annotations 't)
    (global-company-mode t))
(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package dashboard
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Welcome to the Cockpit!")
  (setq dashboard-startup-banner "~/.emacs.d/themes/pngegg.png")
  (setq dashboard-center-content nil)
  (setq dashboard-items '((recents . 5)
			  (agenda . 5)
			  (bookmarks . 3)
			  (projects . 3)
			  (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package diminish)

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
)

(use-package flycheck
  :defer t
  :diminish
  :init (global-flycheck-mode))

(set-face-attribute 'default nil
  :family "BigBlueTerm437 Nerd Font Mono"
  :height 110
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :family "BigBlueTerm437 Nerd Font Mono"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :family "BigBlueTerm437 Nerd Font Mono"
  :height 110
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "BigBlueTerm437 Nerd Font Mono"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(setq inhibit-startup-screen t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package doom-themes
:custom
(doom-themes-enable-bold t )   ; if nil, bold is universally disabled
    (doom-themes-enable-italic t)
    :config
(load-theme 'doom-sourcerer t)
)  ;

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(use-package counsel
	:diminish
        :after ivy
        :config (counsel-mode))
    (use-package ivy
      :diminish
      :bind
      (("C-c C-r" . ivy-resume)
       ("C-x B" . ivy-switch-buffer-other-window))
      :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
(ivy-mode))
(use-package nerd-icons-ivy-rich
  :diminish
  :init (nerd-icons-ivy-rich-mode 1))
(use-package ivy-rich
  :diminish
  :after ivy
  :init (ivy-rich-mode 1)
  :custom
  (ivy-virtual-abbreviate 'full
			  ivy-rich-switch-buffer-align-virtual-buffer t
			  ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
			       'ivy-rich-switch-buffer-transformer))

(use-package lua-mode)

(use-package doom-modeline
	      :init (doom-modeline-mode 1)
	      :config
	      (setq doom-modeline-height 20
		    doom-modeline-bar-width 5
		    doom-modeline-persp-name t
		    doom-modeline-persp-icon t))

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 55
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil))))
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "j") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "k") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
(evil-define-key 'insert neotree-mode-map (kbd "j") 'neotree-next-line)
    (evil-define-key 'insert neotree-mode-map (kbd "k") 'neotree-previous-line)
)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)

(require 'org-tempo)

(use-package projectile
  :config
  (projectile-mode 1))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

;; on windows, make pwsh the default shell
(when (eq system-type 'windows-nt)
  (let ((xlist
         '(
          "~/AppData/Local/Microsoft/WindowsApps/pwsh.exe"
          "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
          ))
        xfound)
    (setq xfound (seq-some (lambda (x) (if (file-exists-p x) x nil)) xlist))
    (when xfound (setq explicit-shell-file-name xfound))))
    (use-package eshell-syntax-highlighting
    :after esh-mode
    :config
    (eshell-syntax-highlighting-global-mode +1))
    
  (setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
        eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands '("bash", "fish", "htop", "ssh", "top", "zsh"))

(require 'shell)
(autoload 'powershell "powershell" "Run powershell as a shell within emacs." t)

(defun powershell-gen-window-width-string ()
  (concat  "$a = (Get-Host).UI.RawUI\n" 
            "$b = $a.WindowSize\n"
            "$b.Width = " (number-to-string  (window-width)) "\n"
            "$a.BufferSize = $b\n"
            "$a.WindowSize = $b")
  )
  

(defvar powershell-prompt-pattern  "PS [^#$%>]+>" 
  "Regexp for powershell prompt.  This isn't really used, because I couldn't figure out how to get it to work."
  )

(defgroup powershell nil
  "Running shell from within Emacs buffers."
  :group 'processes
  )


(defcustom powershell-need-rawui-resize t
  "set when powershell needs to be resized"
  :group 'powershell
)

;;;###autoload
(defun powershell (&optional buffer)
  "Run an inferior powershell, by invoking the shell function. See the help for shell for more details.
\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
         (read-buffer "Shell buffer: "
                      (generate-new-buffer-name "*PowerShell*")))))
  ; get a name for the buffer
  (setq buffer (get-buffer-create (or buffer "*PowerShell*")))

  (let (
        (tmp-shellfile explicit-shell-file-name)
        )
                                        ; set arguments for the powershell exe.
                                        ; This needs to be tunable.
    (setq explicit-shell-file-name "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")  
    (setq explicit-powershell.exe-args '("-Command" "-" )) ; interactive, but no command prompt
  
                                        ; launch the shell
    (shell buffer)

    ; restore the original shell
    (if explicit-shell-file-name
        (setq explicit-shell-file-name tmp-shellfile)
      )
    )
  
  (let (
        (proc (get-buffer-process buffer))
        )
    
    ; This sets up the powershell RawUI screen width. By default,
    ; the powershell v1.0 assumes terminal width of 80 chars.
    ;This means input gets wrapped at the 80th column.  We reset the
    ; width of the PS terminal to the window width. 
    (add-hook 'window-size-change-functions 'powershell-window-size-changed)

    (powershell-window-size-changed)
    
    ; ask for initial prompt
    (comint-simple-send proc "prompt")
    )

  ; hook the kill-buffer action so we can kill the inferior process?
  (add-hook 'kill-buffer-hook 'powershell-delete-process)

  ; wrap the comint-input-sender with a PS version
  ; must do this after launching the shell! 
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'powershell-simple-send)

  ; set a preoutput filter for powershell.  This will trim newlines after the prompt.
  (add-hook 'comint-preoutput-filter-functions 'powershell-preoutput-filter-for-prompt)

  ;(run-hooks 'powershell-launch-hook)

  ; return the buffer created
  buffer
)


(defun powershell-window-size-changed (&optional frame)
  ; do not actually resize here. instead just set a flag.
  (setq powershell-need-rawui-resize t)
)



(defun powershell-delete-process (&optional proc)
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  (and (processp proc)
       (delete-process proc))
  )



;; This function trims the newline from the prompt that we
;; get back from powershell.  It is set into the preoutput
;; filters, so the newline is trimmed before being put into
;; the output buffer.
(defun powershell-preoutput-filter-for-prompt (string)
   (if
       ; not sure why, but I have not succeeded in using a variable here???  
       ;(string-match  powershell-prompt-pattern  string)

       (string-match  "PS [^#$%>]+>" string)
       (substring string 0 -1)
     
     string

     )
   )



(defun powershell-simple-send (proc string)
  "Override of the comint-simple-send function, specific for powershell.
This just sends STRING, plus the prompt command. Normally powershell is in
noninteractive model when run as an inferior shell with stdin/stdout
redirected, which is the case when running as a shell within emacs.
This function insures we get and display the prompt. "
  ; resize if necessary. We do this by sending a resize string to the shell,
  ; before sending the actual command to the shell. 
  (if powershell-need-rawui-resize
      (and
       (comint-simple-send proc (powershell-gen-window-width-string))
       (setq powershell-need-rawui-resize nil)
       )
    )
  (comint-simple-send proc string)
  (comint-simple-send proc "prompt")
)

(use-package which-key
  :diminish
  :init
   (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
    which-key-sort-order #'which-key-key-order-alpha
    whick-key-sort-uppercase-first nil
    which-key-add-column-padding 1
    which-key-max-display-columns nil
    which-key-min-display-lines 6
    which-key-side-window-slot -10
    which-key-side-window-max-height 0.25
    which-key-idle-delay 0.8
    which-key-max-description-length 25
    which-key-allow-imprecise-window-fit t
    which-key-separator " > " ))
