;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

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

;; Set up the python DAP debugger
(after! dap-mode
  ;; Important: Be sure to run M-x dap-cpptools-setup to setup
  ;; the adapter before using it the first time.
  (require 'dap-cpptools)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)

  ;; Make the buffer read-only when dap is running
  ;; Source: https://emacs-lsp.github.io/dap-mode/page/how-to/#activate-minor-modes-when-stepping-through-code
  (define-minor-mode +dap-running-session-mode
    "A mode for adding keybindings to running sessions"
    nil
    nil
    (make-sparse-keymap)
    nil
    ;; The following code adds to the dap-terminated-hook
    ;; so that this minor mode will be deactivated when the debugger finishes
    (when +dap-running-session-mode
      ;; Make all buffers in debug mode read only except for the REPL
      (unless (equal (buffer-name) "*dap-ui-repl*")
        (read-only-mode 1))

      (let ((session-at-creation (dap--cur-active-session-or-die))
            (out-buffer-name (format "*%s out*" (dap--debug-session-name (dap--cur-active-session-or-die)))))
        (when (get-buffer-window out-buffer-name)
          ;; Expand the output buffer a little
          (window-resize (select-window (get-buffer-window out-buffer-name) nil) 20 t))
        (add-hook 'dap-terminated-hook
                  (lambda (session)
                    (when (eq session session-at-creation)
                      (read-only-mode -1)
                      ;; Kill some stragling buffers that don't get cleaned up
                      (when (get-buffer "*dap-ui-repl*")
                        (kill-buffer "*dap-ui-repl*"))
                      (when (get-buffer out-buffer-name)
                        (kill-buffer out-buffer-name))
                      (+dap-running-session-mode -1)))))))

  ;; Activate this minor mode when dap is initialized
  (add-hook 'dap-session-created-hook '+dap-running-session-mode)

  ;; Activate this minor mode when hitting a breakpoint in another file
  (add-hook 'dap-stopped-hook '+dap-running-session-mode)

  ;; Activate this minor mode when stepping into code in another file
  (add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                            (when (dap--session-running session)
                                              (+dap-running-session-mode 1))))

  ;; Activate the REPL when the debugger is started
  (add-hook 'dap-session-created-hook
            (lambda (arg)
              (call-interactively #'dap-ui-repl)))

  ;; Move the cursor to the REPL window after the DAP server has responded
  (add-hook 'dap-executed-hook
            (lambda (arg1 arg2) (select-window (get-buffer-window "*dap-ui-repl*") nil)))

  ;; Make dap-debugging have some reasonable templates
  (dap-register-debug-template
   "C++ :: Debug"
   (list :type "cppdbg"
         :request "launch"
         :name "C++ :: Debug"
         :MIMode "lldb"
         :TargetArchitecture "arm64"
         :program "${fileDirname}/${fileBasenameNoExtension}"
         :cwd "${workspaceFolder}"))

  (dap-register-debug-template
   "Python :: Debug"
   (list :type "python"
         :args "-m debugpy"
         :request "launch"
         :name "Python :: Debug"
         :module nil
         :program "${fileDirname}/${fileBasename}"
         :cwd "${fileDirname}")))

(after! dap-ui
  (setq dap-ui-buffer-configurations
        `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.20)))
          (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
          (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
          (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
          (,dap-ui--debug-window-buffer . ((side . left) (slot . 3) (window-width . 0.2)))
          (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-width . 0.2))))))

;; Give dap some reasonable key bindings
(map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug start"   "s" #'+debugger/start
      :desc "dap debug choose"  "S" #'dap-debug
      :desc "dap debug quit"    "q" #'+debugger/quit

      ;; debug
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message
      :desc "dap breakpoint delete"      "d" #'dap-breakpoint-delete
      :desc "dap breakpoint delete all"  "D" #'dap-breakpoint-delete-all)

(map! :map +dap-running-session-mode-map
      :desc "next" "C-n" #'dap-next
      :desc "continue" "C-c" #'dap-continue
      :desc "step in" "C-i" #'dap-step-in
      :desc "step out" "C-o" #'dap-step-out
      :desc "restart" "C-r" #'dap-debug-restart
      :desc "quit" "C-q" #'+debugger/quit)

;; Set dash-docs to use the system default browser
(after! dash-docs
  (setq dash-docs-browser-func #'browse-url))

;; Set the recommended length of a single line in a git commit message to 68 characters
(after! magit
  (setq git-commit-summary-max-length 68)
  (setq magit-todos-insert-after '(bottom)))

;; Create a modified Stroustrup style for c/c++ files
(after! cc-mode
  (c-add-style "modified-stroustrup"
               '("stroustrup"
                 (c-basic-offset . 2)))
  (setq c-default-style "modified-stroustrup")

  ;; Set clang-format to use the modified Stroustrup style
  (set-formatter!
    'clang-format
    '("clang-format"
      (format "--assume-filename=%S" (or buffer-file-name mode-result ""))
      (format "--style=file:%s/.doom.d/clang-format-modified-stroustrup.yaml" (expand-file-name "~")))
    :modes
    '(c-mode
      c++-mode
      java-mode
      objc-mode
      protobuf-mode)))

;; Set the keybindings after package has been loaded
;; to overwrite any settings that may have been set
(after! company
  (map! :map company-active-map
        "TAB" #'company-complete-selection
        "<tab>" #'company-complete-selection)
  (global-company-mode t))

;; If one needs to add more frontends when using company-box
;; (add-hook 'company-box-mode-hook
;;           (lambda ()
;;             (add-to-list 'company-frontends 'company-preview-frontend)))

;; Disable poetry-tracking-mode in Python buffers
(after! python
  (remove-hook 'python-mode-hook #'poetry-tracking-mode))

;; Enable auto-revert-mode for all buffers
(after! autorevert
  (global-auto-revert-mode t)
  (setq global-auto-revert-non-file-buffers t)  ; Also auto-revert buffers like dired
  (setq auto-revert-verbose nil))                ; Don't show a message every time a buffer is reverted

;; Set the default ispell dictionary to en_US
(after! ispell
  (setq ispell-dictionary "en_US"))

;; Enable lsp-pyright for python files
(use-package! lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))  ; or lsp-deferred
  :config
  ;; Set the venv path to the .venv directory if it exists in
  ;; the git root or current directory if not in a git repo
  (defun lsp-pyright-set-venv-path ()
    (let* ((git-root (locate-dominating-file default-directory ".git"))
           (venv-path ".venv")
           (full-venv-path (if git-root
                               (expand-file-name venv-path git-root)
                             (expand-file-name venv-path default-directory))))
      (when (file-directory-p full-venv-path)
        (setq-local lsp-pyright-venv-path full-venv-path))))

  ;; Set the venv path before LSP is initialized
  (add-hook 'lsp-before-open-hook
            (lambda ()
              (when (eq major-mode 'python-mode)
                (lsp-pyright-set-venv-path)))))

;; Set the auth-source files
(after! auth-source
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg")))

;; Set up grip
(use-package! grip-mode
  :config
  (setq grip-update-after-change nil)
  (require 'auth-source)
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

;; Enable visual line mode globally
(global-visual-line-mode t)

;; Disable the helper `which-key' mode since it is extremely slow
(after! which-key
  (which-key-mode -1))

(defun shrink-tool-bar ()
  "Enable and disable tool bar to shrink it."
  (tool-bar-mode 1)
  (tool-bar-mode -1))

;; For initial frame at startup
(shrink-tool-bar)

;; For any new frames created later
(defun shrink-tool-bar-new-frame (frame)
  "Enable and disable tool bar to shrink it on FRAME."
  (with-selected-frame frame
    (shrink-tool-bar)))

(add-hook 'after-make-frame-functions #'shrink-tool-bar-new-frame)

;; (use-package aidermacs
;;   :config
;;   (let ((anthropic-credential (auth-source-user-and-password "api.anthropic.com"))
;;         (openai-credential (auth-source-user-and-password "api.openai.com")))
;;     (setq aidermacs-use-architect-mode t)
;;     (setq aidermacs-architect-model "sonnet")
;;     (setq aidermacs-editor-model "sonnet")
;;     (setq aidermacs-backend 'vterm)
;;     (setq aidermacs-auto-commits t)
;;     (setq aidermacs-extra-args
;;           (list "--anthropic-api-key" (cadr anthropic-credential)
;;                 "--openai-api-key" (cadr openai-credential)
;;                 "--cache-prompts"
;;                 "--cache-keepalive-pings" "12"
;;                 "--no-suggest-shell-commands"
;;                 "--no-auto-lint"
;;                 "--dark-mode"
;;                 "--watch-files"))))

;; (defun my-other-window (&optional arg)
;;   "Like `other-window' but tries twice if first window is *aidermacs:...*"
;;   (interactive "P")
;;   (let ((orig-window (selected-window)))
;;     (other-window (or arg 1))
;;     (when (and (string-match "^\\*aidermacs:\\(.*?\\)\\*$"
;;                              (buffer-name))
;;                (not (eq (selected-window) orig-window)))
;;       (other-window (or arg 1)))))

(after! llm
  (setq llm-warn-on-nonfree nil))

(after! magit
  ;; Configure magit-gptcommit
  (use-package! magit-gptcommit
    :init
    (require 'llm-openai)
    :custom
    (magit-gptcommit-llm-provider
     (make-llm-openai-compatible :url "https://openrouter.ai/api/v1" :key (cadr (auth-source-user-and-password "openrouter.ai")) :chat-model "google/gemma-3n-e4b-it:free"))

    :config
    (magit-gptcommit-status-buffer-setup)
    )

  ;; Configure magit-forge
  (use-package! forge)
  )

;; Keybindings with no package loading dependency
(map! :map 'override
      :desc "Go to beginning of function" "C-M-;" #'beginning-of-defun
      :desc "Go to end of function" "C-M-'" #'end-of-defun

      ;; :desc "Aidermacs other window" "C-x o" #'my-other-window

      :leader
      :desc "Compile" "c C" #'compile
      :desc "Recompile" "c c" #'recompile

      ;; :desc "Aider" "a" #'aidermacs-transient-menu

      :desc "Avy goto char timer" "j" #'avy-goto-char-timer

      :desc "Vertico Project Search" "s p" #'+vertico/project-search
      :desc "Vertico Project Search CWD" "s d" #'+vertico/project-search-from-cwd
      :desc "Projectile Find File" "s f" #'projectile-find-file

      :desc "Restore last session" "w r" #'+workspace/restore-last-session
      :desc "Rename workspace" "w R" #'+workspace/rename
      :desc "Switch to left workspace" "w <left>"#'+workspace/switch-left
      :desc "Switch to right workspace" "w <right>" #'+workspace/switch-right)

;; Custom variables
(setq kill-whole-line t
      tab-always-indent t
      display-line-numbers-type t
      org-directory "~/org/"

      ;; Make the command key the M meta and option super ('s')
      mac-command-modifier 'meta
      mac-option-modifier 'super

      ;; Disable auto-comments
      comment-line-break-function nil)
