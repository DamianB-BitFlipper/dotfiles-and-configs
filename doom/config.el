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

;; From `lookup' module.
;; Set dash-docs to use the system default browser
(after! dash-docs
  (setq dash-docs-browser-func #'browse-url))

;; From `magit' module
;; Set the recommended length of a single line in a git commit message to 68 characters
(after! magit
  (setq git-commit-summary-max-length 68)
  (setq magit-todos-insert-after '(bottom)))

;; From `cc' module
;; Create a modified Stroustrup style for c/c++ files
(after! cc-mode
  (c-add-style "modified-stroustrup"
               '("stroustrup"
                 (c-basic-offset . 2)))
  (setq c-default-style "modified-stroustrup"))

;; Built-in `autorevert' module
;; Enable auto-revert-mode for all buffers
(after! autorevert
  (global-auto-revert-mode t)
  (setq global-auto-revert-non-file-buffers t)  ; Also auto-revert buffers like dired
  (setq auto-revert-verbose nil))                ; Don't show a message every time a buffer is reverted

;; From `spell' module
;; Set the default ispell dictionary to en_US
(after! ispell
  (setq ispell-dictionary "en_US"))

;; Built-in `auth-source' module
;; Set the auth-source files
(after! auth-source
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg")))

;; From `markdown' module
;; Set up grip
(use-package! grip-mode
  :config
  (require 'auth-source)
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

;; Emacs built-in
;; Enable visual line mode globally
(global-visual-line-mode t)

;; Doom Emacs built-in
;; Disable the helper `which-key' mode since it is extremely slow
(after! which-key
  (which-key-mode -1))

;; From `magit-gptcommit' package
(after! llm
  (setq llm-warn-on-nonfree nil))

(after! magit
  ;; Configure magit-gptcommit
  (use-package! magit-gptcommit
    :init
    (require 'llm-openai)
    :custom
    (magit-gptcommit-llm-provider
     (make-llm-openai-compatible :url "http://localhost:4141" :key "no-api-key" :chat-model "claude-haiku-4.5"))

    :config
    (magit-gptcommit-status-buffer-setup)
    )

  ;; Configure magit-forge
  (use-package! forge))

;; Keybindings with no package loading dependency
(map! :map 'override
      :desc "Go to beginning of function" "C-M-;" #'beginning-of-defun
      :desc "Go to end of function" "C-M-'" #'end-of-defun

      :leader
      :desc "Compile" "c C" #'compile
      :desc "Recompile" "c c" #'recompile

      :desc "Avy goto char timer" "j" #'avy-goto-char-timer

      :desc "Vertico Project Search" "s p" #'+vertico/project-search
      :desc "Vertico Project Search CWD" "s d" #'+vertico/project-search-from-cwd
      :desc "Projectile Find File" "s f" #'projectile-find-file

      :desc "Restore last session" "w r" #'+workspace/restore-last-session
      :desc "Rename workspace" "w R" #'+workspace/rename
      :desc "Switch to left workspace" "w <left>"#'+workspace/switch-left
      :desc "Switch to right workspace" "w <right>" #'+workspace/switch-right)

;; Custom variables
(setq!
 ;; If killing a line from the begging, also kill any trailing newlines
 kill-whole-line t

 ;; Hitting <TAB> tries to indent first, if already indented
 ;; then sends signal for possible tab completion
 tab-always-indent t

 ;; Display lines as absolute numbers
 display-line-numbers-type t

 ;; Tells org where org files live for agenda management
 org-directory "~/.emacs-org/"

 ;; Make the command key the M meta and option super ('s')
 mac-command-modifier 'meta
 mac-option-modifier 'super

 ;; Disable auto-comments when on a comment and hitting newline
 comment-line-break-function nil)
