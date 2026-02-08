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

;; Doom internal overrides
(after! doom
  (advice-add 'doom/quickload-session :around
              (lambda (orig-fn &rest _args)
                (funcall orig-fn t))))

;; From `company' module
;; Force company-mode to complete on TAB rather than cycle to next option
(after! company
  (map! :map company-active-map
        "TAB" #'company-complete-selection
        "<tab>" #'company-complete-selection))

;; From `lookup' module.
;; Set dash-docs to use the system default browser
(after! dash-docs
  (setq dash-docs-browser-func #'browse-url))

;; Built-in `auth-source' module
;; Set the auth-source files
(after! auth-source
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg")))

;; From `magit' module
(after! magit
  ;; Set the recommended length of a single line in a git commit message to 68 characters
  (setq git-commit-summary-max-length 68)
  (setq magit-todos-insert-after '(bottom))

  ;; Configure magit-gptcommit
  (use-package! magit-gptcommit
    :init
    (require 'llm-openai)
    (require 'auth-source)

    :config
    ;; (setq magit-gptcommit-llm-provider
    ;;       (make-llm-openai-compatible :url "http://localhost:4141" :key "no-api-key" :chat-model "claude-haiku-4.5"))
    (let ((credential (auth-source-user-and-password "api.openai.com")))
      (setq magit-gptcommit-llm-provider
            (make-llm-openai :key (cadr credential) :chat-model "gpt-4o-mini")))
    (setq magit-gptcommit-prompt "You are an expert programmer writing a Git commit message.
You have carefully reviewed every file diff included in this commit.

First, choose the most appropriate label for the changes.
Here are the labels you can choose from:
- build: Changes that affect the build system or external dependencies (e.g., gulp, broccoli, npm)
- chore: Routine tasks like updating dependencies, licenses, or repo settings
- ci: Changes to CI configuration files or scripts (e.g., GitHub Actions, CircleCI)
- docs: Documentation-only changes (e.g., fixing typos, adding examples)
- feat: Introduces a new feature to the codebase
- fix: Patches a bug in the codebase
- perf: Improves performance without changing behavior
- refactor: Code changes that neither fix bugs nor add features
- style: Non-functional changes like formatting or whitespace
- test: Adds or corrects tests

Next, write a high-level summary of the commit.
- Keep it to a single line, no more than 50 characters
- Use the imperative tense (e.g., 'Add logging' not 'Added logging')
- Ensure the message reflects a clear and cohesive change
- Do not end the summary with a period
- Do not use backticks (`) anywhere in the response

Examples:

- chore: Bump dependencies to latest versions
- ci: Add caching to GitHub Actions workflow
- docs: Update README with setup instructions
- feat: Add user authentication with JWT
- fix: Prevent crash when config file is missing
- perf: Reduce database query latency
- refactor: Simplify data processing pipeline
- style: Reformat codebase with standard rules
- test: Add coverage for edge case validation

THE FILE DIFFS:
```
%s
```
Now, write the commit message using this format: [label]: [summary]")
    (magit-gptcommit-mode 1)
    (magit-gptcommit-status-buffer-setup))

  ;; Configure magit-forge
  (use-package! forge))

;; From `javelin.el' package
(use-package! javelin
  :config
  (setq javelin-disable-confirmation t)
  (global-javelin-minor-mode 1))

;; From `magit-pre-commit.el' package
(use-package! magit-pre-commit
  :hook (magit-mode . magit-pre-commit-mode))

;; From `disable-mouse' package
(use-package! disable-mouse
  :config
  (global-disable-mouse-mode))

;; From `kubel' package
(use-package! kubel
  :config
  (kubel-global-mode 1))

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

;; From `magit-gptcommit' package
(after! llm
  (setq llm-warn-on-nonfree nil))

;; From `lsp-mode' package
(after! lsp-mode
  (setq lsp-lens-enable nil)
  (setq lsp-signature-render-documentation nil))

;; From `lsp-ui' package
(after! lsp-ui
  (setq lsp-ui-peek-peek-height 40))

;; Emacs built-in
;; For a more ergonomic Emacs and `dape' experience
(repeat-mode 1)
(setq repeat-exit-key (kbd "RET"))

;; Add multiple cursors to a repeat map
(defvar mc-repeat-map
  (let ((map (make-sparse-keymap)))
    ;; Keys from your list (suffix after C-c m …)
    (define-key map (kbd "n") #'mc/mark-next-like-this)
    (define-key map (kbd "p") #'mc/mark-previous-like-this)
    (define-key map (kbd "N") #'mc/unmark-next-like-this)
    (define-key map (kbd "P") #'mc/unmark-previous-like-this)

    (define-key map (kbd "s") #'mc/skip-to-next-like-this)
    (define-key map (kbd "S") #'mc/skip-to-previous-like-this)

    map)
  "Repeat map for the multiple-cursors keys shown in the user’s binding list.")

;; Disable the RET for in multiple cursors, used for repeat mode exit
(map! :map mc/keymap
      "RET" nil
      "<return>" nil)

(dolist (cmd '(mc/mark-next-like-this
               mc/mark-previous-like-this
               mc/unmark-next-like-this
               mc/unmark-previous-like-this
               mc/skip-to-next-like-this
               mc/skip-to-previous-like-this))
  (put cmd 'repeat-map mc-repeat-map))

;; From `dape' package
;; Load it immediately
(use-package! dape
  :defer nil)

;; From `scroll-around' package
(use-package! scroll-around
  :config
  (scroll-around-mode 1))

;; avy keybindings
(map! :map global-map
      :desc "Avy goto char timer" "C-j C-j" #'avy-goto-char-timer
      :desc "Avy goto char timer" "C-j j" #'avy-goto-char-timer
      :desc "Avy goto char in line" "C-j C-k" #'avy-goto-char-in-line
      :desc "Avy goto char in line" "C-j k" #'avy-goto-char-in-line
      :desc "Avy goto line" "C-j C-l" #'avy-goto-line
      :desc "Avy goto line" "C-j l" #'avy-goto-line
      :desc "Avy goto end of line" "C-j L" #'avy-goto-end-of-line
      :desc "Avy pop mark" "C-j C-p" #'avy-pop-mark
      :desc "Avy pop mark" "C-j p" #'avy-pop-mark)

;; python mode overrides
(map! :after python
      :map (python-mode-map python-ts-mode-map)
      :desc "Python forward block" "M-<right>" #'python-nav-forward-statement
      :desc "Python backward block" "M-<left>" #'python-nav-backward-statement
      :desc "Python forward block" "M-<down>" #'python-nav-forward-block
      :desc "Python backward block" "M-<up>" #'python-nav-backward-block
      :desc "Python forward defun" "C-M-<down>" #'end-of-defun
      :desc "Python backward defun" "C-M-<up>" #'beginning-of-defun
      )

;; Keybindings with no package loading dependency
(map! :map 'override
      :desc "Find references" "M-/" #'+lookup/references
      :desc "Go to beginning of function" "C-M-;" #'beginning-of-defun
      :desc "Go to end of function" "C-M-'" #'end-of-defun

      :desc "Kill whole line" "C-S-k" #'kill-whole-line
      
      :leader ;;  C-c
      :desc "Toggle Demap" "o m" #'demap-toggle
      
      :desc "Compile" "c C" #'compile
      :desc "Recompile" "c c" #'recompile

      :desc "Vertico Project Search" "s p" #'+vertico/project-search
      :desc "Vertico Project Search CWD" "s d" #'+vertico/project-search-from-cwd
      :desc "Projectile Find File" "s f" #'projectile-find-file

      :desc "Restore last session" "w r" #'+workspace/restore-last-session
      :desc "Rename workspace" "w R" #'+workspace/rename
      :desc "Switch to left workspace" "w <left>"#'+workspace/switch-left
      :desc "Switch to right workspace" "w <right>" #'+workspace/switch-right)

;; Unmap any unneeded keybindings
(map! :map ctl-x-map
      "f" #'find-file ;; Accidental C-x f should still `find-file' rather than `set-fill-column'
      "C-c" nil ;; Prefer to kill emacs via `C-c q q'
      )

(map! :map 'override      
      :leader ;; C-c      
      "t m" nil ;; `demap-toggle' default keybinding
      )

;; Custom variables
(setq!
 ;; Do not prompt when killing emacs
 confirm-kill-emacs nil
 
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
 comment-line-break-function nil

 ;; Wait time for avg-goto-char-timer
 avy-timeout-seconds 0.25)
