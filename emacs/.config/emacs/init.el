;; -*- mode: emacs-lisp; coding: utf-8; -*-

(message (current-time-string))

;; Set register for the user init file
(set-register ?I (cons 'file user-init-file))

;; Emacs load paths --------------------------------------------------

;; (add-to-list 'load-path "~/.local/share/emacs/lisp")
(add-to-list 'custom-theme-load-path "~/.local/share/emacs/etc/themes")

;; Customize ---------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(frame-brackground-mode (quote dark))
 '(inferior-R-args "--no-save")
 '(package-selected-packages
   (quote
    (ledger-mode rust-mode e2wm cmake-mode gitignore-mode gruvbox-theme spacemacs-theme org-bullets org-download ssh-config-mode jupyter zoom-window systemd ace-window chronos color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow csv-mode direnv django-mode docker dockerfile-mode dotenv-mode dracula-theme ecb edit-indirect elpy emamux ess eterm-256color graphviz-dot-mode grip-mode gruber-darker-theme hl-todo htmlize ivy json-reformat magit markdown-mode mmm-jinja2 mmm-mode monokai-theme neotree nginx-mode org org-plus-contrib pandoc-mode password-store pinentry poly-markdown powerline realgud sql-indent super-save transpose-frame unicode-fonts web-mode which-key xclip yaml-mode)))
 '(safe-local-variable-values
   (quote
    ((eval org-content 3)
     (eval org-content 2)
     (eval org-content 4)
     (make-backup-files)
     (org-confirm-babel-evaluate)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Package.el --------------------------------------------------------
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(setq
 package-archives '(
                    ("org" . "https://orgmode.org/elpa/")
                    ("gnu" . "http://elpa.gnu.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")
                    ("melpa" . "https://melpa.org/packages/")
                    )
 package-archive-priorities '(
                              ("org" . 20)
                              ("gnu" . 15)
                              ("melpa-stable" . 10)
                              ("melpa" . 5)
                              )
 )
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; Theme -------------------------------------------------------------

(load-theme 'gruber-darker t)

(load-theme 'gruvbox t)

;; (load-theme 'spacemacs-light t)
;; (load-theme 'user-org t)

;; General -----------------------------------------------------------
(setq
 make-backup-files t
 auto-save-default t
 create-lockfiles nil
 inhibit-splash-screen t
 visible-bell t
 sentence-end-double-space nil
 )
(setq-default
 fill-column 70
 case-fold-search t
 indent-tabs-mode nil
 truncate-lines nil
 show-trailing-whitespace nil
 )
(global-auto-revert-mode t)
(tool-bar-mode 0)
(if (display-graphic-p)
    (progn
      ;; (menu-bar-mode 0)
      (toggle-scroll-bar 0))
  (progn
    (xterm-mouse-mode)
    (mouse-wheel-mode)
    (global-set-key (kbd "<mouse-4>") (kbd "<wheel-up>"))
    (global-set-key (kbd "<mouse-5>") (kbd "<wheel-down>"))
    )
  )

;; Directories for backup and auto-save files
(let ((backup-directory (concat user-emacs-directory "backup"))
      (auto-save-directory (concat user-emacs-directory "auto-save")))
  (dolist (dir (list backup-directory auto-save-directory))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq
   backup-directory-alist `(("." . ,backup-directory))
   auto-save-file-name-transforms `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                     ,(concat auto-save-directory "/\\2") t)
                                    ("\\([^/]*/\\)*\\([^/]*\\)"
                                     ,(concat auto-save-directory "/\\2") t)
                                    )
   )
  )

;; misc
(require 'misc)
;; Move forward until encountering the beginning of a word.
(global-set-key (kbd "M-F") 'forward-to-word)
;; Move backward until encountering the end of a word.
(global-set-key (kbd "M-B") 'backward-to-word)

;; Mouse-Wheel mode
(mouse-wheel-mode)
(setq mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil)

;; Update time stamp before saving.
(setq time-stamp-format "%:y-%02m-%02d %3a %02H:%02M:%02S %Z")
(add-hook 'before-save-hook 'time-stamp)

;; eterm-256color
(require 'eterm-256color)
(add-hook 'term-mode-hook #'eterm-256color-mode)

;; Frame configurations
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(width . 82))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(font . "Monospace-9"))

;; browser-url.el
(setq
 ;; The default value of this variable is browse-url-default-browser,
 ;; which calls browse-url-can-use-xdg-open to determine whether it
 ;; can use xdg-open. browse-url-can-use-xdg-open seems to require
 ;; that it be running under either GNOME, KDE, Xfce, or LXDE, which
 ;; isn't actually a necessary condition to using xdg-open.
 browse-url-browser-function 'browse-url-xdg-open
 )

;; Mode line ---------------------------------------------------------

;; powerline
(require 'powerline)
(powerline-default-theme)

;; (setq mode-line-format '("%e"
;;                          mode-line-front-space
;;                          mode-line-mule-info
;;                          mode-line-client
;;                          mode-line-modified
;;                          mode-line-remote
;;                          mode-line-frame-identification
;;                          mode-line-buffer-identification
;;                          "   "
;;                          mode-line-position
;;                          (vc-mode vc-mode)
;;                          "  "
;;                          mode-line-modes
;;                          mode-line-misc-info
;;                          mode-line-end-spaces))

;; Hi Lock mode ------------------------------------------------------
(global-hi-lock-mode 1)

;; https://github.com/jorgenschaefer/emacs-tdd/ ----------------------

(when (not (require 'tdd nil t)) (message "Warning: Failed to load tdd"))

(require 'vc)
(setq vc-follow-symlinks t)

;; unicode-fonts (https://github.com/rolandwalker/unicode-fonts)
(require 'unicode-fonts)
;(setq unicode-fonts-block-font-mapping
;      '(("Emoticons"
;         ("Noto Color Emoji" "Apple Color Emoji" "Symbola" "Quivira")))
;      unicode-fonts-fontset-names '("fontset-default"))
(unicode-fonts-setup)

;; helm
;; (require 'helm-config)
;; (global-set-key (kbd "M-x") #'helm-M-x)
;; (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
;; (helm-mode 1)

;; Ivy
(ivy-mode 1)
(global-set-key (kbd "C-c n") 'counsel-fzf)
;; Redefine ivy-switch-buffer to ignore other-buffer's preference for
;; non-visible buffers.
(defun ivy-switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :preselect (buffer-name (other-buffer (current-buffer) t))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer))

;; direnv
(require 'direnv)
(direnv-mode)
(setq
 direnv-always-show-summary t
 direnv-show-paths-in-summary t
 direnv-use-faces-in-summary t
 )

;; Winner mode
(winner-mode t)

;; ace-window
(global-set-key (kbd "M-o") 'ace-window)
;; Use `;' to flip to the previous window. See
;; https://github.com/abo-abo/ace-window/issues/125 for details.
;; (add-to-list 'aw-dispatch-alist '(?\; aw-flip-window))

;; xclip: use xclip to copy&paste
(xclip-mode)

;; Tramp
(setq tramp-default-method "ssh")

;; Polymode
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown-mode))

;; pandoc-mode
(require 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;; Markdown mode
(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'pandoc-mode)
(setq-default
 markdown-hide-urls t
 )

;; grip-mode
(setq grip-binary-path "/usr/bin/grip")

;; dotenv-mode
(require 'dotenv-mode)

;; change-log-mode
(setq change-log-version-info-enabled t)


;; Elpy --------------------------------------------------------------
(require 'elpy)
(setq elpy-rpc-ignored-buffer-size (* 2 102400)
      elpy-rpc-python-command "python3"
      elpy-shell-starting-directory 'project-root
      )
(setq python-shell-interpreter "python"
      python-shell-interpreter-args "-i")
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil
;;       )
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")
(elpy-enable)

;; Use black to format code.
(define-key elpy-refactor-map (kbd "f")
  (cons (format "%sormat code"
                (propertize "f" 'face 'bold))
        'elpy-black-fix-code))

;; Make Jupyter use the default matplotlib backend. See
;; https://github.com/jorgenschaefer/elpy/issues/1769 for details.
(defun elpy-shell--use-interactive-plots-in-jupyter ()
  "Make sure we use an interactive backend with Jupyter"
  (when (not (null (string-match "jupyter" python-shell-interpreter)))
    (let ((process (python-shell-get-process)))
      (python-shell-send-string-no-output "%matplotlib")
      process)))
(add-hook 'python-shell-first-prompt-hook
          'elpy-shell--use-interactive-plots-in-jupyter t)


;; ESS ---------------------------------------------------------------
(require 'ess-r-mode)
(setq ess-ask-for-ess-directory nil)

;; EasyPG ------------------------------------------------------------
(require 'epa-file)
(setq
 epa-armor nil
 )

;; Emacs pinentry server
(setq epa-pinentry-mode 'loopback)
(pinentry-start)

;; web-mode.el
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; MMM Mode – https://github.com/purcell/mmm-mode
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php)

;; mmm-jinja2 – https://github.com/glynnforrest/mmm-jinja2
(require 'mmm-jinja2)
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))
(mmm-add-mode-ext-class 'sql-mode "\\.sql\\'" 'jinja2)

;; which-key
(require 'which-key)
(which-key-mode)

;; Magit -------------------------------------------------------------
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; pyvenv ------------------------------------------------------------

;; FIXME: Need to add jupyter to org-babel-load-languages in order to
;; do this.
;;
;; Update language aliases after activating a virtual environment.
;; (add-hook 'pyvenv-post-activate-hooks
;;           'org-babel-jupyter-aliases-from-kernelspecs
;;           )

;; org-bullets -------------------------------------------------------

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; Set bullet to zero-width space
;; (setq org-bullets-bullet-list '("\u200b"))

;; Org-mode ----------------------------------------------------------

(require 'org)
(require 'org-tempo)
(require 'ox-md nil t)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq
 org-directory (cond
                ((file-directory-p "~/Dropbox (Personal)") "~/Dropbox (Personal)/org")
                ((file-directory-p "~/Dropbox") "~/Dropbox/org")
                (t "~/org")
                )
 org-default-notes-file (concat org-directory "/notes.org")
 org-ellipsis "▶"
 org-adapt-indentation nil
 org-src-fontify-natively t
 org-src-preserve-indentation nil
 org-edit-src-content-indentation 0
 org-enforce-todo-dependencies t
 org-enforce-todo-checkbox-dependencies nil
 org-fontify-whole-heading-line t
 org-fontify-done-headline t
 org-fontify-quote-and-verse-blocks t
 org-hide-emphasis-markers nil
 org-image-actual-width '(400)
 org-log-into-drawer t
 org-log-done 'time
 org-pretty-entities nil
 org-startup-indented nil
 org-startup-with-inline-images t
 org-use-property-inheritance nil
 org-use-tag-inheritance t
 org-todo-keywords '(
                     (sequence
                      "TODO(t)"
                      "PROJ(P)"
                      "PEND(p)"
                      "WAIT(w)"
                      "HOLD(h)"
                      "SDMB(m)"
                      "|"
                      "CANC(c)"
                      "DONE(d)"
                      )
                     )
 org-todo-keyword-faces '(
                          ("PROJ" . "tan")
                          ("PEND" . "cyan")
                          ("WAIT" . "yellow")
                          ("SDMB" . "gray")
                          ("CANC" . "darkgray")
                          )
 org-tag-alist '(
                 (:startgroup . nil)
                 ("@call" . ?c)
                 ("@home" . ?h)
                 ("@out" . ?o)
                 ("@ws" . ?s)
                 ("wf" . ?w)
                 (:endgroup . nil)
                 )
 org-agenda-dim-blocked-tasks t
 org-agenda-files (append `(
                            ,(expand-file-name "inbox.org" org-directory)
                            ,(expand-file-name "actions.org" org-directory)
                            ,(expand-file-name "calendar.org" org-directory)
                            )
                          )
 org-agenda-include-diary t
 org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                               (todo category-up tag-down priority-down todo-state-up alpha-up)
                               (tags priority-down category-keep)
                               (search category-keep))
 org-agenda-start-on-weekday 1
 org-agenda-todo-list-sublevels t
 org-agenda-todo-ignore-scheduled 'future
 org-deadline-warning-days 3
 org-capture-templates `(
                         ("t" "To-do [inbox]" entry
                          (file ,(expand-file-name "inbox.org" org-directory))
                          "* TODO %?\n%U\n%i"
                          )
                         ("j" "Journal" entry
                          (file+datetree ,(expand-file-name "journal.org.gpg" org-directory))
                          "* %?\n%i"
                          )
                         )
 org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                        (vm-imap . vm-visit-imap-folder-other-frame)
                        (gnus . org-gnus-no-new-news)
                        (file . find-file)
                        (wl . wl-other-frame))
 org-outline-path-complete-in-steps nil
 org-refile-targets `(
                      (org-agenda-files :maxlevel . 2)
                      (,(expand-file-name "maybe.org" org-directory) :level . 2)
                      )
 org-refile-use-outline-path 'file
 org-hierarchical-todo-statistics nil   ; Consider all entries in subtree
 org-file-apps '(
                 (auto-mode . emacs)
                 ("\\.mm\\'" . default)
                 ("\\.x?html?\\'" . default)
                 ("\\.pdf\\'" . "xdg-open \"%s\"")
                 ("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1")
                 ("\\.swf\\'" . "gnash \"%s\"")
                 )
 org-tags-column -77
 )
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "M-n") 'outline-next-visible-heading)
  (define-key org-mode-map (kbd "M-p") 'outline-previous-visible-heading)
  )

;; Replace bullet markers in Org-mode lists.
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Org-babel ---------------------------------------------------------

(setq org-babel-no-eval-on-ctrl-c-ctrl-c nil
      ;; org-babel-default-header-args:jupyter-python '((:kernel . "python")
      ;;                                                (:async . "no"))
      org-babel-python-command "python"
      org-confirm-babel-evaluate '(lambda
                                    (lang body)
                                    (not (or (string= lang "R")
                                             (string= lang "sql")
                                             (string= lang "jupyter-python")
                                             (string= lang "python"))))
      )
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (shell . t)
   (sql . t)
   (R . t)
   (python . t)
   ;; FIXME: The following line causes an Emacs error on startup:
   ;; Error retrieving kernelspecs: (file-missing "Searching for program" "No such file or directory" "jupyter")
   ;; (jupyter . t)
   )
 )
(add-to-list 'org-src-lang-modes '("jupyter-python". python))

(setq user/org-agenda-files-default org-agenda-files)
(put 'user/org-agenda-files
     'safe-local-variable
     (lambda (x) (and (listp x)
                      (cl-every 'identity (mapcar 'stringp x)))))
(defun user/update-org-agenda-files ()
  "Update Org-mode agenda files based on the value of user/org-agenda-files."
  (interactive)
  (if (boundp 'user/org-agenda-files)
      (setq org-agenda-files user/org-agenda-files)
    (setq org-agenda-files user/org-agenda-files-default)
    )
  )
(add-hook 'org-mode-hook
          (lambda () (progn
                       ;; (setq-local line-spacing 0.1)
                       (local-set-key (kbd "C-;") 'user/update-org-agenda-files)
                       )))

;; Org-mode: LaTeX
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq
 org-latex-listings 'minted
 org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
 org-latex-prefer-user-labels t
 )

;; Org-mode: Bimodal cycling
;; https://emacs.stackexchange.com/questions/36232/org-mode-property-to-make-subtree-visibility-bimodal
(advice-add 'org-cycle :around #'apl/org-cycle)

(defun apl/toggle-bimodal-cycling (&optional pos)
  "Enable/disable bimodal cycling behavior for the current heading."
  (interactive)
  (let* ((enabled (org-entry-get pos "BIMODAL-CYCLING")))
    (if enabled
        (org-entry-delete pos "BIMODAL-CYCLING")
      (org-entry-put pos "BIMODAL-CYCLING" "yes"))))

(defun apl/org-cycle (fn &optional arg)
  "Make org outline cycling bimodal (FOLDED and SUBTREE) rather than trimodal (FOLDED, CHILDREN, and SUBTREE) when a heading has a :BIMODAL-CYCLING: property value."
  (interactive)
  (if (and (org-at-heading-p)
           (org-entry-get nil "BIMODAL-CYCLING"))
      (apl/toggle-subtree)
    (funcall fn arg)))

(defun apl/toggle-subtree ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
        (outline-hide-subtree)
      (outline-show-subtree))))

;; org-download
(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

;; Ledger-mode -------------------------------------------------------
(require 'ledger-mode nil 'noerror)
(if (featurep 'ledger-mode)
    (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode)))
(setq
 ledger-init-file-name "~/.ledgerrc"
 ledger-highlight-xact-under-point nil
 ledger-clear-whole-transactions t
 ledger-post-amount-alignment-column 64
 ledger-report-auto-refresh t
 ledger-report-auto-refresh-sticky-cursor t
 ledger-reports (quote
                 (("account-real" "%(binary) -R reg '^%(account)$'")
                  ("account-real-cleared" "%(binary) -CR reg '^%(account)$'")
                  ("bal" "%(binary) -f %(ledger-file) bal")
                  ("reg" "%(binary) -f %(ledger-file) reg")
                  ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                  ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 )
(add-hook 'ledger-mode-hook
          (lambda ()
            (setq-local tab-always-indent 'complete)
            (setq-local completion-cycle-threshold t)
            (setq-local ledger-complete-in-steps t)
            (setq-local comment-column 48)
            (setq-local comment-fill-column 96)))
(defun apl-insert-date (prefix)
  "Insert the current date. With prefix-argument, use dd.mm.yyyy
   format. With two prefix arguments, write out the day and month
   name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%d.%m.%Y")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))
(with-eval-after-load "ledger-mode"
  (define-key ledger-mode-map (kbd "C-c d") 'apl-insert-date)
  (define-key ledger-mode-map (kbd "M-N") 'ledger-navigate-next-uncleared)
  (define-key ledger-mode-map (kbd "M-P") 'ledger-navigate-previous-uncleared)
  )

;; sql-indent
(require 'sql-indent)
(add-hook 'sql-mode-hook 'sqlind-minor-mode)

;; sql-postgres
(add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))

;; chronos
(require 'chronos)
(setq
 chronos-notification-wav "~/Dropbox/Reference/Sounds - Zen Buddhist Temple Bell/Zen Buddhist Temple Bell.mp3"
 chronos-shell-notify-program "mpv"
 chronos-shell-notify-parameters '(
                                   "--really-quiet"
                                   "--af=scaletempo=speed=pitch"
                                   "--speed=0.65"
                                   "--loop-file=3"
                                   "~/Dropbox/Reference/Sounds - Zen Buddhist Temple Bell/Zen Buddhist Temple Bell.mp3"
                                   )
 chronos-text-to-speech-program "espeak"
 chronos-text-to-speech-program-parameters "-s 100"
 chronos-expiry-functions '(
                            chronos-message-notify
                            ;; chronos-sound-notify
                            ;; chronos-desktop-notifications-notify
                            chronos-buffer-notify
                            chronos-shell-notify
                            ;; chronos-dunstify
                            chronos-text-to-speech-notify
                            )
 )

;; ECB
(require 'ecb)
(setq
 ecb-source-path '("~/Documents/GitHub")
 )

;; csv-mode
(add-hook 'csv-mode-hook
          (lambda ()
            (define-key csv-mode-map (kbd "C-c C-M-a")
              (defun csv-align-visible (&optional arg)
                "Align visible fields"
                (interactive "P")
                (csv-align-fields nil (window-start) (window-end))
                )
              )
            )
          )

;; Functions
(defun user-org-insert-created-time-stamp ()
  "Insert a created timestamp if none exists."
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (not (org-entry-get nil "TIMESTAMP_IA")))
    (save-excursion
      (org-back-to-heading)
      (org-show-entry)
      (let ((contents-begin (org-element-property
                             :contents-begin (org-element-at-point))))
        (if contents-begin
            (goto-char contents-begin)
          (forward-line)
          )
        )
      (let ((first-element (org-element-at-point)))
        (when (eq 'property-drawer (car first-element))
          (goto-char (org-element-property :end first-element))
          )
        )
      (org-insert-time-stamp (current-time) t t)
      (open-line 1)
      (indent-for-tab-command)
      )
    )
  )
(global-set-key (kbd "C-c s") 'user-org-insert-created-time-stamp)

(defun user-maximize-frame-height ()
  "Maximize frame height"
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullheight)
  )
(global-set-key (kbd "C-M-<up>") 'user-maximize-frame-height)

(defun adjust-window-width ()
  "Set window width to 80 characters."
  (interactive)
  (let ((tgt 80))
    (if (> (window-width) tgt)
        (shrink-window-horizontally (- (window-width) tgt))
      (enlarge-window-horizontally (- tgt (window-width)))
      )
    )
  )

(defun adjust-frame-width ()
  "Set frame width for a single 80-character window."
  (interactive)
  (let ((tgt 88))
    (if (> (window-width) tgt)
        (set-frame-width (selected-frame)
                         (- (frame-width) (- (window-width) tgt)))
      (set-frame-width (selected-frame)
                       (+ (frame-width) (- tgt (window-width))))
      )
    )
  )
(defun user-timerange-to-seconds (range)
  (if (string-match "[[<]\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\}\\) \\([0-9][0-9]:[0-9][0-9]\\)-\\([0-9][0-9]:[0-9][0-9]\\)[]>]" range)
      (let ((datestr (match-string 1 range))
            (timestart (match-string 2 range))
            (timeend (match-string 3 range))
            )
        (let (
              (dt0 (format "[%s %s]" datestr timestart))
              (dtT (format "[%s %s]" datestr timeend))
              )
          (round (- (org-time-string-to-seconds dtT)
                    (org-time-string-to-seconds dt0)))))
    nil))

(defun buffer-backed-by-file-p (buffer)
  "Return true if a buffer is backed by a file.

From https://emacs.stackexchange.com/a/35907/8574."
  (let ((backing-file (buffer-file-name buffer)))
    (if (buffer-modified-p buffer)
        t
      (if backing-file
          (file-exists-p (buffer-file-name buffer))
        t))))

(defun kill-unbacked-buffers ()
  "Kill all buffers not backed by a file."
  (interactive)
  (mapc 'kill-buffer (-remove 'buffer-backed-by-file-p (buffer-list)))
  )

(defun date-add-month (x)
  (let* ((time-components (parse-time-string (concat x " 00:00:00")))
         (internal-time (apply #'encode-time time-components))
         ;; (year (nth 5 internal-time))
         ;; (month (nth 4 internal-time))
         ;; (days-this-month (ledger-schedule-days-in-month 3 year))
         ;; (seconds-this-month (* (* 24 (* 60 60)) days-this-month))
         )
    internal-time
    ;; (format-time-string "%Y-%m-%d"
    ;;                     (time-add internal-time seconds-this-month))
    )
  )
(defun date-add-month (x)
  "Return the ISO date for next month."
  (let* ((time-components (parse-time-string (concat x " 00:00:00")))
         (internal-time (apply #'encode-time time-components))
         (month (nth 4 time-components))
         (year (nth 5 time-components))
         (days-this-month (ledger-schedule-days-in-month month year))
         (seconds-this-month (* (* 24 (* 60 60)) days-this-month))
         (time-month-seconds (seconds-to-time seconds-this-month))
         (next-month (time-add internal-time time-month-seconds))
         )
    (format-time-string "%Y-%m-%d" next-month)
    )
  )
(defun date-subtract-month (x)
  "Return the ISO date for last month."
  (let* ((time-components (parse-time-string (concat x " 00:00:00")))
         (internal-time (apply #'encode-time time-components))
         (month (nth 4 time-components))
         (year (nth 5 time-components))
         (month-last (if (= month 1) 12 (- month 1)))
         (year-last (if (> month-last month) (- year 1) year))
         (days-last-month (ledger-schedule-days-in-month month-last year-last))
         (seconds-last-month (* (* 24 (* 60 60)) days-last-month))
         (time-month-seconds (seconds-to-time seconds-last-month))
         (last-month (time-subtract internal-time time-month-seconds))
         )
    (format-time-string "%Y-%m-%d" last-month)
    )
  )
(defun date-add-month-interactive ()
  (interactive)
  (let ((pos (point)))
    (search-backward-regexp "\\(^\\|[[:space:]]\\)[[:digit:]]")
    (set-mark-command nil)
    (search-forward-regexp "[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}")
    (deactivate-mark)
    (let ((beg (mark))
          (end (point))
          (selection (buffer-substring-no-properties (mark) (point))))
      (delete-region beg end)
      (insert (date-add-month selection))
      )
    (goto-char pos)
    )
  )
(defun date-subtract-month-interactive ()
  (interactive)
  (let ((pos (point)))
    (search-backward-regexp "\\(^\\|[[:space:]]\\)[[:digit:]]")
    (set-mark-command nil)
    (search-forward-regexp "[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}")
    (deactivate-mark)
    (let ((beg (mark))
          (end (point))
          (selection (buffer-substring-no-properties (mark) (point))))
      (delete-region beg end)
      (insert (date-subtract-month selection))
      )
    (goto-char pos)
    )
  )

;; Unbind macOS Command-key sequences
(global-unset-key (kbd "s-q"))          ; save-buffers-kill-emacs
(global-unset-key (kbd "s-w"))          ; delete-frame
(global-unset-key (kbd "C-z"))          ; suspend-frame
(global-unset-key (kbd "s-n"))          ; make-frame

;; emamux
(require 'emamux)
(global-set-key (kbd "C-z") emamux:keymap)

;; Visual Line mode --------------------------------------------------
(dolist (hook '(markdown-mode-hook
                rst-mode-hook
                )
              )
  (add-hook hook 'visual-line-mode)
  )

;; Code
(dolist (hook '(c-mode-hook
                conf-mode-hook
                csv-mode-hook
                dockerfile-mode-hook
                dotenv-mode-hook
                graphviz-dot-mode-hook
                json-mode-hook
                ledger-mode-hook
                makefile-mode-hook
                markdown-mode-hook
                emacs-lisp-mode-hook
                ess-mode-hook
                nxml-mode-hook
                org-mode-hook
                python-mode-hook
                js-mode-hook
                rust-mode-hook
                sh-mode-hook
                sql-mode-hook
                yaml-mode-hook
                web-mode-hook
                )
              )
  (add-hook hook
            (lambda ()
              (setq show-trailing-whitespace t
                    truncate-lines t)
              )
            )
  (add-hook hook
            (if (fboundp 'display-line-numbers-mode)
                'display-line-numbers-mode
              'linum-mode)
            )
  )

;; hl-todo
(dolist (hook '(c-mode-hook
                dockerfile-mode-hook
                emacs-lisp-mode-hook
                ess-mode-hook
                js-mode-hook
                makefile-mode-hook
                nxml-mode-hook
                python-mode-hook
                inferior-python-mode-hook
                sh-mode-hook
                sql-mode-hook
                web-mode-hook
                yaml-mode-hook
                )
              )
  (add-hook hook (lambda ()
                   (hl-todo-mode)
                   (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
                   ;; (define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
                   (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
                   (define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert)
                   )
            )
  )

;; hs-minor-mode
(dolist (hook '(python-mode-hook
                )
              )
  (add-hook hook 'hs-minor-mode)
  )

;; variable-pitch-mode
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; Key binding for compilation
(global-set-key [f9]
                (lambda ()
                  (interactive)
                  (compile (format "make -kC %s"
                                   (locate-dominating-file "." "Makefile")))))


;; Key bindings for macOS
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
;; [2018-08-29 Wed] C-s-f with left command:
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)

;; Enable default-disabled functions
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Registers
(set-register ?F (cons 'file (expand-file-name "Family/Family.org.gpg" org-directory)))
(set-register ?H (cons 'file (expand-file-name "Health/Health.org.gpg" org-directory)))
(set-register ?J (cons 'file (expand-file-name "journal.org.gpg" org-directory)))
(set-register ?L (cons 'file "~/.local/share/ledger/journal.ledger"))
(set-register ?N (cons 'file (expand-file-name "Notes/Notes.org.gpg" org-directory)))
(set-register ?P (cons 'file (expand-file-name "Passphrases/Passphrases.org.gpg" org-directory)))
(set-register ?A (cons 'file (expand-file-name "actions.org" org-directory)))
(set-register ?c (cons 'file (expand-file-name "calendar.org" org-directory)))
(set-register ?g (cons 'file (expand-file-name "general.org" org-directory)))
(set-register ?  (cons 'file (expand-file-name "inbox.org" org-directory)))
(set-register ?l (cons 'file (expand-file-name "lists.org" org-directory)))
(set-register ?m (cons 'file (expand-file-name "maybe.org" org-directory)))
(set-register ?n (cons 'file (expand-file-name "notes.org" org-directory)))
(set-register ?p (cons 'file (expand-file-name "projects.org" org-directory)))
(set-register ?r (cons 'file (expand-file-name "reading.org" org-directory)))
(set-register ?s (cons 'file (expand-file-name "scratch.org" org-directory)))
