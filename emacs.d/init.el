;; -*- coding: utf-8; mode: Emacs-Lisp; -*-

(message (current-time-string))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(inferior-R-args "--no-save")
 '(package-selected-packages
   (quote
    (poly-markdown ecb markdown-mode org-bullets pandoc-mode ace-window chronos csv-mode direnv dotenv-mode dracula-theme elpy emamux ess eterm-256color exec-path-from-shell helm-dash htmlize json-mode json-reformat ledger-mode magit mmm-jinja2 mmm-mode neotree org password-store pinentry pipenv realgud sql-indent super-save svg-clock unicode-fonts web-mode which-key xclip yaml-mode)))
 '(safe-local-variable-values (quote ((make-backup-files) (org-confirm-babel-evaluate)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(setq
 package-archives '(
		    ("gnu" . "http://elpa.gnu.org/packages/")
		    ("melpa-stable" . "https://stable.melpa.org/packages/")
		    ("melpa" . "https://melpa.org/packages/")
		    )
 package-archive-priorities '(
			      ("gnu" . 15)
			      ("melpa-stable" . 10)
			      ("melpa" . 5)
			      )
 )
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; Set register for the user init file
(set-register ?I (cons 'file user-init-file))

; X11: Interpret alt key-press as meta
(if (boundp 'x-alt-keysym)
    (setq x-alt-keysym 'meta))

;; Backup and auto-save files
(let ((backup-dir (concat user-emacs-directory "backup"))
      (auto-save-dir (concat user-emacs-directory "auto-save")))
  (dolist (dir (list backup-dir auto-save-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
(setq
 backup-directory-alist `(("." . ,backup-dir))
 auto-save-file-name-transforms `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat auto-save-dir "/\\2") t)
                                  ("\\([^/]*/\\)*\\([^/]*\\)" ,(concat auto-save-dir "/\\2") t)
                                  )
 )
)

;; General
(setq
 x-super-keysym 'meta
 make-backup-files t
 auto-save-default t
 create-lockfiles nil
 inhibit-splash-screen t
 visible-bell t
 sentence-end-double-space nil
 )
(setq-default
 case-fold-search t
 indent-tabs-mode nil
 truncate-lines t
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

;; eterm-256color
(require 'eterm-256color)
(add-hook 'term-mode-hook #'eterm-256color-mode)

;; Frame configurations
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(width . 82))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(font . "Inconsolata-11"))

;; Hi Lock mode
(global-hi-lock-mode 1)

(require 'vc)
(setq vc-follow-symlinks t)

;; https://www.emacswiki.org/emacs/UnicodeFonts
(require 'unicode-fonts)
(setq unicode-fonts-block-font-mapping
      '(("Emoticons"
	 ("Apple Color Emoji" "Symbola" "Quivira")))
      unicode-fonts-fontset-names '("fontset-default"))
(unicode-fonts-setup)

;; exec-path-from-shell
(require 'exec-path-from-shell)
;; (when (memq window-system '(mac ns))
(exec-path-from-shell-initialize)
;; )

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

;; Theme
;; (if (display-graphic-p)
;;     (load-theme 'misterioso)
;;   (load-theme 'misterioso)
;;   )
(load-theme 'dracula t)

;; xclip: use xclip to copy&paste
(xclip-mode)

;; Ido Mode
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq
 ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
		      "*Messages*" "Async Shell Command")
 ido-default-buffer-method 'selected-window
 )

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

;; dotenv-mode
(require 'dotenv-mode)

;; pipenv.el
(add-hook 'python-mode-hook #'pipenv-mode)

;; Elpy
(elpy-enable)
;; Pipenv
;; (setq
;;   python-shell-interpreter "pipenv"
;;   python-shell-interpreter-args "run jupyter console --simple-prompt"
;;   python-shell-prompt-detect-failure-warning nil
;;   )
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "pipenv")
;; Jupyter console
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil
;;       )
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")
;; IPython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(setq
 elpy-rpc-ignored-buffer-size (* 2 102400)
 elpy-shell-use-project-root nil
 )
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook (lambda () (column-number-mode 1)))
(add-hook 'inferior-python-mode-hook (lambda () (setq truncate-lines t)))
(with-eval-after-load "python"
  (define-key python-mode-map (kbd "C-c TAB") 'hs-toggle-hiding)
  )

;; GUD
;; (setq gud-pdb-command-name "pipenv run python -m pdb")

;; RealGUD
;; (require 'realgud)
;; (setq
;;  realgud:pdb-command-name "pipenv run python -m pdb"
;;  realgud:ipdb-command-name "pipenv run python -m ipdb"
;;  realgud:trepan2-command-name "pipenv run trepan2"
;;  realgud:trepan3k-command-name "pipenv run trepan3k"
;;  )

;; ESS
(require 'ess)
(setq ess-ask-for-ess-directory nil)

;; EasyPG
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

;; Magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Org-mode
(require 'org)
(require 'ox-md nil t)
(require 'org-tempo)
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
 org-log-into-drawer t
 org-log-done 'time
 org-use-property-inheritance nil
 org-use-tag-inheritance t
 org-babel-no-eval-on-ctrl-c-ctrl-c t
 org-confirm-babel-evaluate '(lambda (lang body) (not (or (string= lang "R") (string= lang "sql"))))
 org-todo-keywords '((sequence "TODO(t)" "PENDING(p)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))
 org-todo-keyword-faces '(("PEND" . "yellow") ("WAIT" . "yellow"))
 org-tag-alist '(("@home" . ?h)
                 ("@ws" . ?w)
                 ("@mobile" . ?m)
                 ("understood" . ?u)
                 )
 org-agenda-files '("~/Dropbox/org/inbox.org"
                    "~/Dropbox/org/projects.org"
                    )
 org-capture-templates '(("t" "To-do [inbox]" entry
                          (file "~/Dropbox/org/inbox.org")
                          "* TODO %?\n%U\n%i"
                          )
                         ("j" "Journal" entry
                          (file+datetree "~/Dropbox/org/Journal/Journal.org.gpg")
                          "* %?\n%i"
                          )
                         )
 org-refile-use-outline-path 'file
 org-refile-targets '((org-agenda-files :maxlevel . 2)
                      ("~/Dropbox/org/maybe.org" :maxlevel . 1)
                      )
 )
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "M-n") 'outline-next-visible-heading)
  (define-key org-mode-map (kbd "M-p") 'outline-previous-visible-heading)
  )
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (sql . t)
   ;; (R . t)
   (python . t)
   )
 )

;; Org-mode: LaTeX
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq
 org-latex-listings 'minted
 org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
 )

;; org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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

;; Ledger-mode
(require 'ledger-mode nil 'noerror)
(if (featurep 'ledger-mode)
    (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode)))
(setq
 ledger-init-file-name "~/.config/ledger/ledgerrc"
 ledger-highlight-xact-under-point nil
 ledger-clear-whole-transactions t
 )
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
  (define-key ledger-mode-map (kbd "C-c d") 'apl-insert-date))

;; sql-indent
(require 'sql-indent)
(add-hook 'sql-mode-hook 'sqlind-minor-mode)

;; sql-postgres
(add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))

;; chronos
(require 'chronos)
(setq
 chronos-notification-wav "~/Dropbox/Sounds - Zen Buddhist Temple Bell/Zen Buddhist Temple Bell.mp3"
 chronos-shell-notify-program "mpv"
 chronos-shell-notify-parameters '(
                                   "--really-quiet"
                                   "--af=scaletempo=speed=pitch"
                                   "--speed=0.65"
                                   "--loop-file=3"
                                   "~/Dropbox/Sounds - Zen Buddhist Temple Bell/Zen Buddhist Temple Bell.mp3"
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
(defun user-stamp-creation-time ()
  "Stamp Org-mode headline with creation time in CREATED property."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-set-property "CREATED"
		      (format-time-string(org-time-stamp-format nil t)))))
(global-set-key (kbd "C-c s") 'user-stamp-creation-time)

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

;; Unbind macOS Command-key sequences
(global-unset-key (kbd "s-q"))          ; save-buffers-kill-emacs
(global-unset-key (kbd "s-w"))          ; delete-frame
(global-unset-key (kbd "C-z"))          ; suspend-frame
(global-unset-key (kbd "s-n"))          ; make-frame

;; emamux
(require 'emamux)
(global-set-key (kbd "C-z") emamux:keymap)

;; line numbers
(dolist (hook '(c-mode-hook
                conf-mode-hook
                csv-mode-hook
                dotenv-mode-hook
                json-mode-hook
                ledger-mode-hook
                makefile-mode-hook
                markdown-mode-hook
                org-mode-hook
                emacs-lisp-mode-hook
                ess-mode-hook
                python-mode-hook
                inferior-python-mode-hook
                sh-mode-hook
                sql-mode-hook
                text-mode-hook
                )
              )
  (add-hook hook (if (fboundp 'display-line-numbers-mode)
                     'display-line-numbers-mode
                   'linum-mode)
            )
  )

;; Key bindings for macOS
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
;; [2018-08-29 Wed] C-s-f with left command:
(global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Enable default-disabled functions
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Registers
(set-register ?P (cons 'file (expand-file-name "Passphrases/Passphrases.org.gpg" org-directory)))
(set-register ?f (cons 'file (expand-file-name "Family/Family.org.gpg" org-directory)))
(set-register ?h (cons 'file (expand-file-name "Health/Health.org.gpg" org-directory)))
(set-register ?j (cons 'file (expand-file-name "Journal/Journal.org.gpg" org-directory)))
(set-register ?m (cons 'file (expand-file-name "maybe.org" org-directory)))
(set-register ?n (cons 'file (expand-file-name "notes.org" org-directory)))
(set-register ?N (cons 'file (expand-file-name "Notes/Notes.org.gpg" org-directory)))
(set-register ?i (cons 'file (expand-file-name "inbox.org" org-directory)))
(set-register ?p (cons 'file (expand-file-name "projects.org" org-directory)))
(set-register ?g (cons 'file (expand-file-name "general.org" org-directory)))
(set-register ?r (cons 'file (expand-file-name "reading.org" org-directory)))
