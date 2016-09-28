;;==========================================================
;;                   Notes 
;;==========================================================
;; longline-mode  ; http://www.emacswiki.org/emacs/LongLines

;; Some "do-s" and "don't-s"
(server-force-delete)
(setq inhibit-startup-message t) 	; don't display welcome message
(setq make-backup-files	nil)	 	; don't make any backup files
(setq auto-save-list-file-name nil)	; don't want any .saves files
(setq auto-save-default t)		; do auto saving
(setq auto-save-interval 20)		; do auto saving between n characters
(setq auto-save-timeout 30)		; do auto saving every n seconds
(setq scroll-step 2)			; scroll 2 lines when cursor at bottom
(setq-default fill-column 70)		; 80 characters per line
(setq require-final-newline t)		; always end a file with a newline
;;(setq next-line-add-newlines nil)	; don't add new lines an the end 
;;(setq scroll-conservatively 5)
(setq comint-scroll-to-bottom-on-input t) ; make more of the last input visible
;;(setq comint-scroll-to-bottom-on-output t); make more of the last output visible
(setq comint-move-point-for-output t)	  ; alias for the above
(setq comint-scroll-show-maximum-output t); make more of the last output visible
(global-auto-revert-mode 1)		  ; auto-revert mode

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer t t))

(transient-mark-mode 1)			  ; the transient-mark mode
(column-number-mode 1)			  ; turn on column number
(show-paren-mode 1)			  ; highlight matching parenthesis
(display-time)				  ; displays the time in the status bar
(fset 'yes-or-no-p 'y-or-n-p)		  ; Changes yes/no ? to y/n type
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;;==========================================================
;; iswitchb-mode bzw. ido-mode
;;==========================================================
(require 'iswitchb)
(iswitchb-mode 1)			; C-k kills a buffer in iswitchb-mode
(add-to-list 'iswitchb-buffer-ignore "^ ")
(add-to-list 'iswitchb-buffer-ignore "*Completions")
(add-to-list 'iswitchb-buffer-ignore "*Messages")
(add-to-list 'iswitchb-buffer-ignore "*Buffer")
(add-to-list 'iswitchb-buffer-ignore "*ECB")
(add-to-list 'iswitchb-buffer-ignore "*ESS")

;; mapping between keybindings
(global-set-key [home] 'beginning-of-buffer) ; [Home] takes you to beginning of buffer
(global-set-key [end] 'end-of-buffer)	     ; [End] take you to end of buffer
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)  ; (global-set-key "\C-c\C-k" 'kill-region)

;; to edit X Window display, see: /etc/X11/Xresources/emacs
;; (M-x list-faces-display) and (M-x list-colors-display)
;(set-background-color "black")
;(set-foreground-color "white")
(set-face-foreground 'region "blue")
(set-face-background 'region "orange")
(set-cursor-color "purple3")
;; comment colors 
(set-face-foreground 'font-lock-comment-face "chocolate")
(set-variable font-lock-comment-face 'font-lock-comment-face)
;; string colors
(set-face-foreground 'font-lock-string-face "green4")
(set-variable font-lock-string-face 'font-lock-string-face)


;;-------------------------------------------------------------------
;; ess (check the newest version: M-x ess-version)
;; sudo apt-get install ess
;;-------------------------------------------------------------------
;; cd .emacs.d/ess-13.09-1
;; make
;; make install
(require 'ess-site) 

;; alternative
;; (add-to-list 'load-path "~/.emacs.d/ess-13.09-1/lisp") ; don't combine with "apt-get install ess"
;; (load "ess-site")

;;-------------------------------------------------------------------
;; mutt
;;-------------------------------------------------------------------
;; M-x ansi-term                                 ;;; Start bash
;; http://www.emacswiki.org/emacs/MuttInEmacs    ;;; probably useful
;; http://fsinfo.noone.org/~abe/mutt/            ;;; copy
(defun rbz-mail-mode-hook () (turn-off-auto-fill) ;;; Auto-Fill is (not) necessary for mails 
  (turn-on-font-lock)				 ;;; Font-Lock is always cool
  (flush-lines "^\\(&gt; \n\\)*&gt; -- \n\\(\n?&gt; .*\\)*") ;;; Kills quoted sigs. 
  (not-modified)	       ;;; We haven't changed the buffer.
  (mail-text)		       ;;; Jumps to the beginning of the mail text 
  (setq make-backup-files nil) ;;; No backups necessary. 
  )

(or (assoc "mutt-" auto-mode-alist) 
    (setq auto-mode-alist (cons '("mutt-" . mail-mode) auto-mode-alist))
    )

(add-hook 'mail-mode-hook 'rbz-mail-mode-hook) 
(server-start) ;;; For use with emacsclient
;; see also
;; http://www.gnu.org/software/emacs/elisp/html_node/Filling.html


;;-------------------------------------------------------------------
;; latex
;;-------------------------------------------------------------------
(load "~/.emacs.d/emacs-latex-functions")
(setq TeX-PDF-mode t)

;;-------------------------------------------------------------------
;; q / kdb+
;; https://code.kx.com/trac/browser/contrib/weaves/tools/emacs
;;-------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/emacs.r1724/")
(load "~/.emacs.d/emacs.r1724/init-01.el")
(global-set-key "\C-h\C-q" 'q-help)
(global-set-key "\C-c\C-q" 'run-q)


;;-------------------------------------------------------------------
;; spelling
;;-------------------------------------------------------------------
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq-default ispell-program-name "aspell")
(setq flyspell-default-dictionary "english")
(setq flyspell-issue-message-flag nil)
;; activate flyspell
(add-hook 'ess-mode-hook 'flyspell-prog-mode t) ; flyspell on comments and strings
(add-hook 'text-mode-hook 'flyspell-mode t)
(add-hook 'message-mode-hook 'flyspell-mode t)
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)   ; (add-hook 'LaTeX-mode-hook 'flyspell-mode t)
(add-hook 'html-mode-hook 'flyspell-mode t)
(add-hook 'rbz-mail-mode-hook 'flyspell-mode t)

;; Requirement: sudo apt-get install aspell aspell-bg aspell-de
(defun dospellcheck-in-english-please ()
  "switch ispell language to english"
  (interactive)
  (ispell-change-dictionary "english"))

(defun dospellcheck-in-bulgarian-please ()
  "switch ispell language to english"
  (interactive)
  (ispell-change-dictionary "bulgarian"))

(defun dospellcheck-in-german-please ()
  "switch ispell language to deutsch"
   (interactive)
   (ispell-change-dictionary "de"))

;;-------------------------------------------------------------------
;; abbreviations
;; C-x a i g 
;;-------------------------------------------------------------------
(setq-default abbrev-mode t)
(read-abbrev-file "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)

;;-------------------------------------------------------------------
;; AUCTeX (just install it)
;; M-x package-list-packages RET
;; i ; mark it 
;; x ; execute installation procedure
;;-------------------------------------------------------------------
(setq TeX-PDF-mode t)
;;(setenv "PATH" (shell-command-to-string "bash -i -c 'echo -n $PATH'"))
(setq reftex-plug-into-AUCTeX t)	
(add-hook 'LaTeX-mode-hook
      (lambda ()
        (reftex-mode t)
        (flyspell-mode t)
 ))
;;-------------------------------------------------------------------
;; org-mode
;; M-x package-list-packages RET
;;-------------------------------------------------------------------
;; the newest version (C-h v org-version)
(require 'org-install)			; load it

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
	(sequence "|" "CANCELED(c)")
	(sequence "|" "MISSED(m)"))) 


;;-------------------------------------------------------------------
;; print-to-pdf
;;-------------------------------------------------------------------
(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " (buffer-name) ".pdf"))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " (buffer-name) ".pdf"))  
  )


;;------------------------------------------------------------------
;; Looking Up Dictionary
;; http://xahlee.org/emacs/emacs_lookup_ref.html
;;------------------------------------------------------------------
(defun lookup-word-definition ()
  "Look up the current word's definition in a browser.
If a region is active (a phrase), lookup that phrase."
  (interactive)
  (let (myword myurl)
    (setq myword
	  (if (and transient-mark-mode mark-active)
	      (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))
    
    (setq myword (replace-regexp-in-string " " "%20" myword))
    ;; (setq myurl (concat "http://www.askoxford.com/concise_oed/" myword))
    ;; (browse-url myurl)
    (setq myurl (concat "http://www.yourdictionary.com/" myword))
    (browse-url myurl)

    ;; (w3m-browse-url myurl) ;; if you want to browse using w3m
    ))

(global-set-key (kbd "<f7>") 'lookup-word-definition)

(defun lookup-word-definition-german ()
  "Look up the current word's definition in a browser.
If a region is active (a phrase), lookup that phrase."
  (interactive)
  (let (myword myurl)
    (setq myword
	  (if (and transient-mark-mode mark-active)
	      (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))
    
    (setq myword (replace-regexp-in-string " " "%20" myword))
    (setq myurl (concat "http://dict.tu-chemnitz.de/?query=" myword))
    (browse-url myurl)

    ;; (w3m-browse-url myurl) ;; if you want to browse using w3m
    ))

(global-set-key (kbd "<f6>") 'lookup-word-definition-german)
(put 'narrow-to-region 'disabled nil)


;; copy/paste from terminal to X11
;; sudo apt-get install xsel
(load "~/.emacs.d/copypaste.el")
