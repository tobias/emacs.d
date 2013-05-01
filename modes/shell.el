(require 'ansi-color)

(defun tc/ido-shell-buffer ()
  (interactive)
  (tc/ido-for-mode "Shell:" 'shell-mode))

;; much of the rest borrowed from
;; http://snarfed.org/why_i_run_shells_inside_emacs

(setq
 comint-scroll-to-bottom-on-input  t         ; always insert at the bottom
 comint-scroll-to-bottom-on-output nil       ; always add output at the bottom
 comint-scroll-show-maximum-output t         ; scroll to show max possible output
 comint-input-ignoredups           t         ; no duplicates in command history
 comint-completion-addsuffix       t         ; insert space/slash after file completion
 comint-buffer-maximum-size        20000     ; max length of the buffer in lines
 comint-prompt-read-only           nil       ; if this is t, it breaks shell-command
 comint-get-old-input              (lambda () "") ; what to run when i press enter on a
                                             ; line above the current prompt
 comint-input-ring-size            5000      ; max shell history size
 )

(setq-default dirtrack-list '("^<< \\(.*\\) >>" 1 t))

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun tc/buf-is-shell-p ()
  (eq major-mode 'shell-mode))

(defun tc/make-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  (if (tc/buf-is-shell-p) 
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))

(add-hook 'comint-output-filter-functions 'tc/make-shell-output-read-only)

(defun tc/turn-on-dirtrack-mode ()
  "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
  (when (tc/buf-is-shell-p)
    (shell-dirtrack-mode 0)
    (dirtrack-mode 1)))

(add-hook 'shell-mode-hook 'tc/turn-on-dirtrack-mode)

; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun tc/set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))

(add-hook 'shell-mode-hook 'tc/set-scroll-conservatively)

(defun tc/enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((tc/buf-is-shell-p) (comint-send-input)))))

(add-hook 'isearch-mode-end-hook 'tc/enter-again-if-enter)

(defadvice tc/comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
      (progn (fset 'message 'ignore) ad-do-it)
    (fset 'message old-message))))

(defadvice tc/comint-send-input (around go-to-end-of-multiline activate)
  "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
  (flet ((end-of-line () (end-of-buffer)))
    ad-do-it))



