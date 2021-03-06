(require 'search-engine)
(require 'cl)
(require 'which-func)

(defvar jcscope-buffer "*jcscope:result*")
(defvar jcscope-process-buffer "*jcscope:process*")

(defun jcscope-filter (proc string)
  (with-current-buffer (get-buffer-create jcscope-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (save-excursion (insert string))
      (forward-line -1)
      (while (re-search-forward "\\\(.*\\\) \\\([<>a-zA-Z_0-9]*\\\) \\\([0-9]+\\\) " nil t)
	(let ((file-name (match-string 1))
	      (context (match-string 2))
	      (line (match-string 3)))
	  (delete-region (line-beginning-position) (point))
	  (insert (format "%s:%s:[%s]: " file-name line context))
	  (add-text-properties (point) (line-end-position) '(face grep-hit-face)))))
    (setq next-error-last-buffer (current-buffer))))

(defun jcscope-init (dir)
  (interactive "Djcscope directory: ")
  (with-current-buffer (get-buffer-create jcscope-process-buffer)
    (when (get-buffer-process (current-buffer))
      (kill-process))
    (setq default-directory dir
	  jcscope-directory dir)
    (set-process-filter (start-file-process "jcscope" jcscope-process-buffer "jcscope" "-ld" "-f" "jcscope.out")
			'jcscope-filter)))

(defun jcscope-generate ()
  (interactive)
  (let ((default-directory jcscope-directory))
    (set-process-sentinel (start-file-process "jcscope-gen" "*jcscope-gen*" "jcscope" "-b" "-q" "-k")
			  (lambda (p e) (when (string= "finished\n" e)
					  (message "jcscope: generation finished.")
					  (jcscope-init jcscope-directory))))))

(defun search-do-find-symbol (symbol-name)
  (interactive (search-symbol-interactive "Find symbol"))
  (jcscope-query "0" symbol-name)
  symbol-name)

(defun search-do-find-symbol-definition (symbol-name)
  (interactive (search-symbol-interactive "Find symbol definition"))
  (jcscope-query "1" symbol-name)
  symbol-name)

(defun search-do-who-i-call (symbol-name)
  (interactive (search-symbol-interactive "Which function do I call" (which-function)))
  (jcscope-query "2" symbol-name)
  symbol-name)

(defun search-do-who-call-me (symbol-name)
  (interactive (search-symbol-interactive "Which function(s) call me" (which-function)))
  (jcscope-query "3" symbol-name)
  symbol-name)

(defun search-do-find-this-text-string (string)
  (interactive (search-symbol-interactive "Find text string"))
  (jcscope-query "4" string)
  string)

(defun search-do-who-include-me ()
  (interactive)
  (jcscope-query "8" (buffer-file-name))
  (buffer-file-name))

(defun search-do-who-assigned-me (symbol-name)
  (interactive (search-symbol-interactive "Who assigned me"))
  (jcscope-query "10" symbol-name)
  symbol-name)

(defvar jcscope-last-search nil)
(defun jcscope-query (type arg)
  (with-current-buffer (get-buffer-create jcscope-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when jcscope-last-search
	(unhighlight-regexp jcscope-last-search))
      (unless (get-buffer-window (current-buffer))
	(display-buffer (current-buffer)))
      (jcscope-mode)
      (highlight-regexp (setq jcscope-last-search (replace-regexp-in-string "\*" "" arg))
			grep-match-face))
    (process-send-string (get-buffer-process jcscope-process-buffer) (concat type arg "\n"))))

(define-compilation-mode jcscope-mode "jcscope"
  (set (make-local-variable 'compilation-error-face) grep-hit-face)
  (setq buffer-invisibility-spec t))

(provide 'jcscope)
