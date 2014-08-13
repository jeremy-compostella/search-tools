(require 'intel-shared)

(defvar search-engine-buffer "*search:results*")
(defvar search-engine-history '())

(defun search-in-sources (&optional fun)
  (interactive)
  (let* ((funs (intel-build-fun-list "search-do-"))
	 (fun (or fun (assoc-default
		       (ido-completing-read "Search method: "
					    (mapcar 'car funs)) funs))))
    (push (cons fun (call-interactively fun)) search-engine-history)))

(defun search-symbol-interactive (prefix &optional default)
  (let* ((sym (symbol-at-point))
	 (default (if default default (when sym (symbol-name sym)))))
    (list (read-string (if default
			   (format "%s (default %s): " prefix default)
			 (format "%s: " prefix))
		       nil 'search-engine-history
		       default))))

(defvar search-engine-marker-ring-length 100)
(defvar search-engine-marker-ring-undo (make-ring search-engine-marker-ring-length))
(defvar search-engine-marker-ring-redo (make-ring search-engine-marker-ring-length))

(defun search-engine-forward (&optional N)
  (interactive)
  (unless (= (or N 1) 0)
    (let* ((undo (if (< N 0) search-engine-marker-ring-undo search-engine-marker-ring-redo))
	   (redo (if (< N 0) search-engine-marker-ring-redo search-engine-marker-ring-undo))
	   (count (1- (abs (or N 1))))
	   (marker (search-engine-pop-mark undo)))
      (while (and marker (> (decf count) 0))
	(setf marker (search-engine-pop-mark undo)))
      (when marker
	(search-engine-push-mark redo)
	(switch-to-buffer (marker-buffer marker))
	(goto-char (marker-position marker))))))

(defun search-engine-backward (&optional N)
  (interactive)
  (search-engine-forward (- (or N 1))))

(defun search-engine-pop-mark (ring)
  (if (ring-empty-p redo)
      (message "Search engine marker history ring is empty.")
    (let ((marker (ring-remove ring 0)))
	(if (not (bufferp (marker-buffer marker)))
	    (search-engine-pop-mark)
	  marker))))

(defun search-engine-push-mark (ring)
  (ring-insert ring (point-marker)))

(defvar search-engine-preview-wc nil)

(defadvice next-error (before next-error-mark
			      (&optional arg reset))
  "Push mark before jumping on the next error."
  (search-engine-push-mark search-engine-marker-ring-undo))
(ad-activate 'next-error)

(defun search-engine-preview-previous (&optional N)
  (interactive)
  (search-engine-preview-next (- (or N 1))))

(defun search-engine-preview-next (&optional N)
  (interactive)
  (unless search-engine-preview-wc
    (setq search-engine-preview-wc (current-window-configuration)))
  (next-error-no-select (or N 1)))

(defun search-engine-select ()
  (interactive)
  (message "Search-Engine-Select")
  (when (window-configuration-p search-engine-preview-wc)
    (set-window-configuration search-engine-preview-wc)
    (setq search-engine-preview-wc nil))
  (next-error 0))

(defvar search-engine-mode-map nil
  "Keymap for search-engine mode.")
(unless search-engine-mode-map
  (setq search-engine-mode-map (make-sparse-keymap))
  (define-key search-engine-mode-map (kbd "p") 'search-engine-preview-previous)
  (define-key search-engine-mode-map (kbd "n") 'search-engine-preview-next)
  (define-key search-engine-mode-map (kbd "<return>") 'search-engine-select)
  (define-key search-engine-mode-map (kbd "C-c C-p") 'search-engine-history-previous)
  (define-key search-engine-mode-map (kbd "C-c C-n") 'search-engine-history-next))

(define-compilation-mode search-engine-mode "search-engine"
  (set (make-local-variable 'compilation-error-face) grep-hit-face)
  (setq buffer-invisibility-spec t))

(provide 'search-engine)
