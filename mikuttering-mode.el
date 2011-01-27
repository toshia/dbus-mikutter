(defconst mikuttering-post-buffer-name "mikuttering post")
(defconst mikuttering-dbus-machine-id-file "/var/lib/dbus/machine-id")
(defconst mikuttering-dbus-mikutter-service "org.mikutter.service")

(defvar mikuttering-post-buffer nil)
(defvar mikuttering-dbus-bus nil)

(defun mikuttering-dbus-init ()
  (setq mikuttering-dbus-bus
	(if (member mikuttering-dbus-mikutter-service
		    (dbus-list-known-names :session))
	    :session
	  ;; Emacs が session bus につなぎにいけないおばかさんなので自分でがんばる
	  (unless (string-match "\\`:\\(.+\\)$"
				x-display-name)
	    (error "Failed to determine bus address"))
	  (let* ((display-name (match-string 1 x-display-name))
		 (machine-id
		  (with-temp-buffer
		    (insert-file-contents mikuttering-dbus-machine-id-file)
		    (goto-char (point-min))
		    (buffer-substring (point-min)
				      (progn
					(search-forward "\n")
					(1- (point))))))
		 (filename (expand-file-name
			    (concat machine-id "-" display-name)
			    "~/.dbus/session-bus")))
	    (with-temp-buffer
	      (unless (file-exists-p filename)
		(error "Failed to determine bus address"))
	      (insert-file-contents filename)
	      (goto-char (point-min))
	      (unless (re-search-forward
		       "^DBUS_SESSION_BUS_ADDRESS=\\(.*\\)$" nil t)
		(error "Failed to determine bus address"))
	      (let ((bus (match-string 1)))
		(unless (member mikuttering-dbus-mikutter-service
				(dbus-list-known-names bus))
		  (error "Failed to determine bus address"))
		bus))))))

(defun mikuttering-dbus-call (method &rest args)
  (unless mikuttering-dbus-bus
    (mikuttering-dbus-init))
  (dbus-call-method mikuttering-dbus-bus
		    mikuttering-dbus-mikutter-service
		    "/org/mikutter/MyInstance"
		    "org.mikutter.events.timeline"
		    method
		    args))

(defun mikuttering-post ()
  (interactive)
  (setq mikuttering-post-buffer
	(get-buffer-create mikuttering-post-buffer-name))
  (pop-to-buffer mikuttering-post-buffer)
  (mikuttering-post-mode))

(define-derived-mode mikuttering-post-mode text-mode
  "mikuttering post"
  (use-local-map mikuttering-post-mode-map))

(when mikuttering-post-mode-map
  (let ((map mikuttering-post-mode-map))
    (define-key map "\C-c\C-c" 'mikuttering-post-send)
    (define-key map "\C-c\C-k" 'mikuttering-post-discard)
    (setq mikuttering-post-mode-map map)))

(defun mikuttering-post-send ()
  (interactive)
  (when (eq major-mode 'mikuttering-post-mode)
    (mikuttering-dbus-call "post"
			   (buffer-string))
    (erase-buffer)
    (set-buffer-modified-p nil)))

(defun mikuttering-post-discard ()
  (interactive)
  (when (eq major-mode 'mikuttering-post-mode)
    (kill-buffer)))
