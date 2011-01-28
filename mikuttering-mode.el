(defconst mikuttering-post-buffer-name "mikuttering post")
(defconst mikuttering-dbus-machine-id-file "/var/lib/dbus/machine-id")
(defconst mikuttering-dbus-mikutter-service "org.mikutter.service")
(defconst mikuttering-dbus-mikutter-path "/org/mikutter/MyInstance")
(defconst mikuttering-dbus-mikutter-interface "org.mikutter.events.timeline")

(defvar mikuttering-reply-id nil)
(defvar mikuttering-reply-user nil)
(defvar mikuttering-reply-contents nil)
(defvar mikuttering-post-buffer nil)
(defvar mikuttering-dbus-bus nil)

(defvar mikuttering-timeline-update-signal-object nil)

(defvar mikuttering-recent-timeline-count 100)
(defvar mikuttering-recent-timeline nil)
(defvar mikuttering-recent-reply nil)

(defun mikuttering-timeline-update-handler (msg)
  (let* ((standard-input msg)
	 (msgobj (read)))
    (setq mikuttering-recent-timeline
	  (cons msgobj
		mikuttering-recent-timeline))
    (when (< mikuttering-recent-timeline-count
	     (length mikuttering-recent-timeline))
      (setf (nthcdr mikuttering-recent-timeline-count
		    mikuttering-recent-timeline)
	    nil))))

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
		bus)))))
  (setq mikuttering-timeline-update-signal-object
	(dbus-register-signal mikuttering-dbus-bus
			      mikuttering-dbus-mikutter-service
			      mikuttering-dbus-mikutter-path
			      mikuttering-dbus-mikutter-interface
			      "timeline_update"
			      'mikuttering-timeline-update-handler)))

(defun mikuttering-dbus-call (method &rest args)
  (unless mikuttering-dbus-bus
    (mikuttering-dbus-init))
  (apply 'dbus-call-method mikuttering-dbus-bus
	 mikuttering-dbus-mikutter-service
	 mikuttering-dbus-mikutter-path
	 mikuttering-dbus-mikutter-interface
	 method
	 args))

(defun mikuttering-post (&optional in-reply-to-id reply-user reply-contents)
  (interactive)
  (setq mikuttering-post-buffer
	(get-buffer-create mikuttering-post-buffer-name))
  (pop-to-buffer mikuttering-post-buffer)
  (mikuttering-post-mode)
  (setq mikuttering-reply-id in-reply-to-id
	mikuttering-reply-user reply-user
	mikuttering-reply-contents reply-contents)
  (let ((inhibit-read-only t)) (erase-buffer))
  (when (or reply-user reply-contents)
    (when reply-contents
      (insert
       (propertize
	(concat reply-contents "\n---\n")
	'read-only t 'rear-nonsticky t 'front-sticky t)))
    (when reply-user
      (insert "@" reply-user " "))))

(define-derived-mode mikuttering-post-mode text-mode
  "mikuttering post"
  (use-local-map mikuttering-post-mode-map)
  (make-local-variable 'mikuttering-reply-id)
  (make-local-variable 'mikuttering-reply-user)
  (make-local-variable 'mikuttering-reply-contents))

(when mikuttering-post-mode-map
  (let ((map mikuttering-post-mode-map))
    (define-key map "\C-c\C-c" 'mikuttering-post-send)
    (define-key map "\C-c\C-k" 'mikuttering-post-discard)
    (define-key map "\C-c\C-y" 'mikuttering-post-cite)
    (define-key map "\C-c\C-m" 'mikuttering-post-retweet)
    (setq mikuttering-post-mode-map map)))

(defun mikuttering-post-send ()
  (interactive)
  (when (eq major-mode 'mikuttering-post-mode)
    (let ((pos (or (next-single-property-change
		    (point-min) 'read-only)
		   (point-min))))
      (mikuttering-dbus-call "post"
			     (buffer-substring pos (point-max))
			     (if mikuttering-reply-id
				 (number-to-string mikuttering-reply-id)
			       "0"))
      (let ((inhibit-read-only t)) (erase-buffer))
      (setq mikuttering-reply-id nil
	    mikuttering-reply-user nil
	    mikuttering-reply-contents nil)
      (set-buffer-modified-p nil))))

(defun mikuttering-post-discard ()
  (interactive)
  (when (eq major-mode 'mikuttering-post-mode)
    (kill-buffer)))

(defun mikuttering-post-cite ()
  (interactive)
  (when (eq major-mode 'mikuttering-post-mode)
    (save-excursion
      (goto-char (point-max))
      (insert "RT " mikuttering-reply-contents))))

(defun mikuttering-post-retweet ()
  )

(defun mikuttering-mobj-to-string (mobj)
  (let ((user (cadr (assq 'user mobj)))
	(msg (cadr (assq 'message mobj))))
    (concat "@" user ": " msg)))
  
(when (featurep 'anything)
  (defun mikuttering-anything ()
    (interactive)
    (let* ((source-action
	    '((name . "Action")
	      (candidates . ("post"))
	      (action . (("Action" 
			  . (lambda (x) (mikuttering-post)))))))
	   (source-recent-timeline
	    '((name . "Recent Timeline")
	      (candidates 
	       . (lambda ()
		   (mapcar (lambda (m)
			     (cons (mikuttering-mobj-to-string m)
				   m))
			   mikuttering-recent-timeline)))
	      (action . (("Reply"
			  . (lambda (m)
			      (mikuttering-post
			       (cadr (assq 'id m))
			       (cadr (assq 'user m))
			       (mikuttering-mobj-to-string m))))))
	      (migemo))))
      (anything
       '(source-action
	 source-recent-timeline)
       nil "Mikuttering: " nil nil))))

