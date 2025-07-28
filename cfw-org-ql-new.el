;; -*- lexical-binding: t; -*-

;; replace calfw functions with org-ql

(require 'calfw)
;; TODO FIX THIS TO ALLOW FOR REPEATERS AND MAKE SURE IT MATCHES ALL TYPES OF TIMESTAMPS
;; 
(defcustom cfw-org-ql-query
  (rx "<"
      (= 4 digit)
      "-"
      (= 2 digit)
      "-"
      (= 2 digit)
      (optional (seq " "
		     upper
		     (= 2 lower)))
      (optional (seq " "
		     (= 2 digit)
		     ":"
		     (= 2 digit)			
		     (optional (seq "-"
				    (= 2 digit)
				    ":"
				    (= 2 digit)))))
      (optional (seq " "
		     (or ".+"
			 "--"
			 "-"
			 "+"
			 "++")
		     digit
		     letter))
      ">")
  "rx query")

(defun jrf/open-calfw ()
  (interactive)
  (setq cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap)
  (tab-bar-new-tab 1)
  (cfw:open-calendar-buffer
   :date (mapcar #'string-to-number
                 (s-split " " (ts-format "%m 1 %Y" (ts-now))))
   :contents-sources
   (list (make-cfw:source
          :name "" :data 'jrf/calfw-make-data))))

(defun jrf/calfw--convert-date-string (date &optional time)
  "Converts an org date string to '(d m yyyy)"
  (let ((date (org-parse-time-string date t)))
    (if time
        (when (nth 2 date)
          `(,(nth 2 date) ,(nth 1 date)))
      (list (nth 4 date)
            (nth 3 date)
            (nth 5 date)))))

(defun jrf/calfw--convert-time-to-string (time)
  (when time 
    (concat 
     (s-pad-left 2 "0" (number-to-string (car time)))
     ":"
     (s-pad-left 2 "0" (number-to-string (cadr time))))))

(defun jrf/calfw-make-data (b e)
  (setq org-ql-cache (make-hash-table :weakness 'key))
  (cl-loop for event in (-flatten (org-ql-select (org-agenda-files)
				    `(and (regexp ,cfw-org-ql-query)
					  (not (or (todo "CANCELLED")
						   (todo "DONE")))
					  )
				    ;; '(and (or (deadline)
				    ;; 	    (ts-active))
				    ;; 	(not (or (todo "DONE")
				    ;; 		 (todo "done"))))
				    :action #'jrf/calfw-parser))
	   if (cl-struct-slot-value 'cfw:event ' end-date event)
	   collect event into periods
	   else
	   collect event into contents   
	   finally
	   return `(,@contents (periods ,@periods))))

(defun jrf/cfw-org-ql--timestamp-parser (type &optional upper-limit lower-limit)
  "TYPES can be:
  active - first active timestamp 
  active-range - first active range 
  active-all - all active timestamps 
  inactive - first inactive timestamp 
  inactive-range - first inactive range 
  inactive-all - all inactive timestamps 
  clock - all clock time stamps 
  planning - deadline, scheduled, closed
  deadline
  scheduled
  closed
  all - everything
Returns an alist.

UPPER-LIMIT and LOWER-LIMIT are the bounds of the region searched."
  (let (timestamps)
    (setq upper-limit (pcase upper-limit
			((pred functionp) (funcall upper-limit))
			(`root (save-excursion (while (org-up-heading-safe))
					       (point)))
			(`nil (org-entry-beginning-position))
			(`file (point-min)))
	  lower-limit (pcase lower-limit
			((pred functionp (funcall lower-limit)))
			(`subtree  (save-excursion (org-end-of-subtree)))
			(`nil (org-entry-end-position))
			(`file (point-max))))
    
    (save-excursion 
      (cl-flet* ((activep (time-string) (equal "<" (substring time-string 0 1)))
		 (rangep (time) (if (stringp time)
				    (> (length (s-split "--" time)) 1)
				  (> (length time) 1)))
		 (clock-line-p nil (save-match-data (org-at-clock-log-p)))
		 (planning-line-p nil (save-match-data (org-at-planning-p)))
		 (process-time
		  (time-string)
		  (let ((x (s-split "--" time-string)))
		    (pcase (list (activep (car x)) (rangep x))
		      (`(t nil)
		       (push (substring (car x) 1 -1)
			     (alist-get
			      'active
			      timestamps)))
		      (`(nil nil)
		       (if (clock-line-p)
			   (push (cons (substring (car x) 1 -1) nil)
				 (alist-get 'clock timestamps))
			 (push  (substring (car x) 1 -1) (alist-get
							  'inactive
							  timestamps))))
		      (`(t t)
		       (push (cons
			      (substring
			       (car x) 1 -1)
			      (substring
			       (cadr x) 1 -1))
			     (alist-get 'active-range timestamps)))
		      (`(nil t)
		       (if (and (clock-line-p)
				(member type '(clock all)))
			   (push 
			    (cons (substring (car x) 1 -1)
				  (substring (cadr x) 1 -1))
			    (alist-get 'clock timestamps))
			 (push 
			  (cons (substring (car x) 1 -1)
				(substring (cadr x) 1 -1))
			  (alist-get 'inactive-range timestamps)))))))
		 (get-deadline nil
			       (push 
				(when-let ((dl
					    (org-entry-get
					     (point)
					     "DEADLINE")))
				  (substring dl 1 -1))
				(alist-get 'deadline timestamps)))
		 (get-scheduled nil
				(push
				 (when-let ((sc
					     (org-entry-get
					      (point)
					      "SCHEDULED")))
				   (substring sc 1 -1))
				 (alist-get 'scheduled timestamps)))
		 (get-closed nil
			     (push 
			      (when-let (cl
					 (org-entry-get
					  (point)
					  "CLOSED"))
				(substring cl 1 -1))
			      (alist-get 'closed timestamps)))
		 (get-times
		  nil
		  (goto-char upper-limit)
		  (cl-loop with time = nil
			   
			   while (re-search-forward
				  (rx (group-n 1
					       (or "<" "[")
					       (= 4 digit)
					       "-"
					       (= 2 digit)
					       "-"
					       (= 2 digit)
					       (opt " " (*\? nonl))
					       (or ">" "]"))

				      (optional (or "-" "--"))
				      (optional
				       (group-n 2
						(or "<" "[")
						(= 4 digit)
						"-"
						(= 2 digit)
						"-"
						(= 2 digit)
						(opt " " (*\? nonl))
						(or ">" "]"))))
				  lower-limit
				  t)

			   do (setq time (match-string-no-properties 0))
			   
			   unless (or
				   (planning-line-p)
				   (and (not (member type '(active-ranges
							    inactive-ranges
							    clock
							    all)))
					(rangep time))
				   (and (member type '(active-ranges
						       inactive-ranges))
					(or (not (rangep time))
					    (clock-line-p)))
				   (and (member type '(active active-ranges))
					(not (activep time)))
				   (and (member type '(inactive inactive-ranges))
					(activep time))
				   (and (eq type 'clock)
					(not (clock-line-p))))

			   ;; everything is by side effect
			   ;; on `timestamps'
			   do (process-time time))))
	(pcase type
	  (`deadline (get-deadline))
	  (`scheduled (get-scheduled))
	  (`closed (get-closed))
	  (`planning (get-deadline)
		     (get-scheduled)
		     (get-closed))
	  (`all (get-deadline)
		(get-scheduled)
		(get-closed)
		(get-times))
	  (_ (get-times)))

	(cl-loop for (type . times) in timestamps
		 if (and (= (length times) 1)
			 (not (consp times)))
		 collect (cons type (car times))
		 else
		 collect (cons type (reverse times)))))))

(defun jrf/calfw-parser ()
  (-let* (((&alist "CATEGORY" category
                   "ITEM" headline
                   "TIMESTAMP" timestamp
                   "TIMESTAMP_IA" timestamp-ia
                   "DEADLINE" deadline
                   "SCHEDULED" scheduled
                   "TODO" todo
                   "ALLTAGS" alltags)
           (org-entry-properties))
	  timestamp timestamp-range)
    (setq timestamp (alist-get 'active (jrf/cfw-org-ql--timestamp-parser 'all)))
    (setq timestamp-range (alist-get 'active-range (alist-get 'active (jrf/cfw-org-ql--timestamp-parser 'all))))
    (let (start-date
	  start-time
	  end-date
	  end-time
	  title
	  location
	  description
	  marker)
      (setq marker (point-marker))
      (cl-flet ((timestamp-range? ()
                  (when timestamp
                    (if (not (s-match "--" timestamp))
                        nil
		      (s-split "--" timestamp)))))
	(-non-nil 
	 (cl-loop for type in '(deadline scheduled timestamp timestamp-range)
		  collect (cond
			   ((and (eq type 'deadline) deadline)
			    (setq start-date (jrf/calfw--convert-date-string deadline))
                            (setq start-time (jrf/calfw--convert-date-string deadline 'time))

			    (setq title headline)
			    ;; (debug nil start-date start-time end-date end-time title)
			    (make-cfw:event :title (progn
						     (add-face-text-property 0 (length todo) 'org-todo nil todo)
						     (propertize 
						      (concat
						       (jrf/calfw--convert-time-to-string start-time)
						       " "
						       todo
						       " "
						       title
						       " (" category ")")
						      'marker
						      marker
						      ))
					    :start-date start-date
					    :start-time start-time
					    :end-date end-date
					    :end-time end-time))
			   ((and (eq type 'scheduled) scheduled)
			    (setq start-date (jrf/calfw--convert-date-string scheduled))
                            (setq start-time (jrf/calfw--convert-date-string scheduled 'time))
			    (setq title headline)
			    ;; (debug nil start-date start-time end-date end-time title)
			    (make-cfw:event :title (progn
						     (add-face-text-property 0 (length todo) 'org-todo nil todo)
						     (propertize 
						      (concat
						       (jrf/calfw--convert-time-to-string start-time)
						       " "
						       todo
						       " "
						       title
						       " (" category ")")
						      'marker
						      marker
						      ))
					    :start-date start-date
					    :start-time start-time
					    :end-date end-date
					    :end-time end-time))
			   ((and (eq type 'timestamp) timestamp)
			    (cl-loop for tt in timestamp
				     append (progn (setq start-date (jrf/calfw--convert-date-string timestamp))
						   (setq start-time (jrf/calfw--convert-date-string timestamp 'time))
						   (when (string-match (rx "-" (group (= 2 digit) ":" (= 2 digit)))
								       timestamp)			   
						     (setq end-date start-date)
						     (setq end-time (cl-loop for each in (s-split ":" (match-string 1 timestamp) t)
									     collect (string-to-number each))))
						   (setq title headline)
						   ;; (debug nil start-date start-time end-date end-time title)
						   (make-cfw:event :title (progn
									    (add-face-text-property 0 (length todo) 'org-todo nil todo)
									    (propertize 
									     (concat
									      (jrf/calfw--convert-time-to-string start-time)
									      " "
									      todo
									      " "
									      title
									      " (" category ")")
									     'marker
									     marker
									     ))
								   :start-date start-date
								   :start-time start-time
								   :end-date end-date
								   :end-time end-time))))
			   ((and (eq type 'timestamp-range) timestamp-range
				 (cl-loop for tt in timestamp-range
					  append (progn (setq start-date (jrf/calfw--convert-date-string (car dates)))
							(setq start-time (jrf/calfw--convert-date-string (car dates) 'time))
							(setq end-date (jrf/calfw--convert-date-string (cadr dates)))
							(setq end-time (jrf/calfw--convert-date-string (cadr dates) 'time))
							(make-cfw:event :title (progn
									    (add-face-text-property 0 (length todo) 'org-todo nil todo)
									    (propertize 
									     (concat
									      (jrf/calfw--convert-time-to-string start-time)
									      " "
									      todo
									      " "
									      title
									      " (" category ")")
									     'marker
									     marker
									     ))
								   :start-date start-date
								   :start-time start-time
								   :end-date end-date
								   :end-time end-time))))))))))))


(defun cfw-org-ql-goto-entry-in-org-file ()
  "goto entry"
  (interactive)
  (when-let ((marker (get-text-property (point) 'marker)))
    (org-goto-marker-or-bmk marker)
    (org-show-entry)))


(define-key cfw:calendar-mode-map (kbd "RET") #'cfw-org-ql-goto-entry-in-org-file)
(provide 'cfw-org-ql)
