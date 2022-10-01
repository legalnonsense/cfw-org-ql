;; -*- lexical-binding: t; -*-

;; replace calfw functions with org-ql

(defcustom cfw-org-ql-query (rx "<" (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) " " upper (= 2 lower) (optional (= 2 digit) ":" (= 2 digit) (optional "-" (= 2 digit) ":" (= 2 digit)))) "rx query")


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
  (cl-loop for event in (org-ql-select (org-agenda-files)
			  `(regexp ,cfw-org-ql-query)
			  ;; '(and (or (deadline)
			  ;; 	    (ts-active))
			  ;; 	(not (or (todo "DONE")
			  ;; 		 (todo "done"))))
			  :action #'jrf/calfw-parser)
	   if (cl-struct-slot-value 'cfw:event 'end-date event)
	   collect event into periods
	   else
	   collect event into contents   
	   finally
	   return `(,@contents (periods ,@periods))))

(defun jrf/calfw-parser ()
  (-let* (((&alist "CATEGORY" category
                   "ITEM" headline
                   "TIMESTAMP" timestamp
                   "TIMESTAMP_IA" timestamp-ia
                   "DEADLINE" deadline
                   "SCHEDULED" scheduled
                   "TODO" todo
                   "ALLTAGS" alltags)
           (org-entry-properties)))
    (let (start-date start-time end-date end-time title location description)
      (cl-flet ((timestamp-range? ()
                                  (when timestamp
                                    (if (not (s-match "--" timestamp))
                                        nil
				      (s-split "--" timestamp)))))
        (cond (deadline (setq start-date (jrf/calfw--convert-date-string deadline))
                        (setq start-time (jrf/calfw--convert-date-string deadline 'time)))
	      ((when-let ((dates (timestamp-range?)))
                 (setq start-date (jrf/calfw--convert-date-string (car dates)))
                 (setq start-time (jrf/calfw--convert-date-string (car dates) 'time))
                 (setq end-date (jrf/calfw--convert-date-string (cadr dates)))
                 (setq end-time (jrf/calfw--convert-date-string (cadr dates) 'time))))

	      (timestamp (setq start-date (jrf/calfw--convert-date-string timestamp))
                         (setq start-time (jrf/calfw--convert-date-string timestamp 'time))
			 (when (string-match (rx "-" (group (= 2 digit) ":" (= 2 digit))) timestamp)			   
			   (setq end-date start-date)
			   (setq end-time (cl-loop for each in (s-split ":" (match-string 1 timestamp) t)
						   collect (string-to-number each))))))
	(setq title headline)
	;; (debug nil start-date start-time end-date end-time title)
	(make-cfw:event :title (progn
				 (add-face-text-property 0 (length todo) 'org-todo nil todo)
				 (concat
				  (jrf/calfw--convert-time-to-string start-time)
				  " "
				  todo
				  " "
				  title
				  " (" category ")"))
			:start-date start-date
			:start-time start-time
			:end-date end-date
			:end-time end-time)))))

(provide 'cfw-org-ql)
