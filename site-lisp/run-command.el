;;; run-command.el --- Library to facilitate running external commands

;; Author:  Peter Breton
;; Created: Tue Nov 09 1999
;; Version: $Id: run-command.el,v 1.8 2001/09/05 16:25:50 pbreton Exp $
;; Keywords:
;; Time-stamp: <2001-05-24 17:10:56 pbreton>

;;; Commentary:

;;; Change log:
;; $Log: run-command.el,v $
;; Revision 1.8  2001/09/05 16:25:50  pbreton
;; Tweak
;;
;; Revision 1.7  2000/11/06 19:41:37  pbreton
;; Added run-command-hooks and run-command-comint-hooks
;;
;; Revision 1.6  2000/10/13 16:51:35  pbreton
;; For run-command-as-comint, switch to designated buffer if it already exists,
;; instead of automatically nuking it.
;;
;; Revision 1.5  2000/09/04 14:53:45  pbreton
;; Made commands interactive
;;
;; Revision 1.4  2000/06/23 15:31:31  pbreton
;; In run-command-process-sentinel, ensure that process buffer still exists
;;
;; Revision 1.3  2000/03/15 16:51:18  pbreton
;; Removed message call
;; Added require comint statement
;;
;; Revision 1.2  2000/03/03 13:03:30  pbreton
;; Added filter argument to run-command functions and run-command-internal.
;; If present, filter is used as as comint-output-filter-functions
;; Removed run-command-argument-list macro
;; Added run-command-program and run-command-program-arguments
;; Enhanced process sentinel
;;
;; Revision 1.1  1999/11/09 23:30:27  pbreton
;; Initial version
;;
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar run-command-display-output-buffer t
  "If non-nil, display the results of the command in a buffer.")

(defvar run-command-echo-commands nil
  "If non-nil, echo the command lines to `run-command-echo-command-buffer'.")

(defvar run-command-echo-command-buffer "*Run Command*"
  "Buffer to echo command lines.")

(defvar run-command-callback nil
  "Callback to invoke when a process completes.
The function will be invoked in the output buffer.")

(make-variable-buffer-local 'run-command-callback)

(defvar run-command-completed nil
  "Non-nil if an asynchronous command has completed.")

(defvar run-command-program nil
  "The program that was run.")

(make-variable-buffer-local 'run-command-program)

(defvar run-command-program-arguments nil
  "Arguments to the program that was run.")

(make-variable-buffer-local 'run-command-program-arguments)

(defvar run-command-history-list nil
  "History list for running commands.")

(defvar run-command-argument-history-list nil
  "History list for run command arguments.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-command-process-sentinel (process state)
  "Runs a callback when PROCESS exits."
  (and (buffer-live-p (process-buffer process))
       (with-current-buffer (process-buffer process)
	 (and (or (string= state "finished\n")
		  (string-match "^exited" state)
		  (string-match "^kill" state)
		  (string-match "^SIG" state))
	      (progn
		(setq run-command-completed t)
		(and run-command-callback
		     (funcall run-command-callback)))
	      ))))

;; Internal workhorse function
(defun run-command-internal (buffer program type start end callback filter &rest args)
  "In BUFFER, run PROGRAM with ARGS. Return BUFFER.
If BUFFER is t, run program in current buffer.

If type is 'synchronous, the command is run as a synchronous process;
START, END, CALLBACK and FILTER are ignored.

If type is 'synchronous-region, the command is run as a synchronous process
on the region defined by START and END; CALLBACK and FILTER are ignored.

If type is 'comint, the command is run as a comint process;
START, END and CALLBACK are ignored. If FILTER is present, it is
used as a comint filter, or list of filters.

If type is 'asynchronous, the command is run as an asynchronous process;
if callback is non-nil, it is invoked when the command completes.
START, END and FILTER are ignored.

The run-command-* functions provide convenience methods for running
commands of the correct type.
"
  (let ((options args)
	(orig-buffer (current-buffer))
	(output-buffer
	 (if (eq buffer 't)
	     (current-buffer)
	   (get-buffer-create buffer)))
	original-filter-functions
	)
    ;; Go to the buffer
    (save-excursion
      (set-buffer output-buffer)
      (or (eq buffer 't)
	  (erase-buffer))

      (run-hooks 'run-command-hooks)

      ;; Figure out what to do, process-wise
       (cond

	;; Simple synchronous call
	((eq type 'synchronous)
	 (apply 'call-process program nil t nil args))

       ;; Synchronous using region
       ((eq type 'synchronous-region)
	(with-current-buffer orig-buffer
	    (apply 'call-process-region start end program nil
		   output-buffer nil options)))

       ;; Asynchronous
       ((eq type 'asynchronous)
	(setq run-command-callback callback)
	(make-local-variable 'run-command-completed)
	(setq run-command-completed nil)
	(set-process-sentinel
	 (apply 'start-process (buffer-name output-buffer)
		output-buffer program options)
	 'run-command-process-sentinel))

       ;; Comint
       ((eq type 'comint)
	(or (get-buffer-process (current-buffer))
	    (progn
	      (require 'comint)
	      (comint-mode)
	      (run-hooks 'run-command-comint-hooks)
	      (comint-exec
	       output-buffer
	       (buffer-name output-buffer)
	       program
	       nil
	       options)
	      (and filter
		   (with-current-buffer output-buffer
		     (setq original-filter-functions
			   comint-output-filter-functions)
		     (make-local-hook 'comint-output-filter-functions)
		     (mapcar
		      (function
		       (lambda(element)
			 (or (member element original-filter-functions)
			     (add-hook
			      'comint-output-filter-functions element t t))
			 )
		       )
		      (if (listp filter) filter (list filter)))
		     )))
	    ))
       (t
	(error "Unknown type %s" type)))

      ;; Keep track of how we were invoked
      (setq run-command-program program)
      (setq run-command-program-arguments options)

      ;; Log the invokation
      (and run-command-echo-commands
	   (with-current-buffer
	       (get-buffer-create run-command-echo-command-buffer)
	     (goto-char (point-max))
	     (insert
	      "[Command Line: \""
	      (mapconcat 'identity
			 (append (list program) options) " ")
	      "\"]\n")))

      ;; Display the output
      (and run-command-display-output-buffer
	   (progn
	     (goto-char (point-min))
	     (display-buffer output-buffer)))

      ;; Return the output buffer
      output-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst run-command-read-arguments  (buf)
  "Read the arguments for a run command interactive call.
Use BUF as buffer name."
  (let (arguments)
    (apply
     'list
     (read-buffer "Run command in buffer: " buf)
     (read-from-minibuffer "Run command (no arguments): " nil nil nil 'run-command-history-list)
     (progn
       (setq arguments
	     (read-from-minibuffer "With arguments: " nil nil nil
				   'run-command-argument-history-list))
       (and arguments (split-string arguments))))))

;;;###autoload
(defun run-command (buffer program &rest args)
  "In BUFFER, run PROGRAM with ARGS."
  (interactive
   (run-command-read-arguments "*Run Command*"))
  (apply 'run-command-internal buffer program 'synchronous nil nil nil nil args))

;;;###autoload
(defun run-command-region (start end buffer program &rest args)
  "In BUFFER, run PROGRAM on region with ARGS."
  (interactive
   (apply 'list
    (region-beginning)
    (region-end)
    (run-command-read-arguments "*Run Command Region*")
    ))
   (apply 'run-command-internal buffer program 'synchronous-region start end
	  nil nil args))

;;;###autoload
(defun run-command-as-comint (buffer program &rest args)
  "In BUFFER, run PROGRAM with ARGS as a comint process."
  (interactive
   (run-command-read-arguments "*Run Command Comint*"))
  (if (get-buffer buffer)
      (switch-to-buffer buffer)
  (apply 'run-command-internal buffer program 'comint nil nil nil nil args)))

;;;###autoload
(defun run-command-as-comint-with-filter (buffer program filter &rest args)
  "In BUFFER, run PROGRAM with ARGS as a comint process.
If optional argument FILTER is present, use as comint filter function."
  (apply 'run-command-internal buffer program 'comint nil nil nil filter args))

;;;###autoload
(defun run-command-asynchronously (buffer program &rest args)
  "In BUFFER, run PROGRAM asynchronously with ARGS."
  (interactive
   (run-command-read-arguments "*Run Command Asynchronously*"))
  (apply 'run-command-internal buffer program 'asynchronous
	 nil nil nil nil args))

;;;###autoload
(defun run-command-asynchronously-with-callback (buffer program &optional callback &rest args)
  "In BUFFER, run PROGRAM asynchronously with ARGS.
When the process finishes, invoke CALLBACK if non-nil."
  (apply 'run-command-internal buffer program 'asynchronous
	 nil nil callback nil args))

(provide 'run-command)

;;; run-command.el ends here

;; Local Variables:
;; autocompile: t
;; End:
