(defun negate (f)
  (lexical-let ((f1 f))
    #'(lambda (&rest args) (not (apply f1 args)))))

(defun is-hidden-file (filepath)
  (let ((basename (file-name-nondirectory filepath)))
    (not (null (string-match "^\\.[^.].*$" basename)))))
(fset 'is-not-hidden-file (negate 'is-hidden-file))

(defun is-dots-file (filepath)
  (let ((basename (file-name-nondirectory filepath)))
    (or (string= "." basename) (string= ".." basename))))

(defun has-suffix (suffix filepath)
  (let ((basename (file-name-nondirectory filepath)))
    (not (null (string-match (concat "^.*" suffix "$") basename)))))
(fset 'has-c-ext #'(lambda (x) (has-suffix ".c" x)))
(fset 'is-o-file #'(lambda (x) (has-suffix ".o" x)))
(fset 'is-backup-file #'(lambda (x) (has-suffix "~" x)))

(fset 'file-not-directory-p (negate 'file-directory-p))

(defun collect-files (dir)
  (let* ((child-names
	  (remove-if 'is-dots-file (directory-files dir)))
	 (child-path
	  (mapcar #'(lambda (x) (concat dir "/" x)) child-names))
	 (dir-path
	  (remove-if 'file-not-directory-p child-path))
	 (file-path
	  (remove-if 'file-directory-p child-path)))
    (let ((files-under-dirs
	   (mapcar 'collect-files dir-path)))
      (reduce
       #'(lambda (acc x) (append acc x))
       files-under-dirs
       :initial-value file-path))))

(defun everyp-iter (f-list &rest args)
  (cond ((null f-list) t)
	(t
	 (let ((result (apply (car f-list) args)))
	   (cond ((not result) nil)
		 (t
		  (apply 'everyp-iter
			 (cons (cdr f-list) args))))))))
(defun everyp-fn (&rest f-list)
  (lexical-let ((f-list-1 f-list))
    #'(lambda (&rest args)
	(apply 'everyp-iter
	       (cons f-list-1 args)))))

(defun anyp-iter (f-list &rest args)
  (cond ((null f-list) nil)
	(t
	 (let ((result (apply (car f-list) args)))
	   (cond ((not (null result)) t)
		 (t
		  (apply 'anyp-iter
			 (cons (cdr f-list) args))))))))
(defun anyp-fn (&rest f-list)
  (lexical-let ((f-list-1 f-list))
    #'(lambda (&rest args)
	(apply 'anyp-iter
	       (cons f-list-1 args)))))
