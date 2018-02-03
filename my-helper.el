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
(defun bind (fn &rest args)
  (lexical-let ((fn-1 fn)
		(build-time-args args))
    #'(lambda (&rest run-time-args)
	(apply fn-1 (append build-time-args run-time-args)))))
(defun enqueue-region (queue)
  (let ((region-string (buffer-substring-no-properties (region-beginning)
						       (region-end))))
    (cons region-string queue)))
(defun child-path-list (root)
  (mapcar (bind 'concat root "/")
	  (remove-if #'(lambda (x) (or (string= "." x)
				       (string= ".." x)))
		     (directory-files root))))
(defun child-directories (root)
  (remove-if (negate 'file-directory-p) (child-path-list root)))
(defun child-files (root)
  (remove-if 'file-directory-p (child-path-list root)))
(defun compose (f-arg &rest g-arg)
  (lexical-let ((f f-arg)
		(g g-arg))
    (cond ((null g) f)
	  ((= 1 (length g)) #'(lambda (&rest args) (funcall f (apply (car g) args))))
	  (t (compose (compose f (car g)) (cdr g))))))
