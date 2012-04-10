;; simple-http-request.lisp

(in-package :cl-web-utils)

(defmacro simple-http-request (url args &key all-args-p additional-headers (convert-dash t)
                               want-stream authorization camel-caps cookie-jar method)
  (flet ((down (el)
	   (awhen (string-downcase el)
	     (cond
	       (camel-caps (camel-caps it))
	       (convert-dash (substitute #\_ #\- it))
	       (t it)))))
    (let ((remote-args
           (if all-args-p
               (loop for arg in args
                     unless (eq arg '&key)
                       collect (if (consp arg)
                                   (cons (car arg) (format nil "&~a=~~a" (down (car arg))))
                                   (cons arg (format nil "~~@[&~a=~~a~~]" (down arg)))))
               (loop with after-key
                     for arg in args
                     if (eq arg '&key) do (setf after-key t)
                       else if (not after-key)
                              collect (cons arg nil)
                     else if (consp arg) collect
                       (cons (car arg)
                             (format nil "&~a=~~a" (down (car arg))))
                     else collect (cons arg (format nil "~~@[&~a=~~a~~]" (down arg)))))))
      `(destructuring-bind (rtn &optional code &rest rest)
           (multiple-value-list
               (http-request
                (format
                 nil
                 ,(format nil "~a?~{~@[~a~]~}" url (mapcar #'cdr remote-args) )
                 ,@(loop for arg in remote-args
                         if (http-arg-no-encode (car arg))
                           collect `(awhen ,(http-arg-no-encode-base (car arg))
                                      (if (stringp it) it (princ-to-string it)))
                         else
                           collect `(awhen ,(car arg)
                                      (url-encode (if (stringp it) it
                                                    (princ-to-string it)) :utf-8))))
                :external-format-out :utf-8
                :external-format-in :utf-8
                ,@(when additional-headers
                    `(:additional-headers ,additional-headers))
                ,@(when want-stream '(:want-stream t))
                ,@(when authorization '(:basic-authorization (list username password)))
                ,@(when cookie-jar `(:cookie-jar ,cookie-jar))
                ,@(when method `(:method ,method))))
         (values-list
          `(,(when (eql code 200)
               ,(if want-stream 'rtn
                  '(if (typep rtn 'string)
                    rtn
                    (byte-array-to-ascii-string rtn))))
             ,code
             ,@rest
             ,(when (not (eql code 200)) rtn)))))))

(defun http-arg-no-encode (arg)
  "Returns true if ``arg'' is a symbol and if the first character of
it is an underscore. "
  (and (symbolp arg) (char= (aref (symbol-name arg) 0) #\_)))

(defun http-arg-no-encode-base (arg)
  "Returns the name of the ``arg'' (without the leading underscore)."
  (intern (subseq (symbol-name arg) 1)))

(defun camel-caps (str)
  "Converts ``str'', a string using dashes as separator, into a camel
capped string.

Example: hello-world-how-are-you
Output:  helloWorldHowAreYou"
  (apply #'concatenate 'string (loop for el in (split-sequence #\- str)
				     for start = t then nil
				     collect (if start el (string-capitalize el)))))

(defmacro define-http-request (name args url &key additional-headers
                               (convert-dash t) camel-caps
                               cookie-jar)
  "Defines a new function named ``name'' that uses ``args'' as its
argument list. The ``additional-headers'' is an assoc-list of settings
for headers. When ``convert-dash'' is true, dashes are converted to
underscores. When ``camel-caps'' is true, converts variable names to
camel capped names. The ``cookie-jar'' is a container for cookies."
  `(defun ,name ,args
     (simple-http-request ,url ,args
                          :additional-headers ,additional-headers
                          :convert-dash ,convert-dash :camel-caps ,camel-caps
                          :cookie-jar ,cookie-jar)))

(defmacro define-json-request (name args url &key (convert-dash t)
                               trim-callback method all-args-p)
  "Defines a new function named ``name'' that uses ``args'' (encoded
and un-encoded arguments) as its argument list. The ``url'' is the URL
to use. When ``convert-dash'' is true, dashes are converted to
underscores. When ``trim-callback'' is true, the function
TRIM-CALLBACK is called with the argument being the output of
FROM-JSON; otherwise returns the output of FROM-JSON.

Sets an additional header 'Accept' to 'applicaton/json'."
  `(defun ,name ,(loop for arg in args
                       if (http-arg-no-encode arg)
                         collect (http-arg-no-encode-base arg)
                       else collect arg)
     (destructuring-bind (rtn code &rest rest)
         (multiple-value-list
             (simple-http-request
              ,url ,args :all-args-p ,all-args-p
              :additional-headers (list (cons "Accept" "application/json"))
              :convert-dash ,convert-dash
              :method ,method))
       (values-list
        `(,(when (eql code 200) ,(if trim-callback
                                   '(trim-callback (from-json rtn))
                                   '(from-json rtn)))
           ,code
           ,@rest)))))

(defun trim-callback (string)
  "Trims unnecessary charcters from ``string''."
  (subseq string (1+ (position #\( string)) (position #\) string :from-end t)))

(defmacro define-xml-request (name args url &key additional-headers
                              (convert-dash t) fields (item-name "item")
                              authorization raw camel-caps cookie-jar
                              remove-whitespace all-args-p)
  (let ((request-args (if authorization
                        (remove-if (lambda (el) (member
                                                 (if (consp el) (car el) el)
                                                 '(username password)))
                                   args)
                        args)))
    `(defun ,name ,(loop for arg in args
			 if (http-arg-no-encode arg)
			   collect (http-arg-no-encode-base arg)
			 else collect arg)
       (let ((stream
	      (simple-http-request
               ,url ,request-args :all-args-p ,all-args-p
               :additional-headers ',(append
                                      (list (cons "Accept" "application/xml"))
                                      additional-headers)
               :convert-dash ,convert-dash
               :want-stream t
               :authorization ,authorization
               :camel-caps ,camel-caps
               :cookie-jar ,cookie-jar)))
	 ,(if raw
            (if remove-whitespace
              `(xml-remove-whitespace (xml-url-raw nil stream))
              `(xml-url-raw nil stream))
            `(simple-xml
              (lambda () (xml-url-dom nil stream))
              ,fields ,item-name))))))

;; FIXME doesn't really remove
(defun xml-remove-whitespace (data)
  (subst-if nil (lambda (el)
                  (and el (every
                           (lambda (elm)
                             (member elm '(#\Space #\Newline #\Tab))) el))) data))
