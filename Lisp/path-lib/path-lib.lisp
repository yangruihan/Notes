(defun component-present-p (value)
    "测试一个路径名的给定组件是否存在"
    (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
    "测试一个路径名是否已经是目录形式"
    (and
        (not (component-present-p (pathname-name p)))
        (not (component-present-p (pathname-type p)))
        p))

(defun pathname-as-directory (name)
    "将任何路径名转换成目录形式的路径名"
    (let ((pathname (pathname name)))
        (when (wild-pathname-p pathname)
            (error "Can't reliably convert wild pathnames."))
        (if (not (directory-pathname-p name))
            (make-pathname
                :directory (append (or (pathname-directory pathname) (list :relative))
                                   (list (file-namestring pathname)))
                :name nil
                :type nil
                :defaults pathname)
            pathname)))

(defun pathname-as-file (name)
    "将任何路径名转换成文件形式的路径名"
    (let ((pathname (pathname name)))
        (when (wild-pathname-p pathname)
            (error "Can't reliably convert wild pathnames."))
        (if (directory-pathname-p name)
            (let* ((directory (pathname-directory pathname))
                   (name-and-type (pathname (first (last directory)))))
                (make-pathname
                    :directory (butlast directory)
                    :name (pathname-name name-and-type)
                    :type (pathname-type name-and-type)
                    :defaults pathname))
            pathname)))

(defun directory-wildcard (dirname)
    "将通配符中的类型组件转换成符合Lisp实现"
    (make-pathname
        :name :wild
        :type #-clisp :wild #+clisp nil
        :defaults (pathname-as-directory dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
    "为 CLisp 实现专门设置参数"
    (make-pathname
        :directory (append (pathname-directory wildcard) (list :wild))
        :name nil
        :type nil
        :defaults wildcard))

(defun list-directory (dirname)
    "根据不同的 Lisp 实现，设置不同的参数，列出某一目录下所有的子目录和文件名"
    (when (wild-pathname-p dirname)
        (error "Can only list concrete directory names."))
    (let ((wildcard (directory-wildcard dirname)))

        #+(or sbcl cmu lispworks)
        (directory wildcard)

        #+openmcl
        (directory wildcard :directories t)

        #+allegro
        (directory wildcard :directories-are-files nil)

        #+clisp
        (nconc
            (directory wildcard)
            (directory (clisp-subdirectories-wildcard wildcard)))

        #-(or sbcl cmu lispworks openmcl allegro clisp)
        (error "list-directory not implemented")))

(defun file-exists-p (pathname)
    "测试一个文件是否存在"
    #+(or sbcl lispworks openmcl)
    (probe-file pathname)

    #+(or allegro cmu)
    (or (probe-file (pathname-as-directory pathname))
        (probe-file pathname))

    #+clisp
    (or (ignore-errors
            (probe-file (pathname-as-file pathname)))
        (ignore-errors
            (let ((directory-form (pathname-as-directory pathname)))
                (when (ext:probe-directory directory-form)
                    directory-form))))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented"))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
    (labels
        ((walk (name)
            (cond
                ((directory-pathname-p name)
                 (when (and directories (funcall test name))
                    (funcall fn name))
                 (dolist (x (list-directory name)) (walk x)))
                ((funcall test name) (funcall fn name)))))
        (walk (pathname-as-directory dirname))))