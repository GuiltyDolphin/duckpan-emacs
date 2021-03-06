;;; duckpan-core-tests.el
;;; Code:

(require 'ert)
(require 'duckpan-core)

(defmacro with-temp-directory (prefix &rest body)
  "Create a temporary directory with prefix PREFIX and execute BODY there like `progn'."
  (declare (indent 1) (debug t))
  (let ((temp-dir (make-symbol "temp-dir")))
    `(let ((,temp-dir (make-temp-file ,prefix t)))
       (unwind-protect
           (with-temp-buffer
             (cd ,temp-dir)
             (progn ,@body))
         (delete-directory ,temp-dir t)))))

(defmacro test-in-directory (dir &rest body)
  "Create directory DIR and execute BODY like `progn' within it."
  (declare (indent 1) (debug t))
  (let ((initial-dir (make-symbol "initial-dir")))
    `(let ((,initial-dir default-directory))
       (make-directory ,dir)
       (cd ,dir)
       (progn ,@body)
       (cd ,initial-dir))))

(ert-deftest test-duckpan-core-project-p ()
  "Test identification of DDG project directories."
    (with-temp-directory "duckpan-core-test"
      (let ((temp-dir default-directory))
        (test-in-directory "invalid-dir"
          (should (equal nil (duckpan-project-p default-directory))))
        (test-in-directory "zeroclickinfo-goodies"
          (should-not (equal nil (duckpan-project-p default-directory)))))))

(ert-deftest test-duckpan-core-with-project-root-macro ()
  "Test execution with the with-duckpan-project-root macro."
  (with-temp-directory "duckpan-core-test"
    (let ((temp-dir default-directory))
      (test-in-directory "invalid-dir"
        (should (equal "Not in a valid duckpan project directory." (with-duckpan-project-root t))))
      (test-in-directory "zeroclickinfo-spice"
        (should (equal t (with-duckpan-project-root t)))))))

(ert-deftest test-uppercase-p ()
  "Test correct detection of uppercase strings."
  (should-not (equal nil (upper-case-p "FOO")))
  (should (equal nil (upper-case-p "Foo")))
  (should (equal nil (upper-case-p "foo"))))

(ert-deftest test-path-in-directory ()
  "Test correct extraction of directory path."
  (should (equal "foo/bar/baz" (duckpan-get-path-with-directory "foo/bar/baz" "baz")))
  (should (equal "foo/bar/baz" (duckpan-get-path-with-directory "foo/bar/baz/qaz" "baz")))
  (should (equal nil (duckpan-get-path-with-directory "foo/bar/baz" "qaz"))))

(ert-deftest test-retrieve-from-cache ()
  "Test retrieving paths from the cache."
  (with-temp-directory "duckpan-tests"
    (let* ((temp-dir default-directory)
          (duckpan-cache-directory (concat temp-dir "/cache")))
      (make-directory "cache/foo" t)
      (should (equal (concat temp-dir "cache/foo") (duckpan-retrieve-path-from-cache "foo")))
      (should (equal nil (duckpan-retrieve-path-from-cache "bar"))))))

(ert-deftest test-write-to-cache ()
  "Test adding files to the cache."
  (with-temp-directory "duckpan-tests"
    (let* ((temp-dir default-directory)
           (duckpan-cache-directory (concat temp-dir "/cache")))
      (duckpan-cache-add-file "quack")
      (should (equal (concat temp-dir "cache/quack") (duckpan-retrieve-path-from-cache "quack")))
      (should (equal nil (duckpan-retrieve-path-from-cache "foo")))
      (duckpan-cache-add-file "foo" "Hello!")
      (should (equal (concat temp-dir "cache/foo") (duckpan-retrieve-path-from-cache "foo")))
      (should (equal "Hello!" (duckpan-cache-read-file "foo")))
      (duckpan-cache-add-file "bar/baz" "Sub-dir")
      (should (equal (concat temp-dir "cache/bar") (duckpan-retrieve-path-from-cache "bar")))
      (should (equal "Sub-dir" (duckpan-cache-read-file "bar/baz"))))))

(provide 'duckpan-core-tests)
;;; duckpan-core-tests.el ends here
