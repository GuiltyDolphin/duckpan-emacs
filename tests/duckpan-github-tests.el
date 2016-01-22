;;; duckpan-github-tests.el
;;; Code:

(require 'ert)
(require 'duckpan-github)

(ert-deftest test-duckpan-github-url ()
  "Test correct user/repo url generation."
  (should (equal "https://github.com/GuiltyDolphin/zeroclickinfo-goodies"
                 (duckpan-github-url "GuiltyDolphin" "zeroclickinfo-goodies"))))

(ert-deftest test-duckpan-github-clone ()
  "Test correct cloning of fork."
  (let* ((temp-dir (make-temp-file "duckpan-github-test" t))
         (make-url (lambda (type name) (format "%s URL: https://github.com/%s/zeroclickinfo-goodies" type name)))
         (goodies-origin-fetch-url (funcall make-url "Fetch" "GuiltyDolphin"))
         (goodies-origin-push-url (funcall make-url "Push" "GuiltyDolphin"))
         (goodies-remote-fetch-url (funcall make-url "Fetch" "duckduckgo"))
         (goodies-remote-push-url (funcall make-url "Push" "duckduckgo")))
    (unwind-protect
        (with-temp-buffer
          (cd temp-dir)
          (duckpan-github-initialize-repo "GuiltyDolphin" "zeroclickinfo-goodies")
          (should (member "zeroclickinfo-goodies" (directory-files temp-dir)))
          (cd "zeroclickinfo-goodies")
          (with-temp-buffer
            (call-process "git" nil t nil "remote" "show" "-n" "origin")
            (let ((check-string (split-string (buffer-string) "\n" t split-string-default-separators)))
              (should (member goodies-origin-fetch-url check-string)))))
      (delete-directory temp-dir t))))

(provide 'duckpan-github-tests)
;;; duckpan-github-tests.el ends here
