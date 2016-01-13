;;; duckpan-duck-co-tests.el
;;; Code:

(require 'ert)
(require 'duckpan-duck-co)

(ert-deftest test-duckpan-duck-co-pipeline-url ()
  "Test generation of pipeline queries."
  (should (equal "https://duck.co/ia/dev/pipeline?q=FooBar"
                 (duckpan-duck-co-pipeline-query "FooBar"))))

(ert-deftest test-duckpan-duck-co-ia-url ()
  "Test generation of url to instant answer page."
  (should (equal "https://duck.co/ia/view/dns"
                 (duckpan-duck-co-ia-url "DNS")))
  (should (equal "https://duck.co/ia/view/foo_bar"
                 (duckpan-duck-co-ia-url "FooBar"))))

(provide 'duckpan-duck-co-tests)
;;; duckpan-duck-co-tests.el ends here
