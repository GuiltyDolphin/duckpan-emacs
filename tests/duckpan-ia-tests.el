;;; duckpan-ia-tests.el
;;; Code:

(require 'ert)
(require 'duckpan-ia)

(ert-deftest test-duckpan-ia-names ()
  "Test correct conversion of instant answer names."
  (should (equal "test_name"
                 (duckpan-ia-name-to-share "TestName")))
  (should (equal "dns"
                 (duckpan-ia-name-to-share "DNS"))))

(ert-deftest test-duckpan-ia-perl-path ()
  "Test correct generation of paths to .pm files."
  (should (equal "lib/DDG/Goodie/Calculator.pm"
                 (duckpan-ia-path-to-perl "Goodie" "Calculator"))))

(ert-deftest test-duckpan-ia-js-path ()
  "Test correct generation of paths to .js files."
  (should (equal "share/spice/abc/abc.js"
                 (duckpan-ia-path-to-js "Spice" "ABC"))))

(ert-deftest test-duckpan-ia-correct-ia-paths ()
  "Test correct generation of paths to important instant answer files."
  (should (equal '("lib/DDG/Goodie/ABC.pm")
                 (duckpan-ia-paths-for-type "Goodie" "ABC")))
  (should (equal '("lib/DDG/Spice/FooBar.pm"
                   "share/spice/foo_bar/foo_bar.js")
                 (duckpan-ia-paths-for-type "Spice" "FooBar"))))

(provide 'duckpan-ia-tests)

;;; duckpan-ia-tests.el ends here
