(require 'ert)

(ert-deftest test-ac-ctags-is-valid-tags-file-p ()
  "A test to check whether a tags file is created by Exuberant
ctags."
  (let ((tags (and (cd "~/repos/git_repos/auto-complete-ctags/test/")
                   "./test.tags"))
        (nonexist "./tags"))
    (should (equal t (numberp (ac-ctags-is-valid-tags-file-p tags))))
    (should (equal t (null (ac-ctags-is-valid-tags-file-p nonexist))))
    ;; check for TAGS created by etags.
    (should (equal t (null (ac-ctags-is-valid-tags-file-p "qt.TAGS"))))))
