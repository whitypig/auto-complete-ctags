(require 'ert)

(defconst test-ac-ctags-valid-tagfile "~/repos/git_repos/auto-complete-ctags/test/test.tags")

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

(ert-deftest test-ac-ctags-visit-tags-file ()
  "A test for ac-ctags-visit-tags-file. No fully implemented, so
this test fails."
  :expected-result :failed
  (let ((ret (call-interactively 'ac-ctags-visit-tags-file)))
    (should (equal t (and (not (null ret))
                          (listp ret))))))

(ert-deftest test-ac-ctags-create-new-list-p ()
  "If the user chooses `yes', then the resutl should be
  `t'. Otherwise nil."
  (let ((tags test-ac-ctags-valid-tagfile))
    ;; The answer is to create new one.
    (should (equal t (ac-ctags-create-new-list-p tags)))
    ;; The answer is to use the current one.
    (should (equal nil (ac-ctags-create-new-list-p tags)))
    ;; tags is already in the current list and the answer is to create
    ;; new one.
    (should (equal t (let ((ac-ctags-current-tags-list (list tags)))
                       (ac-ctags-create-new-list-p tags))))
    ;; tags is already in the current list and the answer is to use
    ;; the current.
    (should (equal nil (let ((ac-ctags-current-tags-list (list tags)))
                         (ac-ctags-create-new-list-p tags))))))
