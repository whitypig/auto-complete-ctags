(require 'ert)
(eval-when-compile
  (require 'cl))

(defconst test-ac-ctags-valid-tagfile "~/repos/git_repos/auto-complete-ctags/test/test.tags")
(defconst test-ac-ctags-valid-gtest-tagfile "~/repos/git_repos/auto-complete-ctags/test/gtest.tags")

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

(ert-deftest test-ac-ctags-insert-tags-into-new-list ()
  (let ((ac-ctags-current-tags-list nil)
        (ac-ctags-tags-list-set nil))
    (ac-ctags-insert-tags-into-new-list "test.tags")
    (should (equal '(("test.tags")) ac-ctags-tags-list-set))
    (should (equal '("test.tags") ac-ctags-current-tags-list)))
  (let ((ac-ctags-current-tags-list '("old.tags"))
        (ac-ctags-tags-list-set '(("old.tags"))))
    (ac-ctags-insert-tags-into-new-list "test.tags")
    (should (equal '(("test.tags") ("old.tags")) ac-ctags-tags-list-set))
    (should (equal '("test.tags") ac-ctags-current-tags-list)))
  ;; Case that the newly created list has already been in hte set.
  ;; The set should not change.
  (let ((ac-ctags-current-tags-list '("old.tags"))
        (ac-ctags-tags-list-set '(("test.tags") ("old.tags"))))
    (ac-ctags-insert-tags-into-new-list "test.tags")
    (should (equal '(("test.tags") ("old.tags")) ac-ctags-tags-list-set))
    (should (equal '("test.tags") ac-ctags-current-tags-list))))

(ert-deftest test-ac-ctags-insert-tags-into-current-list ()
  "A test for inserting tags into the current tags list."
  (let ((ac-ctags-current-tags-list nil)
        (ac-ctags-tags-list-set nil))
    (ac-ctags-insert-tags-into-current-list "new.tags")
    (should (equal '("new.tags")
                   ac-ctags-current-tags-list))
    (should (equal '(("new.tags"))
                   ac-ctags-tags-list-set)))
  (let ((ac-ctags-current-tags-list '("tags1"))
        (ac-ctags-tags-list-set '(("tags1") ("tags2"))))
    (ac-ctags-insert-tags-into-current-list "new.tags")
    (should (equal '("new.tags" "tags1")
                   ac-ctags-current-tags-list))
    (should (equal '(("new.tags" "tags1") ("tags2"))
                   ac-ctags-tags-list-set)))
  (let ((ac-ctags-current-tags-list '("tags1"))
        (ac-ctags-tags-list-set '(("tags2"))))
    (ac-ctags-insert-tags-into-current-list "new.tags")
    (should (equal '("new.tags" "tags1")
                   ac-ctags-current-tags-list))
    (should (equal '(("new.tags" "tags1") ("tags2"))
                   ac-ctags-tags-list-set))))

(ert-deftest test-ac-ctags-build-tagdb-from-tags ()
  (let* ((tags (expand-file-name test-ac-ctags-valid-tagfile))
        (db (ac-ctags-build-tagdb-from-tags tags)))
    (should (and (> (length db) 1)
                 (listp db)))
    (should (listp (car db)))
    ;; Check if the length of each element is 3.
    (should (loop for e in db
                  do (unless (= (length e) 3) (return nil))
                  finally return t)))
  (let* ((tags (expand-file-name test-ac-ctags-valid-gtest-tagfile))
        (db (ac-ctags-build-tagdb-from-tags tags)))
    (should (and (> (length db) 1)
                 (listp db)))
    (should (listp (car db)))
    ;; Check if the length of each element is 3.
    (should (loop for e in db
                  do (unless (= (length e) 3) (return nil))
                  finally return t))))

(ert-deftest test-ac-ctags-trim-whitespace ()
  (should (string= "Hi" (ac-ctags-trim-whitespace "  	Hi")))
  (should (string= "Hi" (ac-ctags-trim-whitespace "Hi   	")))
  (should (string= "Hi" (ac-ctags-trim-whitespace "  	Hi		  ")))
  (should (string= "Hi" (ac-ctags-trim-whitespace "Hi"))))

(ert-deftest test-ac-ctags-build-tagdb ()
  (let* ((tags-list (list test-ac-ctags-valid-tagfile
                         test-ac-ctags-valid-gtest-tagfile))
         (db (ac-ctags-build-tagdb tags-list)))
    (should (and (> (length db) 1)
                 (listp db)))
    (should (listp (car db)))
    (should (loop for e in db
                  do (unless (= (length e) 3) (return nil))
                  finally return t))))

(ert-deftest test-ac-ctags-build-completion-table ()
  (let* ((db (ac-ctags-build-tagdb (list test-ac-ctags-valid-tagfile)))
         (tbl (ac-ctags-build-completion-table db)))
    (should (> (length tbl) 1))))

(ert-deftest test-ac-ctags-get-signature ()
  (let ((db (ac-ctags-build-tagdb (list test-ac-ctags-valid-tagfile))))
    (should (equal '("()")
                   (ac-ctags-get-signature "normal_func" db)))
    (should (equal '("()")
                   (ac-ctags-get-signature "TestClass::normal_func" db)))
    (should (equal '("(int i)" "(double d)")
                   (ac-ctags-get-signature "overloaded_func" db)))))

(ert-deftest test-ac-ctags-visit-tags-file:list-is-empty ()
  (cd "~/repos/git_repos/auto-complete-ctags/test/")
  (let ((test-tagsfile (expand-file-name test-ac-ctags-valid-tagfile))
        (default-tagsfile (expand-file-name "./tags")))
    ;; Try to insert a new tag into an emtpy tags list.
    (let ((ac-ctags-current-tags-list nil)
          (ac-ctags-tags-list-set nil))
      (ac-ctags-visit-tags-file test-tagsfile 'new)
      (should (equal `(,test-tagsfile)
                     ac-ctags-current-tags-list))
      (should (equal `(,ac-ctags-current-tags-list)
                     ac-ctags-tags-list-set)))))

(ert-deftest test-ac-ctags-visit-tags-file:list-has-already-the-same-tags ()
  ;; Try to insert a tags into a list which has already that tags.
  ;; won't create a new list.
  (let* ((test-tagsfile (expand-file-name test-ac-ctags-valid-tagfile))
         (ac-ctags-current-tags-list `(,test-tagsfile))
         (ac-ctags-tags-list-set `((,test-tagsfile))))
    (ac-ctags-visit-tags-file test-tagsfile 'current)
    (should (equal `(,test-tagsfile)
                   ac-ctags-current-tags-list))
    (should (equal `(,ac-ctags-current-tags-list)
                   ac-ctags-tags-list-set))))

(ert-deftest test-ac-ctags-visit-tags-file:try-to-insert-the-same-tags ()
    ;; Try to insert a tags into a new list.  Try to crate a new list,
    ;; but the elements are the same as those of
    ;; ac-ctags-current-tags-list, so actually does not create a new
    ;; list even if the answer to the create-a-new-list question is
    ;; yes.
  (let* ((test-tagsfile (expand-file-name test-ac-ctags-valid-tagfile)))
    (ac-ctags-visit-tags-file test-tagsfile 'new)
    (should (equal `(,test-tagsfile)
                   ac-ctags-current-tags-list))
    (should (equal `(,ac-ctags-current-tags-list)
                   ac-ctags-tags-list-set))))

(ert-deftest test-ac-ctags-visit-tags-file:try-to-insert-a-new-tags-into-the-current-list ()
  ;; Try to insert a new tags file into the current list which has
  ;; one elements.
  (let* ((test-tagsfile (expand-file-name test-ac-ctags-valid-tagfile))
         (default-tagsfile (and (cd "~/repos/git_repos/auto-complete-ctags/test/")
                                (expand-file-name "./tags")))
         (ac-ctags-current-tags-list `(,test-tagsfile))
         (ac-ctags-tags-list-set `((,test-tagsfile))))
    (ac-ctags-visit-tags-file default-tagsfile 'current)
    (should (equal `(,default-tagsfile ,test-tagsfile)
                   ac-ctags-current-tags-list))
    (should (equal `(,ac-ctags-current-tags-list)
                   ac-ctags-tags-list-set))))

(ert-deftest test-ac-ctags-visit-tags-file:insert-tags-into-a-new-list ()
  ;; Try to insert a tags into a new list.
  (let* ((test-tagsfile (expand-file-name test-ac-ctags-valid-tagfile))
         (default-tagsfile (and (cd "~/repos/git_repos/auto-complete-ctags/test/")
                                (expand-file-name "./tags")))
         (ac-ctags-current-tags-list `(,test-tagsfile))
         (ac-ctags-tags-list-set `((,test-tagsfile))))
    (ac-ctags-visit-tags-file default-tagsfile 'new)
    (should (equal `(,default-tagsfile)
                   ac-ctags-current-tags-list))
    (should (equal `(,ac-ctags-current-tags-list (,test-tagsfile))
                   ac-ctags-tags-list-set))))

(ert-deftest test-ac-ctags-visit-tags-file:list-A-into-AB ()
  ;; ac-ctags-current-tags-list => (tagsB)
  ;; ac-ctags-tags-list-set => ((tagsA) (tagsB))
  ;; visiting tagsA
  (let* ((test-tagsfile (expand-file-name test-ac-ctags-valid-tagfile))
         (default-tagsfile (and (cd "~/repos/git_repos/auto-complete-ctags/test/")
                                (expand-file-name "./tags")))
         (ac-ctags-current-tags-list `(,default-tagsfile))
         (ac-ctags-tags-list-set `((,test-tagsfile) (,default-tagsfile))))
    (ac-ctags-visit-tags-file test-tagsfile 'new)
    (should (equal `(,test-tagsfile) ac-ctags-current-tags-list))
    ;; ac-ctags-tags-list-set should stay the same.
    (should (equal `((,test-tagsfile) (,default-tagsfile))
                   ac-ctags-tags-list-set))))

(ert-deftest test-ac-ctags-visit-tags-file:list-A-into-AB ()
  ;; ac-ctags-current-tags-list => (tagsA)
  ;; ac-ctags-tags-list-set => ((tagsA) (tagsB))
  ;; visiting tagsA
  (let* ((test-tagsfile (expand-file-name test-ac-ctags-valid-tagfile))
         (default-tagsfile (and (cd "~/repos/git_repos/auto-complete-ctags/test/")
                                (expand-file-name "./tags")))
         (ac-ctags-current-tags-list `(,test-tagsfile))
         (ac-ctags-tags-list-set `((,test-tagsfile) (,default-tagsfile))))
    (ac-ctags-visit-tags-file test-tagsfile 'new)
    (should (equal `(,test-tagsfile) ac-ctags-current-tags-list))
    ;; ac-ctags-tags-list-set should stay the same.
    (should (equal `((,test-tagsfile) (,default-tagsfile))
                   ac-ctags-tags-list-set))))
