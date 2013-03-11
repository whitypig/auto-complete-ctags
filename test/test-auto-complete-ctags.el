(require 'ert)
(require 'cl)

(defun test-ac-ctags-before (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (eval-buffer)))

(test-ac-ctags-before "../auto-complete-ctags.el")
(test-ac-ctags-before "../auto-complete-ctags-java.el")

(defconst test-ac-ctags-valid-tagfile "cpp.ctags")
(defconst test-ac-ctags-valid-gtest-tagfile "gtest.tags")
(defconst test-ac-ctags-cpp-tagsfile "cpp.ctags")
(defconst test-ac-ctags-cpp-tagsfile2 "test_with_q.ctags")
(defconst test-ac-ctags-java-tagsfile "java.tags")
(defconst test-ac-ctags-java-tagsfile2 "java.ctags")
(defconst test-ac-ctags-java-goos-tagfile "goos.ctags")
(defconst test-ac-ctags-c-tagsfile "c.ctags")
(defconst test-ac-ctags-java-tagsfile-for-update "java.updated.ctags")
(defconst test-ac-ctags-java-inf-tagsfile "inf.ctags")

(defconst test-ac-ctags-node-length 7)

(defun test-ac-ctags-fixture (body)
  (let ((ac-ctags-current-major-mode nil)
        (ac-ctags-tags-db nil)
        (ac-ctags-current-tags-list nil)
        (ac-ctags-tags-list-set nil)
        (ac-ctags-completion-table nil)
        (ac-ctags-current-completion-table nil)
        (ac-ctags-tags-db-created-time nil))
    (funcall body)))

(ert-deftest test-ac-ctags-is-valid-tags-file-p ()
  "A test to check whether a tags file is created by Exuberant
ctags."
  (let ((tags test-ac-ctags-valid-tagfile)
        (nonexist "./non.tags"))
    (should (numberp (ac-ctags-is-valid-tags-file-p tags)))
    (should (null (ac-ctags-is-valid-tags-file-p nonexist)))
    ;; check for TAGS created by etags.
    (should (null (ac-ctags-is-valid-tags-file-p "e.TAGS")))))

(ert-deftest test-ac-ctags-create-new-list-p ()
  "If the user chooses `yes', then the resutl should be
  `t'. Otherwise nil."
  (let ((tags test-ac-ctags-valid-tagfile))
    ;; The answer is to create new one.
    ;; You have to answer `yes'
    (should (ac-ctags-create-new-list-p tags))
    ;; The answer is to use the current one.
    ;; You have to answer `no'
    (should (null (ac-ctags-create-new-list-p tags)))
    ;; tags is already in the current list and the answer is to create
    ;; new one.
    ;; You have to answer `yes'
    (should (let ((ac-ctags-current-tags-list (list tags)))
              (ac-ctags-create-new-list-p tags)))
    ;; tags is already in the current list and the answer is to use
    ;; the current.
    ;; You have to answer `no'
    (should (null (let ((ac-ctags-current-tags-list (list tags)))
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
  ;; Case that the newly created list has already been in the set.
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

(ert-deftest test-ac-ctags-build-tagsdb-from-tags:c-tags ()
  (let* ((tags (expand-file-name test-ac-ctags-c-tagsfile))
         (db nil)
         (db (ac-ctags-build-tagsdb-from-tags tags db)))
    (should (listp db))
    (should (not (null db)))
    (should (> (length db) 0))
    (should (listp (car db)))
    (should (> (length (car db)) 1))
    (should (not (null (cdar db))))
    (should (listp (cdar db)))
    (should (string= "C" (caar db)))
    ;; Check if the length of each element is test-ac-ctags-node-length.
    (should (loop for e in (cdar db)
                  do (unless (= (length e) test-ac-ctags-node-length)
                       (return nil))
                  finally return t))))

(ert-deftest test-ac-ctags-build-tagsdb-from-tags:cpp-tags ()
  (let* ((db nil)
         (tags (expand-file-name test-ac-ctags-cpp-tagsfile))
         (db (ac-ctags-build-tagsdb-from-tags tags db)))
    (should (listp db))
    (should (not (null db)))
    (should (> (length db) 0))
    (should (listp (car db)))
    (should (> (length (car db)) 1))
    (should (not (null (cdar db))))
    (should (listp (cdar db)))
    (should (string= "C++" (caar db)))
    ;; Check if the length of each element is equal to test-ac-ctags-node-length
    (should (loop for e in (cdar db)
                  do (unless (= (length e) test-ac-ctags-node-length)
                       (return nil))
                  finally return t))))

(ert-deftest test-ac-ctags-build-tagsdb-from-tags:java-tags ()
  (let* ((db nil)
         (tags (expand-file-name test-ac-ctags-java-tagsfile))
         (db (ac-ctags-build-tagsdb-from-tags tags db)))
    (should (listp db))
    (should (not (null db)))
    (should (> (length db) 0))
    (should (listp (car db)))
    (should (> (length (car db)) 1))
    (should (not (null (cdar db))))
    (should (listp (cdar db)))
    (should (string= "Java" (caar db)))
    ;; Check if the length of each element is equal to test-ac-ctags-node-length
    (should (loop for e in (cdar db)
                  do (unless (= (length e) test-ac-ctags-node-length)
                       (return nil))
                  finally return t))))

(ert-deftest test-ac-ctags-build-tagsdb-from-tags:java-check-returntype ()
  (let* ((db nil)
         (tags (expand-file-name test-ac-ctags-java-tagsfile2))
         (db (ac-ctags-build-tagsdb-from-tags tags db))
         (tbl nil)
         (case-fold-search nil))
    (setq tbl (cdr (assoc-default "Java" db)))
    (should tbl)
    (loop for entry in tbl
          when (and (string= "method"
                             (ac-ctags-node-kind entry))
                    (string-match "^[a-z].*$" (ac-ctags-node-name entry)))
          do (should (ac-ctags-node-returntype entry)))))

(ert-deftest test-ac-ctags-build-tagsdb-from-tags:cpp-check-returntype ()
  (let* ((db nil)
         (tags (expand-file-name test-ac-ctags-cpp-tagsfile))
         (db (ac-ctags-build-tagsdb-from-tags tags db))
         (tbl nil)
         (case-fold-search nil))
    (setq tbl (cdr (assoc-default "C++" db)))
    (should tbl)
    (loop for entry in tbl
          when (and (string= "function"
                             (ac-ctags-node-kind entry))
                    (string-match "^[a-z].*$" (ac-ctags-node-name entry)))
          do (should (ac-ctags-node-returntype entry)))))

(ert-deftest test-ac-ctags-build-tagsdb-from-tags:c-check-returntype ()
  (let* ((db nil)
         (tags (expand-file-name test-ac-ctags-c-tagsfile))
         (db (ac-ctags-build-tagsdb-from-tags tags db))
         (tbl nil)
         (case-fold-search nil))
    (setq tbl (cdr (assoc-default "C" db)))
    (should tbl)
    (loop for entry in tbl
          when (and (string= "function"
                             (ac-ctags-node-kind entry))
                    (string-match "^[A-Za-z].*$" (ac-ctags-node-name entry)))
          do (should (ac-ctags-node-returntype entry)))))

(ert-deftest test-ac-ctags-trim-whitespace ()
  (should (string= "Hi" (ac-ctags-trim-whitespace "  	Hi")))
  (should (string= "Hi" (ac-ctags-trim-whitespace "Hi   	")))
  (should (string= "Hi" (ac-ctags-trim-whitespace "  	Hi		  ")))
  (should (string= "Hi" (ac-ctags-trim-whitespace "Hi"))))

(ert-deftest test-ac-ctags-build-tagsdb:cpp-and-java ()
  (let* ((tags-list `(,test-ac-ctags-cpp-tagsfile ,test-ac-ctags-java-tagsfile))
         (db nil)
         (db (ac-ctags-build-tagsdb tags-list db))
         (cpp-db (assoc "C++" db))
         (java-db (assoc "Java" db)))
    ;; Check cpp-db
    (should (listp cpp-db))
    (should (> (length cpp-db) 1))
    (should (string= "C++" (car cpp-db)))
    (should (listp (cdr cpp-db)))
    (should (loop for e in (cdr cpp-db)
                  do (unless (= (length e) test-ac-ctags-node-length)
                       (return nil))
                  finally return t))
    ;; Check java-db
    (should (listp java-db))
    (should (> (length java-db) 1))
    (should (string= "Java" (car java-db)))
    (should (listp (cdr java-db)))
    (should (loop for e in (cdr java-db)
                  do (unless (= (length e) test-ac-ctags-node-length)
                       (return nil))
                  finally return t))))

(ert-deftest test-ac-ctags-build-completion-table:cpp-tags ()
  (let* ((db nil)
         (db (ac-ctags-build-tagsdb `(,test-ac-ctags-cpp-tagsfile) db))
         (tbl (ac-ctags-build-completion-table db)))
    ;; tbl => (("C++" . [n1 n2 n3...]))
    (should (not (null tbl)))
    (should (> (length tbl) 0))
    (should (string= "C++" (caar tbl)))
    (should (> (length (cdar tbl)) 0))
    (should (vectorp (cdar tbl)))
    (should (intern-soft "overloaded_func" (cdar tbl)))))

(ert-deftest test-ac-ctags-build-completion-table:cpp-and-java-tags ()
  (let* ((db nil)
         (db (ac-ctags-build-tagsdb
              `(,test-ac-ctags-cpp-tagsfile ,test-ac-ctags-java-tagsfile)
              db))
         (tbl (ac-ctags-build-completion-table db)))
    ;; tbl => (("C++" . [n1 n2...]) ("Java" . [m1 m2...]))
    (should (not (null tbl)))
    (should (= 2 (length tbl)))
    (let ((cpp-tbl (assoc "C++" tbl)) (java-tbl (assoc "Java" tbl)))
      ;; cpp-tbl => ("C++" . [n1 n2...])
      (should (string= "C++" (car cpp-tbl)))
      (should (> (length (cdr cpp-tbl)) 1))
      (should (intern-soft "overloaded_func" (cdr cpp-tbl)))
      ;; java-tbl => ("Java" . [n1 n2...])
      (should (string= "Java" (car java-tbl)))
      (should (> (length (cdr java-tbl)) 1))
      (should (intern-soft "helloWorld" (cdr java-tbl))))))

(ert-deftest test-ac-ctags-get-signature ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-reset)
     (ac-ctags-visit-tags-file test-ac-ctags-cpp-tagsfile 'new)
     (should (equal '("void normal_func()")
                    (ac-ctags-get-signature "normal_func" ac-ctags-tags-db "C++")))
     (should (equal '("void TestClass::normal_func()")
                    (ac-ctags-get-signature "TestClass::normal_func" ac-ctags-tags-db "C++")))
     (should (equal '("void overloaded_func(int i)" "void overloaded_func(double d)")
                    (ac-ctags-get-signature "overloaded_func" ac-ctags-tags-db "C++")))
     (should (equal '("void risky_func() throw (int)")
                    (ac-ctags-get-signature "risky_func" ac-ctags-tags-db "C++")))
     (should (null (ac-ctags-get-signature "TestClass" ac-ctags-tags-db "C++")))
     (should (null (ac-ctags-get-signature "nonexist" ac-ctags-tags-db "C++"))))))

(ert-deftest test-ac-ctags-get-signature:gtest ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-valid-gtest-tagfile 'new)
     (should
      (equal '("GTEST_API_ void InitGoogleTest(int* argc, wchar_t** argv)"
               "GTEST_API_ void InitGoogleTest(int* argc, char** argv)")
             (ac-ctags-get-signature "InitGoogleTest" ac-ctags-tags-db "C++")))
     (should
      (null (ac-ctags-get-signature "EXPECT_EQ"
                                    ac-ctags-tags-db
                                    "C++"))))))

(ert-deftest test-ac-ctags-get-signature-by-mode ()
  (let* ((db nil)
         (db (ac-ctags-build-tagsdb `(,test-ac-ctags-cpp-tagsfile) db)))
    (should (equal '("void normal_func()")
                   (ac-ctags-get-signature "normal_func" db "C++")))))

(ert-deftest test-ac-ctags-c++-document ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-cpp-tagsfile)
     (should
      (string= "void overloaded_func(double d)\nvoid overloaded_func(int i)"
               (ac-ctags-c++-document "overloaded_func")))
     (should
      (string= "void normal_func()"
               (ac-ctags-c++-document "normal_func")))
     (should
      (string= "void risky_func() throw (int)"
               (ac-ctags-c++-document "risky_func"))))))

(ert-deftest test-ac-ctags-c-document ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-c-tagsfile)
     (should
      (string= "void simple_func(void)"
               (ac-ctags-c-document "simple_func")))
     (should
      (string= "void simple_func2(int a, int b)"
               (ac-ctags-c-document "simple_func2")))
     (should
      (string= ac-ctags-no-document-message
               (ac-ctags-c-document "old_style_func"))))))

(ert-deftest test-ac-ctags-get-mode-string ()
  (should (equal '("C++" "C")
                 (ac-ctags-get-mode-string 'c++-mode)))
  (should (equal '("Java")
                 (ac-ctags-get-mode-string 'java-mode)))
  (should (equal '("Others")
                 (ac-ctags-get-mode-string 'foo-mode))))

(ert-deftest test-ac-ctags-visit-tags-file:list-is-empty ()
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
  (let* ((test-tagsfile (expand-file-name test-ac-ctags-valid-tagfile))
         (ac-ctags-current-tags-list nil)
         (ac-ctags-tags-list-set nil))
    (ac-ctags-visit-tags-file test-tagsfile 'new)
    (should (equal `(,test-tagsfile)
                   ac-ctags-current-tags-list))
    (should (equal `(,ac-ctags-current-tags-list)
                   ac-ctags-tags-list-set))))

(ert-deftest test-ac-ctags-visit-tags-file:try-to-insert-a-new-tags-into-the-current-list ()
  ;; Try to insert a new tags file into the current list which has
  ;; one elements.
  (let* ((test-tagsfile (expand-file-name test-ac-ctags-valid-tagfile))
         (default-tagsfile (expand-file-name "./c.tags"))
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
         (default-tagsfile (expand-file-name "./tags"))
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
         (default-tagsfile (expand-file-name "./tags"))
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
         (default-tagsfile (and (expand-file-name "./tags")))
         (ac-ctags-current-tags-list `(,test-tagsfile))
         (ac-ctags-tags-list-set `((,test-tagsfile) (,default-tagsfile))))
    (ac-ctags-visit-tags-file test-tagsfile 'new)
    (should (equal `(,test-tagsfile) ac-ctags-current-tags-list))
    ;; ac-ctags-tags-list-set should stay the same.
    (should (equal `((,test-tagsfile) (,default-tagsfile))
                   ac-ctags-tags-list-set))))

(ert-deftest test-ac-ctags-strip-cmd ()
  (let ((cmd "public function EscapeToken($token, $chars = null) {"))
    (should (string= "public function EscapeToken($token, $chars = null) {"
                     (ac-ctags-strip-cmd cmd))))
  (let ((cmd "/^		$xmlText = '<' . '?xml version=\"1.0\" encoding=\"UTF-8\"?><tags><tag><id>1<\/id><name>defect<\/name><\/tag><tag><id>2<\/id><name>enhancement<\/name><\/tag><\/tags>';$/"))
    (should (string= "		$xmlText = '<' . '?xml version=\"1.0\" encoding=\"UTF-8\"?><tags><tag><id>1<\/id><name>defect<\/name><\/tag><tag><id>2<\/id><name>enhancement<\/name><\/tag><\/tags>'"
                     (ac-ctags-strip-cmd cmd)))))

(ert-deftest test-ac-ctags-construct-signature ()
  (test-ac-ctags-fixture
   (lambda ()
     (should
      (string= "void normal_func()"
               (ac-ctags-construct-signature "normal_func"
                                             "void normal_func() {}"
                                             "function"
                                             "()")))
     (should
      (string= "int get() const"
               (ac-ctags-construct-signature "get"
                                             "int get() const { return 0; }"
                                             "function"
                                             "() const")))
     (should
      (string= "void TestClass::normal_func()"
               (ac-ctags-construct-signature "TestClass::normal_func"
                                             "void normal_func() {}"
                                             "function"
                                             "()")))
     (should
      (string= "GTEST_API_ void InitGoogleTest(int* argc, wchar_t** argv)"
               (ac-ctags-construct-signature
                "InitGoogleTest"
                "GTEST_API_ void InitGoogleTest(int* argc, wchar_t** argv)"
                "prototype"
                "(int* argc, wchar_t** argv)"))))))

(ert-deftest test-ac-ctags-construct-signature:throw ()
  (should
   (string= "void risky_func() throw (int)"
            (ac-ctags-construct-signature
             "risky_func"
             "void risky_func() throw (int)"
             "prototype"
             "()"))))

(ert-deftest test-ac-ctags-construct-signature:java ()
  (should
   (string= "public void helloWorld()"
            (ac-ctags-construct-signature "helloWorld"
                                          "public void helloWorld() {"
                                          "method"
                                          "()")))
  (should
   (string= "public void Test.helloWorld()"
            (ac-ctags-construct-signature "Test.helloWorld"
                                          "public void helloWorld() {"
                                          "method"
                                          "()")))
  (should
   (string= "helloWorld()"
            (ac-ctags-construct-signature "helloWorld"
                                          "helloWorld()"
                                          "method"
                                          "()"))))

(ert-deftest test-ac-ctags-strip-class-name ()
  (should (string= "normal_func"
                   (ac-ctags-strip-class-name "TestClass::normal_func")))
  (should (string= "helloWorld"
                   (ac-ctags-strip-class-name "Test.helloWorld"))))

;; node => (name cmd kind signature)
(ert-deftest test-ac-ctags-node-access ()
  (let ((node '("name" "cmd" "kind" "class" "interface" "signature")))
    (should (string= "name"
                     (ac-ctags-node-name node)))
    (should (string= "cmd"
                     (ac-ctags-node-command node)))
    (should (string= "kind"
                     (ac-ctags-node-kind node)))
    (should (string= "class"
                     (ac-ctags-node-class node)))
    (should (string= "signature"
                     (ac-ctags-node-signature node)))
    (should (null
             (ac-ctags-node-kind '("name" "cmd" nil "class" "interface" "signature"))))
    (should (null
             (ac-ctags-node-signature '("name" "cmd" nil "class" "interface" nil))))))

(ert-deftest test-ac-ctags-get-signature:java ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-tagsfile 'new)
     (should
      (equal '("public void helloWorld()")
             (ac-ctags-get-signature "helloWorld"
                                     ac-ctags-tags-db
                                     "Java"))))))

(ert-deftest test-ac-ctags-java-document ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-tagsfile 'new)
     (should
      (string= "public void helloWorld()"
               (ac-ctags-java-document "helloWorld")))
     (should
      (string= "private int helloAnotherWorld() throws NullPointerException"
               (ac-ctags-java-document "helloAnotherWorld")))
     (should
      (string= "public void Test.helloWorld()"
               (ac-ctags-java-document "Test.helloWorld")))
     )))

(ert-deftest test-ac-ctags-java-method-candidates-1 ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-tagsfile2 'new)
     (should
      (equal
       '("SampleClass()" "SampleClass(int arg1, String arg2)" "helloAnotherWorld()"
         "helloWorld()" "methodThatHasArgument(int i, String str)"
         "methodThatSpansMultipleLines()" "methodWithGenerics()")
       (mapcar #'substring-no-properties
               (ac-ctags-java-method-candidates-1 "SampleClass" nil))))
     (should
      (equal
       '("helloAnotherWorld()")
       (mapcar #'substring-no-properties
               (ac-ctags-java-method-candidates-1 "SampleClass" "helloA"))))
     (should
      (null
       (ac-ctags-java-method-candidates-1 "SampleClass" "none")))
     (should
      (null (ac-ctags-java-method-candidates-1 nil nil)))
     )))

(ert-deftest test-ac-ctags-java-method-candidates-1-check-text-property ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-tagsfile2 'new)
     (let* ((cand (car (ac-ctags-java-method-candidates-1 "SampleClass" "helloA")))
            (prop (get-text-property 0 'view cand)))
       (should prop)
       ;; method name
       (should (string= "helloAnotherWorld()" (substring-no-properties cand)))
       ;; and view property
       (should (string= "helloAnotherWorld()             :int - SampleClass"
                        prop))
       ))))

(ert-deftest test-ac-ctags-java-collect-methods-in-class ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-tagsfile2 'new)
     (should
      (equal
       '("SampleClass()" "SampleClass(int arg1, String arg2)" "helloAnotherWorld()"
         "helloWorld()" "methodThatHasArgument(int i, String str)"
         "methodThatSpansMultipleLines()" "methodWithGenerics()")
       (mapcar #'substring-no-properties
               (ac-ctags-java-collect-methods-in-class "SampleClass"))))
     (should
      (null (ac-ctags-java-collect-methods-in-class "NoneExist")))
     )))

(ert-deftest test-ac-ctags-java-collect-methods-in-interface ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-inf-tagsfile 'new)
     (should
      (equal
       '("method1(int i)" "method2()")
       (mapcar #'substring-no-properties
               (ac-ctags-java-collect-methods-in-class "SomeInterface")))))))

(ert-deftest test-ac-ctags-java-make-method-candidate ()
  (let ((node1 '("method" "cmd" "method" "SomeClass" nil "()" nil "int"))
        (node2 '("anotherMethod" "cmd" "method" "SomeClass" nil "(int i, String s)" nil "void"))
        (node-ctor '("SampleClass" "cmd" "method" "SampleClass" nil "()" nil nil)))
    (should
     (string= "method()"
              (ac-ctags-java-make-method-candidate node1)))
    (should
     (string= "method()                :int - SomeClass"
              (get-text-property 0 'view (ac-ctags-java-make-method-candidate node1))))
    (should
     (string= "()"
      (get-text-property 0 'signature (ac-ctags-java-make-method-candidate node1))))
    (should
     (string= "anotherMethod(int i, String s)"
              (ac-ctags-java-make-method-candidate node2)))
    (should
     (string= "anotherMethod(int i, String s)    :void - SomeClass"
              (get-text-property 0 'view (ac-ctags-java-make-method-candidate node2))))
    (should
     (string= "(int i, String s)"
      (get-text-property 0 'signature (ac-ctags-java-make-method-candidate node2))))
    (should
     (string= "SampleClass()"
              (ac-ctags-java-make-method-candidate node-ctor)))
    (should
     (string= "SampleClass()              - SampleClass"
              (get-text-property 0 'view
                                 (ac-ctags-java-make-method-candidate node-ctor))))
    (should
     (string= "()"
      (get-text-property 0 'signature (ac-ctags-java-make-method-candidate node-ctor))))
    ))

(ert-deftest test-ac-ctags-make-yasnippet-template-from-signature ()
  (should
   (string=
    "(${1:int i}, ${2:String s})$0"
    (ac-ctags-make-yasnippet-template-from-signature "(int i, String s)")))
  (should
   (string= "()$0"
            (ac-ctags-make-yasnippet-template-from-signature "()")))
  (should
   (string= "(${1:int i})$0"
            (ac-ctags-make-yasnippet-template-from-signature "(int i)")))
  (should
   (string=
    "(${1:Map<String, String> map})$0"
    (ac-ctags-make-yasnippet-template-from-signature "(Map<String, String> map)")))
  (should
   (string=
    "(${1:Map<String, String> map}, ${2:int j})$0"
    (ac-ctags-make-yasnippet-template-from-signature "(Map<String, String> map, int j)")))
  )


(ert-deftest test-ac-ctags-split-signature-string ()
  (should
   (equal '("int i" "int j")
          (ac-ctags-split-signature-string "(int i, int j)" "[,]")))
  (should
   (equal '("Map<String, String> map" "int j")
          (ac-ctags-split-signature-string "(Map<String, String> map, int j)" "[,]")))
  (should
   (equal '("int i")
          (ac-ctags-split-signature-string "(int i)" ",")))
  (should
   (null (ac-ctags-split-signature-string "()" ",")))
  (should
   (equal '("Map<Map<int, int>, Map<int, int>> map" "int i")
          (ac-ctags-split-signature-string "(Map<Map<int, int>, Map<int, int>> map, int i)" ","))))

(ert-deftest test-ac-ctags-java-extract-class-name ()
  (should
   (string= "ClassName"
            (ac-ctags-java-extract-class-name
             "ClassName varname;"
             "varname")))
  (should
   (string= "ClassName"
            (ac-ctags-java-extract-class-name
             "ClassName<Some<Another>> varname;"
             "varname")))
  (should
   (string= "ClassName"
            (ac-ctags-java-extract-class-name
             "public void someMethod(ClassName varname) {"
             "varname"))))

(ert-deftest tet-ac-ctags-java-collect-fields-in-class ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-tagsfile2 'new)
     (should
      (equal
       '("CONSTANT" "_intField" "_strField")
       (ac-ctags-java-collect-fields-in-class "SampleClass"))))))

(ert-deftest test-ac-ctags-java-field-candidates-1 ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-tagsfile2 'new)
     (should
      (equal
       '("CONSTANT" "_intField" "_strField" "_strMap")
       (ac-ctags-java-field-candidates-1 "SampleClass" nil)))
     (should
      (equal
       '("CONSTANT")
       (ac-ctags-java-field-candidates-1 "SampleClass" "CON")))
     (should
      (null
       (ac-ctags-java-field-candidates-1 "SampleClass" "none")))
     (should
      (null (ac-ctags-java-field-candidates-1 nil nil)))
     )))

(ert-deftest test-ac-ctags-java-make-field-candidate ()
  (let ((node1 '("_strField" "/^  private String _strField;$/;\""
                 "field" "SomeClass" nil nil nil))
        (node2 '("_strMap" "/^  private Map<String, String> _strMap;$/;\""
                 "field" "SampleClass" nil nil nil))
        (no-type-node
         '("_strMap" "/^  private _strMap;$/;\""
           "field" "SampleClass" nil nil nil)))
    (should
     (string= "_strField"
              (ac-ctags-java-make-field-candidate node1)))
    (should
     (string= "_strField                                  :String"
              (get-text-property 0 'view (ac-ctags-java-make-field-candidate node1))))
    (should
     (string= "_strMap"
              (ac-ctags-java-make-field-candidate node2)))
    (should
     (string= "_strMap                       :Map<String, String>"
              (get-text-property 0 'view (ac-ctags-java-make-field-candidate node2))))
        (should
     (string= "_strMap"
              (ac-ctags-java-make-field-candidate no-type-node)))
    (should
     (null (get-text-property 0 'view (ac-ctags-java-make-field-candidate no-type-node))))
    ))

(ert-deftest test-ac-ctags-java-parse-field-node ()
  (should
   (string= "String"
            (ac-ctags-java-parse-field-node
             '("_strField" "/^  private String _strField;$/;\""
               "field" "SomeClass" nil nil nil))))
  (should
   (string= "Map<String, String>"
            (ac-ctags-java-parse-field-node
             '("_strMap" "/^  protected Map<String, String> _strMap;$/;\""
               "field" "SampleClass" nil nil nil))))
  (should
   (string= "int"
            (ac-ctags-java-parse-field-node
             '("CONSTANT" "/^  public static final int CONSTANT;$/;\""
               "field" "SampleClass" nil nil nil))))
  (should
   (null (ac-ctags-java-parse-field-node
          '("CONSTANT" "/^  CONSTANT;$/;\""
               "field" "SampleClass" nil nil nil)))))

(ert-deftest test-ac-ctags-make-signature ()
  (should (string= "(int, int)"
                   (ac-ctags-make-signature "(int a, int b)")))
  (should (string= "(int, int)"
                   (ac-ctags-make-signature "(int, int)")))
  (should (string= "()"
                   (ac-ctags-make-signature "()")))
  (should (string= "(int, String)"
                   (ac-ctags-make-signature "(int i, String str)")))
  (should (string= "()"
                   (ac-ctags-make-signature "(void)")))
  (should (string= "(String...)"
                   (ac-ctags-make-signature "(String... strs)")))
  (should (string= "(int, String...)"
                   (ac-ctags-make-signature "(int i, String... strs)")))
  (should (string= "(int[], int[])"
                   (ac-ctags-make-signature "(int[] arraya, int[] arrayb)")))
  (should (string= "(String)"
                   (ac-ctags-make-signature
                    "(final String s)")))
  (should (string= "(Object, Collection<String>)"
                   (ac-ctags-make-signature "(Object object, Collection<String> strings)")))
  )

(ert-deftest test-ac-ctags-update-ac-sources ()
  (let ((ac-sources '(ac-source-words-in-same-mode-buffers)))
    (should (not (member 'ac-source-ctags-java-method
                         ac-sources)))
    ;; transition from nil to java-mode
    (ac-ctags-update-ac-sources nil 'java-mode)
    (should (member 'ac-source-ctags-java-method ac-sources))
    ;; transition from java-mode to c++-mode
    (ac-ctags-update-ac-sources 'java-mode 'c++-mode)
    (should (not (member 'ac-source-ctags-java-method
                         ac-sources)))
    ;; transition from c++-mode to java-mode
    (ac-ctags-update-ac-sources 'c++-mode 'java-mode)
    (should (member 'ac-source-ctags-java-method
                    ac-sources))))

(ert-deftest test-ac-ctags-get-ac-sources-by-mode ()
  (should
   (equal '(ac-source-ctags-java-method)
          (ac-ctags-get-ac-sources-by-mode 'java-mode))))

(ert-deftest test-ac-ctags-candidates-1 ()
  (test-ac-ctags-fixture
   (lambda ()
     (let ((major-mode 'java-mode))
       (ac-ctags-visit-tags-file test-ac-ctags-java-tagsfile2 'new)
       (ac-ctags-update-current-completion-table 'java-mode)
       (should (ac-ctags-candidates-1 "methodWith"))
       (should-not (ac-ctags-candidates-1 "nonexist"))))))

(ert-deftest test-ac-ctags-check-tags-file-updated ()
  (test-ac-ctags-fixture
   (lambda ()
     (let ((major-mode 'java-mode))
       ;; first, create tags file
       (shell-command
        (concat "ctags -f"
                test-ac-ctags-java-tagsfile-for-update
                " --jcode=utf8 --fields=+aiKlmnsSztT"
                " SampleClassOld.java"))
       (ac-ctags-visit-tags-file test-ac-ctags-java-tagsfile-for-update 'new)
       (should-not
        (ac-ctags-candidates-1 "methodWith"))
       ;; next, update tags file
       (sit-for 1)
       (shell-command
        (concat "ctags -f"
                test-ac-ctags-java-tagsfile-for-update
                " --jcode=utf8 --fields=+aiKlmnsSztT"
                " SampleClassNew.java"))
       (should
        (ac-ctags-candidates-1 "methodWith"))
       ))))


(ert-deftest test-ac-ctags-tagsdb-needs-update-p ()
  ;; db created time is newer than tags file modification time
  (should-not
   (ac-ctags-tagsdb-needs-update-p (current-time)))
  ;; older than tags file modification time
  (should
   (ac-ctags-tagsdb-needs-update-p '(0 0)))
  )

(ert-deftest test-ac-ctags-java-parse-before-dot ()
  (should
   (string= "method1"
            (ac-ctags-java-parse-before-dot-part "method1()")))
  (should
   (string= "method1"
            (ac-ctags-java-parse-before-dot-part
             "method1(method2())")))
  (should
   (string= "varname"
            (ac-ctags-java-parse-before-dot-part
             "varname")))
  (should
   (string= "varname"
            (ac-ctags-java-parse-before-dot-part
             "(varname)")))
  (should
   (string= "method1"
            (ac-ctags-java-parse-before-dot-part
             "method1(method2(), method3())")))
  (should
   (string= "method1"
            (ac-ctags-java-parse-before-dot-part
             "method1(method2(), method3(method4()))")))
  (should
   (string= "method2"
            (ac-ctags-java-parse-before-dot-part
             "method1(method2()")))
  (should
   (string= "method3"
            (ac-ctags-java-parse-before-dot-part
             "method1(method2(method3()")))
  (should
   (string= "method1"
            (ac-ctags-java-parse-before-dot-part
             "method1(method2().toString())")))
  (should
   (string= "method1"
            (ac-ctags-java-parse-before-dot-part
             "return method1()")))
  (should
   (string= "SampleClass"
            (ac-ctags-java-parse-before-dot-part
             "new SampleClass()")))
  (should
   (string= "method1"
            (ac-ctags-java-parse-before-dot-part
             "method1(new HashMap<String, new HashMap<int, int>>)"))))

(ert-deftest test-ac-ctags-java-get-method-return-type ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-tagsfile2 'new)
     (should
      (string= "int"
               (ac-ctags-java-get-method-return-type "helloAnotherWorld")))
     (should
      (null (ac-ctags-java-get-method-return-type "SampleClass"))))))

(ert-deftest test-ac-ctags-java-collect-packages ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-goos-tagfile 'new)
     (should
      (equal '("auctionsniper.ui" "auctionsniper.util" "auctionsniper.xmpp")
             (ac-ctags-java-collect-packages "auctionsniper")))
     (should
      (equal '("auctionsniper.ui" "auctionsniper.util")
             (ac-ctags-java-collect-packages "auctionsniper.u")))
     (should
      (equal '("auctionsniper.ui" "auctionsniper.util" "auctionsniper.xmpp")
             (ac-ctags-java-collect-packages "auctionsniper.")))
     (should
      (null (ac-ctags-java-collect-packages "nonexist"))))))

(ert-deftest test-ac-ctags-java-collect-classes-in-package ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-goos-tagfile 'new)
     (should
      (equal '("Announcer" "Defect")
             (ac-ctags-java-collect-classes-in-package "auctionsniper.util")))
     (should
      (null (ac-ctags-java-collect-classes-in-package "non-exisit")))
     )))

(ert-deftest test-ac-ctags-java-package-candidates-1 ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-goos-tagfile 'new)
     (should
      (equal '("auctionsniper.xmpp")
             (ac-ctags-java-package-candidates-1 "auctionsniper.x")))
     (should
      (equal '("auctionsniper.AuctionSniperDriver")
             (ac-ctags-java-package-candidates-1 "auctionsniper.AuctionSniperD"))))))

(ert-deftest test-ac-ctags-java-collect-constructors ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-java-tagsfile2 'new)
     (should
      (equal '("SampleClass()" "SampleClass(int arg1, String arg2)")
             (mapcar #'substring-no-properties
                     (ac-ctags-java-collect-constructors "Samp")))))))

(ert-deftest test-ac-ctags-cpp-parse-before-dot-or-arrow-part ()
  (should
   (string= "varname"
            (ac-ctags-cpp-parse-before-dot-or-arrow-part "varname")))
  (should
   (string= "SomeClass::func1"
            (ac-ctags-cpp-parse-before-dot-or-arrow-part "SomeClass::func1()")))
  (should
   (string= "SomeClass::func1"
            (ac-ctags-cpp-parse-before-dot-or-arrow-part "SomeClass::func1(func2(), func3())")))
  (should
   (string= "varname"
            (ac-ctags-cpp-parse-before-dot-or-arrow-part "(*varname)"))))

(ert-deftest test-ac-ctags-cpp-line-has-typeinfo-p ()
  (should
   (ac-ctags-cpp-line-has-typeinfo-p "iter"
                                     "std::vector<int>::const_iterator iter;"))
  (should
   (ac-ctags-cpp-line-has-typeinfo-p "var"
                                     "int* var;"))
  (should
   (ac-ctags-cpp-line-has-typeinfo-p "var"
                                     "SomeClass * var;"))
  (should
   (ac-ctags-cpp-line-has-typeinfo-p
    "var"
    "SomeClass *foo_, *bar_, *var, *buzz_;")))

(ert-deftest test-ac-ctags-cpp-extract-type-name ()
  (should
   (string= "QLabel"
            (ac-ctags-cpp-extract-type-name "QLabel *namedLabel_;"
                                            "namedLabel_")))
  (should
   (string= "SomeClass"
            (ac-ctags-cpp-extract-type-name
             "SomeClass *foo_, *bar_, *var, *buzz_;"
             "var")))
  (should
   (string= "std::vector<int>::const_iterator"
            (ac-ctags-cpp-extract-type-name
             "std::vector<int>::const_iterator iter;"
             "iter")))
  (should
   (string= "std::map<int, int>"
            (ac-ctags-cpp-extract-type-name
             "const std::map<int, int> map1, map2, map3;"
             "map2")))
  (should
   (string= "std::vector<int>"
            (ac-ctags-cpp-extract-type-name
             "std::vector<int> vec(10);"
             "vec")))
  )

(ert-deftest test-ac-ctags-cpp-get-function-return-type ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-cpp-tagsfile 'new)
     (should
      (string= "void"
               (ac-ctags-cpp-get-function-return-type "set")))
     (should
      (string= "const ParamGeneratorInterface*"
               (ac-ctags-cpp-get-function-return-type
                "BaseGenerator")))
     (should
      (null (ac-ctags-cpp-get-function-return-type "non-exisit"))))))

(ert-deftest test-ac-ctags-cpp-collect-member-functions ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-cpp-tagsfile2 'new)
     (should
      (equal '("set(int i)")
             (ac-ctags-cpp-collect-member-functions "TestClass" "se")))
     (should
      (equal "set(int i)             :void - TestClass"
             (get-text-property 0
                                'view
                                (car
                                 (ac-ctags-cpp-collect-member-functions "TestClass" "set")))))
     (should
      (equal '("TestClass()")
             (ac-ctags-cpp-collect-member-functions "TestClass" "TestC")))
     (should
      (equal '("normal_func()")
             (ac-ctags-cpp-collect-member-functions "TestClass" "normal_"))))))

(ert-deftest test-ac-ctags-cpp-get-typename-of-variable-1 ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-cpp-tagsfile2 'new)
     (should
      (string= "int"
               (ac-ctags-cpp-get-typename-of-variable-1 "i_"))))))

(ert-deftest test-ac-ctags-cpp-get-members-by-scope-operator ()
  (test-ac-ctags-fixture
   (lambda ()
     (ac-ctags-visit-tags-file test-ac-ctags-cpp-tagsfile2 'new)
     (should
      (equal '("get()")
             (mapcar #'substring-no-properties
                     (ac-ctags-cpp-get-members-by-scope-operator "TestClass" "ge"))))
     (should
      (equal '("i_")
             (mapcar #'substring-no-properties
                     (ac-ctags-cpp-get-members-by-scope-operator
                      "TestClass" "i")))))))

(ert-deftest test-ac-ctags-cpp-split-string-by-separator ()
  (should
   (equal '("std" "::" "vector<int>" "::")
          (ac-ctags-cpp-split-string-by-separator "std::vector<int>::" "::")))
  (should
   (equal '("std" "::" "vector<int>")
          (ac-ctags-cpp-split-string-by-separator "std::vector<int>" "::"))))

;; std::vector<std::Map>::[]const_iterator
(ert-deftest test-ac-ctags-cpp-parse-before-scope-operator-1 ()
  (should
   (string=
    "vector"
    (ac-ctags-cpp-parse-before-scope-operator-1 "std::vector<std::Map>")))
  (should
   (string=
    "map"
    (ac-ctags-cpp-parse-before-scope-operator-1 "std::map<std::vector<int>, std::string>"))))

(ert-deftest test-ac-ctags-cpp-strip-angle-brackets ()
  (should
   (string=
    "std::vector"
    (ac-ctags-cpp-strip-angle-brackets "std::vector<int>")))
  (should
   (string=
    "std::map"
    (ac-ctags-cpp-strip-angle-brackets "std::map<std::vector<int>, std::string>"))))

(ert-deftest test-ac-ctags-cpp-remove-trailing-keyword-from-signature ()
  (should
   (string= "()"
            (ac-ctags-cpp-remove-trailing-keyword-from-signature "() const")))
  (should
   (string= "(int i, int j)"
            (ac-ctags-cpp-remove-trailing-keyword-from-signature "(int i, int j)"))))