(load "../auto-complete-ctags.el")
(require 'ert)
(eval-when-compile
  (require 'cl))

(defconst test-ac-ctags-valid-tagfile "cpp.tags")
(defconst test-ac-ctags-valid-gtest-tagfile "gtest.tags")
(defconst test-ac-ctags-cpp-tagsfile "cpp.tags")
(defconst test-ac-ctags-java-tagsfile "java.tags")
(defconst test-ac-ctags-c-tagsfile "c.tags")

(defun test-ac-ctags-fixture (body)
  (let ((ac-ctags-tags-db nil)
        (ac-ctags-current-tags-list nil)
        (ac-ctags-tags-list-set nil)
        (ac-ctags-completion-table nil)
        (ac-ctags-current-completion-table nil))
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
    ;; Check if the length of each element is 3.
    (should (loop for e in (cdar db)
                  do (unless (= (length e) 4) (return nil))
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
    ;; Check if the length of each element is 3.
    (should (loop for e in (cdar db)
                  do (unless (= (length e) 4) (return nil))
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
    ;; Check if the length of each element is 3.
    (should (loop for e in (cdar db)
                  do (unless (= (length e) 4) (return nil))
                  finally return t))))

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
                  do (unless (= (length e) 4) (return nil))
                  finally return t))
    ;; Check java-db
    (should (listp java-db))
    (should (> (length java-db) 1))
    (should (string= "Java" (car java-db)))
    (should (listp (cdr java-db)))
    (should (loop for e in (cdr java-db)
                  do (unless (= (length e) 4) (return nil))
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

(ert-deftest test-ac-ctags-strip-class-name ()
  (should (string= "normal_func"
                   (ac-ctags-strip-class-name "TestClass::normal_func"))))

;; node => (name cmd kind signature)
(ert-deftest test-ac-ctags-node-access ()
  (should (string= "name"
                   (ac-ctags-node-name '("name" "cmd" "kind" "signature"))))
  (should (string= "cmd"
                   (ac-ctags-node-command '("name" "cmd" "kind" "signature"))))
  (should (string= "kind"
                   (ac-ctags-node-kind '("name" "cmd" "kind" "signature"))))
  (should (string= "signature"
                   (ac-ctags-node-signature '("name" "cmd" "kind" "signature"))))
  (should (null
           (ac-ctags-node-kind '("name" "cmd" nil "signature"))))
  (should (null
           (ac-ctags-node-signature '("name" "cmd" nil nil)))))

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
               (ac-ctags-java-document "helloAnotherWorld"))))))
