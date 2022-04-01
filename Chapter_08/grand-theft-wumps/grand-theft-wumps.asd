(defsystem "grand-theft-wumps"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "grand-theft-wumps/tests"))))

(defsystem "grand-theft-wumps/tests"
  :author ""
  :license ""
  :depends-on ("grand-theft-wumps"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for grand-theft-wumps"
  :perform (test-op (op c) (symbol-call :rove :run c)))
