;;; mydaybook-tests.el --- Tests for mydaybook.el
(require 'xtest)

(xt-deftest cleanUp
  (xt-note "Testing the insert procedure.")
    (xtd-setup= (lambda (_) (my/daybook--cleanLastEntry))
		(";; -*- date: \"(1 1 2016)\"; -*-" ";; -*- date: \"(1 1 2016)\"; -*--!-")
		("x\n    " "x-!-")
		("x\n/17:00/, *Je pense*" "x-!-")
		("x\n/17:00/, *Je pense*\n" "x-!-")
		("x\n/17:00/, *Je pense*\ns" "x\n/17:00/, *Je pense*\ns-!-")))

(xt-deftest getEntries
  (xt-note "Testing the insert procedure.")
    (xtd-return= (lambda (_) (my/daybook--getEntries))
		(";; -*- date: \"(1 1 2016)\"; -*-\n\n/17:00/, *Je pense*\ns" "\n/17:00/, *Je pense*\ns")))

(defun xt-date (_)
  (my/daybook--setDate)
  (my/daybook--getDate))

(xt-deftest date
  (xt-note "Testing the insert procedure.")
    (xtd-return= 'xt-date
		("" (org-today))))
