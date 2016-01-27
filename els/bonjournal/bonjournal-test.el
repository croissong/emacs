;;; bonjournal-tests.el --- Tests for bonjournal.el
(require 'xtest)

(defun getTestEntry (i)
  (pcase i
    (1 "/17:00/, *Je pense*\ns")
    (2 "")))

(pcase 2 (1 2) (2 3))
(xt-deftest cleanUp
  (xt-note "Testing the insert procedure.")
  (xtd-setup= (lambda (_) (bonjournal--cleanLastEntry))
	      (";; -*- date: \"(1 1 2016)\"; -*-" ";; -*- date: \"(1 1 2016)\"; -*--!-")
	      ("x\n    " "x-!-")
	      ("x\n/17:00/, *Je pense*" "x-!-")
	      ("x\n/17:00/, *Je pense*\n" "x-!-")
	      ("x\n/17:00/, *Je pense*\ns" "x\n/17:00/, *Je pense*\ns-!-")))

(xt-deftest getEntries
  (xt-note "Testing the insert procedure.")
    (xtd-return= (lambda (_) (bonjournal--getEntries))
		 (";; -*- date: \"(1 1 2016)\"; -*-\n\n/17:00/, *Je pense*\ns" "\n/17:00/, *Je pense*\ns")))

(xt-deftest insertEntry
  (xt-note "diesdas")
  (xtd-setup= (lambda(_)
		(bonjournal--insertEntry (getTestEntry 1) (org-today)))
	      ("-!-" "\n* 2016\n** 2016-01 Januar\n*** 2016-01-27 Mittwoch\n<2016-01-27 Mi>\n/17:00/, *Je pense*\ns-!-")))

(defun xt-date (_)
  (bonjournal--setDate)
  (bonjournal--getDate))

(xt-deftest date
  (xt-note "Testing the insert procedure.")
    (xtd-return= 'xt-date
		("" (org-today))))
