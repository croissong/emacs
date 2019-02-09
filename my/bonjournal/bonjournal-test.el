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
	      ("-!-" (concat (getDatetreeEntry) "/17:00/, *Je pense*\ns-!-"))))

(defun getDatetreeEntry ()
  (with-temp-buffer
    (org-mode)
    (org-datetree-find-date-create (calendar-gregorian-from-absolute (org-today)))
  (buffer-substring-no-properties (point-min) (point-max))))

(defun xt-date (_)
  (bonjournal--setDate)
  (bonjournal--getDate))

(xt-deftest date
  (xt-note "Testing the insert procedure.")
    (xtd-return= 'xt-date
		("-!-" (org-today))))

(xt-deftest hashAtPoint
  (xt-note "Testing the insert procedure.")
  (xtd-return= (lambda(_) (bonjournal--getHashAtPoint))
	      ("#-!-sohn" "sohn")))

(xt-deftest removeHash
  (xt-note "nvm hashtag parsing is not working its weird this test is kinda useless")
  (xtd-setup= (lambda(_)
	        (bonjournal--removeHash))
	      ("#-!-hash" "#-!-")))

