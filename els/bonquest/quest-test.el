(ert-deftest testQuestJson () 
  (with-temp-buffer
    (org-mode)
    (bonquest-addQuest "test")
    (org-entry-put (point) "TODO" "TODO")
    (let ((json (bonquest--parseQuestJson)))
      (should (equal json
                     "{\"title\":\"test\",\"type\":\"daily\",\"state\":\"todo\",\"content\":null}")))))

(ert-deftest testQuestPost () 
  (with-temp-buffer
    (org-mode)
    (bonquest-addQuest "test")
    (org-entry-put (point) "TODO" "TODO")
    (let ((json (bonquest--parseQuestJson)))
      (deferred:$
          (bonquest--postQuest json)
          (deferred:nextc it
              (lambda (resp)
                (let* ((data (request-response-data resp))
                       (id (alist-get 'id data))) 
                  (should id))))))))

(ert-deftest testBonjournalJson () 
  (with-temp-buffer
    (org-mode)
    (bonjournal-new-entry)
    (let ((json (bonjournal--parseEntryJson)))
      (should (equal json
                     "{\"entry\":\"\\n\\n/05:48/, *Je pense*\\n\"}")))))

(ert-deftest testQuestPost () 
  (with-temp-buffer
    (org-mode)
    (bonjournal-new-entry)
    (let ((json (bonjournal--parseEntryJson)))
      (deferred:$
          (bonjournal--postEntry json)
          (deferred:nextc it
              (lambda (resp)
                (let* ((data (request-response-data resp))
                       (lines (alist-get 'lines data))) 
                  (should (equal lines 4)))))))))
