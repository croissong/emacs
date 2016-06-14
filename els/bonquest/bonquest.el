;;; bonquest.el --- Write journal in emacs org-mode  -*- lexical-binding: t; -*-

(require 'request-deferred)

(defun bonquest--parseContent (headline) 
  (org-element-map headline 'paragraph
    (lambda (h) (buffer-substring-no-properties
                 (org-element-property :contents-begin h)
                 (org-element-property :contents-end h)))
    nil t))

(defun bonquest-getOpenQuests ()
  (deferred:$
      (bonquest--fetchQuests "todo")
      (deferred:nextc it
          (lambda (resp)
            (let ((quests (request-response-data resp)))
              (-map 'bonquest--insertQuest quests))))
    ))

(defun bonquest--insertQuest (quest)
  (org-insert-heading-respect-content)
  (insert (alist-get 'title quest))
  (let ((point (point))
        (content (alist-get 'content quest))) 
    (org-entry-put point "TODO" (alist-get 'state quest))
    (org-entry-put point "type" (alist-get 'type quest))
    (org-entry-put point "id" (alist-get 'id quest))
    (when content
      (newline)
      (insert content))))

(defun bonquest--fetchQuests (state)
  (request-deferred
   "localhost:4000/getQuests"
   :headers '(("Content-Type" . "application/json"))
   :type "POST"
   :data (json-encode `(:state ,state))
   :parser 'json-read))

(defun bonquest--fetchInfo (character)
  (request-deferred
   "localhost:4000/characterInfo"
   :headers '(("Content-Type" . "application/json"))
   :type "POST"
   :data (json-encode `(:character ,character))
   :parser 'json-read))

(defun bonquest--getInfo()
  (interactive) 
  (deferred:$
      (bonquest--fetchInfo "Skender")
      (deferred:nextc it
          (lambda (resp)
            (let* ((data (request-response-data resp))
                   (char (alist-get 'char data)))
              (message "%S" char)
              )))))

(defun bonquest--sendQuest()
  (interactive)
  (if (org-at-heading-p)
      (progn
        (org-narrow-to-element)
        (org-entry-put (point) "TODO" "todo")
        (let* ((json (bonquest--parseQuestJson)))
          (deferred:$
              (bonquest--postQuest json)
              (deferred:nextc it
                  (lambda (resp)
                    (let* ((data (request-response-data resp))
                           (id (alist-get 'id data)))
                      (org-entry-put (point) "id" id)
                      )))))
        (widen))
    (message "Not on headline")))

(defun bonquest--parseQuestJson()
  (let* ((buff (org-element-parse-buffer))
         (headline (org-element-map buff 'headline (lambda (x) x) nil t))
         (quest `(:title ,(org-element-property :raw-value headline)
                         :type ,(org-element-property :QUEST-TYPE headline)
                         :state ,(org-element-property :todo-type headline)
                         :content ,(bonquest--parseContent headline))))
    (json-encode-list quest)))

(defvar bonquest--standardQuests '(("c" "Cleaning")
                                   ("cw". ("Wäsche" (:title "Wäsche")))))

(defun bonquest-finishStandard ()
  (let* ((quest (org-mks bonquest--standardQuests "Quests")))
    (setq quest `(:title ,(plist-get (nth 2 quest) :title)
                         :type "standard"
                         :state "done"
                         :content ""))
    (setq quest (json-encode-list quest))
    (deferred:$
        (bonquest--postQuest quest)
        (deferred:nextc it
            (lambda (resp)
              (let* ((data (request-response-data resp))
                     (id (alist-get 'id data)))
                (message id)
                )))))
  )

(defun bonquest-addQuest (title)
  (interactive "sQuest title: ")
  (org-insert-heading-respect-content)
  (let ((point (point)))
    (insert title) 
    (org-entry-put point "quest-type" "daily")))


(defun bonquest--postQuest(questJson)
  (message "sending quest %s" questJson)
  (request-deferred
   "localhost:4000/addQuest"
   :headers '(("Content-Type" . "application/json"))
   :type "POST"
   :data questJson
   :parser 'json-read))

(defun org-json-straigten-tree (tree)
  "Null out circular references in the org-element TREE"
  (org-element-map tree (append org-element-all-elements
                                org-element-all-objects '(plain-text))

    ;; the crux of this is to nullify references that turn the tree
    ;; into a Circular Object which the json module can't handle
    (lambda (x) 
      ;; guaranteed circluar
      (if (org-element-property :parent x)
          (org-element-put-property x :parent "none"))
      ;; maybe circular if multiple structures accidently identical
      (if (org-element-property :structure x)
          (org-element-put-property x :structure "none"))
      ))
  tree)

(defun stringToSymbols (list)
  (when (stringp (car-safe list))
    (setq list (-replace-at 0 (make-symbol (car list)) list)))
  (-map
   (lambda(x) (if (listp x)
                  (stringToSymbols x)
                x))
   list))



;; (defun testRpg ()
;;   (org-element-map (org-element-parse-buffer) 'headline
;;     (lambda (headline)
;;       (if (org-element-property :TODO))
;;       (message "%s" headline)
;;       (setq headline (org-json-straigten-tree headline)) 
;;       (setq headline (json-encode-list headline)) 
;;       (setq headline (let ((json-object-type 'plist)
;;                            (json-array-type 'list) )
;;                        (json-read-from-string headline)))
;;       (setq headline (stringToSymbols headline))
;;       (org-element-interpret-data headline))))

;; (add-hook 'org-after-todo-state-change-hook
;;           (lambda () (message "%S" org-state)))

;; (let* ((main (assoc 'main spaceline--mode-lines))
;;        (right (cdr main)))
;;   (setcdr main (push 'rpg right))
;;   (assq-delete-all 'main spaceline--mode-lines)
;;   (push spaceline--mode-lines ')
;;   main)
;; (spaceline-compile)


(provide 'bonquest)
