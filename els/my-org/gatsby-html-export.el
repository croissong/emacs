
(require 'ox)
(require 's)
;;; Code:

(org-export-define-derived-backend 'gatsby-html 'html
  :translate-alist '((footnote-reference . gatsby-html-footnote-reference)
                     (footnote-definition . gatsby-html-footnote-definition)
                     (template . (lambda (contents _) contents))
                     (headline . org-gatsby-headline)
                     (link . gatsby-html-margin-note-link)
                     (src-block . gatsby-html-src-block)))

(defun org-gatsby-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (section-number (and numbers
                                (mapconcat #'number-to-string numbers "-")))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents "")) 
           (ids (delq nil
                      (list (org-element-property :CUSTOM_ID headline)
                            (org-export-get-reference headline info)
                            (org-element-property :ID headline))))
           (preferred-id (concat (car ids) " "
                                 (s-replace " " "_" (org-element-property :raw-value headline))))
           (extra-ids
            (mapconcat
             (lambda (id)
               (org-html--anchor
                (if (org-uuidgen-p id) (concat "ID-" id) id)
                nil nil info))
             (cdr ids) "")))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((type (if numberedp 'ordered 'unordered))
                 (itemized-body
                  (org-html-format-list-item
                   contents type nil info nil
                   (concat (org-html--anchor preferred-id nil nil info)
                           extra-ids
                           full-text))))
            (concat (and (org-export-first-sibling-p headline info)
                         (org-html-begin-plain-list type))
                    itemized-body
                    (and (org-export-last-sibling-p headline info)
                         (org-html-end-plain-list type))))
        (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
              (first-content (car (org-element-contents headline))))
          ;; Standard headline.  Export it as a section.
          (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                  (org-html--container headline info)
                  (concat "outline-container-"
                          (org-export-get-reference headline info))
                  (concat (format "outline-%d" level)
                          (and extra-class " ")
                          extra-class)
                  (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                          level
                          preferred-id
                          extra-ids
                          (concat
                           (and numberedp
                                (format
                                 "<span class=\"section-number-%d\">%s</span> "
                                 level
                                 (mapconcat #'number-to-string numbers ".")))
                           full-text)
                          level)
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
                  ;; `org-info.js'.
                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  (org-html--container headline info)))))))

(defun gatsby-html-footnote-reference (footnote-reference contents info)
  "Create a footnote according to the gatsby css format.
FOOTNOTE-REFERENCE is the org element, CONTENTS is nil.  INFO is
a plist holding contextual information."
  (format "<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label><input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/><span class=\"sidenote\">%s</span>"
          (org-export-get-footnote-number footnote-reference info)
          (org-export-get-footnote-number footnote-reference info)
          (org-trim (org-export-data (org-export-get-footnote-definition footnote-reference info) info))))
(defun gatsby-html-margin-note-link (link desc info)
  "LINK is the margin note (or not).
If it is not, it willl be passed onto the original function in
order to be handled properly.  DESC is the description part of
the link.  INFO is a plist holding contextual information."
  (let ((path (split-string (org-element-property :path link) ":")))
    (if (and (string= (org-element-property :type link) "fuzzy")
             (string= (car path) "mn"))
        (format "<label for=\"%s\" class=\"margin-toggle\">&#8853;</label><input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/> <span class=\"marginnote\">%s</span>"
                (cadr path) (cadr path)
                desc)
      (org-html-link link desc info))))
;; This function has no use.
;; (defun gatsby-html-footnote-definition (footnote-definition contents info)
;;   "Create a footnote according to the gatsby css format.  FOOTNOTE-DEFINITION is the org element, CONTENTS is nil.  INFO is a plist holding contextual information."
;;   "")
(defun gatsby-html-src-block (src-block contents info)
  "Transcode an SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "<pre class=\"code\">%s</pre>"
          (org-html-format-code src-block info)))
;; This function was used to verify my output.  It's pretty similar
;; (read copied) from the beamer export.
(defun org-gatsby-html-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Myhtml buffer.
If narrowing is active in the current buffer, only export its
narrowed part.
If a region is active, export that region.
A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.
When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.
When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.
When optional argument BODY-ONLY is non-nil, only write code
between the bo
EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.
Export is done in a buffer named \"*Org MYHTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'gatsby-html "*Org MYHTML Export*"
    async subtreep visible-only body-only ext-plist (lambda () (html-mode))))
(provide 'gatsby-html)
;;; gatsby-html.el ends here
