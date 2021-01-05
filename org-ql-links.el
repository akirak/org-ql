;;; org-ql-link.el --- Search links in Org mode -*- lexical-binding: t -*-

(require 'tablist)
(require 'org)
(require 'ol)
(require 'org-ql)

(defgroup org-ql-link
  nil
  "Display Org mode links in a table"
  :group 'org)

(cl-defstruct org-ql-link-match-data
  "Data extracted from an Org bracket link."
  uri description metadata link-string)

;;;; Querying

(cl-defun org-ql-link (buffers-or-files query &key match narrow)
  (declare (indent 1))
  (->> (org-ql-select buffers-or-files query
         :action #'org-ql-link--links
         :narrow narrow)
       (-flatten-n 1)))

(defun org-ql-link-heading-metadata ()
  `((buffer . ,(buffer-name))
    (outline . ,(org-ql-link-format-outline))
    (tags . ,(org-get-tags))))

(defun org-ql-link-format-outline ()
  (org-format-outline-path (org-get-outline-path t t)))

(defun org-ql-link--links ()
  (let ((metadata (org-ql-link-heading-metadata))
        (end (save-excursion
               (forward-char)
               (when (re-search-forward org-heading-regexp nil t)
                 (org-back-to-heading)
                 (point))))
        result)
    (while (re-search-forward org-link-bracket-re end t)
      (let ((link-string (match-string 0))
            (uri (substring-no-properties (match-string 1)))
            (description (substring-no-properties (match-string 2))))
        (push (make-org-ql-link-match-data
               :uri uri
               :description description
               :metadata metadata
               :link-string link-string)
              result)))
    (nreverse result)))

;;;; Helm

(defun helm-org-ql-link-format (x)
  (let ((desc (org-ql-link-match-data-description x))
        (uri (org-ql-link-match-data-uri x))
        (metadata (org-ql-link-match-data-metadata x)))
    (concat (alist-get 'outline metadata)
            "\n  "
            (if desc
                (concat desc "  " uri)
              uri))))

(defun helm-org-ql-link (buffers-files)
  (interactive (list (current-buffer)))
  (let ((helm-input-idle-delay helm-org-ql-input-idle-delay))
    (helm :prompt "Query"
          :sources (helm-org-ql-link-source buffers-files))))

(cl-defun helm-org-ql-link-source (buffers-files &key (name "helm-org-ql-link"))
  "Return Helm source named NAME that searches BUFFERS-FILES with `helm-org-ql'."
  ;; Expansion of `helm-build-sync-source' macro.
  (helm-make-source name 'helm-source-sync
    :candidates (lambda ()
                  (let* ((query `(and (link)
                                      ,(org-ql--query-string-to-sexp helm-pattern)))
                         (window-width (window-width (helm-window))))
                    (when query
                      (with-current-buffer (helm-buffer-get)
                        (setq helm-org-ql-buffers-files buffers-files))
                      (ignore-errors
                        ;; Ignore errors that might be caused by partially typed queries.
                        (-map (lambda (x)
                                (cons (helm-org-ql-link-format x)
                                      x))
                              (org-ql-link helm-org-ql-buffers-files query
                                           :narrow t))))))
    :match #'identity
    :fuzzy-match nil
    :multimatch nil
    :nohighlight t
    :volatile t
    :multiline t
    :action
    (lambda (x)
      (org-link-open-from-string (org-ql-link-match-data-link-string x)))
    ;; :keymap helm-org-ql-link-map
    ;; :action helm-org-ql-link-actions
    ))

(defvar-local org-ql-link-links nil)

(defcustom org-ql-link-tablist-format
  [("Description" 30 t)
   ("URI" 10 t)
   ("Buffer" 15 t)
   ("Outline" 50 t)
   ("Tags" 30 t)]
  "`tabulated-list-format' for the table of Org links.")

(defvar-local org-ql-link-links nil)

(defun org-ql-link-tablist-refresh ()
  (setq tabulated-list-entries
        (-map (lambda (x)
                (let ((metadata (org-ql-link-match-data-metadata x)))
                  (vector
                   (or (org-ql-link-match-data-description x) "")
                   (org-ql-link-match-data-uri x)
                   (alist-get 'buffer metadata)
                   (alist-get 'outline metadata)
                   (string-join (alist-get 'tags metadata) ","))))
              org-ql-link-links)))

(cl-defun org-ql-link-in-headings (heading-markers name
                                                   &key
                                                   subtrees)
  (let ((links (->> heading-markers
                    (-map `(lambda (marker)
                             (org-ql-link-links-in-entry marker
                                                         :subtrees ,subtrees
                                                         :no-properties t
                                                         :metadata-func
                                                         #'org-ql-link-heading-metadata)))
                    (-flatten-n 1)))
        (buffer (generate-new-buffer (format "*oldisplay %s*" name))))
    (with-current-buffer buffer
      (setq org-ql-link-links links)
      (org-ql-link-tablist-refresh)
      (pop-to-buffer buffer '((display-buffer-in-side-window)
                              . ((side . bottom))))
      (org-ql-link-tablist-mode)
      (tablist-revert))))

(define-derived-mode org-ql-link-tablist-mode tabulated-list-mode "Oldisplay Tablist"
  "Major mode for displaying a list of Org links."
  (setq tabulated-list-format org-ql-link-tablist-format
        tabulated-list-sort-key '("Description" . nil)
        tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'org-ql-link-tablist-refresh t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

;; Just for debugging.
(defun org-ql-link-display-as-expression ()
  (pp-display-expression
   (save-excursion
     (org-back-to-heading)
     (org-ql-link-links-in-entry (point-marker)
                                 :subtrees t
                                 :no-properties t))
   "*oldisplay expression*"))

(defun org-ql-link-this-headline ()
  (interactive)
  (let ((marker (save-excursion
                  (org-back-to-heading)
                  (point-marker)))
        (name (org-get-heading)))
    (org-ql-link-in-headings (list marker)
                             name
                             :subtrees t)))

(provide 'ol-display)
;;; ol-display.el ends here
