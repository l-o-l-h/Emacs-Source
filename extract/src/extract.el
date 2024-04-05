;;; extract.el - Attach files -*- mode: elisp -*-
;;; Time-stamp: <2024-04-05 07:40:01 minilolh>
;;; Version 0.1.3 [2024-04-04 16:45]

;;; Commentary:

;; 1. Attach Court File PDFs to a Case Note
;; 2. Extract PDFs from Complaint and attach to Exhibits
;; 3. Update PDFs

;;; Code:

;;;===================================================================

(require 'org-attach)

(defconst *lolh/process-dir*
  (expand-file-name "~/Downloads/process"))
(defconst *lolh/pdftk-jar-path*
  (expand-file-name "~/.local/share/bin/pdftk-all.jar"))
(defconst *lolh/first-last-name-re*
  "^\\([^[:space:]]+\\).*[[:space:]]\\([^[:space:]]+\\)$")
(defconst *lolh/docket-date-re*
  "^\\([[:digit:]*]+\\))[[:space:]]\\([[:digit:][-]+]\\)\\.pdf$")
(defconst *lolh/docket-date-name-re*
  "^\\([[:digit:]*)]\\{3,4\\}[[:space:]]\\)\\{0,1\\}\\(\\[\\([[:digit:]-]\\{10\\}\\)\\]\\)\\{0,1\\}[[:space:]]?\\(.*\\)[.pPdDfF]\\{4\\}$"
  ;;; 1                                   1           2     3                      3     2                       4    4
  "1: Optional docket: 02*) (needs `string-trim')
   2: Optional date including brackets: [2024-04-01]
   3: Optional date w/out brackets: 2024-04-01
   4: Optional name: blah")
(defconst *lolh/props-re*
  "^\\(.*\\)[[:space:]]\\[\\([[:digit:]-]+\\)\\][[:space:]]\\([[:digit:]]+\\)[[:space:]]\\([[:digit:]]+\\)$")
(defconst *lolh/exhibit-or-source-re*
  "^EXHIBIT-[[:alnum:]]\\|^SOURCE")

(defvar *lolh/note-tree*)

(defun lolh/note-tree () (setq *lolh/note-tree* (org-element-parse-buffer)))

;;; The following command requires that there be a main heading titled
;;; * RTC CASE
;;; that contains the Properties
;;; :CAUSE: with a valid cause number, e.g. 24-2-99999-06
;;; :DEF-1: with the defendant's caption name, e.g., John Smith
;;; It also requires a subheading of
;;; ** COURT FILES
;;; Given those requirements, this command attaches the Google Drive
;;; Court File documents to this subheading
;;; in a subdirectory .../data/24-2-99999-06 John Smith/Court File/


(keymap-global-set "C-x p a" #'lolh/court-files-attach)
(keymap-global-set "C-x p h" #'lolh/extract-pdfs)
(keymap-global-set "C-x p u" #'lolh/update-pleadings)


(defun lolh/court-files-attach ()
  (interactive)
  (lolh/pdf-attach "Court File" "COURT FILES"))

(defun lolh/pdf-attach (subdir hl)
  "Attach all of the Google documents from a SUBDIR to the current note HL.

  An example would be all documents in the Court File for a case."

  (interactive)
  (lolh/note-tree)

  (let* ((court-file-attach-dir (lolh/attach-dir subdir)) ; e.g. "Court File"
         (gd-court-file-dir (lolh/gd-cause-dir subdir)) ; e.g. "Court File"
         (cause (lolh/cause)))        ; e.g. "COURT FILES"
    (lolh/set-note-property-in-headline hl "DIR" court-file-attach-dir)
    (mapc (lambda (f) (org-attach-attach f nil 'lns))
          (directory-files gd-court-file-dir :full directory-files-no-dot-files-regexp))))


(defun lolh/extract-pdfs ()
  (interactive)
  (lolh/note-tree)
  ;; first get data from the note buffer
  (let* ((nps (lolh/extract-properties))
         ;; find the identity of the document to extract from
         (source (assoc "SOURCE" nps))
         ;; find the list of document data to extract
         (exs (assoc-delete-all "SOURCE" (copy-alist nps)))
         (attach-dir (lolh/attach-dir "Notices & Lease")))

    (when nps ; don't process if nps is nil (no note properties found)
      ;; copy the source into ~/Downloads/process directory
      (let ((complaint (file-name-concat *lolh/process-dir* "complaint.pdf"))
            (src (lolh/gd-source-url (cdr source) "Court File")))
        (copy-file src complaint t))

      ;; map over the plist of document data to extract
      ;; ((EXHIBIT-1 . Lease[date] 1 2) (EXHIBIT-2 . Notice[date] 3 4) ...)
      (mapc (lambda (ex)
              (let* ((key (car ex))   ; e.g. EXHIBIT-1
                     (val (cdr ex))   ; e.g. Lease [date] 1 2
                     (props (if
                                (string-match *lolh/props-re* val)
                                (list (cons :full (match-string 0 val))
                                      (cons :type (match-string 1 val))
                                      (cons :date (match-string 2 val))
                                      (cons :beg (match-string 3 val))
                                      (cons :end (match-string 4 val)))
                              (error "Improper format found: %s" val))))

                (let* ((type (cdr (assq :type props))) ; e.g. Lease
                       (date (cdr (assq :date props)))
                       (beg-end (format "%s-%s"
                                        (cdr (assq :beg props))
                                        (cdr (assq :end props))))
                       (gd-file-name (lolh/create-gd-file-name nil date (format "%s %s" key type)))
                       (output-name (expand-file-name
                                     (file-name-concat
                                      *lolh/process-dir* gd-file-name))))
                  ;; pdftk / java -jar pdftk-all.jar
                  (call-process-shell-command
                   (combine-and-quote-strings
                    (list
                     "java" "-jar" *lolh/pdftk-jar-path*
                     (file-name-concat *lolh/process-dir* "complaint.pdf")
                     "cat" beg-end
                     "output" output-name))))))
            exs)

      (lolh/set-note-property-in-headline "EXHIBITS" "DIR"
                                          (lolh/attach-dir "Notices & Lease"))
      ;; Move the extracted documents into their home and then attach them
      (mapc (lambda (ex)
              (let ((dest-dir (file-name-concat (lolh/gd-cause-dir "Notices & Lease")
                                                (file-name-nondirectory ex))))
                (rename-file ex dest-dir t)
                (org-attach-attach dest-dir nil 'lns)))
            (directory-files *lolh/process-dir* t "EXHIBIT"))
      (delete-file complaint))))


(defun lolh/add-file-to-gd (file dest)
  "Place a FILE found in *lolh/process-dir* into DEST in the Google Drive.

  Also attach it."

  (interactive)

  (let ((new-name (lolh/create-gd-file-name nil )))))


(defun lolh/update-pleadings ()
  "Update attachment documents for case note.

  New documents should be downloaded from Onbase and placed into the Process
  directory with the docket number for any starred files, and the docket number
  and date for any new files not yet in the Google Drive.

  This command will add the starred files to the Google Drive using the correct
  case name, and will then ask for the file name for the remaining files and
  also place them into the Google Drive.

  All new files will be sym-linked into the attachment directory."

  (interactive)
  (lolh/note-tree)
  (let* ((attach-dir (lolh/attach-dir "Court File"))
         (court-file (lolh/gd-cause-dir "Court File"))
         ;; Find the old files in the Google Drive (those with *s)
         (pleadings (directory-files court-file nil "[[:digit:]]+[*])"))
         (new-files (directory-files *lolh/process-dir* nil
                                     directory-files-no-dot-files-regexp)))
    ;; Rename files with * to final names without *
    (mapc (lambda (pleading)
            (unless
                (string-match "^[[:digit:]]+" pleading)
              (error "Something is wrong with %" pleading)) ; find the docket number
            (let* ((f (match-string 0 pleading)) ; docket number
                   (old-dir (file-name-concat court-file pleading)) ; file to be deleted
                   ;; file in attachment dir with asterisk that is to be deletednnn
                   (new-attach-dir (file-name-concat attach-dir pleading))
                   ;; find the new file with the same docket number
                   (new-pleading (seq-find (lambda (a) (string-match-p f a)) new-files nil))
                   ;; give the new file name a full path in the Process dir
                   (new-pleading-dir (file-name-concat *lolh/process-dir* new-pleading))
                   ;; create the new file name without an asterisk
                   (p-new (format "%s%s" (substring pleading 0 2) (substring pleading 3)))
                   ;; give the new name a full path in the Google Drive
                   (new-dir (file-name-concat court-file p-new)))
              (rename-file new-pleading-dir new-dir)
              ;; attach new-dir
              (make-symbolic-link new-dir attach-dir)

              ;; delete the old files
              (delete-file old-dir)
              (delete-file new-attach-dir)))
          pleadings)
    ;; Add new files, with and without *
    (let* ((new-pleadings (directory-files *lolh/process-dir* nil directory-files-no-dot-files-regexp))
           (new-pleadings (seq-remove (lambda (f) (string= ".DS_Store" f)) new-pleadings)))
      (mapc (lambda (f)
              ;; find the docket number and date of new files
              (unless (string-match *lolh/docket-date-re* f)
                (error "Something is wrong with %s" f))
              (let* ((f-dir (file-name-concat *lolh/process-dir* f))
                     (docket (match-string 1 f))
                     (date (string-trim (match-string 2 f) "\\[" "\\]"))
                     (new-str (concat (lolh/create-gd-file-name docket date) "? "))
                     (new-name (read-string new-str))
                     (new-full-name (lolh/create-gd-file-name docket date new-name))
                     (new-full-name-dir (file-name-concat court-file new-full-name))
                     (attach-dir-file (file-name-concat attach-dir new-full-name)))
                (rename-file f-dir new-full-name-dir)
                ;; attach new-full-name-dir
                (make-symbolic-link new-full-name-dir attach-dir-file)))
            new-pleadings))))


;;;===================================================================


(defun lolh/gd-year (year)
  "Given a YEAR ('2024'), return local path to the Google Drive for that YEAR.

The GOOGLE_DRIVE environment variables must be set and named correctly."

  (unless (and
           (getenv "GOOGLE_DRIVE_2022")
           (getenv "GOOGLE_DRIVE_2023")
           (getenv "GOOGLE_DRIVE_2024"))
    (error "Check that the google_drive environment variables are set properly"))
  (or (getenv (concat "GOOGLE_DRIVE_" year))
      (error "Year %s did not return a value" year)))


(defun lolh/gd-cause-dir (&optional subdir)
  "Return the path of the Google Drive Year for CAUSE of current case note.

If optional argument SUBDIR is present, add it as a component."

  (let* ((cause (lolh/cause))
         (cause-year (format "20%s" (substring cause 0 2)))
         (gd-url (lolh/gd-year cause-year))
         (gd-cause-dir (car (directory-files gd-url t cause))))
    (file-name-as-directory (file-name-concat gd-cause-dir subdir))))


(defun lolh/gd-source-url (source dir)
  "Find and return DIR/SOURCE url in the Google Drive.

E.g., a Complaint"

  (let* ((gd-court-url (lolh/gd-cause-dir dir))
         (gd-source-url (car (directory-files gd-court-url t source))))
    (or gd-source-url (error "Could not find the source: %s" source))))


(defun lolh/get-headline-element (headline)
  "Given the name of a HEADLINE, return the element from the note-tree."

  (org-element-map *lolh/note-tree* 'headline
    (lambda (hl) (when
                     (string= headline (org-element-property :raw-value hl))
                   hl))
    nil t t))


(defun lolh/put-tag-in-headline (tag headline)
  "Put a TAG into a HEADLINE."

  (let* ((hl (lolh/get-headline-element headline))
         (begin (org-element-property :begin hl))
         (tags (org-element-property :tags hl))
         (new-tags (push tag tags)))
    (save-excursion
      (goto-char begin)
      (org-set-tags new-tags)))
  (lolh/note-tree))


(defun lolh/note-property (property &optional hl)
  "Return the PROPERTY from the current note TREE optionally in headline HL.

Return NIL if there is no PROPERTY."

  (org-element-map *lolh/note-tree* 'node-property
    (lambda (np) (when (string= (org-element-property :key np) property)
                   (setq val (string-trim-left (org-element-property :value np) "-- "))
                   (if hl
                       (let* ((p1 (org-element-property :parent np)) ; property-drawer
                              (p2 (org-element-property :parent p1)) ; section
                              (p3 (org-element-property :parent p2)) ; headline
                              (v (org-element-property :raw-value p3))
                              (in-hl (string= v hl)))
                         (and in-hl val))
                     val)))
    nil t t))


(defun lolh/cause ()
  "Return the CAUSE for the current note."

  (lolh/note-property "CAUSE"))


(defun lolh/set-note-property-in-headline (headline new-property new-value)
  "Set a NEW-PROPERTY to NEW-VALUE in the property drawer found within HEADLINE."

  (let ((hl (lolh/get-headline-element headline)))
    (let  ((begin (org-element-property :begin hl)))
      (goto-char begin)
      (org-entry-put begin new-property new-value)))
  (lolh/note-tree))


(defun lolh/note-date-name (date name)
  "Return a file name with cause-date-name."

  (let ((cause (lolh/note-property "CAUSE"))
        (def-1 (lolh/note-property "DEF-1")))
    (format "%s [%s] %s -- %s" cause date def-1 name)))


(defun lolh/attach-dir (&optional dir)
  "Return a path to the local parent attachment directory for a case file.

Point must be in the note for which the attachment directory is associated.
The path will look something like:
~/path-to/notes/ccvlp/cases/data/24-2-99999-06 John Smith/

The optional argument DIR is added as a final path if it is included."

  (let* ((parent-dir (abbreviate-file-name
                      (file-name-parent-directory (buffer-file-name))))
         (cause (lolh/note-property "CAUSE"))
         (def1 (lolh/note-property  "DEF-1"))
         (attach-dir (file-name-as-directory
                      (file-name-concat
                       parent-dir "data" (format "%s %s" cause def1) dir))))
    attach-dir))


(defun lolh/extract-properties ()
  "Find the PROPERTIES that are to be extracted and return them.

All such headlines will have an EXTRACT tag but not an ATTACH tag.
If a headlines has an ATTACH tag, ignore it as it has already been processed.
For example, the headline ** EXHIBITS containts a :SOURCE: property, an
EXHIBIT-1 property, and an EXHIBIT-2 property, with appropriate values.
Ignore all other properties.

The returned value will either be nil or a plist with the form:
((SOURCE . <url>) (EXTRACT-1 . <data>) (EXTRACT-2 . <data>) ... ))"

  ;; Find an :EXTRACT: tag attached to a headline (e.g. ** EXHIBITS)
  ;; but return nil it if it has already been attached (has an :ATTACH: tag)
  (let* ((return-val (org-element-map (lolh/note-tree) 'headline
                       (lambda (hl)
                         (when (and
                                (member "EXTRACT" (org-element-property :tags hl))
                                (not (member "ATTACH" (org-element-property :tags hl))))
                           ;; find the property drawer
                           (let ((pd (org-element-map hl 'property-drawer #'identity nil t)))
                             ;; process each node property, gathering just the SOURCE and EXHIBITS
                             (org-element-map pd 'node-property
                               (lambda (np)
                                 (let ((key (org-element-property :key np))
                                       (val (string-trim-left (org-element-property :value np) "-- ")))
                                   (when (string-match-p *lolh/exhibit-or-source-re* key)
                                     ;; return the found data
                                     (cons
                                      (format "%s" key)
                                      (format "%s" val))))))))))))
    (car return-val)))


(defun lolh/attach-dired-to-subtree (url &optional filter)
  "Attach all files at the given URL to the prepared attachment directory.

If FILTER is set to a regexp, attach the matched files."
  (dired-other-window url)
  (if filter
      (dired-mark-files-regexp filter)
    (dired-toggle-marks))
  (org-attach-dired-to-subtree (dired-get-marked-files))
  (dired-unmark-all-marks)
  (delete-window))


(defun lolh/first-last-name ()
  "Return the parsed first and last name for DEF-1."
  (let ((def-1 (lolh/note-property "DEF-1")))
    (unless (string-match *lolh/first-last-name-re* def-1)
      (error "Name \"%s\" appears to be malformed." def-1))
    (let* ((first-name (match-string 1 def-1))
           (last-name (upcase (match-string 2 def-1)))
           (name (format "%s,%s -- " last-name first-name)))
      name)))


(defun lolh/extract-date-name (file-name &optional type)
  "Give a FILE-NAME in *lolh/process-dir*, extract a date and a name."

  (unless (string-match ".*\\[\\([[:digit:]-]+\\)\\][[:space:]]+\\(.*\\)[.pPdDfF]\\{4\\}$" file-name)
    (error "Failed to parse the file-name: %s" file-name))
  ;;(format "Found date: %s and name: %s" (match-string 1 file-name) (match-string 2 file-name))
  (list :date (match-string 1 file-name) :name (format "%s(%s)" (if type (concat type " ") "") (match-string 2 file-name))))

(defun lolh/extract-docket-date-name (file-name)
  "Given a FILE-NAME, extract optional docket number, date, and name.

The file-name should include the suffix .pdf or .PDF."

  )


(defun lolh/create-gd-file-name (&optional docket date body)
  "With point in a note, return a file name with DOCKET, DATE, and BODY.

Use an empty string for any missing arguments."

  (let* ((docket (if docket (format "%s) " docket) ""))
         (date (or date "yyyy-mm-dd"))
         (body (or body "Need File Name"))
         (cause (lolh/cause))
         (def-1 (lolh/note-property "DEF-1"))
         (def-2 (lolh/note-property "DEF-2"))
         (last-first-1 (lolh/create-first-last def-1))
         (last-first-2 (lolh/create-first-last def-2)))
    (format "%s%s [%s] %s%s -- %s.pdf" docket cause date last-first-1 (format "%s" (if last-first-2 (concat "-" last-first-2) "")) body)))


-----------------------------------------------------------------------


(defun lolh/process-dir (&optional body)
  "Run through all files in *lolh/process-dir* and rename them.

If BODY is non-nil, then request an additional name for each file."

  (interactive)

  (let ((files (directory-files *lolh/process-dir* nil "^[^.]"))
        new-files)
    (dolist (f files new-files)
      (let* ((body (when body (read-string (concat f "? "))))
             (nf (lolh/create-gd-file-name-2 f (if (string= "" body) nil body))))
        (push nf new-files)))))


(defun lolh/create-gd-file-name-2 (file-name &optional body)
  "Given a FILE-NAME from *lolh/process-dir* and a note, create new file-name with optional BODY."

  (let* ((ex (lolh/extract-docket-date-name file-name))
         (body (when body (concat body " "))) ; BODY can be nil and will be ignore
         (docket (lolh/get-extracted ex :docket)) ; DOCKET can be nil and will be ignored
         (date (or (lolh/get-extracted ex :date) "YYYY-MM-DD")) ; DATE can be nil; YYYY-MM-DD will be substituted
         (name (lolh/get-extracted ex :name)) ; NAME can be empty string; () will be inserted
         (cause (lolh/cause))                 ; CAUSE must exist
         (def-1 (lolh/note-property "DEF-1")) ; DEF-1 must exist
         (def-2 (lolh/note-property "DEF-2")) ; DEF-2 is optional
         (last-first-1 (lolh/create-first-last def-1))
         (last-first-2 (lolh/create-first-last def-2)))
    (format "%s%s [%s] %s%s -- %s(%s).pdf"
            (if docket (concat docket " ") "")
            cause
            date
            last-first-1
            (format "%s" (if last-first-2 (concat "-" last-first-2) ""))
            (or body "") name)))


(defun lolh/extract-docket-date-name (file-name)
  "Extract docket, date, and name from FILE-NAME.

FILE-NAME must end with either `.pdf' or `.PDF'.

Returns a plist: (:docket ... :date ... :name ...)

All elements are optional.  If a `name' is not supplied,
an empty string will be returned.  Nonpresent docket or date
will return `'nil''."

  (unless (string-match *lolh/docket-date-name-re* file-name)
    (error "Unable to parse file-name %s" file-name))
  (let* ((docket-space (match-string 1 file-name))
         (docket (when docket-space (string-trim docket-space)))
         (date (match-string 3 file-name))
         (name (match-string 4 file-name)))
    (list :docket docket :date date :name name)))

(defun lolh/get-extracted (ex part)
  "Given an extracted file-name EX and a PART, return the part.

PART must be one of
- 'docket
- 'date
- 'name"

  (let ((com (car (memq part '(:docket :date :name)))))
    (unless com
      (error "Part: %s is invalid; it must be one of :docket, :date, or :name" part))
    (plist-get ex com)))

(provide 'extract)

;;; End extract.el
