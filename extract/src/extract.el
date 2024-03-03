;;; extract.el -- Extract parts of a pdf into separate files -*- mode: elisp; -*-
;;; Time-stamp: <2024-02-27 15:39:55 minilolh>

;;; Commentary:
;; Given a pdf, such as complaint.pdf, extract the different parts,
;; such as a lease.pdf, notice.pdf, service.pdf etc.
;; The buffer should hold a section that includes:
;; - -----
;; - (optional) Directory/ (default is current directory); must end with /
;; - Source PDF.pdf; must end with .pdf
;; - Name of section PDF.pdf (string) {beginning page no.} {optional ending page no.}
;; - .. repeat for each subpart
;; - -----

;;; Goto point-min: (beginning-of-buffer)
;;; Search for '-----':
;;; Read line: (read (current-buffer))
;;; If a directory,
;;;   Change to that directory: (cd ...)
;;;   Goto beginning of next line: (forward-line)
;;; End if
;;; Read line as Source filename: source.pdf \n
;;; Goto next line (forward-line)
;;; Loop through lines until next '-----'
;;;   Read string name, beginning page no., ending page no.
;;;     (read (current-buffer)) -> Name.pdf
;;;     (read (current-buffer)) -> Beginning page
;;;     (read (current-buffer)) -> Ending page
;;        If no second number, use same number as beginning (one page)
;;;     Run: pdftk Source cat beginning end output SubName
;;; End

;;; Code:

(defconst *process* "~/Downloads/process"
  "The location at which documents are found to be processed.")

(keymap-global-set "C-x p a" #'lolh/pdf-attach)

(defun lolh/pdf-attach ()
  "Command to attach pdf files to the case note.

  This command relies upon the command `lolh/find-court-file-dir', which
  relies upon the environment variables:
  - GOOGLE_DRIVE_2022
  - GOOGLE_DRIVE_2023
  - GOOGLE_DRIVE_2024
  etc. being set properly.  If something does not work quite right,
  check those first."

  (interactive)

  (unless (and
           (getenv "GOOGLE_DRIVE_2022")
           (getenv "GOOGLE_DRIVE_2023")
           (getenv "GOOGLE_DRIVE_2024"))
    (error "Check that the google_drive environment variables are set properly"))

  ;; Put point in the main heading
  ;; Extract information from the Properties
  ;; Calculate the attachment directory paths
  ;; Calculate the Complaint.pdf url
  (goto-char (point-min))
  (search-forward "RTC CASE")

  (let* (;; The path of the currently open note
         (parent (abbreviate-file-name
                  (file-name-parent-directory (org-entry-get nil "FILE"))))

         ;; e.g., 23-2-01234-06
         (cause (string-trim-left
                 (org-entry-get nil "CAUSE")
                 "-- "))
         ;; e.g. 2023
         (year (concat "20" (substring cause 0 2)))

         ;; The first defendant's name
         (def1 (string-trim-left
                (org-entry-get nil "DEF-1")
                "--"))

         ;; Get all exhibit data
         (exhibit-data (lolh/find-all-exhibit-data))
         ;;(lease (lolh/find-exhibit-data "LEASE" (org-entry-get nil "LEASE")))
         ;;(notice (lolh/find-exhibit-data "NOTICE" (org-entry-get nil "NOTICE")))

         ;; Some constants
         (data "data")
         (files "Court Files")
         (exhibits "Exhibits")

         ;; Paths to the attachment directories
         (dir (file-name-concat parent data (concat cause def1) files))
         (exs (file-name-concat parent data (concat cause def1) exhibits))

         ;; Paths to the Google Drive case for this cause
         ;; Find the Court File url first
         ;; All of the pleadings as PDFs are inside
         (court-file-url (lolh/find-court-file-dir cause))

         ;; Calculate its parent directory url, the root
         (cause-url (file-name-parent-directory court-file-url))

         ;; Calculate the Exhibits directory url (Notices & Lease)
         (exhibits-url (file-name-as-directory
                        (car (directory-files cause-url t "Notices \& Lease"))))

         ;; Get the Complaint.pdf url
         ;; This will present a problem if there are amended complaints
         ;; TODO: refactor to take into account amended complaints
         ;; probably need to get all and then take the last one.
         (complaint-url (car (directory-files court-file-url t "Complaint"))))

    ;; now attach data directories
    (search-forward "COURT FILES")
    (org-entry-put nil "DIR" dir)

    ;; The follow function attaches court file docs from Google Drive
    ;; to the COURT FILES subsubheading using dired.
    ;; Dired must be used in a separate window because the function
    ;; `org-attach-dired-to-subtree' requires it.
    (lolh/attach-dired-to-subtree court-file-url)

    ;; Now extract the exhibits, place them into the Google Drive
    ;; in the `Notices & Lease' directory, and attach them to the
    ;; data directory as with the Court Files.

    ;; The following function extracts the exhibits from the complaint
    ;; into the directory ~/Downloads/process/ as
    ;; Lease-Exhibit-1, Notice-Exhibit-2, etc.
    ;; Then, this function places the exhibits into the Google Drive next
    ;; to the Court Files, e.g., into the `Notices & Lease' directory.
    ;; TODO: refactor code to take into account more than 2 exhibits.
    ;; (lolh/extract-pdfs "complaint.pdf" complaint-url cause-url (list lease notice))
    (lolh/extract-pdfs "complaint.pdf" complaint-url cause-url exhibit-data)
    ;; There needs to be a little delay to let the Google Drive sync
    ;; with the local drive
    (sit-for 3)

    ;; Add the attachment directory
    (search-forward "EXHIBITS")
    (org-entry-put nil "DIR" exs)

    ;; Attach the Exhibits to the Exhibits subtree
    ;; EXHIBIT is a filter for dired
    (lolh/attach-dired-to-subtree exhibits-url "EXHIBIT")))

(defun lolh/attach-dired-to-subtree (url &optional filter)
  "Function to attach files to the attachment directory.

The note must be open for this to work, of course, with no other windows
open.

INPUT VALUE:
- URL: URL to the Google Drive files to be attached.
- FILTER: value for which to filter file names, such as EXHIBIT
          otherwise all files in the directory will be chosen.

RETURN VALUE:
- T for successful operation."

  ;; Either mark all files in the directory, or if FILTER is set, mark
  ;; just the files that are filtered.
  (dired-other-window url)
  (if filter
      (dired-mark-files-regexp filter)
    (dired-toggle-marks))
  (org-attach-dired-to-subtree (dired-get-marked-files))
  (dired-unmark-all-marks)
  (delete-window))

(defun lolh/find-court-file-dir (cause &optional closed)
  "Return path to the Case/Court File/ Google Drive dir from a cause number.

  INPUTS:
  `CAUSE': string representing a cause number, such as `23-2-01234-06'.
  `CLOSED': boolean that should be set to t if the case is a closed one.

  RETURN:
  `URL': represents the Google Drive path to the cause.

  This command works only with active cases that have had their
  Google Drive case file set up prior to being called.

  TODO: Modify this to work for closed cases as well.  This will
  involve invoking error catching and trying again in the closed
  directory.

  NOTE: Environment variables must be set properly, as follows:
  - GOOGLE_DRIVE_2022
  - GOOGLE_DRIVE_2023
  - GOOGLE_DRIVE_2024
  - ETC."

  (interactive)

  ;; Place code here to verify integrity of `cause' variable.
  ;; Then extract the year, and the applicable Google Drive env var.
  (let* ((year (substring cause 0 2))
         (gd-year (getenv (concat "GOOGLE_DRIVE_20" year)))
         (case-file (car (directory-files gd-year t cause)))
         (court-file (car (directory-files case-file t "Court File"))))
    ;; The directory portion of `court-file' is the `cause' url
    (file-name-as-directory court-file)))

(defun lolh/find-all-exhibit-data ()
  "Return a list of all exhibit data as a list of lists.

  There must be a heading titled `EXHBITS' which contains a `Properties'
  drawer.  The property node keys are the names of the exhibits, while
  the property node values are the exhibit numbers and page numbers of
  start and end pages.

  EXAMPLE:

  :PROPERTIES:
  :LEASE: Exhibit-1 5 8
  :NOTICE: Exhibit-2 9 9
  :END

  If the node data is not syntactically correct, the command will stop
  with an error message.

  The command can be started while point is anywhere inside the buffer.

  RETURN VALUE:
  -------------
  LIST OF LISTS of the form ((key value beg end) (key value beg end) ...)"

  (interactive)

  (save-excursion
    ;; Locate the * EXHIBITS heading
    (goto-char (point-min))
    (re-search-forward "\\* EXHIBITS")
    ;; Locate the Properties drawer and then go forward one line into the nodes
    (while (not (looking-at-p org-element-drawer-re))
      (forward-line))
    (forward-line)
    ;; Collect all of the node properties and values into a list `return-val'
    (let (return-val)
      (while (not (looking-at-p org-element-drawer-re))
        (if (re-search-forward
             ;; :KEY: EXHIBIT-# beg# end#
             ":\\([[:alpha:]]+\\):[[:space:]]+\\(EXHIBIT-[[:alnum:]]\\{1,\\}\\)[[:space:]]+\\([[:digit:]]+\\)[[:space:]]+\\([[:digit:]]+\\)"
             (pos-eol) t)
            (push (list (match-string-no-properties 1) (match-string-no-properties 2) (match-string-no-properties 3) (match-string-no-properties 4)) return-val)
          (error "There was an error parsing Exhibit data: %s" (thing-at-point 'line t)))
        (forward-line))
      return-val)))

(defun lolh/find-exhibit-data (name str)
  "Command to return data about the EXHIBITs found in the Properties.

  INPUT VALUES:
  - NAME: a string representing the word LEASE or EXHIBIT, etc., which
          is an exhibit in the Complaint.pdf.
  - STR: a string representing the matched property data from LEASE or
         EXHIBIT.

  RETURN VALUE:
  - list: a list of the form '(NAME EXHIBIT BEG END)

  WHERE:
  - NAME: a string representing LEASE or NOTICE, etc.
  - EXHIBIT: a string representing EXHIBIT-1 or EXHIBIT-2 etc.
  - BEG: a string integer representing the beginning page number of this
         exhibit in the PDF.
  - END: a string integer representing ending page number of this exhibit
         in the PDF."

  (interactive)

  (string-match "-- \\(EXHIBIT-[[:alnum:]]\\{1,\\}\\) \\([[:digit:]]+\\) \\([[:digit:]]+\\)" str)
  (list name (match-string 1 str) (match-string 2 str) (match-string 3 str)))

(defun lolh/extract-pdfs (name name-url cause-url exhibits)
  "Command to extract pdf exhibits out of a complaint.

  If successful, then the extracted exhibits will be in the Google
  Drive and can be attached next.

  INPUTS:
  - NAME: a string, the name of the document to be processed; eg. complaint.pdf.
  - NAME-URL: a url pointing to the location of the document to be processed.
  - CAUSE-URL; a url pointing to the top of the Google Drive directory
    for this cause number, e.g. `/GoogleDrive/.../24-2-99999-06 Plaintiff v. Defendant'.
  - EXHIBITS: an assoc-list of the form
    `((EXHIBIT EXHIBIT-# BEG-# END-#) (...) ...)'

  WHERE:
  - EXHIBIT: a string representing the name of the exhibit.
  - EXHIBIT-#: a string of the form EXHIBIT-1, EXHIBIT-2, etc.
  - BEG-#: a digit representing the beginning page number in the complaint
  - END-#: a digit representing the ending page number in the complaint.

  RETURN VALUE:
  The list of exhibits extracted and moved upon success."

  (interactive)

  ;; Copy the complaint into ~/Downloads/process/.
  ;; Create this directory if it does not exist.
  (unless (file-exists-p *process*)
    (mkdir *process*))
  (copy-file name-url (file-name-concat *process* name ) t)

  ;; Map over the exhibits data, extracting each part into the
  ;; ~/Downloads/process directory
  ;; lolh/extract-pdf returns the full pathname of the extracted file
  ;; and so accummulate these as a list.
  ;; exhibits-file-url is the Google Drive path to `Notices & Lease'
  ;; under the root Cause directory
  (let ((urls (mapcar #'lolh/extract-pdf exhibits))
        (exhibits-file-url
         (file-name-as-directory
          (car (directory-files cause-url t "Notices \& Lease")))))
    (delete-file "~/Downloads/process/complaint.pdf")

    ;; Map over the list of extracted names and move them into the
    ;; Google Drive
    ;; PROBLEM: The process seems to happen too fast for the Google
    ;; files to be sync'ed into the local file system, so need to
    ;; create some delay.
    ;; (sleep-for 2)
    (mapc (lambda (url) (rename-file url exhibits-file-url 1)) urls)))

(defun lolh/extract-pdf (exhibit-data)
  "Given a single list of exhibit data, use `pdftk' to extract the exhibit.

  This command cd's into the directory ~/Downloads/process, in
  which should exist a file named `complaint.pdf'.  Inside this file
  are exhibits. These exhibits are to be extracted into separate
  files named by the `exhibit-data' argument, e.g.,
  `NAME-EXHIBIT-#.pdf'.  It will be up to the calling program to
  process these extracted files.

  INPUTS:
  - EXHIBIT-DATA: a list containg (NAME EXHIBIT-# BEG-# END-#)

  WHERE:
  - NAME: a string represents the name of the exhibit
  - EXHIBIT-#: a string of the form EXHIBIT-1, EXHIBIT-2, etc.
  - BEG-#: a string integer represents the beginning page of the exhibit
           in the complaint
  - END-#: a string integer represents the ending page of the exhibit in
           the complaint.

  RETURN VALUE:
  - 0 if successful
  - non-0 if unsuccessful (representing an error code)"

  (save-excursion
    (cd "~/Downloads/process/")
    (let ((complaint "complaint.pdf")
          (process "pdftk")
          (func "cat")
          (beg-end (concat (caddr exhibit-data) "-" (cadddr exhibit-data)))
          (output "output")
          (output-name (concat (car exhibit-data) "-" (cadr exhibit-data) ".pdf")))
      (call-process
       process nil nil nil
       complaint func beg-end output output-name)
      (file-name-concat default-directory output-name))))

(provide 'extract)

;;; End extract.el
