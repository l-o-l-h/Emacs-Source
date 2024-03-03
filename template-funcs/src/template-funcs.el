;;; template-funcs.el -- Template Functions -*- mode: elisp; -*-
;;; Time-stamp: <2024-03-02 19:37:48 lolh-mbp-16>

;;; Commentary:
;;; Provide functions for the denote-templates function.

;;; Code:

;; Denote Templates Functions

(defun blank ()
  "Create a blank Denote note."
  "* ")

(defun newclient ()
  "Create a new Denote note for a new client."
  (concat
   "* CLIENT INFO\n"
   ":PROPERTIES:\n"
   ":NAME:\t--\n"
   ":ID:\t\t--\n"
   ":DOB:\t\t--\n"
   ":PHONE:\t--\n"
   ":EMAIL:\t--\n"
   ":ADDR:\t--\n"
   ":RACE:\t--\n"
   ":GENDER:\t--\n"
   ":LANG:\t--\n"
   ":DISABLED:\t--\n"
   ":VETERAN:\t--\n"
   ":CITIZEN:\t--\n"
   ":ADULTS:\t--\n"
   ":CHILDREN:\t--\n"
   ":END:\n\n\n"
   "* CASES\n\n\n"
   "* RTC INTERVIEW\n\n\n"))

(defun newcase ()
  "Create a new Denote note for a new RTC case."
  (concat
   "* RTC CASE\n"
   ":PROPERTIES:\n"
   ":O/C:\t\t--\n"
   ":CAUSE:\t--\n"
   ":DEPT:\t--\n"
   ":PL-1:\t--\n"
   ":PL-2:\t--\n"
   ":APRTMNT:\t\t--\n"
   ":DEF-1:\t--\n"
   ":DEF-2:\t--\n"
   ":LEASE:\t--\n"
   ":NOTICE:\t--\n"
   ":SUMMONS:\t--\n"
   ":COMPLAINT:\t--\n"
   ":OSC-1:\t--\n"
   ":OSC-2:\t--\n"
   ":APPOINT:\t--\n"
   ":NOA:\t\t--\n"
   ":LEDGER:\t--\n"
   ":END:\n\n\n"

   "** OSC\n\n\n"
   "*** OSC-1\n\n\n"
   "*** OSC-2\n\n\n"
   "** DOCUMENTS\n\n\n"
   "*** COURT FILES\n\n\n"
   "*** EXHIBITS\n"
   ":PROPERTIES:\n"
   ":LEASE:\t--\n"
   ":NOTICE:\t--\n"
   ":SERVICE:\t--\n"
   ":END:\n\n\n"

   "*** LEDGERS\n"
   ":PROPERTIES:\n"
   ":LEDGER-1:\t--[DATE]\n"
   ":LEDGER-2:\t--[DATE]\n"
   ":END:\n\n\n"

   "* CLIENT\n\n\n"
   "* O/C\n\n"
   "** O/C INFO\n\n\n"
   "** O/C COMMUNICATION\n\n\n"
   "* PLAN\n\n\n"
   "* ISSUES\n\n\n"))

(defun newcase-with-newclient (case pl def cl info)
  (interactive
   "sCase: \nsPlaintiff: \nsDefendants: \nsClient: \nsClient Info: ")
  (print (format "Case: %s  Plaintiff: %s  Defendants: %s  Client: %s  Info: %s"
                 case pl def cl info) (current-buffer)))

;; Denote Last Name

(defun lastname (note)
  )

(provide 'template-funcs)

;;; End template-funcs.el
