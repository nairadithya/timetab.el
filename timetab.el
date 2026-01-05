;;; timetab.el --- Generate org-agenda from Amrita timetables -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Adithya Nair
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
;; Keywords: calendar, org, education
;; URL: https://github.com/nairadithya/timetab.el

;;; Commentary:

;; This package fetches timetable data from the Amrita Timetable Registry
;; and generates org-agenda compatible entries for academic scheduling.
;;
;; Usage:
;;   M-x timetabel-select-timetable
;;
;; The plugin will guide you through selecting:
;;   - Academic year
;;   - Branch/Department
;;   - Semester
;;   - Configuration options (batch, electives, etc.)
;;   - Start date and number of weeks
;;   - Target org file
;;
;; It will then generate properly formatted org-agenda entries with
;; timestamps, properties, and organize them by week.

;;; Code:

(require 'request)
(require 'json)
(require 'org)
(require 'cl-lib)

;;; Customization

(defgroup amrita-timetable nil
  "Generate org-agenda from Amrita timetables."
  :group 'org
  :prefix "timetabel-")

(defcustom amrita-timetable-cache-dir
  (expand-file-name "amrita-timetable" user-emacs-directory)
  "Directory to cache timetable data."
  :type 'directory
  :group 'amrita-timetable)

(defcustom amrita-timetable-default-org-file nil
  "Default org file for inserting timetables."
  :type '(choice (const nil) file)
  :group 'amrita-timetable)

(defcustom amrita-timetable-start-time "09:00"
  "Start time for first period (HH:MM format)."
  :type 'string
  :group 'amrita-timetable)

(defcustom amrita-timetable-period-duration 50
  "Duration of each period in minutes."
  :type 'integer
  :group 'amrita-timetable)

(defcustom amrita-timetable-break-duration 10
  "Duration of break between periods in minutes."
  :type 'integer
  :group 'amrita-timetable)

(defcustom amrita-timetable-lunch-after-period 4
  "Period number after which lunch break occurs (0 for no lunch)."
  :type 'integer
  :group 'amrita-timetable)

(defcustom amrita-timetable-lunch-duration 60
  "Duration of lunch break in minutes."
  :type 'integer
  :group 'amrita-timetable)

(defcustom amrita-timetable-default-weeks 16
  "Default number of weeks to generate schedule for."
  :type 'integer
  :group 'amrita-timetable)

;;; Internal Variables

(defvar amrita-timetable--index-url
  "https://timetable-registry.amrita.town/v2/index.json"
  "URL for the timetable index.")

(defvar amrita-timetable--base-url
  "http://timetable-registry.amrita.town/v2/files"
  "Base URL for timetable files.")

(defvar amrita-timetable--cached-index nil
  "Cached index data.")

(defvar amrita-timetable--cache-time nil
  "Time when index was last cached.")

;;; Utility Functions

(defun amrita-timetable--ensure-cache-dir ()
  "Ensure cache directory exists."
  (unless (file-exists-p amrita-timetable-cache-dir)
    (make-directory amrita-timetable-cache-dir t)))

(defun amrita-timetable--cache-file (year branch semester)
  "Return cache file path for YEAR BRANCH SEMESTER."
  (expand-file-name
   (format "%s-%s-%s.json" year branch semester)
   (expand-file-name "timetables" amrita-timetable-cache-dir)))

(defun amrita-timetable--day-offset (day-name)
  "Return numeric offset (0-6) for DAY-NAME where 0 is Monday."
  (pcase day-name
    ('Monday 0)
    ('Tuesday 1)
    ('Wednesday 2)
    ('Thursday 3)
    ('Friday 4)
    ('Saturday 5)
    ('Sunday 6)
    (_ 0)))

;;; Data Fetching

(defun amrita-timetable-fetch-index (callback)
  "Fetch index.json and call CALLBACK with parsed data."
  (if (and amrita-timetable--cached-index
           amrita-timetable--cache-time
           (< (float-time (time-since amrita-timetable--cache-time)) 86400))
      ;; Use cached version if less than 24 hours old
      (funcall callback amrita-timetable--cached-index)
    ;; Fetch fresh data
    (request amrita-timetable--index-url
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq amrita-timetable--cached-index data
                        amrita-timetable--cache-time (current-time))
                  (funcall callback data)))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Failed to fetch timetable index: %S. Check your connection." error-thrown))))))

(defun amrita-timetable-fetch-timetable (year branch semester callback)
  "Fetch timetable for YEAR BRANCH SEMESTER and call CALLBACK with data."
  (let* ((cache-file (amrita-timetable--cache-file year branch semester))
         (url (format "%s/%s/%s/%s.json"
                      amrita-timetable--base-url year branch semester)))
    (if (file-exists-p cache-file)
        ;; Use cached version
        (with-temp-buffer
          (insert-file-contents cache-file)
          (funcall callback (json-read-from-string (buffer-string))))
      ;; Fetch from network
      (request url
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    ;; Cache the data
                    (amrita-timetable--ensure-cache-dir)
                    (let ((cache-dir (file-name-directory cache-file)))
                      (unless (file-exists-p cache-dir)
                        (make-directory cache-dir t)))
                    (with-temp-file cache-file
                      (insert (json-encode data)))
                    (funcall callback data)))
        :error (cl-function
                (lambda (&key error-thrown &allow-other-keys)
                  (message "Failed to fetch timetable from %s: %S" url error-thrown)))))))

;;; Slot Resolution

(defun amrita-timetable--resolve-slot (slot-def config-selections)
  "Resolve SLOT-DEF based on CONFIG-SELECTIONS.
Returns subject key or 'FREE'."
  (let ((match (alist-get 'match slot-def))
        (choices (alist-get 'choices slot-def)))
    (cond
     ;; Simple format: match is a string
     ((stringp match)
      (let ((config-value (alist-get (intern match) config-selections)))
        (or (alist-get (intern config-value) choices) "FREE")))
     
     ;; Complex format: match is an array
     ((vectorp match)
      (let ((config-values (mapcar (lambda (key)
                                     (alist-get (intern key) config-selections))
                                   (append match nil))))
        (or (cl-loop for choice across choices
                     for pattern = (append (alist-get 'pattern choice) nil)
                     when (cl-every (lambda (pair)
                                      (let ((pattern-val (car pair))
                                            (config-val (cdr pair)))
                                        (or (string= pattern-val "*")
                                            (string= pattern-val config-val))))
                                    (cl-mapcar #'cons pattern config-values))
                     return (alist-get 'value choice))
            "FREE")))
     
     (t "FREE"))))

(defun amrita-timetable--resolve-schedule (timetable config-selections)
  "Resolve schedule in TIMETABLE based on CONFIG-SELECTIONS.
Returns alist of (day . resolved-periods)."
  (let ((schedule (alist-get 'schedule timetable))
        (slots (alist-get 'slots timetable)))
    (mapcar
     (lambda (day-entry)
       (cons (car day-entry)
             (mapcar (lambda (period)
                       (let ((period-str (if (stringp period) period
                                           (format "%s" period))))
                         (if-let ((slot-def (alist-get (intern period-str) slots)))
                             (amrita-timetable--resolve-slot slot-def config-selections)
                           period-str)))
                     (append (cdr day-entry) nil))))
     schedule)))

;;; Time Calculation

(defvar amrita-timetable--theory-slots
  '((1 "08:10" "09:00")
    (2 "09:00" "09:50")
    (3 "09:50" "10:40")
    (4 "11:00" "11:50")
    (5 "11:50" "12:40")
    (6 "14:00" "14:50")
    (7 "14:50" "15:40"))
  "Theory class time slots as (slot-number start-time end-time).")

(defvar amrita-timetable--lab-slots
  '((1 "08:10" "10:25")
    (2 "10:50" "13:05")
    (3 "13:25" "15:40"))
  "Lab class time slots as (slot-number start-time end-time).")

(defun amrita-timetable--parse-time-on-date (time-str base-date)
  "Parse TIME-STR (HH:MM) on BASE-DATE and return encoded time."
  (encode-time (parse-time-string
                (format "%s %s:00"
                        (format-time-string "%Y-%m-%d" base-date)
                        time-str))))

(defun amrita-timetable--get-slot-times (slot-number is-lab base-date)
  "Get start and end times for SLOT-NUMBER.
IS-LAB determines whether to use lab or theory slots.
BASE-DATE is the day.
Returns (start-time . end-time) as encoded times."
  (let* ((slots (if is-lab amrita-timetable--lab-slots amrita-timetable--theory-slots))
         (slot-info (assoc slot-number slots)))
    (when slot-info
      (let ((start-str (nth 1 slot-info))
            (end-str (nth 2 slot-info)))
        (cons (amrita-timetable--parse-time-on-date start-str base-date)
              (amrita-timetable--parse-time-on-date end-str base-date))))))

;;; Subject Expansion

(defun amrita-timetable--expand-subject (subject-key subjects)
  "Expand SUBJECT-KEY using SUBJECTS dictionary.
Handles _LAB suffix.
Returns (is-lab . subject-data) or nil."
  (let* ((is-lab (string-suffix-p "_LAB" subject-key))
         (base-key (if is-lab
                       (substring subject-key 0 -4)
                     subject-key))
         (subject (alist-get (intern base-key) subjects)))
    (when subject
      (cons is-lab subject))))

;;; Period Grouping and Slot Assignment

(defun amrita-timetable--group-periods (periods)
  "Group consecutive identical PERIODS.
Returns list of (subject-key start-slot count is-lab)."
  (let ((groups '())
        (current-subject nil)
        (current-start nil)
        (current-count 0))
    (cl-loop for period in periods
             for idx from 1
             do (cond
                 ;; Skip FREE periods
                 ((or (string= period "FREE") (not period))
                  (when current-subject
                    (let ((is-lab (string-suffix-p "_LAB" current-subject)))
                      (push (list current-subject current-start current-count is-lab) groups))
                    (setq current-subject nil current-count 0)))
                 
                 ;; Same as current group
                 ((and current-subject (string= period current-subject))
                  (setq current-count (1+ current-count)))
                 
                 ;; New subject
                 (t
                  (when current-subject
                    (let ((is-lab (string-suffix-p "_LAB" current-subject)))
                      (push (list current-subject current-start current-count is-lab) groups)))
                  (setq current-subject period
                        current-start idx
                        current-count 1))))
    ;; Add last group
    (when current-subject
      (let ((is-lab (string-suffix-p "_LAB" current-subject)))
        (push (list current-subject current-start current-count is-lab) groups)))
    (nreverse groups)))

(defun amrita-timetable--map-to-actual-slots (grouped-periods)
  "Map GROUPED-PERIODS to actual time slots.
The schedule array has 7 positions representing different time slots:
- Positions 1-5: Theory slots (morning + lunch break)
- Positions 6-7: Can be theory (afternoon) OR part of a lab session

Lab sessions in the schedule:
- Positions 1-3 (3 consecutive _LAB) -> Morning lab (08:10-10:25)
- Positions 4-5 (2 consecutive _LAB after morning theory) -> Mid-day lab (10:50-13:05)
- Positions 6-7 (2 consecutive _LAB at end) -> Afternoon lab (13:25-15:40)

Returns list of (subject-key slot-number is-lab)."
  (let ((result '())
        (position 1))
    (dolist (group grouped-periods)
      (let* ((subject-key (nth 0 group))
             (start-pos (nth 1 group))
             (count (nth 2 group))
             (is-lab (nth 3 group)))
        (if is-lab
            ;; Lab: determine which lab slot based on position and count
            (let ((lab-slot (cond
                             ;; Position 1-3 with 3 periods = Morning lab
                             ((and (= start-pos 1) (= count 3)) 1)
                             ;; Position 4-5 with 2 periods = Mid-day lab
                             ((and (>= start-pos 4) (<= start-pos 5) (= count 2)) 2)
                             ;; Position 6-7 with 2 periods = Afternoon lab
                             ((and (>= start-pos 6) (<= start-pos 7) (= count 2)) 3)
                             ;; Fallback: guess based on count
                             ((= count 3) 1)
                             ((and (>= start-pos 4) (= count 2)) 2)
                             (t 3))))
              (push (list subject-key lab-slot t) result))
          ;; Theory: map position to theory slot
          (dotimes (i count)
            (let ((theory-pos (+ start-pos i)))
              (when (<= theory-pos 7)
                (push (list subject-key theory-pos nil) result)))))))
    (nreverse result)))

;;; Org Entry Generation

(defun amrita-timetable--format-org-entry (subject-info start-time end-time)
  "Format org entry for SUBJECT-INFO with START-TIME and END-TIME.
SUBJECT-INFO is (is-lab . subject-data).
Uses weekly repeater (+1w) for recurring classes."
  (let* ((is-lab (car subject-info))
         (subject (cdr subject-info))
         (name (alist-get 'name subject))
         (code (alist-get 'code subject))
         (faculty (append (alist-get 'faculty subject) nil))
         (short-name (alist-get 'shortName subject)))
    (format "** %s%s
<%s +1w>
:PROPERTIES:
:COURSE_CODE: %s
:FACULTY: %s
:END:
"
            name
            (if is-lab " - Lab" "")
            (format "%s-%s"
                    (format-time-string "%Y-%m-%d %a %H:%M" start-time)
                    (format-time-string "%H:%M" end-time))
            code
            (mapconcat #'identity faculty ", "))))

;;; Schedule Generation

(defun amrita-timetable-generate-org-agenda (resolved-schedule subjects start-date)
  "Generate org-agenda entries for RESOLVED-SCHEDULE.
SUBJECTS is the subjects dictionary.
START-DATE is the semester start date.
Returns formatted org string with weekly repeaters."
  (let ((output ""))
    ;; Process each day of the week (only once, repeater handles recurrence)
    (dolist (day-entry resolved-schedule)
      (let* ((day-name (car day-entry))
             (periods (cdr day-entry))
             (day-offset (amrita-timetable--day-offset day-name))
             (day-date (time-add start-date (* day-offset 24 60 60)))
             (grouped-periods (amrita-timetable--group-periods periods))
             (mapped-slots (amrita-timetable--map-to-actual-slots grouped-periods)))
        
        ;; Generate entries for each mapped slot
        (dolist (slot-entry mapped-slots)
          (let* ((subject-key (nth 0 slot-entry))
                 (slot-number (nth 1 slot-entry))
                 (is-lab (nth 2 slot-entry))
                 (subject-info (amrita-timetable--expand-subject subject-key subjects))
                 (slot-times (amrita-timetable--get-slot-times slot-number is-lab day-date)))
            (when (and subject-info slot-times)
              (let ((start-time (car slot-times))
                    (end-time (cdr slot-times)))
                (setq output (concat output
                                     (amrita-timetable--format-org-entry
                                      subject-info start-time end-time)))))))))
    output))

;;; File Insertion

(defun amrita-timetable--insert-to-file (content year branch semester)
  "Insert CONTENT to org file under appropriate heading.
YEAR BRANCH SEMESTER are used for the heading title."
  (let* ((target-file (or amrita-timetable-default-org-file
                          (read-file-name "Select org file: " nil nil nil nil
                                          (lambda (f) (string-suffix-p ".org" f)))))
         (heading (format "* Timetable - %s %s Semester %s\n" year branch semester)))
    (with-current-buffer (find-file-noselect target-file)
      (goto-char (point-max))
      (insert "\n" heading content)
      (save-buffer)
      (message "Timetable inserted into %s" target-file))))

;;; Cache Management

;;;###autoload
(defun amrita-timetable-refresh-cache ()
  "Clear cached index and force refresh on next fetch."
  (interactive)
  (setq amrita-timetable--cached-index nil
        amrita-timetable--cache-time nil)
  (message "Timetable cache cleared"))

;;; Main Interactive Command

;;;###autoload
(defun amrita-timetable-select-timetable ()
  "Select and generate timetable from Amrita registry."
  (interactive)
  (amrita-timetable-fetch-index
   (lambda (index-data)
     (let* ((timetables (alist-get 'timetables index-data))
            (years (mapcar (lambda (x) (format "%s" (car x))) timetables))
            (year (completing-read "Select Year: " years nil t))
            (year-data (alist-get (intern year) timetables))
            (branches (mapcar (lambda (x) (format "%s" (car x))) year-data))
            (branch (completing-read "Select Branch: " branches nil t))
            (branch-data (alist-get (intern branch) year-data))
            (semesters (append branch-data nil))
            (semester (completing-read "Select Semester: " semesters nil t)))
       
       (amrita-timetable-fetch-timetable
        year branch semester
        (lambda (timetable-data)
          (let* ((config (alist-get 'config timetable-data))
                 (config-selections
                  (when config
                    (mapcar
                     (lambda (cfg-entry)
                       (let* ((cfg-key (car cfg-entry))
                              (cfg-data (cdr cfg-entry))
                              (label (alist-get 'label cfg-data))
                              (values (append (alist-get 'values cfg-data) nil))
                              (options (mapcar (lambda (v) (alist-get 'label v)) values))
                              (choice (completing-read (format "%s: " label) options nil t))
                              (choice-id (alist-get 'id
                                                    (cl-find-if
                                                     (lambda (v) (string= (alist-get 'label v) choice))
                                                     values))))
                         (cons cfg-key choice-id)))
                     config)))
                 (resolved-schedule (amrita-timetable--resolve-schedule
                                     timetable-data config-selections))
                 (subjects (alist-get 'subjects timetable-data))
                 (start-date-str (org-read-date nil nil nil "Semester start date: "))
                 (start-date (org-read-date nil t start-date-str)))
            
            (message "Generating timetable...")
            (let ((org-content (amrita-timetable-generate-org-agenda
                                resolved-schedule subjects start-date)))
              (amrita-timetable--insert-to-file org-content year branch semester)
              (message "Timetable generation complete!")))))))))

(provide 'amrita-timetable)

;;; amrita-timetable.el ends here
