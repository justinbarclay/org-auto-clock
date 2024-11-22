;;; org-clock-auto.el --- Automatically clock in and out of your tasks  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Barclay

;; Author: Justin Barclayx <github@justincbarclay.ca>
;; Version: 0.0.0-alpha
;; Package-Requires: ((emacs "28.1") (org "9.7"))
;; Keywords: convenience, calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Get automatically prompted to clock into your agenda tasks based on
;; a list of projects.

;;; Code:

(require 'org)
(require 'org-clock)
(eval-when-compile
  (declare-function org-heading-components "org")
  (declare-function org-format-outline-path "org")
  (declare-function org-get-outline-path "org")
  (declare-function org-get-todo-face "org")
  (declare-function org-clock-in "org-clock")
  (declare-function org-clocking-p "org-clock")
  (declare-function org-map-entries "org")
  (defvar org-agenda-files))

(with-eval-after-load 'projectile
  (declare-function projectile-project-name "projectile"))

(with-eval-after-load 'project
  (declare-function project-name "project")
  (declare-function project-current "project"))

(defgroup org-auto-clock nil
  "Automatically clock into tasks when working in a project."
  :group 'org-clock)

(defcustom org-auto-clock-projects '()
  "List of project to auto-clock into when switching to a buffer."
  :type '(list string)
  :group 'org-auto-clock)

(defvar org-auto-clock-selecting nil "Whether `org-auto-clock-select-task' is running.")

(defun org-auto-clock--always ()
  "Always return t, regardless of the buffer's project."
  't)

(defun org-auto-clock--resolve-project ()
  "Returns the name of the buffer's current project as resolved by `project.el'."
  (project-name (project-current)))

(defcustom org-auto-clock-always-clock-in nil
  "Clock-in regardless of what project the current buffer is in."
  :type 'boolean)

(defcustom org-auto-clock-match-query "TODO<>\"DONE\""
  "Query to match headings in the agenda file.

See the function `org-map-entries' for details."
  :type 'string)

(defcustom org-auto-clock-project-name-function #'org-auto-clock--resolve-project
  "Function to determine the project name for the current buffer.

Used by `org-auto-clock' to match the current buffer's project name
against the names in `org-auto-clock-project-names'.

`project.el' is the default name resolution strategy."
  :group 'org-auto-clock
  :type '(radio (function-item projectile-project-name)
                (function-item org-auto-clock--resolve-project)
                (function :tag "Function"))
  :group 'org-auto-clock)

(defun org-auto-clock--extract-headings (agenda &optional match)
  "Extract headings from an `AGENDA' file.

Use `MATCH' to filter the headings in the agenda.  See info node `(org)Matching
tags and properties' for more information."
  (with-current-buffer (find-file-noselect agenda)
    (org-map-entries
     (lambda ()
       (let* ((todo (nth 2 (org-heading-components)))
              (path (org-format-outline-path
                     (org-get-outline-path 't 't)
                     100))
              (todo (when todo
                      (propertize todo 'face (org-get-todo-face todo)))))
         (add-text-properties 0 1
                              (list 'marker (point-marker)
                                    'todo-state todo)
                              path)
         path))
     match
     'file)))

(defun org-auto-clock-select-task (&optional match)
  "Clock in to an agenda tasks in another buffer.

Use `MATCH' to filter the headings in the agenda file.  See info
node `(org)Matching tags and properties' for more information."
  (interactive)
  (unless org-agenda-files
    (user-error "Unable to find any agenda files.`org-agenda-files' is nil"))
  ;; This requires Emacs 28.1
  (let* ((org-auto-clock-selecting 't)
         (minibuffer-allow-text-properties 't)
         (agenda (completing-read "Clock into file: " org-agenda-files))
         (headings (org-auto-clock--extract-headings agenda match))
         (heading (completing-read
                   "Clock into which heading:"
                   headings)))

    (with-current-buffer (marker-buffer
                          (get-text-property 0 'marker heading))
      (save-mark-and-excursion
        (goto-char (marker-position (get-text-property 0 'marker heading)))
        (org-clock-in)))))

(defun org-auto-clock--can-run-p ()
  "Return non-nil if `org-auto-clock' can run in current buffer."
  (or org-auto-clock-always-clock-in
      (member (funcall org-auto-clock-project-name-function)
              org-auto-clock-projects)))

(defun org-auto-clock--clock-in (_)
  "Clock into a task when opening a buffer or file."
  (when (and (not (or (minibufferp)
                      (org-clocking-p)
                      org-auto-clock-selecting))
             (org-auto-clock--can-run-p))
    (org-auto-clock-select-task org-auto-clock-match-query)))

(define-minor-mode org-auto-clock-mode
  "Automatically clock into a task when opening any buffer or file."
  :require 'org
  :lighter " AutoClock"
  :global t
  (if org-auto-clock-mode
      (progn
        (add-hook 'window-buffer-change-functions #'org-auto-clock--clock-in)
        (unless (memq 'org-auto-clock-clockout org-clock-in-hook)
          (add-hook 'org-clock-in-hook #'org-clock-auto-clockout t)))
    (remove-hook 'org-clock-in-hook #'org-clock-auto-clockout)
    (remove-hook 'window-buffer-change-functions #'org-auto-clock--clock-in)))
(provide 'org-auto-clock)

;;; org-auto-clock.el ends here