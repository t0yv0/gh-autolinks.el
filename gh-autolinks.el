;;; gh-autolinks.el --- GitHub autolinked references and URLs  -*- lexical-binding:t -*-
;;;
;;; Version: 1
;;;
;;; Commentary:
;;;
;;; Automatically reformat buffers to recognize GitHub references and convert them to links.
;;;
;;; See https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/autolinked-references-and-urls

;;; Code:


(defcustom gh-autolinks-add-title t
  "Whether to add the title of the issue or pull request to the link."
  :type 'boolean
  :group 'gh-autolinks)


(defcustom gh-autolinks-use-overlays nil
  "Display title of the issue or pull request in an overlay."
  :type 'boolean
  :group 'gh-autolinks)


(defun gh-autolinks--patterns ()
  "Returns link patterns to search for.

See https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/autolinked-references-and-urls for more information."
  (list
   (rx (group (or line-start " "))
       "https://github.com/"
       (group (+ (not "/")))
       "/"
       (group (+ (not "/")))
       (or "/issues/" "/pull/")
       (group (+ num)))
   (rx (group (or line-start " "))
       (group (+ alnum))
       "/"
       (group (+ alnum))
       "#"
       (group (+ num)))))


;;;###autoload
(defun gh-autolinks-org-buffer ()
  "Detects GitHub references in the current org-mode buffer and
automatically links them."
  (interactive)
  (dolist (pattern (gh-autolinks--patterns))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (let* ((space (match-string 1))
               (owner (match-string 2))
               (repo (match-string 3))
               (num (match-string 4))
               (url (format "https://github.com/%s/%s/issues/%s" owner repo num)))
          (cond
           (gh-autolinks-use-overlays
            (gh-autolinks--ensure-overlay
             (match-beginning 2) ;; owner
             (match-end 4) ;; num
             (lambda ()
               (gh-autolinks--issue-or-pullreq-title url))))
           (gh-autolinks-add-title
            (replace-match (format "%s[[%s][%s/%s#%s: %s]]"
                                   space url owner repo num
                                   (gh-autolinks--issue-or-pullreq-title url))))
           (t (replace-match (format "%s[[%s][%s/%s#%s]]"
                                     space url owner repo num)))))))))


(defun gh-autolinks--ensure-overlay (beg end fetch-text)
  "Adds an overlay to a region.

First checks if there are existing overlays, in which case it
does nothing as it assumes the work has been done already.

Otherwise it displays the result of FETCH-TEXT after the region."
  (when (seq-empty-p (seq-filter (lambda (o) (overlay-get o 'after-string))
                                 (overlays-in beg end)))
    (let ((new-overlay nil))
      (setq new-overlay (make-overlay beg end))
      (overlay-put new-overlay 'evaporate t)
      (overlay-put new-overlay 'after-string
                   (format ": %s" (funcall fetch-text))))))


(defun gh-autolinks--issue-or-pullreq-title (url)
  "Uses GitHub CLI to find the issue or pull request title for the given URL."
  (string-trim (shell-command-to-string
                (format "gh issue view --json title --jq '.title' %s"
                        (gh-autolinks--shell-quote url)))))


(defun gh-autolinks--shell-quote (str)
  "Quotes STR for use in a shell command."
  (format "'%s'" (replace-regexp-in-string "'" "'\\\\''" str)))


(provide 'gh-autolinks)
;;; gh-autolinks.el ends here
