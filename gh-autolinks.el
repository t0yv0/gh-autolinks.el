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


;;;###autoload
(defun gh-autolinks-org-buffer ()
  "Detects GitHub references in the current org-mode buffer and
automatically links them."
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx (group (or line-start " "))
                                  (group "https://github.com/"
                                         (group (+ (not "/"))) "/"
                                         (group (+ (not "/")))
                                         (or "/issues/" "/pull/")
                                         (group (+ num))))
                              nil t)
      (let ((space (match-string 1))
            (url (match-string 2))
            (owner (match-string 3))
            (repo (match-string 4))
            (num (match-string 5)))
        (if gh-autolinks-add-title
            (replace-match (format "%s[[%s][%s/%s#%s: %s]]"
                                   space url owner repo num (gh-autolinks--issue-or-pullreq-title url)))
          (replace-match (format "%s[[%s][%s/%s#%s]]"
                                 space url owner repo num)))))))


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
