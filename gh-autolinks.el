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


(defun gh-autolinks-org-buffer ()
  "Detects GitHub references in the current org-mode buffer and
automatically links them."
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx "https://github.com/"
                                  (group (+ (not "/"))) "/"
                                  (group (+ (not "/")))
                                  (or "/issues/" "/pull/")
                                  (group (+ num))) nil t)
      (replace-match (format "[[%s][%s/%s#%s]]"
                             (match-string 0)
                             (match-string 1)
                             (match-string 2)
                             (match-string 3))))))


(provide 'gh-autolinks)
;;; gh-autolinks.el ends here
