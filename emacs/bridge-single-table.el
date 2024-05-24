;; -*- lexical-binding: t -*-
;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'bridge)
(require 'omnibridge-server-interface)

(defun bridge-single-table-start (boards)
  "Create a single-table game score-sheet with BOARDS."
  (interactive "nHow many boards? ")
  (let ((inhibit-read-only t))
    (org-table-create (format "5x%d" (+ 3 boards)))
    (org-table-put 1 1 "Board")
    (org-table-put 1 2 "Contract")
    (org-table-put 1 3 "Score")
    (org-table-put 1 4 "Expected")
    (org-table-put 1 5 "IMP")
    (dotimes (b boards)
      (org-table-put (+ 3 b) 1 (format "%d." (+ b 1))))
    (org-table-goto-line (+ boards 3))
    (org-table-insert-hline 'above)
    (org-table-put (+ boards 3) 1 "Total:")
    (org-table-align)
    (bridge-mode-send-request )))


(provide 'bridge-single-table)
;;; bridge-single-table.el ends here

