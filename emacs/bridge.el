;;; Package --- Summary
;;; Commentary:
;;; Code:

(require 'org)

(defcustom bridge-suit-symbols '((S . "♠") (H . "♥") (D . "♦") (C . "♣"))
  "Alist of suit symbols for bridge suits."
  :type '(alist :key-type string :value-type string)
  :group 'bridge)

(defcustom bridge-dealer-bin "/usr/local/bin/dealer"
  "Path to the dealer binary."
  :type 'string
  :group 'bridge)


(defun bridge-board-number (b)
  "Return the board number of the bridge board B."
  (cadr b))

(defun bridge-board-hand (side b)
  "Return the contents of the hand SIDE in B."
  (cadr (assoc side (cddr b))))

(defun bridge-hand-suit (suit hand)
  "Return the cards in the SUIT of the HAND."
  (cadr (assoc suit hand)))

(defun bridge-holding-to-string (holding)
  "Return a string representation of the HOLDING."
  (mapconcat (lambda (x)
               (if (symbolp x)
                   (upcase (symbol-name x))
                 (number-to-string x)))
             holding ""))

(defun bridge-board-vulnerability (board)
  "Return BOARD's vulnerability based on its number."
  (let* ((n (% (- (bridge-board-number board) 1) 16))
         (vuln (% (+ (% n 4) (/ n 4)) 4)))
    (cond ((equal vuln 0) "None")
          ((equal vuln 1) "NS")
          ((equal vuln 2) "EW")
          ((equal vuln 3) "Both"))))

(defun bridge-board-dealer (board)
  "Return the dealer of the BOARD based on number."
  (let ((dealers '(N E S W)))
    (nth (% (- (bridge-board-number board) 1) 4) dealers)))

(defun bridge-put-hand (line column hand)
  "Put the HAND representation in LINE / COLUMN of the table."
  (dolist (suit '(S H C D))
          (org-table-put
           line
           column
           (format "%s %s" (cdr (assoc suit bridge-suit-symbols))
                   (bridge-holding-to-string (bridge-hand-suit suit hand)))
           (setq line (+ line 1)))))

(defun bridge-board-display (board)
  "Display the BOARD as an org table."
  (org-table-create "3x12")
  (org-table-move-row-down)
  (org-table-goto-line 4)
  (org-table-insert-hline)
  (org-table-goto-line 8)
  (org-table-insert-hline)
  (org-table-goto-line 12)
  (org-table-insert-hline)

  (org-table-put 1 1 (number-to-string (bridge-board-number board)))
  (org-table-put 2 1 (bridge-board-vulnerability board))
  (org-table-put 3 1 (format "Dealer: %s" (bridge-board-dealer board)))

  (bridge-put-hand 1 2 (bridge-board-hand 'N board))
  (bridge-put-hand 5 1 (bridge-board-hand 'W board))
  (bridge-put-hand 5 3 (bridge-board-hand 'E board))
  (bridge-put-hand 9 2 (bridge-board-hand 'S board))
  (org-table-align))

(defun bridge-insert-random-board (numbers)
  "Insert a random board with NUMBERS."
  (interactive "sBoard number: ")
  (with-current-buffer "*bridge-dealer*" (erase-buffer))
  (call-process-shell-command
   (format "%s %s" bridge-dealer-bin numbers)
   nil
   "*bridge-dealer*"
   nil)
  (with-current-buffer "*bridge-dealer*"
    (goto-char (point-min)))
  (while (with-current-buffer "*bridge-dealer*"
           (not (equal (point) (point-max))))
    (bridge-board-display
     (with-current-buffer "*bridge-dealer*"
       (read (current-buffer))))
    (forward-line 2)
    (insert "\n")
    (with-current-buffer "*bridge-dealer*"
      (forward-line 1))))

(provide 'bridge)
;;; bridge.el ends here
