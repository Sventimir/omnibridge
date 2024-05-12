;;; Package --- Summary
;;; Commentary:
;;; Code:

(require 'org)

(defcustom bridge-suit-symbols '((NT . "NT") (S . "♠") (H . "♥") (D . "♦") (C . "♣"))
  "Alist of suit symbols for bridge suits."
  :type '(alist :key-type string :value-type string)
  :group 'bridge)

(defcustom bridge-dealer-bin "/usr/local/bin/dealer"
  "Path to the dealer binary."
  :type 'string
  :group 'bridge)


(defun bridge-board-number (b)
  "Return the board number of the bridge board B."
  (cadr (assoc 'board b)))

(defun bridge-board-hand (side b)
  "Return the contents of the hand SIDE in B."
  (cadr (assoc side b)))

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
  (dolist (suit '(S H D C))
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

;; contract is a list (board level trump double? declarer lead? tricks)
(defun bridge-contract-board (contract)
  "Get the board number of a CONTRACT."
  (car contract))

(defun bridge-contract-level (contract)
  "Get the level of a CONTRACT.
A number between 1 and 7."
  (nth 1 contract))

(defun bridge-contract-trump (contract)
  "Get the trump suit of a CONTRACT.
A symbol C D H S or NT."
  (nth 2 contract))

(defun bridge-is-valid-double (dbl)
  "Return t if DBL is a valid value for double."
  (cond ((null dbl) t)
        ((equal dbl 'x) t)
        ((equal dbl 'xx) t)
        (t nil)))

(defun bridge-contract-double (contract)
  "Get the double status of a CONTRACT.
A symbol: x xx or nil."
  (let ((elem (nth 3 contract)))
    (if (bridge-is-valid-double elem) elem nil)))

(defun bridge-contract-declarer (contract)
  "Get the declarer of a CONTRACT.
A symbol N E S or W."
  (if (bridge-is-valid-double (nth 3 contract))
      (nth 4 contract)
    (nth 3 contract)))

(defun bridge-card-p (obj)
  "Return t if OBJ is a valid card.
That is a pair of a suit and a rank."
  (and (listp obj)
       (eq 2 (length obj))
       (member (car obj) '(C D H S NT c d h s nt))
       (member (cadr obj) '(2 3 4 5 6 7 8 9 10 T t J j Q q K k A a))))

(defun bridge-contract-lead (contract)
  "Get the lead card of a CONTRACT.
Expressed as pair (SUIT RANK)."
  (let ((idx 3))
    (progn
      (if (bridge-is-valid-double (nth 3 contract))
          (setq idx (+ idx 1)))
      (let ((lead (nth idx contract)))
        (if (bridge-card-p lead) lead nil)))))

(defun bridge-contract-tricks-taken (contract)
  "Get the number of tricks taken in a CONTRACT."
  (car (last contract)))

(defun bridge-trump-format (trump)
  "Return a string representation of the TRUMP suit."
  (cdr (assoc (intern (upcase (symbol-name trump))) bridge-suit-symbols)))

(defun bridge-card-format (card)
  "Return a string representation of the CARD."
  (if (null card) ""
    (format "%s%s"
            (bridge-trump-format (car card))
            (cadr card))))

(defun bridge-double-format (dbl)
  "Return a string representation of the DBL status."
  (if (null dbl) ""
    (symbol-name dbl)))

(provide 'bridge)
;;; bridge.el ends here
