;; -*- lexical-binding: t -*-
;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'subr-x)
(require 'bridge)
(require 'bridge-match)
(require 'omnibridge-server-interface)

(defun omnibridge ()
  "Open a new buffer connected to a omnibridge server instance."
  (interactive)
  (pop-to-buffer "*omnibridge*")
  (let ((proc (get-buffer-process "*omnibridge*")))
    (if proc (delete-process proc)))
  (bridge-mode))

(define-derived-mode bridge-mode special-mode
  "Bridge Mentor client."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (clrhash omnibridge-server-response-handlers)
  (make-process :name omnibridge-server-process
                :buffer (current-buffer)
                :command (list bridge-mode-server-bin)
                :connection-type 'pipe
                :filter 'bridge-mode-filter)
  (if omnibridge-log-buffer
      (progn
        (pop-to-buffer omnibridge-log-buffer)
        (erase-buffer)
        (read-only-mode)
        (switch-to-buffer (process-buffer (get-process omnibridge-server-process))))))

(defun bridge-mode-filter (process msg)
  "Pass MSG to the first callback from the queue; ignore PROCESS."
  (bridge-mode-log-comm 'server msg)
  (let ((inhibit-read-only t))
    (condition-case e
        (let* ((expr (read msg))
               (request-id (symbol-name (cadr (assoc 'request_id expr))))
               (callback (gethash request-id omnibridge-server-response-handlers))
               (result (or (assoc 'error expr) (assoc 'ok expr))))
          (progn
            (cond ((eq (car result) 'error) (message "%s" result))
                  ((eq (car result) 'ok)
                   (funcall callback (cadr result))))
            (remhash request-id omnibridge-server-response-handlers)))
      ('error (progn
                (message "%s" (string-trim msg))
                (bridge-mode-log-comm 'error (prin1-to-string e)))))))

(defun bridge-mode-insert-board (resp)
  "Insert the board from RESP at the end of the current buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert "\n")
    (bridge-board-display resp)))

(defun bridge-mode-insert-random-board (number)
  "Insert a random bridge board with NUMBER into the buffer."
  (interactive "NBoard number: ")
  (bridge-mode-send-request (list 'deal number) 'bridge-mode-insert-board))

(defun bridge-mode-contract-score (contract)
  "Display score for the given CONTRACT in the minibuffer."
  (interactive "sContract: ")
  (let ((ctr (read contract)))
        (bridge-mode-send-request
         (cons 'score ctr)
         (bridge-mode-display-contract-score ctr))))

(defun bridge-mode-display-contract-score (contract)
  "Make a function to display score for the CONTRACT in minibuffer."
  (lambda (score)
    (message (format "%d. %d%s%s %s %s %d: %d"
                     (bridge-contract-board contract)
                     (bridge-contract-level contract)
                     (bridge-trump-format (bridge-contract-trump contract))
                     (bridge-double-format (bridge-contract-double contract))
                     (upcase (symbol-name (bridge-contract-declarer contract)))
                     (bridge-card-format (bridge-contract-lead contract))
                     (bridge-contract-tricks-taken contract)
                     score))))

(defvar-keymap bridge-mode-map
  :doc "Keymap for bridge-mode."
  "m" 'bridge-mode-match-protocol)

(provide 'bridge-mode)
;;; bridge-mode.el ends here
