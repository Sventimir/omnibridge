;; -*- lexical-binding: t -*-
;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'subr-x)
(require 'bridge)

;; (defcustom bridge-mode-server-bin "/usr/local/bin/bridge-mentor"
(defcustom bridge-mode-server-bin "/home/sven/code/rust/bridge-mentor/target/debug/server"
  "Path to the bridge-mentor server binary."
  :type 'string
  :group 'bridge)

(defcustom bridge-mentor-server-process "bridge-mentor-server"
  "Name of the bridge-mentor server process."
  :type 'string
  :group 'bridge)

(defcustom bridge-mentor-log-buffer nil
  "Buffer to log communication with bridge-mentor server."
  :type '(choice string (const nil))
  :group 'bridge)

(defvar bridge-mode-response-handlers (make-hash-table :test 'equal)
  "Mapping from request identifiers to callbacks meant to handle responses to these requests.")

(defun bridge-mentor ()
  "Open a new buffer connected to a bridge-mentor server instance."
  (interactive)
  (pop-to-buffer "*bridge-mentor*")
  (let ((proc (get-buffer-process "*bridge-mentor*")))
    (if proc (delete-process proc)))
  (bridge-mode))

(defun bridge-mode-log-comm (side msg)
  "Log MSG from SIDE in the bridge-mentor-log-buffer."
  (if bridge-mentor-log-buffer
      (with-current-buffer bridge-mentor-log-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "%s> %s\n" side (string-trim msg)))))))

(defun bridge-mode-send-request (request callback)
  "Send the REQUEST to server and register CALLBACK as response handler."
  (let ((msg (format "%s\n" request)))
    (progn
      (bridge-mode-log-comm 'client msg)
      (puthash (secure-hash 'sha256 msg) callback bridge-mode-response-handlers)
      (process-send-string nil msg))))

(define-derived-mode bridge-mode special-mode
  "Bridge Mentor client."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (clrhash bridge-mode-response-handlers)
  (make-process :name bridge-mentor-server-process
                :buffer (current-buffer)
                :command (list bridge-mode-server-bin)
                :connection-type 'pipe
                :filter 'bridge-mode-filter)
  (if bridge-mentor-log-buffer
      (progn
        (pop-to-buffer bridge-mentor-log-buffer)
        (erase-buffer)
        (read-only-mode)
        (switch-to-buffer (process-buffer (get-process bridge-mentor-server-process))))))

(defun bridge-mode-filter (process msg)
  "Pass MSG to the first callback from the queue; ignore PROCESS."
  (bridge-mode-log-comm 'server msg)
  (let ((inhibit-read-only t))
    (condition-case e
        (let* ((expr (read msg))
               (request-id (symbol-name (cadr (assoc 'request_id expr))))
               (callback (gethash request-id bridge-mode-response-handlers))
               (result (or (assoc 'error expr) (assoc 'ok expr))))
          (progn
            (cond ((eq (car result) 'err) (message result))
                  ((eq (car result) 'ok)
                   (funcall callback (cadr result))))
            (remhash request-id bridge-mode-response-handlers)))
      ('error (progn
                (message "%s" (string-trim msg))
                (bridge-mode-log-comm 'error e))))))

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

(provide 'bridge-mode)
;;; bridge-mode.el ends here
