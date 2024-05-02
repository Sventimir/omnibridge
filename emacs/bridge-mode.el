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

(defvar bridge-mode-response-handlers (make-hash-table :test 'equal)
  "Mapping from request identifiers to callbacks meant to handle responses to these requests.")

(defun bridge-mentor ()
  "Open a new buffer connected to a bridge-mentor server instance."
  (interactive)
  (pop-to-buffer "*bridge-mentor*")
  (let ((proc (get-buffer-process "*bridge-mentor*")))
    (if proc (delete-process proc)))
  (bridge-mode))

(defun bridge-mode-send-request (request callback)
  "Send the REQUEST to server and register CALLBACK as response handler."
  (let ((msg (format "%s\n" request)))
    (progn
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
                :filter 'bridge-mode-filter))

(defun bridge-mode-filter (process msg)
  "Pass MSG to the first callback from the queue; ignore PROCESS."
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
      ('error (message (string-trim msg))))))

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

(provide 'bridge-mode)
;;; bridge-mode.el ends here
