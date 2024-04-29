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

(defvar bridge-mode-callback-queue nil
  "Queue of callbacks to be called when the server sends a message.")

(define-derived-mode bridge-mode special-mode
  "Bridge Mentor client."
  (erase-buffer)
  (add-to-list 'bridge-mode-callback-queue 'bridge-mode-greeting t (lambda (_ _) nil))
  (make-process :name bridge-mentor-server-process
                :buffer (current-buffer)
                :command (list bridge-mode-server-bin)
                :connection-type 'pipe
                :filter 'bridge-mode-filter))

(defun bridge-mode-filter (process msg)
  "Pass MSG to the first callback from the queue; ignore PROCESS."
  (let ((inhibit-read-only t))
    (condition-case e
        (let ((expr (read msg))
              (callback (pop bridge-mode-callback-queue)))
          (progn
            (cond ((eq (car expr) 'Error) (message expr))
                  ((eq (car expr) 'Ok)
                   (funcall callback (cadr expr))))))
      ('error (message (string-trim msg))))))

(defun bridge-mode-greeting (msg)
  "Handle the greeting MSG from the bridge-mentor server."
  (message msg))

(defun bridge-mode-insert-board (resp)
  "Insert the board from RESP into the buffer."
  (bridge-board-display resp))

(defun bridge-mode-insert-random-board (number)
  "Insert a random bridge board with NUMBER into the buffer."
  (interactive "NBoard number: ")
  (add-to-list 'bridge-mode-callback-queue 'bridge-mode-insert-board t (lambda (_ _) nil))
  (process-send-string nil (format "%s\n" (list 'deal number))))

(provide 'bridge-mode)
;;; bridge-mode.el ends here
