;; -*- lexical-binding: t -*-
;;; Package --- Summary
;;; Commentary:
;;; Code:
(require 'bridge)

;; (defcustom bridge-mode-server-bin "/usr/local/bin/omnibridge"
(defcustom bridge-mode-server-bin "/home/sven/code/rust/omnibridge/target/debug/server"
  "Path to the omnibridge server binary."
  :type 'string
  :group 'bridge)

(defcustom omnibridge-server-process "omnibridge-server"
  "Name of the omnibridge server process."
  :type 'string
  :group 'bridge)

(defcustom omnibridge-log-buffer nil
  "Buffer to log communication with omnibridge server."
  :type '(choice string (const nil))
  :group 'bridge)

(defvar omnibridge-server-response-handlers (make-hash-table :test 'equal)
  "Mapping from request identifiers to callbacks meant to handle responses to these requests.")

(defun bridge-mode-log-comm (side msg)
  "Log MSG from SIDE in the omnibridge-log-buffer."
  (if omnibridge-log-buffer
      (with-current-buffer omnibridge-log-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "%s> %s\n" side (string-trim msg)))))))

(defun bridge-mode-send-request (request callback)
  "Send the REQUEST to server and register CALLBACK as response handler."
  (let ((msg (format "%s\n" request)))
    (progn
      (bridge-mode-log-comm 'client msg)
      (puthash (secure-hash 'sha256 msg) callback omnibridge-server-response-handlers)
      (process-send-string nil msg))))

(provide 'omnibridge-server-interface)
;;; omnibridge-server-interface.el ends here
