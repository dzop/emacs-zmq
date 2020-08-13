;;; zmq.el --- ZMQ bindings in elisp -*- lexical-binding: t -*-

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 05 Jan 2018
;; URL: https://github.com/dzop/emacs-zmq
;; Keywords: comm
;; Version: 0.10.10
;; Package-Requires: ((cl-lib "0.5") (emacs "26"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Bindings to the ZMQ distributed messaging library in
;; Emacs.

;;; Code:

(require 'cl-lib)

(defgroup zmq nil
  "ZMQ bindings for Emacs"
  :group 'communication)

(defconst zmq-emacs-version "v0.10.10")

(defconst zmq-module-file
  (when module-file-suffix
    (expand-file-name
     (concat "emacs-zmq" module-file-suffix)
     (file-name-directory (locate-library "zmq.el"))))
  "The module file for ZMQ or nil if modules are not supported.")

(defvar zmq-current-context nil
  "The context set by the function `zmq-current-context'.")

(defun zmq-current-context ()
  "Return the value of the variable `zmq-current-context' if non-nil.
Otherwise, create a new `zmq-context', bind it to the
variable `zmq-current-context', and return the newly created
context."
  (or zmq-current-context
      (setq zmq-current-context (zmq-context))))

(defun zmq-assoc (item list)
  "Find the first match whose car is equal to ITEM in LIST.
`zmq-equal' is used for comparison."
  (cl-assoc item list :test #'zmq-equal))

;;; Tunneling

;; TODO: Add password handling
(defun zmq-make-tunnel (lport rport server &optional remoteip)
  "Forward traffic from LPORT on the localhost to REMOTEIP:RPORT on SERVER.
If REMOTEIP is nil, forward LPORT to RPORT on SERVER.
Forwarding is done with ssh."
  (or remoteip (setq remoteip "127.0.0.1"))
  (start-process
   "zmq-tunnel" nil
   "ssh"
   ;; Run in background
   "-f"
   ;; Wait until the tunnel is open
   "-o ExitOnForwardFailure=yes"
   ;; Local forward
   "-L" (format "127.0.0.1:%d:%s:%d" lport remoteip rport)
   server
   ;; Close the tunnel if no other connections are made within 60
   ;; seconds
   "sleep 60"))

;;; Socket functions

(defun zmq-bind-to-random-port (sock addr &optional min-port max-port max-tries)
  "Bind SOCK to ADDR on a random port.

ADDR must be an address string without the port.

Randomly bind SOCK to ADDR on a port in the range

    [MIN-PORT,MAX-PORT)

If the port assigned is in use on ADDR, try a different port. If
SOCK could not be bound after MAX-TRIES return nil, otherwise
return the port SOCK was bound to. MIN-PORT defaults to 49152,
MAX-PORT defaults to 65536, and MAX-TRIES defaults to 100."
  (setq min-port (or min-port 49152)
        max-port (or max-port 65536)
        max-tries (or max-tries 100))
  (let (port)
    (catch 'bound
      (dotimes (_i max-tries)
        (setq port (+ (cl-random (- max-port min-port)) min-port))
        (condition-case nil
            (progn
              (zmq-bind sock (format "%s:%d" addr port))
              (throw 'bound port))
          ((zmq-EACCES zmq-EADDRINUSE) nil))))))

;;; Encoding/decoding messages and socket options

(defun zmq-send-encoded (sock message &optional coding-system flags)
  "Send a message on SOCK, encoded it before sending.
MESSAGE is the message string to encoded using CODING-SYSTEM.
CODING-SYSTEM defaults to utf-8. FLAGS has the same meaning as in
`zmq-send'."
  (setq coding-system (or coding-system 'utf-8))
  (zmq-send sock (encode-coding-string message coding-system) flags))

(defun zmq-recv-decoded (sock &optional coding-system flags)
  "Receive a message on SOCK, return the decoded message.
Use CODING-SYSTEM to decode the message received on SOCK.
CODING-SYSTEM defaults to utf-8. FLAGS has the same meaning as in
`zmq-recv'."
  (setq coding-system (or coding-system 'utf-8))
  (decode-coding-string (zmq-recv sock flags) coding-system))

(defun zmq-socket-set-encoded (sock option value &optional coding-system)
  "Set a SOCK OPTION, encoding its value before setting.
Use CODING-SYSTEM to encode VALUE. CODING-SYSTEM defaults to
utf-8."
  (setq coding-system (or coding-system 'utf-8))
  (zmq-set-option sock option (encode-coding-string value coding-system)))

(defun zmq-socket-get-decoded (sock option &optional coding-system)
  "Get a SOCK OPTION, return its decoded value.
Use CODING-SYSTEM to decode OPTION's value. CODING-SYSTEM
defaults to utf-8."
  (setq coding-system (or coding-system 'utf-8))
  (decode-coding-string (zmq-get-option sock option) coding-system))

;;; Sending/receiving multipart messages

(defun zmq-send-multipart (sock parts &optional flags)
  "Send a multipart message on SOCK.
PARTS is a list of message parts to send on SOCK. FLAGS has the
same meaning as `zmq-send'."
  (or flags (setq flags 0))
  (while parts
    (let ((part (zmq-message (car parts))))
      (zmq-message-send
       part sock (if (not (null (cdr parts)))
                     (logior flags zmq-SNDMORE)
                   flags))
      (setq parts (cdr parts)))))

(defun zmq-recv-multipart (sock &optional flags)
  "Receive a multipart message from SOCK.
FLAGS has the same meaning as in `zmq-recv'."
  (let (res)
    (catch 'recvd
      (while t
        (let ((part (zmq-message)))
          (zmq-message-recv part sock flags)
          (setq res (cons (zmq-message-data part) res))
          (unless (zmq-message-more-p part)
            (throw 'recvd (nreverse res))))))))

;;; Setting/getting options from contexts, sockets, messages

(defun zmq--set-get-option (set object option &optional value)
  "Helper function for `zmq-get-option' and `zmq-set-option'.
If SET is non-nil, set OBJECT's OPTION to VALUE. Otherwise get
OPTION's value, ignoring any VALUE argument."
  (let ((fun (cond
              ((zmq-socket-p object)
               (if set #'zmq-socket-set #'zmq-socket-get))
              ((zmq-context-p object)
               (if set #'zmq-context-set #'zmq-context-get))
              ((zmq-message-p object)
               (if set #'zmq-message-set #'zmq-message-get))
              (t (signal 'wrong-type-argument
                         (list
                          '(zmq-socket-p zmq-context-p zmq-message-p)
                          object))))))
    (if set (funcall fun object option value)
      (funcall fun object option))))

(defun zmq-set-option (object option value)
  "For OBJECT, set OPTION to VALUE.

OBJECT can be a `zmq-socket', `zmq-context', or a `zmq-message'.
The OPTION set should correspond to one of the options available
for that particular object."
  (zmq--set-get-option 'set object option value))

(defun zmq-get-option (object option)
  "For OBJECT, get OPTION's value.

OBJECT can be a `zmq-socket', `zmq-context', or a `zmq-message'.
The OPTION to get should correspond to one of the options
available for that particular object."
  (zmq--set-get-option nil object option))

(defconst zmq-message-properties '((:socket-type . "Socket-Type")
                                   (:identity . "Identity")
                                   (:resource . "Resource")
                                   (:peer-address . "Peer-Address")
                                   (:user-id . "User-Id"))
  "Alist mapping keywords to their corresponding message property.
A message's metadata property can be accessed through
`zmq-message-property'.")

(defun zmq-message-property (message property)
  "Get a MESSAGE's metadata PROPERTY.

PROPERTY is a keyword and can only be one of those in
`zmq-message-properties'."
  (let ((prop (cdr (assoc property zmq-message-properties))))
    (unless prop
      (signal 'args-out-of-range
              (list (mapcar #'car zmq-message-properties) prop)))
    (decode-coding-string (zmq-message-gets message prop) 'utf-8)))

;;; Subprocesses

(define-error 'zmq-subprocess-error "Error in ZMQ subprocess")

(defun zmq-flush (stream)
  "Flush STREAM.
STREAM can be one of `stdout', `stdin', or `stderr'."
  (set-binary-mode stream t)
  (set-binary-mode stream nil))

(defun zmq-prin1 (sexp)
  "Print SEXP using `prin1' and flush `stdout' afterwards."
  (prin1 sexp)
  (zmq-flush 'stdout))

(defvar zmq--subprocess-debug nil
  "If non-nil, capture backtraces in subprocesses.
Send backtraces as subprocess errors to the parent Emacs process.
In addition, log any stderr messages produced by the subprocess
as messages in the parent Emacs process.")

(defvar zmq-backtrace nil
  "The backtrace stored when debugging the subprocess.")

(defun zmq--init-subprocess (&optional backtrace)
  "Initialize the ZMQ subprocess.
Call `zmq-subprocess-read' and assuming the read s-expression is
a function, call the function. If the function accepts a single
argument, pass the `zmq-context' created by a call to the
function `zmq-current-context' as the argument.

If BACKTRACE is non-nil and an error occurs when evaluating, send
the backtrace to the parent process. This should only be used for
debugging purposes."
  (if (not noninteractive) (error "Not a subprocess")
    (let* ((debug-on-event nil)
           (coding-system-for-write 'utf-8-auto)
           (signal-hook-function
            (when backtrace
              (lambda (&rest _)
                (setq zmq-backtrace
                      (with-temp-buffer
                        (let ((standard-output (current-buffer)))
                          (backtrace))
                        (buffer-string))))))
           (sexp (eval (zmq-subprocess-read)))
           (wrap-context (= (length (cadr sexp)) 1)))
      (condition-case err
          (if wrap-context
              (funcall sexp (zmq-current-context))
            (funcall sexp))
        (error
         (zmq-prin1 (cons 'error (list (car err)
                                       (or zmq-backtrace
                                           (cdr err))))))))))

(defvar-local zmq--subprocess-parse-state nil
  "The parse state of the output read from a subprocess.")

(defsubst zmq--subprocess-skip-delete-to-sexp ()
  "Skip to the start of a list.
Delete the region between `point' and the start of the next list.
Only skip and delete if `zmq--subprocess-parse-state' is nil."
  (unless zmq--subprocess-parse-state
    (delete-region
     (point) (+ (point) (skip-syntax-forward "^(")))))

(defun zmq--subprocess-read-output (output)
  "Return a list of s-expressions read from OUTPUT.
OUTPUT is inserted into the `current-buffer' and `read' until the
first incomplete s-expression or until all s-expressions of
OUTPUT have been processed. After reading, the contents of the
`current-buffer' from `point-min' up to the last valid
s-expression is removed and a list of all the valid s-expressions
in OUTPUT is returned.

Any other text in OUTPUT that is not an s-expression is ignored.

Also note, for this function to work properly the same buffer
should be current for subsequent calls."
  (let (accum)
    (save-excursion (insert output))
    (zmq--subprocess-skip-delete-to-sexp)
    (while (/= (point) (point-max))
      (setq zmq--subprocess-parse-state
            (parse-partial-sexp
             (point) (point-max) 0
             nil zmq--subprocess-parse-state))
      (when (= (nth 0 zmq--subprocess-parse-state) 0)
        ;; When a complete top-level s-expression has been
        ;; parsed, collect into the accumulated list of
        ;; complete s-expressions.
        (let ((beg (nth 2 zmq--subprocess-parse-state))
              (end (point)))
          (goto-char beg)
          (unwind-protect
              (push (read (current-buffer)) accum)
            (setq zmq--subprocess-parse-state nil)
            (goto-char end)
            (delete-region beg end))))
      (zmq--subprocess-skip-delete-to-sexp))
    (nreverse accum)))

(defun zmq--subprocess-handle-stderr (process)
  "Print PROCESS' stderr as messages.
Do this only if the PROCESS has a non-nil :debug property."
  (when (process-get process :debug)
    (let ((stderr (process-get process :stderr)))
      (unless (zerop (buffer-size (process-buffer stderr)))
        (with-current-buffer (process-buffer stderr)
          (let ((prefix (concat "STDERR[" (process-name process) "]: ")))
            (goto-char (point-min))
            (while (/= (point) (point-max))
              (message "%s%s" prefix (buffer-substring
                                      (line-beginning-position)
                                      (line-end-position)))
              (forward-line))
            (erase-buffer)))))))

(defun zmq--subprocess-filter (process output)
  "Create a stream of s-expressions based on PROCESS' OUTPUT.
If PROCESS has a non-nil `:filter' property then it should be a
function with the same meaning as the EVENT-FILTER argument in
`zmq-start-process', OUTPUT will be converted into a list of
s-expressions and the `:filter' function called for every valid
s-expression in OUTPUT.

As a special case, if the `car' of an s-expression is the symbol
error, a `zmq-subprocess-error' is signaled using the `cdr' of
the list for the error data."
  (zmq--subprocess-handle-stderr process)
  (with-current-buffer (process-buffer process)
    (goto-char (process-mark process))
    (let ((filter (process-get process :filter)))
      (when filter
        (let ((stream (let ((inhibit-read-only t))
                        (zmq--subprocess-read-output output))))
          (cl-loop
           for event in stream
           if (and (listp event) (eq (car event) 'error)) do
           ;; TODO: Better way to handle this
           (signal 'zmq-subprocess-error (cdr event))
           else do (with-demoted-errors "Error in ZMQ subprocess filter: %S"
                     (funcall filter event))))))
    (set-marker (process-mark process) (point-max))))

(defun zmq--subprocess-sentinel (process event)
  "Sentinel function for ZMQ subprocesses.
If a PROCESS has a `:sentinel' property that is a function, the
function is called with identical arguments as this function.

When EVENT represents any of the events that notify when a
subprocess has exited, kill the process buffer only when the
`:owns-buffer' property of the PROCESS is non-nil. Otherwise the
process buffer is left alive and assumed to be a buffer that was
initially passed to `zmq-start-process'."
  (let ((sentinel (process-get process :sentinel)))
    (unwind-protect
        (when (functionp sentinel)
          (funcall sentinel process event))
      (when (memq (process-status process) '(exit signal))
        (delete-process process)
        (when (process-get process :owns-buffer)
          (kill-buffer (process-buffer process)))
        (let ((stderr (process-get process :stderr)))
          (when (process-live-p stderr)
            (delete-process stderr))
          (when (buffer-live-p (process-buffer stderr))
            (kill-buffer (process-buffer stderr))))))))

(defvar zmq--subprocess-send-buffer nil)

;; Adapted from `async--insert-sexp' in the `async' package :)
(defun zmq-subprocess-send (process sexp)
  "Send an s-expression to PROCESS' STDIN.
PROCESS should be an Emacs subprocess that decodes messages sent
on its STDIN using `zmq-subprocess-read'.

The SEXP is first encoded with the `utf-8-auto' coding system and
then encoded using Base 64 encoding before being sent to the
subprocess."
  (declare (indent 1))
  (let ((print-circle t)
        (print-escape-nonascii t)
        print-level print-length)
    (with-current-buffer
        (if (buffer-live-p zmq--subprocess-send-buffer)
            zmq--subprocess-send-buffer
          (setq zmq--subprocess-send-buffer
                (get-buffer-create " *zmq-subprocess-send*")))
      (erase-buffer)
      (prin1 sexp (current-buffer))
      (encode-coding-region (point-min) (point-max) 'utf-8-auto)
      (base64-encode-region (point-min) (point-max) t)
      (goto-char (point-min)) (insert ?\")
      (goto-char (point-max)) (insert ?\" ?\n)
      (process-send-region process (point-min) (point-max)))))

(defun zmq-subprocess-read ()
  "Read a single s-expression from STDIN.
This does the decoding of the encoding described in
`zmq-subprocess-send' and returns the s-expression. This is only
meant to be called from an Emacs subprocess."
  (if (not noninteractive) (error "Not in a subprocess")
    (read (decode-coding-string
           (base64-decode-string (read-minibuffer ""))
           'utf-8-unix))))

(defun zmq-set-subprocess-filter (process event-filter)
  "Set the event filter function for PROCESS.
EVENT-FILTER has the same meaning as in `zmq-start-process'."
  (process-put process :filter event-filter))

(defun zmq-set-subprocess-sentinel (process sentinel)
  "Set the sentinel function for PROCESS.
SENTINEL has the same meaning as in `zmq-start-process'."
  (process-put process :sentinel sentinel))

(defun zmq-set-subprocess-buffer (process buffer)
  "Set PROCESS' buffer to BUFFER.
Delete PROCESS' current buffer if it was automatically created
when `zmq-start-process' was called. It is the responsibility of
the caller to cleanup BUFFER when PROCESS exits after a call to
this function."
  (let ((marker (process-mark process)))
    ;; Copy over any pending results
    (with-current-buffer (process-buffer process)
      (copy-to-buffer buffer (point-min) (point-max)))
    (set-process-buffer process buffer)
    (set-marker marker (marker-position marker) buffer)
    (when (process-get process :owns-buffer)
      (kill-buffer (process-buffer process))
      (setf (process-get process :owns-buffer) nil))))

(cl-defun zmq-start-process (sexp &key filter sentinel buffer debug)
  "Start an Emacs subprocess initializing it with SEXP.
Return the newly created process.

SEXP is either a lambda form or a function symbol. In both cases
the function can either take 0 or 1 arguments. If SEXP takes 1
argument, then a new context object will be passed as the
argument of the function.

FILTER is a function that takes a single argument, a complete
s-expression read from the process' stdout. This means that care
should be taken when writing SEXP to ensure that it only prints
out lists. Anything other value that SEXP prints will be ignored.

SENTINEL has the same meaning as in `make-process'.

BUFFER will be set as the `process-buffer' for the returnd
process if non-nil. When BUFFER is nil, a new buffer will be
created. When a BUFFER is supplied, it should not be used for any
other purpose after a call to this function since it will be used
to store intermediate output from the subprocess. If this
function creates a new buffer, that buffer will be killed on
process exit, but it is the responsiblity of the caller to kill
the buffer if a buffer is supplied to this function.

If DEBUG is non-nil, then the subprocess returns a backtrace if
it errors out and prints its stderr as messages in the parent
Emacs process."
  (or debug (setq debug zmq--subprocess-debug))
  (cond
   ((functionp sexp)
    (unless (listp sexp)
      (setq sexp (symbol-function sexp))))
   (t (error "Can only send functions to processes")))
  (unless (member (length (cadr sexp)) '(0 1))
    (error "Invalid function to send to process, can only have 0 or 1 arguments"))
  (let* ((zmq-path (locate-library "zmq"))
         (cmd (format "(zmq--init-subprocess %s)" (when debug t)))
         ;; stderr is split from stdout since the former is used by
         ;; Emacs to print messages that we don't want intermixed
         ;; with what the subprocess returns.
         (stderr (make-pipe-process
                  :name "zmq stderr"
                  :buffer (generate-new-buffer " *zmq*")
                  :noquery t
                  :stop (not debug)))
         (process (make-process
                   :name "zmq"
                   :buffer (or buffer (generate-new-buffer " *zmq*"))
                   :noquery t
                   :connection-type 'pipe
                   :coding-system 'no-conversion
                   :filter #'zmq--subprocess-filter
                   :sentinel #'zmq--subprocess-sentinel
                   :stderr stderr
                   :command (list
                             (file-truename
                              (expand-file-name invocation-name
                                                invocation-directory))
                             "-Q" "-batch"
                             "-L" (file-name-directory zmq-path)
                             "-l" zmq-path "--eval" cmd))))
    (process-put process :debug debug)
    (process-put process :stderr stderr)
    (process-put process :filter filter)
    (process-put process :sentinel sentinel)
    (process-put process :owns-buffer (null buffer))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (special-mode))
    (zmq-subprocess-send process (macroexpand-all sexp))
    process))

;;; Download releases

(defun zmq--system-configuration-list (string)
  (save-match-data
    (string-match "\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)\\(?:-\\(.+\\)\\)?" string)
    (let ((arch (match-string 1 string))
          (vendor (match-string 2 string))
          (sys (match-string 3 string))
          (abi (match-string 4 string)))
      (list arch vendor sys abi))))

(defun zmq--system-configuration ()
  (cl-destructuring-bind (arch vendor sys abi)
      (zmq--system-configuration-list system-configuration)
    ;; Attempt to handle common systems
    (cond
     ((string-prefix-p "darwin" sys)
      ;; On OSX the sys part of the host is like
      ;; darwin17.1.0, but the binaries are compatible across
      ;; multiple versions. TODO: Figure out the actual ABI
      ;; compatibility.
      (concat arch "-" vendor "-darwin"))
     ((and (member vendor '("pc" "unknown" "none"))
           (equal sys "linux"))
      (concat arch "-" sys "-" abi))
     (t
      system-configuration))))

(defun zmq--insert-url-contents (url)
  ;; Attempt to download URL using various methods
  (cond
   ((and (executable-find "curl")
         ;; -s = silent, -L = follow redirects
         (zerop (call-process "curl" nil (current-buffer) nil
                              "-s" "-L" url))))
   ((and (executable-find "wget")
         ;; -q = quiet, -O - = output to stdout
         (zerop (call-process "wget" nil (current-buffer) nil
                              "-q" "-O" "-" url))))
   (t
    (require 'url-handlers)
    (let ((buf (url-retrieve-synchronously url)))
      (url-insert buf)))))

(defmacro zmq--download-url (url &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (zmq--insert-url-contents ,url)
     (goto-char (point-min))
     ,@body))

(defun zmq--download-and-extract-file (url tgz-file)
  (message "Downloading %s/%s" url tgz-file)
  (let ((sig (zmq--download-url (concat url "/" tgz-file ".sha256")
               (forward-sexp)
               (let ((end (point)))
                 (buffer-substring
                  (progn (backward-sexp) (point))
                  end)))))
    (message "Verifying sha256 signature of %s" tgz-file)
    (zmq--download-url (concat url "/" tgz-file)
      (if (not (equal sig (secure-hash 'sha256 (buffer-string))))
          (error "Signature did not match content")
        ;; We write the tar.gz file so that Emacs properly uncompresses
        ;; it through `jka-compr' otherwise `tar-untar-buffer' will
        ;; fail with a weird error.
        (set-buffer-multibyte nil)
        (write-region nil nil tgz-file)))
    (let ((buffer (find-file-noselect tgz-file)))
      (unwind-protect
          (with-current-buffer buffer
            (require 'tar-mode)
            (tar-untar-buffer))
        (kill-buffer buffer)
        (delete-file tgz-file)))))

(defvar json-object-type)

(defun zmq--download-module (tag)
  (let ((msg "Check%s for compatible module binary to download%s"))
    (when (or noninteractive (y-or-n-p (format msg "" "? ")))
      (when noninteractive
        (message msg "ing" ""))
      (catch 'failure
        (let* ((api-url "https://api.github.com/repos/dzop/emacs-zmq/")
               (repo-url "https://github.com/dzop/emacs-zmq/")
               (release-url (concat api-url "releases/"))
               (info (zmq--download-url (concat release-url tag)
                       (require 'json)
                       (let ((json-object-type 'plist))
                         (ignore-errors (json-read)))))
               (tag-name (or (plist-get info :tag_name)
                             (throw 'failure nil)))
               (ezmq-sys (concat "emacs-zmq-" (zmq--system-configuration)))
               (assets (cl-remove-if-not
                        (lambda (x) (string-prefix-p ezmq-sys x))
                        (mapcar (lambda (x) (plist-get x :name))
                           (append (plist-get info :assets) nil)))))
          (when assets
            (let ((default-directory (file-name-directory (locate-library "zmq"))))
              ;; We have a signature file and a tar.gz file for each binary so the
              ;; minimum number of files is two.
              (if (> (length assets) 2)
                  (error "TODO More than one file found")
                (let* ((tgz-file (cl-find-if (lambda (x) (string-suffix-p "tar.gz" x))
                                             assets))
                       (lib (expand-file-name
                             (concat "emacs-zmq" module-file-suffix)
                             (expand-file-name
                              (file-name-sans-extension
                               (file-name-sans-extension tgz-file))))))
                  (zmq--download-and-extract-file
                   (concat repo-url "releases/download/" tag-name) tgz-file)
                  (copy-file lib zmq-module-file)
                  t)))))))))

;;; Load emacs-zmq

(defun zmq-load ()
  "Load the ZMQ dynamic module."
  ;; Assume the module is already loaded when one of its functions is defined.
  (unless (functionp #'zmq--cleanup-globrefs)
    (if zmq-module-file
        (if (file-exists-p zmq-module-file)
            (progn
              (load-file zmq-module-file)
              (add-hook 'post-gc-hook #'zmq--cleanup-globrefs))
          ;; Can also be "latest"
          (if (zmq--download-module (concat "tags/" zmq-emacs-version))
              (zmq-load)
            (let ((msg "ZMQ module not found. Build%s it%s"))
              (when (or noninteractive (y-or-n-p (format msg "" "? ")))
                (when noninteractive
                  (message msg "ing" ""))
                (let ((default-directory
                        (file-name-directory (locate-library "zmq")))
                      (emacs
                       (when (and invocation-directory invocation-name)
                         (file-truename
                          (expand-file-name invocation-name
                                            invocation-directory)))))
                  (cl-labels
                      ((load-zmq
                        (_buf status)
                        (if (string= status "finished\n")
                            (zmq-load)
                          (message "Something went wrong when compiling the ZMQ module!"))
                        (remove-hook 'compilation-finish-functions #'load-zmq)
                        (exit-recursive-edit)))
                    (add-hook 'compilation-finish-functions #'load-zmq)
                    (compile (concat "make" (when emacs (concat " EMACS=" emacs))))
                    (recursive-edit)))))))
      (user-error "Modules are not supported"))))

(zmq-load)

(provide 'zmq)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; zmq.el ends here
