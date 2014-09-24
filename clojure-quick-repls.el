; Version 0.0.1 alpha

(setq repls-nrepl-connected-fn nil)

(defun noop-repls-nrepl-connected-fn ()
  (fset 'repls-nrepl-connected-fn (lambda (buf) nil)))

(noop-repls-nrepl-connected-fn)

(setq repls-current-buffer nil)
(setq nrepl-connect-done nil)

(defun clear-con-bufs ()
  (setq clj-con-buf nil)
  (setq cljs-con-buf nil))

(add-hook 'nrepl-connected-hook (lambda ()
                                  (setq nrepl-connect-done t)))

(add-hook 'nrepl-disconnected-hook (lambda ()
                                     (clear-con-bufs)))

(run-with-timer 15 5
                (lambda ()
                  (when nrepl-connect-done 
                    (setq nrepl-connect-done nil)
                    (repls-nrepl-connected-fn repls-current-buffer))))

(defun repls-connect ()
  (interactive)
  (setq repls-current-buffer (current-buffer))
  (noop-repls-nrepl-connected-fn)
  (cider-jack-in)

  (clear-con-bufs)

  (lexical-let* ((cljs-fn (lambda (buf)  
                            (with-current-buffer buf
                              (noop-repls-nrepl-connected-fn)
                              (nrepl-sync-request:eval "(require 'cljs.repl.browser)")
                              (nrepl-sync-request:eval "(cemerick.piggieback/cljs-repl
                    :repl-env (cljs.repl.browser/repl-env :port 9000))")
                              (setq cljs-con-buf (nrepl-current-connection-buffer))
                              (message "Clj connection buffer: %s Cljs connection buffer %s" clj-con-buf cljs-con-buf)
                              (message "Cljs browser repl ready")
                              ; Make the clj buf default after completion 
                              (nrepl-make-connection-default clj-con-buf))))
                 (clj-fn (lambda (buf)
                           (with-current-buffer buf
                             (noop-repls-nrepl-connected-fn)
                             (fset 'repls-nrepl-connected-fn cljs-fn)
                             (setq clj-con-buf (nrepl-current-connection-buffer))
                             (message "Creating nrepl connection for cljs")
                             (new-repl-connection)))))
    (fset 'repls-nrepl-connected-fn clj-fn)))

(defun new-repl-connection ()
  (interactive)
  (let* ((host (nrepl-current-host))
         (port (nrepl-default-port)))
    (message "Creating repl connection to nrepl server  on port %s, host %s" host port)
    (cider host port)))

(defun bound-truthy-p (s)
  (and (boundp s) (symbol-value s)))

(defun buffer-extension (buffer)
  (let ((name (buffer-name buffer)))
    (-when-let (p-loc (string-match-p "\\." name))
      (substring name (1+ p-loc) nil))) )

(defun repls-set-connection (f h)
  (let ((ext (buffer-extension (current-buffer))))
    (if (and (bound-truthy-p 'clj-con-buf)
             (bound-truthy-p 'cljs-con-buf)
             ext
             (or (string= ext "clj") (string= ext "cljs")))
        (progn
          (if (string= ext "cljs")
              (nrepl-make-connection-default cljs-con-buf)
            (nrepl-make-connection-default clj-con-buf))
          (when (fboundp f)
            (funcall f)))
      (when (fboundp h)
            (funcall h)))))

(defun repls-switch-to-relevant-repl (arg)
  (interactive)
  (repls-set-connection 'cider-switch-to-current-repl-buffer 'cider-switch-to-relevant-repl-buffer))

(defadvice nrepl-current-session (before repl-switch)
  (repls-set-connection nil nil)
  (message "Current repl connection buffer %s" (nrepl-current-connection-buffer)))

(ad-activate 'nrepl-current-session)

