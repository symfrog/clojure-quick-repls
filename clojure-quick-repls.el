; Version 0.0.1 alpha

(setq repls-nrepl-connected-fn nil)
(defun noop-repls-nrepl-connected-fn () (fset 'repls-nrepl-connected-fn (lambda (buf) nil)))

(noop-repls-nrepl-connected-fn)

(setq repls-timer nil)
(setq nrepl-connect-done nil)

(add-hook 'nrepl-connected-hook (lambda ()
                                  (message "nrepl connected hook called...")
                                 (setq nrepl-connect-done t)))

(defun repls-connect ()
  (interactive)
  (noop-repls-nrepl-connected-fn)
  (cider-jack-in)

  (setq clj-con-buf nil)
  (setq cljs-con-buf nil)

  (lexical-let* ((cljs-fn (lambda (buf)  
                    (with-current-buffer buf
                      (message "Initiating browser repl in %s" (nrepl-current-connection-buffer))
                      (noop-repls-nrepl-connected-fn)
                      (cider-eval-sync "(require 'cljs.repl.browser)")
                      (cider-eval-sync "(cemerick.piggieback/cljs-repl
                    :repl-env (cljs.repl.browser/repl-env :port 9000))")
                      (setq cljs-con-buf (nrepl-current-connection-buffer))
                      (message "Cljs browser repl ready")
                      (message "Clj connection buffer: %s Cljs connection buffer %s" clj-con-buf cljs-con-buf)
                      )))
         (clj-fn (lambda (buf)
                   (with-current-buffer buf
                     (message "clj-fn called..")
                     (noop-repls-nrepl-connected-fn)
                     (message "done setting no-op")
                     (if (boundp 'cljs-fn) (message "cljs-fn good") (message "cljs-fn bad"))
                     (message "cljs-fn %s" cljs-fn)
                     (fset 'repls-nrepl-connected-fn cljs-fn)
                     (message "done fesetting cljs-fn")
                     (setq clj-con-buf (nrepl-current-connection-buffer))
                     (message "Creating nrepl connection for cljs")
                     (new-repl-connection))) ))
          (fset 'repls-nrepl-connected-fn clj-fn)
          (run-with-timer 5 5
                          (lambda (buf)
                            (message "timer called: %s" nrepl-connect-done)
                            (when nrepl-connect-done 
                                (setq nrepl-connect-done nil)
                                (repls-nrepl-connected-fn buf)
                              ))
                          (current-buffer))))


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

(defadvice nrepl-current-session (before repl-switch)
  (when (and (bound-truthy-p 'clj-con-buf) (bound-truthy-p 'cljs-con-buf))
    (-when-let (ext (buffer-extension (current-buffer)))
      (when (or (string= ext "clj") (string= ext "cljs"))
        (if (string= ext "cljs")
            (nrepl-make-repl-connection-default cljs-con-buf)
          (nrepl-make-repl-connection-default clj-con-buf)))))
  (message "Current repl connection buffer %s" (nrepl-current-connection-buffer)))

(ad-activate 'nrepl-current-session)

