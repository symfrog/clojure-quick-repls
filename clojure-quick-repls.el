; Version 0.0.1 alpha

(defun repls-connect ()
  (interactive)
  (cider-jack-in)

  (setq clj-con-buf nil)
  (setq cljs-con-buf nil)

  (run-with-timer 25 nil (lambda (buf)
                           (with-current-buffer buf
                             (setq clj-con-buf (nrepl-current-connection-buffer))
                             (message "Creating nrepl connection for cljs")
                             (new-repl-connection)
                             (run-with-timer 25 nil (lambda (buf)
                                                      (with-current-buffer buf
                                                        (message "Initiating browser repl in %s" (nrepl-current-connection-buffer))
                                                        (cider-eval-sync "(require 'cljs.repl.browser)")
                                                        (cider-eval-sync "(cemerick.piggieback/cljs-repl
                    :repl-env (cljs.repl.browser/repl-env :port 9000))")

                                                        (setq cljs-con-buf (nrepl-current-connection-buffer))
                                                        (message "Cljs browser repl ready")
                                                        (message "Clj connection buffer: %s Cljs connection buffer %s" clj-con-buf cljs-con-buf))) (current-buffer)))) (current-buffer)))


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
