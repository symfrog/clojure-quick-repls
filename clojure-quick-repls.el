;;; clojure-quick-repls.el --- Emacs functions for quickly creating Clojure and ClojureScript repls for a project.

;; Copyright (C) 2014 symfrog

;; URL: https://github.com/symfrog/clojure-quick-repls
;; Keywords: languages, clojure, cider, clojurescript
;; Version: 0.2.0-cvs
;; Package-Requires: ((cider "0.8.1"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs functions for quickly creating Clojure and ClojureScript repls for a project.
;; Once the repls are created the usual CIDER commands can be used in either a clj/cljs buffer and the forms will be routed automatically via the correct connection.
;; So no need to manually switch connections!

;;; Installation:

;; Available as a package in melpa.org.
;; M-x package-install clojure-quick-repls

;;; Usage:

;;     (require 'clojure-quick-repls)

;;; Code:

(require 'cider)

(setq repls-nrepl-connected-fn nil)

(setq repls-cljs-setup
      "(require 'cljs.repl.browser)
       (cemerick.piggieback/cljs-repl
                    :repl-env (cljs.repl.browser/repl-env :port 9000))")

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
                              (if (string= "ex" (cadr (nrepl-sync-request:eval repls-cljs-setup)))
                                  (message "Failed to initialize cljs connection with form %s" repls-cljs-setup)
                                (progn
                                  (setq cljs-con-buf (nrepl-current-connection-buffer))
                                  (message "Clj connection buffer: %s Cljs connection buffer %s" clj-con-buf cljs-con-buf)
                                  (message "Cljs browser repl ready")
                                        ; Make the clj buf default after completion 
                                  (nrepl-make-connection-default clj-con-buf))))))
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
         (port (nrepl-extract-port)))
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

;;; clojure-quick-repls.el ends here
