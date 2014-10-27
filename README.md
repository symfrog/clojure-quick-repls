clojure-quick-repls
===================

Emacs functions for quickly creating Clojure and ClojureScript repls for a project. 

Once the repls are created the usual [CIDER](https://github.com/clojure-emacs/cider) commands can be used in either a clj/cljs buffer and the forms will be routed automatically via the correct connection.

So no need to manually switch connections! 

Installation
------------

Add clojure-quick-repls.el to your init.el via e.g. (load-file "~/.emacs.d/clojure-quick-repls.el"). 

Make sure you have [piggieback](https://github.com/cemerick/piggieback) as a dependency in project.clj.

Also requires [CIDER](https://github.com/clojure-emacs/cider). The latest CIDER snapshot should be used since clojure-quick-repls maintains compatibility with the latest CIDER snapshot as closely as possible.  


Usage
-----

Open any Clojure file in the project that you wish to have Clojure/ClojureScript repls for, then 

M-x repls-connect

Wait for about 1 minute and check *Messages* buffer for "Clj connection buffer: .\* Cljs connection buffer .\*". 

Do not attempt to execute repls-connect twice, if you need to restart run cider-quit and then repls-connect. 

You can now connect the browser and execute any cider command in either a clj/cljs buffer and the forms will be routed to the correct connection.

cider-switch-to-relevant-repl-buffer (C-c C-z) 
----------------------------------------------

If you would like to be able to switch to the relevant repl buffer based on if you are currently in a clj or cljs buffer then you need to set the custom variable cider-switch-to-repl-command to repls-switch-to-relevant-repl 
 
You can set a custom variable using: 

M-x customize

After setting the custom variable you should have an form under custom-set-variables that looks like: 

```
    '(cider-switch-to-repl-command (quote repls-switch-to-relevant-repl))
```

so a custom-set-variables form with only this variable set would look like: 

```
    (custom-set-variables
    ;; custom-set-variables was added by Custom.
    ;; If you edit it by hand, you could mess it up, so be careful.
    ;; Your init file should contain only one such instance.
    ;; If there is more than one, they won't work right.
    '(cider-switch-to-repl-command (quote repls-switch-to-relevant-repl)))
```

License
-------

Distributed under the GNU General Public License, version 3
