clojure-quick-repls
===================

Emacs functions for quickly creating clojure and clojurescript repls for a project. 

Once the repls are created the usual [cider](https://github.com/clojure-emacs/cider) commands can be used in either a clj/cljs buffer and the forms will be routed automatically via the correct connection.

So no need to manually switch connections! 

Installation
------------

Add clojure-quick-repls.el to your init.el via e.g. (load-file "~/.emacs.d/clojure-quick-repls.el"). 

Make sure you have [piggieback](https://github.com/cemerick/piggieback) as a dependency in project.clj.

Also requires [cider](https://github.com/clojure-emacs/cider). 


Usage
-----

Open any clojure file in the project that you wish to have clojure/clojurescript repls for, then 

M-x repls-connect

Wait for about 1 minute and check *Messages* buffer for "Clj connection buffer: .\* Cljs connection buffer .\*". 

Do not attempt to execute repls-connect twice, if you need to restart run cider-quit and then repls-connect. 

You can now connect the browser and execute any cider command in either a clj/cljs buffer and the forms will be routed to the correct connection.

Known issues
------------

Currently cider-switch-to-relevant-repl-buffer (C-c C-z) does not open the correct repl buffer. I have not had a time to check why this is, but I suspect that it does not use nrepl-current-session and therefore the nrepl-switch advice is not executed.


License
-------

Distributed under the GNU General Public License, version 3
