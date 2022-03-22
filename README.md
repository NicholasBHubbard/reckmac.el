# -*- mode:org;mode:auto-fill;fill-column:79 -*-
#+title: reckmac.el 
#+author: Nicholas Hubbard

Reckmac stands for *recursive keyboard macros*.

By default in emacs you cannot call another keyboard macro while you are
currently defining a keyboard macro unless you first give that other macro a
name.

Reckmac solves this problem.

* Functions

Here is documentation for all the interactive functions that reckmac provides.

*** reckmac-start-or-end-macro

If not currently recording a macro then start recording a macro. If currently
recording a macro then finish recording.

Before starting to record a macro you will be prompted to pick what register you
want to record into.

*** reckmac-execute-macro

Execute a reckmac macro. You will be prompted for the register of the macro you
want to execute. 

If this function is called while currently recording a macro then the macro you
execute will be added to the macro you are recording.

*** reckmac-execute-last-macro

Execute the most recently recorded macro. This is just a convenience wrapper
around =reckmac-execute-macro=.

*** reckmac-name-macro

Name a reckmac macro. You will be prompted for the register of the macro you
to name. Internally this function uses =name-last-kbd-macro= to give the macro
a name.

*** reckmac-name-last-macro

Name the most recently recorded macro. This is just a convenience wrapper
around =reckmac-name-macro=.

* Tip

You will probably want to bind keys to =reckmac-start-or-end-macro=,
=reckmac-execute-macro=, and =reckmac-execute-last-macro=.

* Installation

*** MELPA

This library is not on MELPA yet.

*** Quelpa

#+BEGIN_SRC
(use-package reckmac
  :quelpa (reckmac :fetcher github :repo "NicholasBHubbard/reckmac.el"))
#+END_SRC

*** Straight

#+BEGIN_SRC 
(use-package reckmac
  :straight (reckmac :type git :host github :repo "NicholasBHubbard/reckmac.el"))
#+END_SRC

*** Manual

Put =reckmac.el= in your =load-path=, then:

#+BEGIN_SRC
(require 'reckmac)
#+END_SRC

* License

MIT
