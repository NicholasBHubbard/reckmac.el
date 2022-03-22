# -*- mode:org;mode:auto-fill;fill-column:79 -*-
#+title: reckmac.el 
#+author: Nicholas Hubbard

Reckmac stands for *recursive keyboard macros*.

By default in emacs you cannot call another keyboard macro while you are
currently defining a keyboard macro unless you first give that other macro a
name.

Reckmac solves this problem.

* Concepts



* Usage

The function =reckmac-start-or-end-macro= is used to start recording or finish
recording a macro. If you are not currently recording a
