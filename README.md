em-lisp
=======

An implementation of expectation maximization and k-means clustering in Common Lisp.

This was implemented as part of a Georgia Tech CS 6601 homework assignment, and is my first real Lisp program.

Please feel free to use as you need, and let me know if you see any bugs or issues.

Dependencies:

A Common Lisp compiler or interpreter (tested on SBCL and CLISP).
Quicklisp loaded in your compiler (used to load Imago).

To run all experiments for HW2, launch your lisp environment and call:
    (load "hw2.lisp")
    (hw2-main)

Each part of the homework can be run individually by executing one of the following statements:

    ; Part 2
    (run-part-2 3 1e-6 10 20)
    (run-part-2 5 1e-6  7 20)
    (run-part-2 8 1e-6  5 20)

    ; Part 3
    (run-part-3-rand)
    (run-part-3-km)

    ; Part 5
    (run-part-2 1 1e-6 10 20)
    (run-part-2 2 1e-6 10 20)
    (run-part-2 4 1e-6  8 20)
    (run-part-2 6 1e-6  7 20)
    (run-part-2 7 1e-6  6 20)

