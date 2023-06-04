# autoCLaracterization

Automatic characterization tests in Common Lisp (targeting 5am). Or almost automatic, at any rate.

It's good to have tests as a sanity check that your changes to a program don't affect more than anticipated. In the absence of normative/prescriptive tests, characterization tests (supposedly) work decently well - at least they're descriptive tests and can notify that changes in unexpected parts have happened etc. But writing a whole bunch of characterization tests by hand's tedious.

Hence autoCLaracterization.

The idea is you change the function definitions you're interested in putting under characterization testing into using `defrecfun` instead of `defun`, supply the appropriate `test`, punt function calls through your system, and then call `write-chracterization-tests` once you're satisfied with your collected characterizations. Pretty up the resultant test forms by e.g. shuffling them into the right test suite. There you go, characterization tests, done.

### Caveats:
* The whole approach is to automatically characterize the functional behaviour of the functions. Side effects do **not** get characterized by autoCLaracterization.
* Relies on `serialize-object` to generate code forms corresponding to the data objects the functions get called on. The initial set of such methods were copied from [SPICLUM](https://github.com/arthev/spiclum). Expect to supply more methods (and possibly change the existing ones) unless your data needs happen to coincide perfectly with that project. Some data objects might not be easily serializable.
* Currently only supports (i.e. only contains a recording version of) `defun`. Want to autoCLaracterize a method? Out of luck.
* Very lightly tested
* Contains a `*recorder-lock*`, so probably lightly hammers performance. Shouldn't matter, given that generating characterization tests is a dev task and not something to do in prod.

### Todo:
* Figure out (and implement (and test)) whether something similar can be done for methods or generic functions too.

### Example:

```common-lisp
AUTOCLARACTERIZATION> (defrecfun fib (n)
                        (if (<= n 1)
                            n
                            (+ (fib (- n 1))
                               (fib (- n 2)))))
FIB
AUTOCLARACTERIZATION> (fib 6)
8
AUTOCLARACTERIZATION> (fib 15)
610
AUTOCLARACTERIZATION> (fib 0)
0
AUTOCLARACTERIZATION> (defrecfun (fib :test #'=) (n)
                        (if (<= n 1)
                            n
                            (+ (fib (- n 1))
                               (fib (- n 2)))))
FIB
AUTOCLARACTERIZATION> (fib 12)
144
AUTOCLARACTERIZATION> (fib 4)
3
AUTOCLARACTERIZATION> (reverse (gethash 'fib *characterization-tests*))
((IT.BESE.FIVEAM:IS (EVERY #'EQL (LIST 8) (MULTIPLE-VALUE-LIST (FIB 6))))
 (IT.BESE.FIVEAM:IS (EVERY #'EQL (LIST 610) (MULTIPLE-VALUE-LIST (FIB 15))))
 (IT.BESE.FIVEAM:IS (EVERY #'EQL (LIST 0) (MULTIPLE-VALUE-LIST (FIB 0))))
 (IT.BESE.FIVEAM:IS (EVERY #'= (LIST 144) (MULTIPLE-VALUE-LIST (FIB 12))))
 (IT.BESE.FIVEAM:IS (EVERY #'= (LIST 3) (MULTIPLE-VALUE-LIST (FIB 4)))))
AUTOCLARACTERIZATION> (write-characterization-tests)
T
AUTOCLARACTERIZATION> *characterization-tests*
#<HASH-TABLE :TEST EQL :COUNT 0 {100257A803}>
```
which writes
```common-lisp
(IT.BESE.FIVEAM:TEST :|FIB:3894870074|
  (IT.BESE.FIVEAM:IS (EVERY #'EQL (LIST 8) (MULTIPLE-VALUE-LIST (FIB 6))))
  (IT.BESE.FIVEAM:IS (EVERY #'EQL (LIST 610) (MULTIPLE-VALUE-LIST (FIB 15))))
  (IT.BESE.FIVEAM:IS (EVERY #'EQL (LIST 0) (MULTIPLE-VALUE-LIST (FIB 0))))
  (IT.BESE.FIVEAM:IS (EVERY #'= (LIST 144) (MULTIPLE-VALUE-LIST (FIB 12))))
  (IT.BESE.FIVEAM:IS (EVERY #'= (LIST 3) (MULTIPLE-VALUE-LIST (FIB 4)))))
```
to the file I had specified in `*characterization-test-output-path*`.