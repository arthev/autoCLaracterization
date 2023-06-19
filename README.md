# autoCLaracterization

## Introduction

Automatic characterization tests in Common Lisp (targeting 5am). Or almost automatic, at any rate.

It's good to have tests as a sanity check that your changes to a program don't affect more than anticipated. In the absence of normative/prescriptive tests, characterization tests (supposedly) work decently well - at least they're descriptive tests and can notify that changes in unexpected parts have happened etc. But writing a whole bunch of characterization tests by hand's tedious.

Hence autoCLaracterization.

The idea is to minimally instrument existing code through specifying an equality comparison, punt function calls through your system, and then call `write-characterization-tests` once you're satisfied with your collected characterizations. Pretty up the resultant test forms by e.g. shuffling them into the right test suite. There you go, characterization tests, done.

## Design

The approach taken is to have a few macros available - `defrecfun`, `defrecgeneric` and `defrecmethod` - to generate 'recording' versions of current function (in the broad sense) definitions. These work as drop-in replacements of defun etc., so minimal code changes are required to use them. This was the original idea.

However, it might be preferable to leave the source code unchanged and instead keep the instrumentation separate. Hence a side-effecty `register-recorder` macro's provided so that instrumentation forms can be kept in e.g. a separate file. This requires registering a set of recorders, switching on characterizing, and then re-loading the source code since it automatically checks for whether to 'upgrade' e.g. encountered `defun`s to `defrecfun`s through use of `*macroexpand-hook*`. Examples of both the explicit macro using approach and the implicit `register-recorder` approach are below in the example section.

AutoCLaracterization only characterizes the functional interface of functions. That is, side effects are not considered. Because arbitrary setup might be done in a method combination, the approach for generic functions is to characterize the functional interface of the generic function, not of the individual methods - setup in other methods might mean calling a method with same arguments as it was called with inside the method combination might yield different return values, after all.

There's an option on whether to characterize signalled conditions (and what conditions to characterize for): `*record-conditions*. That way, it's easy to e.g. capture that certain input generates errors.

Dealing with recursion also presents a design choice: characterize the outer recorder only, characterize the entry call into any recursive function, or characterize the whole set of subcalls too? This can be set on a per-instrumented-function basis, but there's also `*default-recorder-strategy*`. A full description is below.

How to apply the specified test by default's also a design point. A `:test` specified predicate (whether through `register-recorder` or e.g. `defrecfun`) generates a test case for whether _all the return values_ satisfies the predicate. If something more custom's required, `:custom-test` instead generates a test case for whether the multiple-value-lists satisfy the predicate and hence offers more control.

## Caveats

* The whole approach is to automatically characterize the functional behaviour of the
functions. Side effects do **not** get characterized by autoCLaracterization. At all.
* Relies on a `serialize-object` generic to generate code forms corresponding to the data objects the functions get called on. The initial set of such methods were copied from [SPICLUM](https://github.com/arthev/spiclum). Expect to supply more methods (and possibly change the existing ones) unless your data needs happen to coincide perfectly with that project. Some data objects (i.e. lambdas) might not be easily serializable.
* Very lightly tested
* Contains a `*recorder-lock*`, so probably lightly hammers performance. Shouldn't matter, given that generating characterization tests is a dev task and not something to do in prod.
* `defrecgeneric` and `defrecmethod` rely on a method-combination, so won't play nice with generic functions that already specify some non-standard method-combination.
* An alternate implementation approach might've been to use advice functionality instead of the macro-heavy approach.

## Example

```common-lisp
AUTOCLARACTERIZATION> *macroexpand-hook*
FUNCALL
AUTOCLARACTERIZATION> (register-recorder some-fn :test #'=)
(SOME-FN :TEST #'= :STRATEGY :ENTRY-ONLY)
AUTOCLARACTERIZATION> (defun move-along-invoker (fn)
                        (handler-bind ((error
                                         (lambda (e)
                                           (invoke-restart 'move-along))))
                          (funcall fn)))
MOVE-ALONG-INVOKER
AUTOCLARACTERIZATION> *characterization-tests*
#<HASH-TABLE :TEST EQL :COUNT 0 {1002562433}>
AUTOCLARACTERIZATION> (turn-on-characterizing)
AUTOCLARACTERIZATION-MACROEXPAND-HOOK
AUTOCLARACTERIZATION> (defun some-fn ()
                        (restart-case
                            (+ 2 'a)
                          (move-along ()
                            0)))
SOME-FN
AUTOCLARACTERIZATION> (move-along-invoker #'some-fn)
0
AUTOCLARACTERIZATION> (reverse (gethash 'some-fn *characterization-tests*))
((IT.BESE.FIVEAM:SIGNALS SIMPLE-TYPE-ERROR
   (SOME-FN))
 (IT.BESE.FIVEAM:IS (EVERY #'= (LIST 0) (MULTIPLE-VALUE-LIST (SOME-FN)))))
AUTOCLARACTERIZATION> ;; now, obviously the latter test's bound to fail in this particular case, since the invocation (some-fn) won't return 0 without the move-along invoking.
; No value
AUTOCLARACTERIZATION> *macroexpand-hook*
AUTOCLARACTERIZATION-MACROEXPAND-HOOK
AUTOCLARACTERIZATION> (turn-off-characterizing)
FUNCALL
AUTOCLARACTERIZATION> *macroexpand-hook*
FUNCALL
AUTOCLARACTERIZATION> ;; We can also use the macros directly:
; No value
AUTOCLARACTERIZATION> (defrecgeneric some-other-fn (a b &rest r))
#<STANDARD-GENERIC-FUNCTION AUTOCLARACTERIZATION::SOME-OTHER-FN (1)>
AUTOCLARACTERIZATION> (defmethod some-other-fn (a b &rest r)
                        (list :a a :b b :r r))
#<STANDARD-METHOD AUTOCLARACTERIZATION::SOME-OTHER-FN (T T) {1004F7FF03}>
AUTOCLARACTERIZATION> (some-other-fn 1 2 3 4 5)
(:A 1 :B 2 :R (3 4 5))
AUTOCLARACTERIZATION> (defrecgeneric (some-other-fn :test #'equal) (a b &rest r))
#<STANDARD-GENERIC-FUNCTION AUTOCLARACTERIZATION::SOME-OTHER-FN (2)>
AUTOCLARACTERIZATION> (some-other-fn 1 2 3 4 5)
(:A 1 :B 2 :R (3 4 5))
AUTOCLARACTERIZATION> (reverse (gethash 'some-other-fn *characterization-tests*))
((IT.BESE.FIVEAM:IS
  (EVERY #'EQL (LIST (LIST :A 1 :B 2 :R (LIST 3 4 5)))
         (MULTIPLE-VALUE-LIST (SOME-OTHER-FN 1 2 3 4 5))))
 (IT.BESE.FIVEAM:IS
  (EVERY #'EQUAL (LIST (LIST :A 1 :B 2 :R (LIST 3 4 5)))
         (MULTIPLE-VALUE-LIST (SOME-OTHER-FN 1 2 3 4 5)))))
AUTOCLARACTERIZATION> ;; whoops, forgot to specify a good predicate first time around.
; No value
AUTOCLARACTERIZATION> ;; let's pretend our currently generated tests are good.
; No value
AUTOCLARACTERIZATION> (write-characterization-tests *characterization-test-output-path*)
T
```
which appends
```common-lisp
(IT.BESE.FIVEAM:TEST :|SOME-FN:3896154869|
  (IT.BESE.FIVEAM:SIGNALS SIMPLE-TYPE-ERROR
    (SOME-FN))
  (IT.BESE.FIVEAM:IS (EVERY #'= (LIST 0) (MULTIPLE-VALUE-LIST (SOME-FN)))))

(IT.BESE.FIVEAM:TEST :|SOME-OTHER-FN:3896154869|
  (IT.BESE.FIVEAM:IS
   (EVERY #'EQL (LIST (LIST :A 1 :B 2 :R (LIST 3 4 5)))
          (MULTIPLE-VALUE-LIST (SOME-OTHER-FN 1 2 3 4 5))))
  (IT.BESE.FIVEAM:IS
   (EVERY #'EQUAL (LIST (LIST :A 1 :B 2 :R (LIST 3 4 5)))
          (MULTIPLE-VALUE-LIST (SOME-OTHER-FN 1 2 3 4 5)))))
```
to the file specified in `*characterization-test-output-path*`. Copy over into appropriate file/test suite.

## Documentation

##### macro: defrecfun (name-and-options lambda-list &body body)

Macro to set up automatic capture of characterization tests for arbitrary functions.

NAME-AND-OPTIONS can be the function name, or a list supplying `:test` xor `:custom-test`, and/or `:strategy`. `:custom-test` takes precedence above `:test`. `:test` designates a predicate that gets `every`d across the multiple return values of the function being defined. `:custom-test` designates an arbitrary predicate that takes two value lists (and hence e.g. allows testing only the first member of the lists). By default, `:custom-test` is nil/unsupplied and `:test` is `#'eql`. `:strategy` is one of (:all :outer-only :entry-only) where :all records all invocations (in case of recursion, whether direct or indirect), :outer-only only records the outermost recorder invocation at any point (so that other recorder invocations inside the outermost one, whether same function or not, get ignored), and :entry-only records the outermost invocation of all invoked recorders. By default, `:strategy` is `*default-recorder-strategy*` (which by default is `:entry-only`).

LAMBDA-LIST and BODY have the same semantics as in DEFUN. (Though if the LAMBDA-LIST contains optional or keyword parameters, those get normalized to contain suppliedp vars using gensyms.)

##### macro: defrecgeneric (name-and-options lambda-list &body options)

Macro to set up automatic capture of characterization tests for arbitrary functions.

NAME-AND-OPTIONS are as for DEFRECFUN.

OPTIONS have the same semantics as in DEFGENERIC save that `:method-combination` is not available (because DEFRECGENERIC fills it automatically).

LAMBDA-LIST has the same semantics as for DEFGENERIC, though in the presence of &keys, a &rest parameter might be inserted if one doesn't exist already.

##### macro: defrecmethod (name-and-options &rest args)

Macro to set up automatic capture of characterization tests for arbitrary functions. In case there's no DEFGENERIC form.

NAME-AND-OPTIONS are as for DEFRECFUN.

Simply creates a suitable DEFRECGENERIC form in a progn and off-loads essentially all work to that one.

##### macro: register-recorder (name &key test custom-test strategy)

Instructs the autoCLaracterization system to instrument (generic) function NAME.

Does NOT preserve lexical environment, so e.g. CUSTOM-TEST can't closure.

TEST, CUSTOM-TEST and STRATEGY are as described for DEFRECFUN.

It's a macro to save on quoting (and hence minimise mistakes) - `'#'equal` looks awkward.

##### function: turn-on-characterizing ()

Switches out `*macroexpand-hook*` so that by recompiling the system code the functions that have been registered as those to turn into recorders (using REGISTER-RECORDER) get appropriately transformed.

##### function: turn-off-characterizing ()

Switches out `*macroexpand-hook*` to its previous value (when TURN-ON-CHARACTERIZING was called) so that by recompiling the system code, functions return to non-autoCLaracterization versions.

##### function: write-characterization-tests (&optional output-path)

Writes collected characterization tests to OUTPUT-PATH (and then clears them out from memory). Writes by appending. OUTPUT-PATH is a pathname designator, by default `*characterization-test-output-path*`.

##### var: \*characterization-tests\*

The table containing currently registered characterization tests (the ones that will get written to file once you invoke WRITE-CHARACTERIZATION-TESTS). Not meant to be mutated by the programmer, but offered for inspection to ensure things have been set up correctly and started recording etc.

##### var: \*record-conditions\*

The typeof conditions to record, e.g. `error`, `warning`, `condition`. Or a list of such type specifiers. Set to nil if you don't want to record conditions that pass thrugh the recorder.

Default value: `error`.

##### var: \*default-recorder-strategy\*

The default `:strategy` to supply to the explicit or implicit `defrecfun`, `defrecgeneric` and `defrecmethod` forms. See the explanation for DEFRECFUN for an explanation of the options.

Default value: `:entry-only`

##### var: \*characterization-test-output-path\*

The default output path for use with WRITE-CHARACTERIZATION-TESTS.

Default value: Something that's only sensible to the library author.

## Portability
Should be supported on at least:

* SBCL 2.2.4+

There's zero usage of anything explicitly implementation-specific, though.

## Installation

From source. E.g. clone the repo and place the folder in your local quicklisp search path.

## License

MIT License.