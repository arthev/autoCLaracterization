(in-package :autoCLaracterization)

(defvar *characterization-test-output-path*
  "C:\\Users\\Arthur\\common-lisp\\recordables-output.lisp"
  "A pathname designator for where to write the characterization tests.")

(defvar *default-recorder-strategy* :entry-only)

(defvar *record-conditions* 'error
  "The type of conditions to record. E.g. ERROR, WARNING, CONDITION. Or a list of such.

If you don't want to record conditions that 'pass through' the recorder,
then set this to nil.")
