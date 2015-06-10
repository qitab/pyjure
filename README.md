# Pyjure

Pyjure is a pure functional language with a syntax and a semantics based on that of Python.

This is an experimental implementation of pyjure, written in Clojure.


## Usage

FIXME

## Differences from Python3

### General principles:
* The Pyjure implementation isn't working yet. WORK IN PROGRESS. HELP WELCOME.
* The general rule is that pyjure is pure-functional whereas Python has side-effects.
* If you know Python, a good heuristic is that Pyjure will do "the right thing"
  for the same kind of computation in absence of side-effect, if meaningful,
  or will disallow the computation if it isn't meaningful without a side-effect.
* Pyjure has exceptions, as an exception to not otherwise having side-effects,
  if you consider exceptions as side-effects (as you should).
* Pyjure is not compatible enough with Python for Python libraries to run unmodified, if at all.
* Pyjure does not have Python classes.

### More details:
* Binding a variable is actually shadowing. No side-effect involved.
* It is forbidden for a variable that is shadowed to escape to inner scopes
  (Just like in Java, only "final" variables can be inherited.)
* It's a syntax error to bind a variable after it's been defined as a function.
* lists, tuples, sets or maps are all immutable.
* += doesn't side-effect the original sequence, set or map.
* There are no classes. There are struct-s, that are kind of blessed maps from string to Object
  that can be accessed with field accessor syntax.
* You can inter-add lists, tuples, sets or maps, returning an object of the same type
  as the first operand. Adding None left or right is the identity function.
* It's a syntax error to declare a variable nonlocal or global after it's been used in any way
  (either bound or referenced).

### Python features not planned:
* Python classes.
* Python reflection protocol.

### Python features planned:
* Python generators (after we implement delimited control).

### Non-Python features planned:
* Macros.
* delimited control (at which time we can implement python generators).

## License

Copyright © 2014 Google, Inc

Distributed under the Apache License version 2.0.

Disclaimer: although written by a Google employee, this software is not supported by Google.
