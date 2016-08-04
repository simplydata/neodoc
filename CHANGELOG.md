Toughts:

* should option-arguments be able to vary in their requiredness across groups?
  if so, this would have an impact on the fatal error handling.

# Changelog

Please note that all these tags mark releases that are available on npm with the
respective version number - unless otherwise noted.

## [0.9.2] - 2016-07-26
## [0.9.1] - 2016-07-26
## [0.9.0] - 2016-07-23
## [0.8.0] - 2016-07-17
## [0.7.0] - 2016-07-03
## [0.6.1] - 2016-00-24
## [0.6.0] - 2016-06-23
## [0.5.0] - 2016-06-17
## [0.4.0] - 2016-06-07
## [0.3.0] - 2016-05-25
## [0.2.1] - 2016-05-11

## [0.2.0] - 2016-05-10

### Changes

* Populate values at parser level

    *** BREAKING CHANGE ***

    This is a fairly large re-think of how argument values are fetched from
    the their various input sources. Before, values would be loaded during
    a step after parsing, where the matched usage branch would be completely
    unrolled, unified and then populated. This had the disadvantage that one
    could not tell which of the mutually exclusive branches produced a
    result, since one value might arrive from ARGV and another one from the
    [default: ...] tag, or the environment, etc. (See #8)

    Now, values are loaded at parse-time, removing these issues. Further,
    "empty" values are elided from the output, representing the user *not
    trying* to match these keys (as opposed to choosing to set them
    explicitly). For example, the user might pass the value 'false' to an
    option and that value will be retained. If the option however yields
    false because there was no user input and because 'false' is its' empty
    fall-back, the value will be omitted. The same goes for matching
    repeating elements into an array. At least one element needs to be
    matched before a value will be yielded.

    The diff of 'testcases.docopt' should highlight these changes better
    than any amount of explanatory text could.

    This patch also removes the "ScoredResult" type as it is no longer
    needed. Scoring is based on the "Origin" of a value now. This somewhat
    simplifies the code.

    Lastly, there's now a recursive step inside `genExhaustiveParser` that
    recursively evaluates missing elements until all options have been
    exhausted or failed.

## [0.1.0] - 2016-05-05

:party: This marks the first minor release for neodoc. Things are stabilising.

### Changes

* Fail fatally if an option's required option-argument is missing from the
  input. The parser won't even attempt to try any other branches &mdash; it will
  fail immediately. The same applies for providing an argument to an option that
  does not take an argument.

### Internals

* Run the compat spec parser on a trampoline to avoid stack overflows

## [0.0.15] - 2016-05-03

### Changes

* Stricter lexing of options:
    * Disallow spaces between `-f` and `[=OPTARG]` (only allow `-f[=OPTARG]`
      without any spaces).
    * Disallow '-' after '--' and '-' to make them distinct
    * Disallow '-' after any option
    * Disallow '[' and '(' right after any option

### Fixes

* Fix number value parser for user input
* Short options - parse option-argument values if the option-argument was
  assigned explicitly

## [0.0.14] - 2016-05-02

### Changes

* Do not interpret quoted options, e.g.: `prog '-x = foo'` would currently yield
  `-x=foo`. As of this version, it will simply yield the string `-x = foo`.
  Explanation: Since the shell does the argument splitting for us, we receive as
  an argument the string `"-x = foo"` as opposed to three arguments `-x`, `=`
  and `foo` (which would, rightfully, result in `-x="=" foo`).
* Enforce EOF at end of usage section
* Ensure `--[no-tags]` won't parse for now since flag negation behavior is not
  yet implemented and the semantics will change once it is.
* Improve overall error reporting

## [0.0.13] - 2016-05-01

### Fixes

* Allow termination via `options-first` inside a group. Parsing will simply
  halt, no matter how deep in a group once a positional / command argument is
  encountered (#21)
* Fix ambiguous fallbacks not resolved across groups (#22)

## [0.0.12] - 2016-04-27

### Fixes

* Fix repeating option not being required at least once.

### Changes

* Values parsed on argv that have commas in them are no longer parsed into a
  list of values. This behavior was too specific, since who decides what the
  delimiter should be? Developers are advised to either a) use repeating
  arguments or b) split the value themselves after parsing. This change does
  not, however, have an impact on the parsing of the `[default]` tag - those
  values will still be parsed into lists since it's value is in the developer's
  control._This could be revised, if necessary_.
* Select fallbacks in ambiguous parses based on a score, where environment
  values based fallbacks rank higher than provided `[default]` values, etc.

## [0.0.11] - 2016-04-26

### New features

* Implement `smart-opts`. This is a new behavior that allows explicitly
  (as opposed to loosely) binding an option-argument to an option, just by
  placing it into a singleton group. For example: `usage: foo [-o FILE]`. In
  this example, given that `options.smartOpts` is `true`, no additional option
  description section is required in order to bind `FILE` to `-o`.

### Changes

* Lift the constraint on positionals / commands qualifying as candidates for
  `options-first`. Before, it was required that this argument be indicated as
  optional. This requirement has now been lifted.

## [0.0.10] - 2016-04-25

### New features

* Implement proper `[options]` expansion. This means that `[options]` is now
  aware of it's surroundings and won't expand options already present.
  This allows to intermix `[options]` with explicitly mentioned options.

## [0.0.9] - 2016-04-16

### New features

* It is now possible to indicate the repeatability of an option in the option
  description section.

### Changes

* In case of ambigious parses across group branches, select the left-most match.

## [0.0.8] - 2016-04-18

### New features

* Add the `options.dontExit` option to `neodoc.run` (defaults to `false`).
  Turning this option on will cause neodoc to throw an error, rather than
  quitting the program.

### Changes

* Error messages now longer show squiggly error lines. The behavior was neither
  aesthetically pleasing nor very helpful as a tool. Render simple error
  messages instead
* Improve error message for mutually exclusive branches in a group
* Add examples to the codebase
* Restrict angle names to not contain a opening angle (`<`). This means that the
  following name is now illegal: `<<foo>`. There is no technical reason to this
  limitation and was introduced merely to avoid subtle parse errors. _This could
  be revised, if necessary_.
* Disallow specifying an option twice in the options section
* Disallow associating an option-argument with a non-trailing stacked option
* Strictly check the adjacent arguments for the correct argument

## [0.0.7] - 2016-04-13

### Changes

* Relax the parsing of "free" groups &mdash; remove the restriction that in
  order for a group to be considered "free" it must be a singleton group.
  Now any group whose branches are all "free" is considered "free" itself.

### Internals

* Add more tests - increase scenario coverage

## [0.0.6] - 2016-04-11

Only internal changes and fix ups. Notably, simplify the lexer module by
removing all references to the `word` lexer which has been previously
decomissioned. Also avoid parsing the `Garbage` token when lexing the usage
section &mdash; let it fail at the lexing stage.

## [0.0.5] - 2016-04-08

### New features

* Add 'options-first' feature
* Add support for optional arguments

### Internals

* Add more tests - increase scenario coverage
* Overhaul the parser generator

## [0.0.4] - 2016-04-05

### Changes

* Print only usage section upon error as opposed to complete help text

## [0.0.3] - 2016-04-03

### Changes

* Lift the restriction on command and positional names. Names can now have
  dashes, underscores and dots

## [0.0.2] - 2016-04-02

:party: This marks the first generally available release of neodoc

[0.9.2]: https://github.com/felixschl/neodoc/compare/v0.9.1...v0.9.2
[0.9.1]: https://github.com/felixschl/neodoc/compare/v0.9.0...v0.9.1
[0.9.0]: https://github.com/felixschl/neodoc/compare/v0.8.0...v0.9.0
[0.8.0]: https://github.com/felixschl/neodoc/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/felixschl/neodoc/compare/v0.6.1...v0.7.0
[0.6.1]: https://github.com/felixschl/neodoc/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/felixschl/neodoc/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/felixschl/neodoc/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/felixschl/neodoc/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/felixschl/neodoc/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/felixschl/neodoc/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/felixschl/neodoc/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/felixschl/neodoc/compare/v0.0.15...v0.1.0
[0.0.15]: https://github.com/felixschl/neodoc/compare/v0.0.14...v0.0.15
[0.0.14]: https://github.com/felixschl/neodoc/compare/v0.0.13...v0.0.14
[0.0.13]: https://github.com/felixschl/neodoc/compare/v0.0.12...v0.0.13
[0.0.12]: https://github.com/felixschl/neodoc/compare/v0.0.11...v0.0.12
[0.0.11]: https://github.com/felixschl/neodoc/compare/v0.0.10...v0.0.11
[0.0.10]: https://github.com/felixschl/neodoc/compare/v0.0.9...v0.0.10
[0.0.9]: https://github.com/felixschl/neodoc/compare/v0.0.8...v0.0.9
[0.0.8]: https://github.com/felixschl/neodoc/compare/v0.0.7...v0.0.8
[0.0.7]: https://github.com/felixschl/neodoc/compare/v0.0.6...v0.0.7
[0.0.6]: https://github.com/felixschl/neodoc/compare/v0.0.5...v0.0.6
[0.0.5]: https://github.com/felixschl/neodoc/compare/v0.0.4...v0.0.5
[0.0.4]: https://github.com/felixschl/neodoc/compare/v0.0.3...v0.0.4
[0.0.3]: https://github.com/felixschl/neodoc/compare/v0.0.2...v0.0.3
[0.0.2]: https://github.com/felixSchl/neodoc/releases/tag/v0.0.2
