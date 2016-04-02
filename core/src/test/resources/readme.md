# scalafmt tests

The tests are organised into directories by formatting style.

* `default` is basically the first style supported by scalafmt. It is mostly
  I style I personally like.
* `unit` is the "unit testing style", same as default except with
  a max column limit 40.

The file extension determines how the code should be parsed

* `*.stat` parses as a statement
* `*.case` parses as a case
* `*.source` parses as a full compilation unit

Example `foo.stat` in `unit` directory:
```
40 columns                              |
<<< basic test, this is the test name
val x = "This can be an arbirary statement"
>>>
val x =
  "This can be an arbirary statement"
<<< ONLY only run this test, useful when testing a single case.
  "This statement is the original code"
>>>
"This statement is the expected formatting output. " +
"Obviously, this test will fail."
<<< SKIP do not run me, useful to temporarily quiet failing tests.
  "I test new features that don't work yet."
>>>
"I test new features that don't work yet."
```
