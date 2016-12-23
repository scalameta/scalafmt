# scalafmt tests

The file extension determines how the code should be parsed

* `*.stat` parses as a statement (most common)
* `*.case` parses as a case
* `*.source` parses as a full compilation unit


Here is an example test suite in a file `test/resources/Foo/foo.stat`:
```
maxColumn = 40 # configuration is defined at the top of the file
<<< This is the test name
val x = "This can be an arbirary statement"
>>>
val x =
  "This can be an arbirary statement"
```

Prefix a test name with `ONLY` to only run that single test.
```
<<< ONLY only run this test, useful when testing a single case.
  "This statement is the original code"
>>>
"This statement is the expected formatting output. " +
"Obviously, this test will fail."
```

Prefix a test name with `SKIP` to skip that single test.
```
<<< SKIP do not run me, useful to temporarily quiet failing tests.
  "I test new features that don't work yet."
>>>
"I test new features that don't work yet."
```

Put `ONLY` at the top of the file to only run tests in this file.
```
ONLY maxColumn = 40
<<< test 1
foo (1)
>>>
foo(1)
<<< test 2
bar (2)
>>>
bar(2)
```

If you are unsure which directory to put your test in, put it in `test/resources/test`.
