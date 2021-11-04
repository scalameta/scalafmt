# scalafmt tests

The file extension determines how the code should be parsed

* `*.stat` parses as a statement (most common)
* `*.case` parses as a case
* `*.source` parses as a full compilation unit

Each file consists of a few lines of file-level configuration overrides, followed by
one or more tests. Each test uses the following format:

```
<<< {test name}
{provided}
>>> [optional filename]
{expected}
```

`[optional filename]` can be provided if we'd like to pass it to the formatter (presumably
in order to change its behaviour; for instance, `scalafmt` works a bit differently for `.sbt`
and `.sc` files).

And `{provided}` can just contain the code to be formatted, or optionally start with a custom
test-level configuration override separated from the code by `===`:

```
key1 = val1
key2 = val2
===
code
```

Here is an example test suite in a file `test/resources/Foo/foo.stat`:
```
maxColumn = 40 # configuration is defined at the top of the file
<<< This is the test name
val x = "This can be an arbitrary statement"
>>>
val x =
  "This can be an arbitrary statement"
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

In case of an error a message will be shown that will point to the exact tests that failed:

```
==> X org.scalafmt.FormatTests.binPack/LiteralList.stat:3: basic                                     |  1.627s munit.ComparisonFailException: /home/tgodzik/Documents/scalafmt/scalafmt-tests/src/test/resources/binPack/LiteralList.stat:3
```

The file link itself should be clickable (via ctrl/cmd + click) when used from VS Code terminal or within Intellij, though in the latter case currently link will be created only if the Android plugin is active as the functionality is tied to that plugin. In the future that functionality might be migrated to the core Intellij code.

