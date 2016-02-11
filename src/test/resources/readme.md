# scalafmt tests

The tests are organised into directories by formatting style.

* `standard` is basically the first style supported by scalafmt. It is mostly
inspired by the
[Scala.js](https://github.com/scala-js/scala-js/blob/master/CODINGSTYLE.md)
coding style.
* `unit` is the "unit testing style", same as standard except with
  a max column limit 40.
  
**TIP.** Prefix the name of a test with

* `ONLY` to only run that test (including other tests marked as ONLY).
* `SKIP` to ignore that test.

Do the same to the first line in the file to apply the rule to all tests
in that file.
