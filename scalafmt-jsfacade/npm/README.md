# scalafmt

A JS API for [`scalafmt`](https://github.com/scalameta/scalafmt).

It exposes a single function:

- `format(code: string): string`: formats the given code

Example:

```js
const { format } = require('scalafmt');

const input = `
object Main {
  case class Foo(
    a: String,
      b: Boolean,
        c: Int
  )
val x =  Foo("a", true
  , 2)
}`;
const output = format(input);
console.log(output);

// Output:

// object Main {
//   case class Foo(
//       a: String,
//       b: Boolean,
//       c: Int
//   )
//   val x = Foo("a", true, 2)
// }
```

