# scalafmt

A JS API for [`scalafmt`](https://github.com/scalameta/scalafmt).

It exposes a single function:

- `format(code: string, configString?: string, ranges?: Range[]): string`: formats the given code

where:

- `code` is the entire source code you want to format
- `configString` [optional] is the Scalafmt configuration (as documented [here](http://scalameta.org/scalafmt/#Configuration)). If not provided, the default one is used.
- `ranges` [optional] is an array of ranges. If not provided, the entire document is formatted. Each range is an object with two properties:
  - `start`: the start line of the range
  - `end`: the end line of the range (not inclusive)

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

