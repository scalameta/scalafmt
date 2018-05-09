---
id: faq
title: FAQ / Troubleshooting
---

## Why not Scalariform?

[Scalariform](http://scala-ide.org/scalariform/) does an excellent job of
tidying up common formatting errors. However,

* Scalariform does not have a `maxColumn` setting, which I personally like and
  is present in many popular coding styles.

* Scalariform preserves most line breaking decisions, leaving it up to you (or
  even worse, your colleagues) to choose a formatting layout. Scalafmt takes
  liberty to add/remove newlines, making your entire codebase look consistent.

Finally, scalafmt is my Master's thesis project. I thought it would be a fun
challenge to write a code formatter :)

## Why is scalafmt so slow?

My benchmarks show that scalafmt is for most common cases around 4-6x slower
than scalariform (btw, scalariform is already impressively fast). This means
that formatting your average 1.000 LOC file on modern hardware will take around
200ms, which should still feel close enough to instant.

The main feature that makes scalafmt slower than scalariform is the column-width
limit. To figure the "best" way to break a long line, Scalafmt may try thousands
of different formatting solutions.

I am sure that scalafmt could benefit greatly from micro optimizations. Any help
here is appreciated.

## Code formatters create unnecessary diffs!

That's not a question, but I agree that code formatters like scalafmt do
sometimes increase the size of diffs in code reviews. I still believe it's worth
it, considering

1.  Proper formatting
    [helps you catch bugs](https://twitter.com/extempore2/status/717716747181096960)!

2.  You can enable non-whitespace diffs during code review. For Github, add
    `?w=1` to the URL to ignore whitespace changes.

3.  `git blame` has a `-w` flag to ignore whitespace changes so you can still
    blame your colleagues for their crappy code.

4.  code is read waaay more often outside of code reviews, for example when you
    are actually coding.

## Which configuration options minimize diffs/conflicts in version control?}

* `align=none` If alignment is enabled a renaming of one entity can impact the
  indentation of other entities.

* `danglingParenthesis=true` Having the closing parenthesis on the same line as
  the last argument makes the diff line include the parenthesis and everything
  following it in case that argument is renamed. So, technically this does not
  reduce the number of diff lines, but the length of them.

## Is the formatting output stable between releases?

No, the formatting rules will evolve even between PATCH releases. I recommend
you inspect the diff for **every** scalafmt update.
