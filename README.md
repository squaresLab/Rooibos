# Rooibos

[![Build Status](https://travis-ci.com/squaresLab/Rooibos.svg?token=NEepgzkwf1KGUTphtdZ4&branch=master)](https://travis-ci.com/squaresLab/Rooibos)

## Match Semantics

#### Exact Matches (`Match.find`)

- Source and Template must match on prefix and suffix to match holes to syntax.
  At this time, this includes leading prefix whitespace and trailing suffix
  whitespace (e.g., newlines).

MATCH:

`x = y;` <-> `x = :[1];`

`:[1]` <-> `x = y`

NO MATCH:

`x = y;` <-> `y = :[1];`

- Whitespace besides leading prefix and trailing suffix is match-insensitive.

MATCH:

`foo  (    bar,    quux   )` <-> `foo       ( :[1], :[2])`

- Multiple possible matches returns the first satisfying non-greedy match.

MATCH:

`x = a + b; x = c + d;` <-> `x = :[1] + :[2]` => `:[1] = a, :[2] = b`

#### Non-exact Matches (`Match.all`)

- Does not require Source and Template prefixes and suffixes to be exact.

MATCH:

`foo = bar; x = y; quux = bazz` <-> `x = :[1];` => `:[1] = y`

- Returns multiple environments for multiple matches

`foo(bar, quux); foo(foobar, bazz)` <-> `foo(:[1], :[2])`

Environment 1: `:[1] = foobar, :[2] = bazz`
Environment 2: `:[1] = bar, :[2] = quux`

Say you add a semicolon to our template: `foo(:[1], :[2]);`, will only return
one match. Yes, it is that good.

- Nested matches work

`foo(foo(bar, bazz),quux)` <-> `foo(:[1], :[2])`

Environment 1: `:[1] = foo(bar, bazz), :[2] = quux`
Environment 2: `:[1] = bar, :[2] = bazz`
