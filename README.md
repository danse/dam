# dam

Toolkit for managing structured text files organized as tagged decks of cards.

Stemmed from the minima project (see
[original-design.md](./original-design.md)) — a minimalist personal
knowledge system combining offline locality with Git-based
sharing. Files are plain text: no client needed to create or edit
them.

## Format

- **Cards** are multi-line text units separated by **two or more consecutive
  newlines**.
- **Expressions** are individual lines within a card, separated by **single
  newlines**.
- **Tags** are whitespace-separated words encoded in **filenames**.

A file named `recipes italian` contains cards tagged with both `recipes` and
`italian`.

```
eggplant
parmesan
salt

pasta
tomato
basil
```

Two cards: the first has lines `eggplant` / `parmesan` / `salt`, the second
has `pasta` / `tomato` / `basil`. The filename `recipes italian` tags both
cards with both tags.

## Tools

| Command | Description |
|---|---|
| `dam-cycle` | Move the first card to the end of the file (rotate) |
| `dam-read` | Pretty-print a deck file |
| `dam-length` | Count the number of cards in a file |
| `dam-profile` | Replace each card with its line count (stdin) |
| `dam-slope` | Sort cards by size then replace with line counts |
| `dam-sediment` | Sort cards by character size (like sediment settling) |
| `dam-reverse` | Reverse the order of cards |
| `dam-dispatch` | Interactive dispatcher: route cards to tagged files via terminal selector |
| `dam-factor` | Deduplicate cards across tagged files — content appearing under multiple tags is factored out into shared files |
| `dam-set-sizes` | List all tags with their unique content counts, sorted ascending |
| `dam-aeson` | Export the tag–content mapping as JSON |

## Export by factorization

Given two files in a directory:

`a`:
```
a
b

c

d
```

`b`:
```
a
b

d

e
```

Running `dam-factor` produces three files:

- `a` — content unique to tag `a`: `c`
- `b` — content unique to tag `b`: `e`
- `a b` — content shared by both tags: `a\nb`, `d`

## Build

Requires [Cabal](https://www.haskell.org/cabal/) and GHC 9.4+.

```sh
cabal build
```

## License

BSD3 (see LICENSE).
