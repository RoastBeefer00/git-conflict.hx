# git-conflict.hx

A [git-conflict.nvim](https://github.com/akinsho/git-conflict.nvim)-style merge
conflict resolver for [helix-steel](https://github.com/mattwparas/helix). Navigate,
highlight, and resolve standard git conflict markers directly on the buffer — with
an optional 3-way ours/working/theirs diff view.

## Requirements

- [mattwparas/helix](https://github.com/mattwparas/helix) built with the `steel` feature
- `git` on your `$PATH`

## Installation

```sh
forge pkg install --git https://github.com/RoastBeefer00/git-conflict.hx.git
```

Or add to your `cog.scm` dependencies:

```scheme
(#:name git-conflict.hx #:git-url "https://github.com/RoastBeefer00/git-conflict.hx.git")
```

## Usage

In your `init.scm`:

```scheme
(require "git-conflict.hx/git-conflict.scm")

(keymap (global)
        (normal (space (g (c (n ":conflict-next")
                             (p ":conflict-prev")
                             (o ":conflict-accept-ours")
                             (t ":conflict-accept-theirs")
                             (a ":conflict-accept-both")
                             (d ":conflict-accept-none")
                             (h ":conflict-highlight")
                             (x ":conflict-clear")
                             (l ":conflict-list")
                             (f ":conflict-files")
                             (s ":conflict-diff")
                             (q ":conflict-diff-close")))))))
```

### Commands

| Command | Description |
|---|---|
| `:conflict-highlight` | Highlight every conflict in the current buffer |
| `:conflict-clear` | Remove conflict highlighting and stop tracking the buffer |
| `:conflict-next` | Jump to the next conflict below the cursor (wraps) |
| `:conflict-prev` | Jump to the previous conflict above the cursor (wraps) |
| `:conflict-accept-ours` | Resolve the current conflict keeping our (HEAD) side |
| `:conflict-accept-theirs` | Resolve the current conflict keeping their (incoming) side |
| `:conflict-accept-both` | Resolve the current conflict keeping both sides |
| `:conflict-accept-none` | Resolve the current conflict by discarding both sides |
| `:conflict-list` | Picker over conflicts in the current buffer; jumps + opens diff view |
| `:conflict-files` | Picker over every repo file with unresolved conflicts |
| `:conflict-diff` | Open a 3-way ours \| working \| theirs split for the current file |
| `:conflict-diff-close` | Close the diff-view side buffers, leaving the working file |

## How it works

The resolver operates directly on the focused document containing standard git
conflict markers:

```
<<<<<<< HEAD          (ours)
...ours lines...
||||||| base          (optional, diff3 style)
...base lines...
=======
...theirs lines...
>>>>>>> other-branch  (theirs)
```

Conflict regions are drawn with overlay script highlights (`diff.plus` for ours,
`diff.delta` for theirs), re-applied after every edit via a `document-changed` hook
so they survive undo/redo. Resolution actions rewrite the conflict block in place.
The 3-way diff view reads the `:1:`/`:2:`/`:3:` merge stages via `git show`, writes
them to temp files, and opens them in a split alongside the working buffer with
lines that differ from the merge base highlighted.
