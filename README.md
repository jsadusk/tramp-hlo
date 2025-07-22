# tramp-hlo
High level operations as tramp handlers

This is an experiment to speed up some tramp operations that are slow in common
usage. The idea is to reduce round-trips by implementing higher level
operations than tramp usually targets as server side scripts. 

# Installation

This isn't in ELPA/MELPA yet, so the easiest way to install would be
using `package-vc` or `elpaca` with `use-package`. 

## `package-vc`
```
(use-package tramp-hlo
    :vc (:url "https://github.com/jsadusk/tramp-hlo" :rev "main")
    :config
    (configure-tramp-hlo)
)
```

## `elpaca`
```
(use-package tramp-hlo
    :ensure (tramp-hlo :type git :host github :repo "jsadusk/tramp-hlo")
    :config
    (configure-tramp-hlo)
    )
```

# Targeted operations

So far these operations have been implemented because they were
affecting me. The approach was very successful, so I'll be searching
for more operations to optimize in similar ways.

## `locate-dominating-file`

The emacs builtin function `locate-dominating-file` doesn't have a
tramp specific implementation, relying on regular directory traversal
and test functions. This can result in 10s of round-trips per-call,
depending on the depth of the path searched and the dominating file
name searched for. Rewriting this as a server-side script reduces this
to a single round-trip. This greatly speeds up packages like `vc`
which repeatedly look for repository root files. 

## `dir-locals`

The function `dir-locals-find-file` not only makes use of
`locate-dominating-file`, but uses it with a predicate that performs
tramp operations of its own. Tweaking the logic to use more server
side scripts also reduces 10s of round-trips to just two.

# Testing

These are functions used by everything in emacs, and I've only tested
them in my own environment. I can't catch every edge case on my
own. I'm trying to match the logic of the original functions as
closely as possible, but there are bound to be differences. Please try
this out and let me know if you see it break anything.

# Current implementation

This is currently built using advice functions, because I don't want
to interfere with the internals of tramp. However these could
definitely be adapted to be part of `tramp-sh` if the core team approves.
