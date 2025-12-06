# tramp-hlo
Higher level emacs functions as optimized tramp operations

Normally a tramp handler only contains implementations of the emacs
primitive file operations. Implementing this set of operations allows
emacs to do anything on a remote host, but many of the common
functions in the emacs standard library will trigger multiple file
operations for a single call. This can cause performance issues,
because each file operation involves a roundtrip to the remote host.

This module implements some of those operations as single round trip
tramp operations. The bulk of the operation is implemented as a server
side bash script, rather than an elisp function. In practice this
makes a lot of day to day editing on remote hosts much more responsive.

# Installation

Install from GNU ELPA:

## package.el

```
M-x package-install <RET> tramp-hlo
M-x tramp-hlo-setup
```

## use-package
```
(use-package tramp-hlo
    :ensure t
    :config
    (tramp-hlo-setup)
)
```

# Configuration

* Enable: `M-x tramp-hlo-setup` or add `(tramp-hlo-setup)` to
`init.el`.
* Disable: `M-x tramp-hlo-remove` or add `(tramp-hlo-remove)` to `init.el`

# Targeted operations

A first set of operations were targetted that perform poorly over
tramp due to roundtrips, andhave a large impact on day to day use,

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
tramp operations of its own. Encapsulating the logic to use one large
server side script also reduces 10s of round-trips to just two.

# Testing

A suite of tests is included using ERT, in addition to using it in my
day to day workflow. If you are using it in an environment I haven't
tested and find an issue, feel free to report issues on github.

