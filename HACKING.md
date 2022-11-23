# Maintainer notes

## liburing updates

`vendor/liburing/` contains the upstream version of liburing. Updating it
is normally done using `git subtree`.

To do an update, first fetch the remote references from the liburing repo:

```
git remote add liburing https://github.com/axboe/liburing.git
git fetch
```

Then run a subtree pull, and substitute `2.3` with the right version:

```
git subtree pull --prefix vendor/liburing https://github.com/axboe/liburing.git liburing-2.3 --squash
```

Then modify the build rules over at `lib/uring/dune` to pick up the
right version of the shared library that's built.

