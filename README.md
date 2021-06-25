# ocaml-uring -- bindings to Linux io_uring

These are OCaml bindings for the Linux [io_uring][] stack
(an alternative to using syscalls such as `select` or `epoll`).

The [Eio][] library provides a higher-level effects-based API
that uses this library to implement its Linux backend,
but ocaml-uring may be useful with single-core non-effects versions of OCaml too.

To use the library directly:

1. Call `Uring.create` to initialise a ring.
2. Add IO requests to the ring using functions such as `Uring.readv`.
3. Call `Uring.submit` to notify the kernel of the new requests.
4. Call `Uring.wait` to wait until an operation is complete.

The `tests` directory contains some examples.

[io_uring]: https://github.com/axboe/liburing
[Eio]: https://github.com/ocaml-multicore/eio
