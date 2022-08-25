# ocaml-uring -- bindings to Linux io_uring

* [API documentation](https://ocaml-multicore.github.io/ocaml-uring/uring/index.html)

These are OCaml bindings for the Linux [io_uring][liburing] stack
(an alternative to using syscalls such as `select` or `epoll`).

The [Eio][] library provides a higher-level effects-based API
that uses this library to implement its Linux backend,
but ocaml-uring may be useful with single-core non-effects versions of OCaml too.

## Example

To use the library directly, you need to use the `uring` ocamlfind library:

```ocaml
# #require "uring";;
```

Call `Uring.create` to initialise a ring:

```ocaml
# let uring = Uring.create ~queue_depth:10 ();;
val uring : '_weak1 Uring.t = <abstr>
```

The `'_weak1` is the type of user data attached to requests, which can be whatever you want.
The `queue_depth` is the size of the submission queue.

We can now submit requests to the ring.
To start, we'll open a file using `Uring.openat2`, which works much like the regular `openat2` system call:

```ocaml
# let open_file =
    Uring.openat2 uring
      ~access:`RW
      ~flags:Uring.Open_flags.(creat + cloexec)
      ~perm:0o644
      ~resolve:Uring.Resolve.beneath
      "test.log"
      `Open_log;;
val open_file : _[> `Open_log ] Uring.job option = Some <abstr>
```

`submit` returns `None` if the submission queue is full.
We could batch up further operations now if desired, but for now this is enough.

Once all the requests have been added to the ring, we can submit them all with a single system call:

```ocaml
# Uring.submit uring;;
- : int = 1
```

The return value is the number of requests submitted.
We can now ask Linux to suspend the process until a result (Completion Queue Entry) is available:

```ocaml
let rec wait_with_retry uring =
  match Uring.wait uring with
  | None -> wait_with_retry uring        (* Interrupted *)
  | Some { result; data } -> result, data;;
```

<!-- $MDX non-deterministic=output -->
```ocaml
# let result, data = wait_with_retry uring;;
val result : int = 8
val data : _[> `Open_log ] = `Open_log
```

The `data` field is the data we passed in when submitting the request, allowing us to recognise this result
(if we submit multiple jobs then they might not complete in order).

The `result` field is the return code,
with the same meaning as the return code from the corresponding system call (`openat2` in this case).

```ocaml
# let fd =
    if result < 0 then failwith ("Error: " ^ string_of_int result);
    (Obj.magic result : Unix.file_descr);;
val fd : Unix.file_descr = <abstr>
```

We can now submit further requests. e.g.

```ocaml
let rec write_all fd = function
  | [] -> ()
  | bufs ->
    let _job : _ Uring.job =
      Uring.writev uring
        fd
        ~file_offset:Optint.Int63.minus_one         (* Use current position *)
        bufs
        `Write_all
      |> Option.get               (* We know we have enough space here *)
    in
    assert (Uring.submit uring = 1);
    let result, data = wait_with_retry uring in
    assert (data = `Write_all);  (* There aren't any other requests pending *)
    assert (result > 0);         (* Check for error return *)
    let bufs = Cstruct.shiftv bufs result in
    write_all fd bufs
```

```ocaml
# write_all fd Cstruct.[of_string "INFO: "; of_string "A log message"];;
- : unit = ()
```

Note:
- As with a regular `writev` call, we keep going until all the data has been written.
- The `Uring.job` returned by `writev` can be used to cancel the job, if needed.

Finally, we close the file:

```ocaml
# Uring.close uring fd `Close_log;;
- : _[> `Close_log | `Open_log | `Write_all ] Uring.job option = Some <abstr>

# Uring.submit uring;;
- : int = 1

# wait_with_retry uring;;
- : int * _[> `Close_log | `Open_log | `Write_all ] = (0, `Close_log)
```

The file has now been written:

```sh
$ cat test.log
INFO: A log message
```

When you're finished with uring, use `exit` to close it down:

```ocaml
# Uring.exit uring;;
- : unit = ()
```

The `tests` directory contains some more examples.

## License

This library is released under the ISC license (see [LICENSE.md](./LICENSE.md)),
but note that the repository also vendors [liburing][] -
see [vendor/liburing/README](./vendor/liburing/README).

[liburing]: https://github.com/axboe/liburing
[Eio]: https://github.com/ocaml-multicore/eio
