## v2.7.0

- Fix use-after-free data race in CQE handlers (@avsm #124, reported #123 by @polytypic).

- Add socket bind and listen operations (@avsm #118).

- Update liburing to 2.7 (@avsm #118).

- Add `Uring.Setup_flags` module (@talex5 #122).

- Clean up configurator code and update primitives.h (@talex5 #121).

- Add `mkdirat` operation (@patricoferris #120).

## v0.9

- Fix statx constant fallback values, and fix with musl 1.2.5 (@alyssais #114).

- Add `Uring.sqe_ready` (@talex5 #115).

## v0.8

- add `linkat` operations. (@LucaSeri @talex5 #105, reviewed by @avsm)

- add `fsync` and `fdatasync` operations. (@avsm #103, reviewed by @talex5)

- add docstrings for many more functions (@avsm #100, reviewed by @talex5 @patricoferris)

- use lintcstubs to generate C prototypes and fix bugs (@talex5 #104)

## v0.7

- Add `statx` support (@patricoferris, @talex5, @avsm #95 #97).

- Add type annotations to tests (@patricoferris #96, reviewed by @talex5).  
  Fixes MDX tests on OCaml 5.1.

- Update liburing to 2.4 (@anmonteiro #93, reviewed by @talex5).

- Fix accidental shadowing in `ocaml_uring_get_msghdr_fds` (@talex5 #91, reviewed by @avsm).

## v0.6

- Fix SIGSEGV on `Uring.wait` (@edwintorok #89).  
  `io_uring_submit_and_wait_timeout` can return a success status but with a NULL `cqe`.

- Remove unused variable in C stub (@talex5 #87).

- Use `caml_enter_blocking_section` when calling `io_uring_submit` (@TheLortex #86).

## v0.5

- Decouple Heap entries from ring size (@haesbaert #81).  
  Before this change, we could only wait for at most `queue_depth` events at a time.

- Update to liburing-2.3 (@avsm #82).

## v0.4

New features:

- Add `Uring.timeout` (@bikallem #59).

- Add `Uring.read` and `Uring.write` (@haesbaert #62).  
  These are simple wrappers for read(2) and write(2).

- Add `Uring.unlink` (@talex5 #65).  
  This uses unlinkat(2), and so can also be used to remove directories.

- Add support for uring probes (@talex5 #70).  
  Allows checking whether a feature is supported by the kernel at runtime.

- Rename `peek` to `get_cqe_nonblocking` (@talex5 #67).  
  The old name was confusing because it does remove the item from the ring.

- Update to liburing 2.2 (@talex5 #56).

- Add `Uring.active_ops` (@talex5 #68).  
  Avoids needing to track the value returned by `submit`, which is important as it is sometimes called automatically.

- Add `Uring.iov_max` constant (@talex5 #76).

- Add `Uring.get_debug_stats` (@talex5 #64).  
  This should make it easier to check that the uring is behaving as expected.

Performance:

- Introduce a Sketch buffer per Uring (@haesbaert #63).  
  The main motivation of this change is to avoid having one malloc per packet in readv(2), writev(2) and friends.

- Use `submit_and_wait` where appropriate (@haesbaert #69).

- Add a `readv` benchmark (@talex5 #64).

- Avoid unnecessary use of `CAMLparam` in the C stubs (@haesbaert #61).

Bug fixes:

- Prevent ring from being used after exit (@talex5 #78).

Build changes:

- Remove use of notty for formatting benchmark results (@talex5 #71).  
  It prevented uring from being tested on OCaml 5.

- Use MDX for README (@talex5 #57).

- Convert tests to MDX (@talex5 #58 #73).

- Use opam-repository syntax for license (@kit-ty-kate #72).

- Remove internal `is_dirty` flag (@talex5 #77).

## v0.3

Breaking changes:

- Don't allocate a fixed buffer by default (@talex5 #53).
  If you want a fixed buffer, you now need to call `set_fixed_buffer`.

New features:

- Add sendmsg and recvmsg (@patricoferris #49).

- Allow sending and receiving FDs using `SCM_RIGHTS` (@talex5 #52).

Other changes:

- Update tests to cmdliner 1.1.0 (@patricoferris #50).

## v0.2

New features:

- Allow running in polling mode (@talex5 #44).

Other changes:

- Update to liburing 2.1 (@talex5 #46).

- Remove bigstringaf dependency (@talex5 #43).

- Cmdliner is only needed for tests (@talex5 #45).

- Remove test dependencies on Bos and Rresult (@talex5 #40).

- Address `Fmt.strf` deprecation error (@bikallem #38).

- Update to cstruct 6.0.1 for `shiftv` (@talex5 #36).

## v0.1

- Initial release.
