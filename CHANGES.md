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
