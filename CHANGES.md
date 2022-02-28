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
