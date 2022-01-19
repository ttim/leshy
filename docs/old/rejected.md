# Removed or rejected ideas

## 2021/10
- Constant native address references: before both `*const` and `*#const` were permitted.
  I don't think const native address is really useful.
  There is workaround by appending const address to stack and using it,
  i.e. `append 777_8` and `*-8`
- `limit` on regular stack references
  - Instead all operations should be guaranteed in scope given their arguments.
    Similar to what I had before
  - We can call them with `_native` suffix, meaning they accept only native addresses

## 2021/10/18
- being stack based language: on pros side it was much easier to inline,
  but actual writing of bytecode is waaaay harder.
  Also to be truly stack it's better to make double unreferencing `#$4` relative
  (i.e. offset by `$4` from 4 position in stack, which is hard to implement without breaking const constructs).
  Also given requirement on specialization anyway it doesn't seem to needed to have this kind of ability to inline in particular.
