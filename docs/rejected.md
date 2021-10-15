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