# Todo
- Add ability to define constants in files
- Introduce labels, and use them in jumps, this makes it possible to implement translator to `c`,
  or maybe not and keep current design just with `call` & `return` instructions to indicate stack frames?
  - Good parts: better security. You can't jump to any other place anymore (at least without protecting stack firstly)
  - Bad parts: more complicated, more concepts?
