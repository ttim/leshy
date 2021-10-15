# Todo
- Add ability to define constants in files
- Introduce labels, and use them in jumps, this makes it possible to implement translator to `c`,
  or maybe not and keep current design just with `call` & `return` instructions to indicate stack frames?
  - Good parts: better security. You can't jump to any other place anymore (at least without protecting stack firstly)
  - Bad parts: more complicated, more concepts?
- Interesting to see how stack based bytes array of 4 elements gets compiled when used from for loop

# Goals
- Implement c compiler where most of work (such as offset calculation) made using specialization abilities
  - I.e. most of the stuff doesn't need to be recompiled, you only need to implement function from function body to bytecode
  - Rest gets resolved through specializations in runtime
  - And efficient because of caching
