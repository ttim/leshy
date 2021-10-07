# Overview

# First principles
## Simple
- Orthogonal
- Coherent
    - Size isn't priority
- Don't over generialize, it's targeting only AMD64 & ARM64 (& maybe RISC-V) in reality
  - So start from learning their assembly
  - And esp how simple c code gets compiled to it

## Specializable
## Dynamic (-friendly)
- Partial code reload
- New classes addition

# Concepts
## Stack machine
## Memory

## Model
- Heap addresses are 8 bytes, stack addresses are 4 bytes
- Length when passed as address is 8 bytes
- Little Endian
- Negative numbers == Positive numbers by modulo of 2^N
- 

## Operands
- Constants (`c`)
  - Literals: `1`, `2`, `10`, `1_4`, `1_8`, `-1_8`, `1_8`
  - On stack: `$0`, `$0_8`, `$-4`
    - Can be repeated, i.e. `$$0` or `$$-4`
    - Default size is 4
- Addresses (`a`)
  - Stack address
    - Direct: `#constant`
      - `#0` - beginning of stack
      - `#-4` - 4 bytes before the end of stack
      - `#$0` - position in stack indicated by positions `[0, 4)` in stack
    - Indirect: `##[constant, constant, constant]`
      - `##[stack address offset, low limit inclusive, high limit exclusive]`
      - `##[0, $4, $8]`
  - Heap address
    - Direct: `*constant` (don't expect to be used)
      - `*0` - beginning of heap
    - Indirect: `*#[constant]`
      - `*#[stack address offset]`
      - `*#0`

## Operations
- Stack operations
  - `extend length_c`
  - `shrink length_c`
  - `check_size length_c`
- Branch operations
  - Modifier
    - Equality: `eq`, `neq`
    - Less: `l`, `l_u`
    - More: `m`, `m_u`
    - Less or equal: `le`, `le_u`
    - More or equal: `me`, `me_u`
  - Instruction
    - `branch modifier_c length_c addr1_ac addr2_ac dest_name_c`
- Call operations
  - `call offset_c dest_name_c`
- Constant operations
  - TBD
- Memory operations. If `length` passed as address it's treated as 8 (?) bytes
  - `copy length_ac src_ac dst_a`
  - `set length_ac bytes_ac dst_a`
- Integer operations. `length_c` can be anything
  - `sum length_c src1_ac src2_ac dst_a`
  - `mult length_c src1_ac src2_ac dst_a`
  - `xor length_c src1_ac src2_ac dst_a`
  - `div length_c src1_ac src2_ac dst_a`
  - `mod length_c src1_ac src2_ac dst_a`
- Float operations. `length_c` can be either 4 or 8.
  - `sum_f length_c src1_ac src2_ac dst_a`
  - `mult_f length_c src1_ac src2_ac dst_a`

# Constant evaluation rules
- For most operations if all operands are constant result considered constant too

# Example
0, 1, ... - 32 bits numbers

0l, 1l ... - 64 bits numbers

#0, #1, #4 - references to stack positions/addresses

##0, ##1, ##4 - indirect stack positions, i.e. it loads position from #1 and then references it

#{#0, 0, 4} <- the only way to access non static position in stack

*#0, *#1, *#4 - same but with heap positions

*0, *4 - absolute address reference to a memory with offset 0 and 4.

```
def fib:
  call_eql_i32 #0 1 _fib_ret_1 #0
  call_more_i32 #0 1 _fib_rec #0

def _fib_ret_1:
  set #0 1
  size 4

def _fib_rec:
  size 8
  add_i32 #4 #0 -1
  call fib #4
  size 12
  add_i32 #8 #0 -2
  call fib #8
  add_i32 #0 #4 #8
  size 4
```

# TODO
- read risc v around branches & co
- Read [arm engineer critique on RISC v](https://news.ycombinator.com/item?id=24958423)

# Ideas
- Implement couple of macros for simple functions calls + bytes slices manipulation?
- It worth doing mental exercise of modeling `put ((get struct .field2) + 1)` with leshy, and checking that result seems reasonable
- Case study with "dynamic enough" c, where you don't need to recompile everything to run new version and instead it carries type info around
	- For example incrementing second int field from struct
- Another case study is variable pointer size
- Use positive & negative numbers to distinguish between from beginning or from end of stack. Might need to think how good this is with inlining - gets a bit less trivial than just to replace with offset. Works almost like that when call offset is positive, and needs normalization otherwise. 
- Add `_start_call` instruction which saves stack offset and later uses it in `call`
	- Doesn't seem worth it, can be modeled with dedicated first 4 bytes, `_start_call` doing `store_4 0 #size`, and `call` doing `call #0`
- We might not need binary representation, text with compression is enough. Results of compilation should be cacheable anyway. Maybe the only requirement is to make text splittable by functions so it's easy to do initial parse and hashing

# Insights
- Customer facing IR & IR good for optimizations are different IRs, and that's something IMO llvm solves wrongly
- llvm in fact isn't low level - it has types and look very much like c from type system perspective. It's not good

# Questions
- Should it be two different set of instructions for stack & heap operations? Or just one operation, and just some data in registers heap address, and sometimes it's static.
- I guess it should be two different kinds of memory pointers: stack pointer & heap pointer. And you can't get stack pointer from another stack pointer content unless it's specialized. You can't go from heap content to stack content either.
- How do you implement generic function which adds something to the end of stack? If you have size manipulations only in `size 8` way then there is no way to put size being current size + something. Seems like there should be something like `extend 4` and `shrink 4` instead? Or maybe just `add #size #size 4`, don't think it's really needed tho
- Makes me think we need relative addresses. I.e. #{size-4}. Seems useful, but also kinda duplicating in a way? But otherwise it's really hard do not create a thing for everything...
- How do you model 32bit vs 64bit (or even vs 16bit?) architectures? What's pointer size?
- How to implement variable number arguments functions in C?
- Do we need to supply length constants in all arithmetic instructions? maybe better to have long and not long versions of them and that's it?
  - It's simpler, esp given only 64bit platforms are targeted
  - But it might be easier for SSE and similar instructions?
  - Overall it does feel like not worth doing and having AVX etc instructions separately
  - But also there are variants with other byte sizes in regular languages, so maybe it's better to make it customizable. At least to some extent?
- Make all operations to work only against stack memory addresses? Apart from one which copies data from memory? Or it goes again of unified memory approach?

# Links
- Risc-V intros
	- https://medium.com/swlh/risc-v-assembly-for-beginners-387c6cd02c49
	- https://erik-engheim.medium.com/risc-v-assembly-code-examples-7bca0e7ebaa3

# Inspirations
- Cwerg https://github.com/robertmuth/Cwerg
- Mir https://github.com/vnmakarov/mir
- Risc-V https://medium.com/swlh/risc-v-assembly-for-beginners-387c6cd02c49
