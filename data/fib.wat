;; copied from https://github.com/malcolmstill/foxwren/blob/master/test/fib.wat
;; `wat2wasm fib.wat` to create `fib.wasm`
(module
  (type $t32 (func (param i32) (result i32)))
  (type $t64 (func (param i32) (result i64)))
  (func $fib32 (type $t32) (param $n i32) (result i32)
    local.get $n
    i32.const 0
    i32.eq
    if $I0
      i32.const 0
      return
    end
    local.get $n
    i32.const 1
    i32.eq
    if $I1
      i32.const 1
      return
    end
    local.get $n
    i32.const 2
    i32.sub
    call $fib32
    local.get $n
    i32.const 1
    i32.sub
    call $fib32
    i32.add
    return)
  (func $fib64 (type $t64) (param $n i32) (result i64)
    local.get $n
    i32.const 0
    i32.eq
    if $I0
      i64.const 0
      return
    end
    local.get $n
    i32.const 1
    i32.eq
    if $I1
      i64.const 1
      return
    end
    local.get $n
    i32.const 2
    i32.sub
    call $fib64
    local.get $n
    i32.const 1
    i32.sub
    call $fib64
    i64.add
    return)
  (export "fib32" (func $fib32))
  (export "fib64" (func $fib64))
)