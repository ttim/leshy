fib(35)

cargo test webasm::fib_tests::test_specialized_interpreter_eval --release
finished in 0.92s

cargo test webasm::fib_tests::test_caching_interpreter_eval --release
finished in 2.42

cargo test webasm::fib_tests::test_cached_node_eval --release
finished in 10.63s

cargo test webasm::fib_tests::test_code_generator_eval --release
finished in 0.13s

cargo test webasm::fib_tests::test_native --release
finished in 0.03s

-----------------
fib(39)
cargo test webasm::fib_tests::test_native --release
finished in 0.17s

cargo test webasm::fib_tests::test_code_generator_eval --release
finished in 0.90s

foxwren (interpreter) https://github.com/malcolmstill/foxwren
finished in 5.00s

wasmtime (cranelift) https://github.com/bytecodealliance/wasmtime
time ./wasmtime fib.wasm --invoke fib 39
finished in 0.44s

wasm3 (interpreter) https://github.com/wasm3/wasm3/blob/main/docs/Installation.md
time wasm3 --func fib fib.wasm 39
finished in 2.95s

wasmer (cranelift)
time wasmer --cranelift -i fib fib.wasm 39
finished in 0.45s
(llvm and singlepass aren't supported in prebuilt version)
