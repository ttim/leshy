fib(35)

cargo test webasm::fib_tests::test_specialized_interpreter_eval --release
finished in 0.92s

cargo test webasm::fib_tests::test_caching_interpreter_eval --release
finished in 2.42

cargo test webasm::fib_tests::test_cached_node_eval --release
finished in 10.63s

cargo test webasm::fib_tests::test_code_generator_eval --release
finished in 1.40s