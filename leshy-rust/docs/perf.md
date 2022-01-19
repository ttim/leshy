fib(35)

cargo test webasm::fib_tests::test_specialized_interpreter_eval --release
finished in 1.05s

cargo test webasm::fib_tests::test_caching_interpreter_eval --release
finished in 10.62s
(was 2.03 before changing to driver)

cargo test webasm::fib_tests::test_cached_node_eval --release
finished in 10.63s
