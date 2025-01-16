#!/bin/bash

echo -e "\033[35mStarting Tests:\033[0m"

run_test() {
    local input_file=$1
    local test_description=$2
    local expected_file=$3

    echo -e "\033[34mRunning Test: $test_description\033[0m"

    ./xcc $input_file -o temp_test.wasm > /dev/null

    if [ $? -ne 0 ]; then
        echo -e "\033[31mTest Failed: $test_description\033[0m"
        return
    fi

    if ! diff $expected_file temp_test.wasm; then
        echo -e "\033[31mTest Failed: $test_description\033[0m"
        return
    else 
        echo -e "\033[32mTest Passed: $test_description\033[0m"
    fi
}

run_test examples/add.xn "Basic Add" examples/compiled/add.wasm
run_test examples/multiple_functions.xn "Multiple Functions" examples/compiled/multiple_functions.wasm
run_test examples/basic_if.xn "Basic If" examples/compiled/basic_if.wasm
run_test examples/and.xn "Basic And" examples/compiled/and.wasm
run_test examples/or.xn "Basic Or" examples/compiled/or.wasm
run_test examples/multiple_if.xn "Multiple If" examples/compiled/multiple_if.wasm
run_test examples/while.xn "While" examples/compiled/while.wasm
run_test examples/is_prime.xn "Is Prime" examples/compiled/is_prime.wasm
run_test examples/is_prime_global.xn "Is Prime Global" examples/compiled/is_prime_global.wasm
run_test examples/my_pow.xn "My Pow" examples/compiled/my_pow.wasm
run_test examples/main.xn "Main" examples/compiled/main.wasm

rm temp_test.wasm

