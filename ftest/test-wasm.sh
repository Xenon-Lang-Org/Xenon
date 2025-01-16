#!/usr/bin/env bash
set -e

# Single array: filename, args, function, expected
wat_test_cases=(
  "add.wat"                 "3 5"       "add"           "8"
  "and.wat"                 "12 15"     "ifand"         "1"
  "basic_if.wat"            "12"        "doif"          "1"
  "branchNested.wat"        "2"         "bnest"         "3"
  "chainOps.wat"            ""          "cops"          "38"
  "factorial.wat"           "5"         "fact"          "120"
  "fibonacci.wat"           "6"         "fibo"          "8"
  "floatArithmetic.wat"     "3.2 1.6"   "farithm"       "4.8"
  "globalVar.wat"           ""          "gvar"          "1"
  "is_prime_global.wat"     "9"         "is_prime"      "0"
  "is_prime.wat"            "11"        "is_prime"      "1"
  "loop.wat"                "10"        "loop"          "55"
  "main.wat"                "2 3"       "pow"           "8"
  "main.wat"                "5"         "is_prime"      "1"
  "main.wat"                "4 6"       "add"           "10"
  "main.wat"                ""          "main"          "256"
  "multiple_functions.wat"  "7 2"       "add"           "9"
  "multiple_functions.wat"  "10 5"      "sub"           "5"
  "multiple_functions.wat"  "3 4"       "mul"           "12"
  "multiple_functions.wat"  "9 3"       "div"           "3"
  "multiple_functions.wat"  "10 3"      "mod"           "1"
  "multiple_if.wat"         "8"         "doelif"        "2"
  "multiple_if.wat"         "1"         "forest"        "1"
  "multiple_if.wat"         "12"        "supegal"       "1"
  "multiple_if.wat"         "8"         "infegal"       "1"
  "my_pow.wat"              "2 4"       "pow"           "16"
  "or.wat"                  "5 15"      "ifor"          "1"
  "subAndMul.wat"           ""          "submul"        "200"
  "while.wat"               "5"         "dowhile"       "5"
)

# Compile all
for ((i=0; i<${#wat_test_cases[@]}; i+=4)); do
  wat_file="ftest/${wat_test_cases[i]}"
  wasm_file="${wat_file%.wat}.wasm"
  wat2wasm "$wat_file" -o "$wasm_file" \
    || { echo -e "\e[31mFailed to compile $wat_file\e[0m"; exit 1; }
    # && echo -e "\e[32mCompiled $wat_file\e[0m" \
done

# Run and check results
for ((i=0; i<${#wat_test_cases[@]}; i+=4)); do
  wat_file="ftest/${wat_test_cases[i]}"
  wat_args="${wat_test_cases[i+1]}"
  wat_function="${wat_test_cases[i+2]}"
  wat_expected="${wat_test_cases[i+3]}"
  wasm_file="${wat_file%.wat}.wasm"

  wasmer_output=$(wasmer run "$wasm_file" --invoke "$wat_function" $wat_args 2>&1 || echo "error")
  wasm_vm_output=$(./xrun "$wasm_file" --invoke "$wat_function" $wat_args 2>&1 || echo "error")
  wasm_vm_last_line=$(echo "$wasm_vm_output" | tail -n1)

  if [[ "$wasmer_output" == "$wat_expected" && "$wasm_vm_last_line" == "$wat_expected" ]]; then
    echo -e "\e[32m$wat_file::$wat_function ✔\e[0m"
  else
    echo -e "\e[31mDiscrepancy in $wat_file::$wat_function\e[0m"
    echo "Expected: $wat_expected"
    echo "Wasmer Output: $wasmer_output"
    echo "Wasm-vm Output: $wasm_vm_output"
  fi
done

# Clean up
rm ftest/*.wasm
