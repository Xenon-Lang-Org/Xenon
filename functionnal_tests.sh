#!/usr/bin/env bash
set -e

# Single array: filename, args, function, expected
test_cases=(
  "add_64"              "3 5"             "add"           "8"
  "add"                 "3 5"             "add"           "8"
  "add"                 "2147483647 1"    "add"           "-2147483648"
  "and"                 "12 15"           "ifand"         "1"
  "basic_if"            "12"              "doif"          "1"
  "branchNested"        "2"               "bnest"         "3"
  "chainOps"            ""                "cops"          "38"
  "factorial"           "5"               "fact"          "120"
  "fibonacci"           "6"               "fibo"          "8"
  "floatArithmetic"     "3.2 1.6"         "add"           "4.8"
  "globalVar"           ""                "gvar"          "1"
  "is_prime_global"     "9"               "is_prime"      "0"
  "is_prime"            "11"              "is_prime"      "1"
  "loop"                "10"              "loop"          "55"
  "main"                "2 3"             "pow"           "8"
  "main"                "5"               "is_prime"      "1"
  "main"                "4 6"             "add"           "10"
  "main"                ""                "main"          "256"
  "multiple_functions"  "7 2"             "add"           "9"
  "multiple_functions"  "10 5"            "sub"           "5"
  "multiple_functions"  "3 4"             "mul"           "12"
  "multiple_functions"  "9 3"             "div"           "3"
  "multiple_functions"  "10 3"            "mod"           "1"
  "multiple_if"         "8"               "doelif"        "2"
  "multiple_if"         "1"               "forest"        "1"
  "multiple_if"         "12"              "supegal"       "1"
  "multiple_if"         "8"               "infegal"       "1"
  "my_pow"              "2 4"             "pow"           "16"
  "or"                  "5 15"            "ifor"          "1"
  "subAndMul"           ""                "submul"        "200"
  "while"               "5"               "dowhile"       "5"
  "gcd"                 "48 18"           "gcd"           "6"
  "gcd"                 "7 5"             "gcd"           "1"
  "bitwise"             "5 3"             "bitwise"       "14"
  "bitwise"             "10 4"            "bitwise"       "28"
  "sum_digits"          "123"             "sum_digits"    "6"
  "sum_digits"          "1018"            "sum_digits"    "10"
  "conditionnal"        "10"              "test_if"       "1"
  "conditionnal"        "-10"             "test_if"       "11"
  "count_to"            "10"              "count_to"      "10"
)

non_working_test_cases=(
  "duplication"
  "loop_return"
  "no_return"
  "partial_return"
  "type"
  "variable_immut"
  "variable_uninit"
)

echo "--- Running tests ---"
echo ""
echo "Compiling non-working .xn files"
echo ""

# Compile non working (return should not be 0, should not compile)
for ((i=0; i<${#non_working_test_cases[@]}; i++)); do
  xn_file="examples/non-working/${non_working_test_cases[i]}.xn"
  wasm_file="examples/non-working/${non_working_test_cases[i]}.wasm"
  set +e
  output=$(./xcc "$xn_file" -o "$wasm_file" 2>&1)
  exit_code=$?
  set -e
  if [ $exit_code -eq 0 ]; then
    echo -e "\e[31m$xn_file should not compile\e[0m"
  else
    echo -e "\e[32m$xn_file does not compile\e[0m"
  fi
done

echo ""
echo "Compiling working .xn files"
echo ""

# Compile all
for ((i=0; i<${#test_cases[@]}; i+=4)); do
  xn_file="examples/${test_cases[i]}.xn"
  wasm_file="examples/${test_cases[i]}.wasm"
  ./xcc "$xn_file" -o "$wasm_file" \
    || { echo -e "\e[31mFailed to compile $xn_file\e[0m"; rm examples/*.wasm && exit 1; }
done

echo ""
echo "Running tests"
echo ""

# Run and check results
for ((i=0; i<${#test_cases[@]}; i+=4)); do
  xn_file="examples/${test_cases[i]}.xn"
  args="${test_cases[i+1]}"
  function="${test_cases[i+2]}"
  expected="${test_cases[i+3]}"
  wasm_file="examples/${test_cases[i]}.wasm"

  wasmer_output=$(wasmer run "$wasm_file" --invoke "$function" $args 2>&1 || echo "error")
  wasm_vm_output=$(./xrun "$wasm_file" --invoke "$function" $args 2>&1 || echo "error")
  wasm_vm_last_line=$(echo "$wasm_vm_output" | tail -n1)

  csv_args=$(echo "$args" | tr ' ' ',')
  xin_echo="echo ${function}(${csv_args})"
  xin_output=$($xin_echo | ./xin "$xn_file" 2>&1 || echo "error")
  xin_last_line=$(echo "$xin_output" | tail -n2 | head -n1)
  xin_last_line=${xin_last_line:3}

  if [[ "$wasmer_output" == "$expected" && "$wasm_vm_last_line" == "$expected" && "$xin_last_line" == "$expected" ]]; then
    echo -e "\e[32m$xn_file::$function ✔\e[0m"
  else
    echo -e "\e[31mDiscrepancy in $xn_file::$function\e[0m"
    echo "Expected: $expected"
    echo "Wasmer Output: $wasmer_output"
    echo "vm Output: $wasm_vm_output"
    echo "xin Output: $xin_last_line"
  fi
done


# Clean up
rm examples/*.wasm
rm examples/non-working/*.wasm