#!/usr/bin/env bash
set -e

# Define the folder to search
folder="examples"

# Initialize an empty array
test_cases=()

test_files=()

# Find all files in the folder and subfolders
while IFS= read -r file; do
  # Extract lines matching the pattern and parse them
  while IFS= read -r line; do
    # Extract filename, arguments, function, and expected output
    if [[ $line =~ ^//\ TEST\ \"([^\"]+)\"\ IN\ \"([^\"]*)\"\ OUT\ \"([^\"]+)\"$ ]]; then
      function="${BASH_REMATCH[1]}"
      args="${BASH_REMATCH[2]}"
      expected="${BASH_REMATCH[3]}"
      filename="${file#$folder/}"
      test_cases+=("$filename" "$args" "$function" "$expected")
      # add filename to test_files if not already present
      if [[ ! " ${test_files[@]} " =~ " ${filename} " ]]; then
        test_files+=("$filename")
      fi
    fi
  done < "$file"
done < <(find "$folder" -type f)

non_working_test_cases=($(find examples/non-working -type f -name "*.xn"))

echo ""
echo -e "\e[34m---------------------------------\e[0m"
echo -e "\e[34m--- \e[0mRUNNING FUNCTIONNAL TESTS\e[34m ---\e[0m"
echo -e "\e[34m---------------------------------\e[0m"
echo ""
echo -e "\e[34mCompiling non-working .xn files\e[0m"
echo ""

returned=0

# Compile non working (return should not be 0, should not compile)
for ((i=0; i<${#non_working_test_cases[@]}; i++)); do
  xn_file="${non_working_test_cases[i]}"
  wasm_file="${non_working_test_cases[i]/.xn/.wasm}"
  set +e
  output=$(./xcc "$xn_file" -o "$wasm_file" 2>&1)
  exit_code=$?
  set -e
  if [ $exit_code -eq 0 ]; then
    echo -e "\e[31m$xn_file should not compile\e[0m"
    rm "$wasm_file"
    returned=1
  else
    echo -e "\e[32m$xn_file does not compile\e[0m"
  fi
done

echo ""
echo -e "\e[34mCompiling working .xn files\e[0m"
echo ""

# Compile all
for ((i=0; i<${#test_files[@]}; i++)); do
  xn_file="examples/${test_files[i]}"
  wasm_file="examples/${test_files[i]/.xn/.wasm}"
  set +e
  output=$(./xcc "$xn_file" -o "$wasm_file" 2>&1)
  exit_code=$?
  set -e
  if [ $exit_code -eq 0 ]; then
    echo -e "\e[32m$xn_file compiled\e[0m"
  else
    echo -e "\e[31mFailed to compile $xn_file\e[0m"
    echo "$output"
    returned=1
  fi
done

echo ""
echo -e "\e[34mRunning tests\e[0m"
echo ""

# Run and check results
for ((i=0; i<${#test_cases[@]}; i+=4)); do
  xn_file="examples/${test_cases[i]}"
  args="${test_cases[i+1]}"
  function="${test_cases[i+2]}"
  expected="${test_cases[i+3]}"
  wasm_file="examples/${test_cases[i]/.xn/.wasm}"

  # If no wasm file, skip
  if [ ! -f "$wasm_file" ]; then
    echo -e "\e[33mSkipping $xn_file::$function (no wasm file)\e[0m"
    continue
  fi

  wasmer_output=$(wasmer run "$wasm_file" --invoke "$function" "--" $args 2>&1 || echo "error")
  wasm_vm_output=$(./xrun "$wasm_file" --invoke "$function" $args 2>&1 || echo "error")
  wasm_vm_last_line=$(echo "$wasm_vm_output" | tail -n1)

  csv_args=$(echo "$args" | tr ' ' ',')
  xin_echo="echo ${function}(${csv_args})"
  xin_output=$($xin_echo | ./xin "$xn_file" 2>&1 || echo "error")
  xin_last_line=$(echo "$xin_output" | tail -n2 | head -n1)
  xin_last_line=${xin_last_line:3}

  # if expected ends with ".0", then add it to wasmer_output
  if [[ "$expected" == *".0" ]]; then
    wasmer_output="$wasmer_output.0"
  fi

  if [[ "$wasmer_output" == "$expected" && "$wasm_vm_last_line" == "$expected" && "$xin_last_line" == "$expected" ]]; then
    echo -e "\e[32m$xn_file::$function($csv_args) = $expected ✔\e[0m"
  else
    echo -e "\e[31mDiscrepancy in $xn_file::$function($csv_args)\e[0m"
    echo "Expected: $expected"
    echo "Wasmer Output: $wasmer_output"
    echo "vm Output: $wasm_vm_output"
    echo "xin Output: $xin_last_line"
    returned=1
  fi
done

# Clean up
for ((i=0; i<${#test_files[@]}; i++)); do
  wasm_file="examples/${test_files[i]/.xn/.wasm}"
  rm -f "$wasm_file"
done

# Return
if [ $returned -eq 0 ]; then
  echo ""
  echo -e "\e[32mAll tests passed\e[0m"
  echo ""
  exit 0
else
  echo ""
  echo -e "\e[31mSome tests failed\e[0m"
  echo ""
  exit 1
fi
