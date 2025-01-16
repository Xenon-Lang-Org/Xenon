COMPILER	=	xcc

VM			=	xrun

INTERPRETER	=	xin

BIN_DIR		=	$(shell stack path --local-install-root)/bin

all:
	stack build

$(COMPILER): all
	cp $(BIN_DIR)/compiler ./$(COMPILER)

$(VM): all
	cp $(BIN_DIR)/wasm-vm ./$(VM)

$(INTERPRETER): all
	cp $(BIN_DIR)/interpreter ./$(INTERPRETER)

clean:
	stack clean

fclean: clean
	rm -f $(COMPILER)
	rm -f $(VM)
	rm -f $(INTERPRETER)

re: fclean $(COMPILER) $(VM) $(INTERPRETER)

unit_tests:
	stack test

func_tests: $(COMPILER) $(VM) $(INTERPRETER)
	./test/tester.sh
	./ftest/test-wasm.sh

tests: unit_tests func_tests

coverage:
	stack test --coverage

.PHONY: all clean fclean re unit_tests func_tests tests coverage
