COMPILER	=	xcc

VM			=	xrun

INTERPRETER	=	xin

BIN_DIR		=	$(shell stack path --local-install-root)/bin

all:
	stack build
	cp $(BIN_DIR)/compiler ./$(COMPILER)
	cp $(BIN_DIR)/wasm-interpreter ./$(VM)
	cp $(BIN_DIR)/interpreter ./$(INTERPRETER)

$(COMPILER):
	stack build && cp $(BIN_DIR)/compiler ./$(COMPILER)

$(VM):
	stack build && cp $(BIN_DIR)/wasm-interpreter ./$(VM)

$(INTERPRETER):
	stack build && cp $(BIN_DIR)/interpreter ./$(INTERPRETER)

clean:
	stack clean

fclean: clean
	rm -f $(COMPILER)
	rm -f $(VM)
	rm -f $(INTERPRETER)

re: fclean all

unit_tests:
	stack test

func_tests:
	./test/tester.sh

tests: unit_tests func_tests

coverage:
	stack test --coverage

.PHONY: xcc xrun xin all clean fclean re unit_tests func_tests tests coverage
