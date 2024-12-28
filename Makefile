##
## EPITECH PROJECT, 2024
## Makefile
## File description:
## root
##

NAME	    =	glados

BIN_PATH 	= 	$(shell stack path --local-install-root)

BIN_EXE		= 	/bin/glados-exe

all:
	stack build
	cp $(BIN_PATH)$(BIN_EXE) $(NAME)

clean :
	@ stack clean

fclean: clean
	@ $(RM) $(NAME)

re: fclean
re: all

tests_run:
	stack test

.PHONY: functionnal-tests
functionnal-tests:
	vangelis ./test/tests.toml --diff

.PHONY: unit-tests
unit-tests:
	stack test glados:glados-test

.PHONY: coverage
coverage:
	stack test glados:glados-test --coverage

test: re
test: unit-tests
test: functionnal-tests

.PHONY: re clean fclean all tests_run
