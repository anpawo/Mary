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
	@ stack build
	@ cp $(BIN_PATH)$(BIN_EXE) $(NAME)

clean :
	@ stack clean

fclean: clean
	@ $(RM) $(NAME)

re: fclean
re: all

tests_run:
	stack test

.PHONY: functional-tests
functional-tests: all
	@ vangelis ./test/func-test/*.toml --diff

.PHONY: unit-tests
unit-tests:
	stack test glados:glados-test

.PHONY: coverage
coverage:
	stack test glados:glados-test --coverage

test: unit-tests
test: functional-tests

.PHONY: re clean fclean all tests_run
