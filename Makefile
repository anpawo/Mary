##
## EPITECH PROJECT, 2024
## Makefile
## File description:
## root
##

NAME	    =	glados

BIN_PATH 		= 	$(shell stack path --local-install-root)

BIN_EXE		= 	/bin/glados-exe

all :
		stack build
		cp $(BIN_PATH)$(BIN_EXE) $(NAME)

clean :
	stack clean

fclean : clean
	rm -f $(NAME)
	find -name '*~' -delete -o -name '#*#' -delete

re : fclean all

tests_run:
	stack test

.PHONY: re clean fclean all tests_run coverage
