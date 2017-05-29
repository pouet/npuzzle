# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: grisbour <marvin@42.fr>                    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2017/05/24 15:35:30 by grisbour          #+#    #+#              #
#    Updated: 2017/05/24 15:37:18 by grisbour         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

NAME = algo.dylib
NAME_SO = algo.so

SRC = link.c

INCLUDES = -I/usr/include/python2.7/ -I"`ocamlc -where`"
LIBS = 

all: $(NAME_SO)

$(NAME_SO):
	# ocamlfind ocamlc -package batteries -linkpkg -o toto main.ml
	# ocamlfind ocamlopt -verbose -fPIC -package batteries -linkpkg -output-obj ml_part.ml -o ml_part_obj.o
	ocamlfind ocamlopt -verbose -fPIC -output-obj -package batteries -linkpkg ml_part.ml -o ml_part_obj.o 
	gcc -fPIC -c $(INCLUDES) -L/usr/lib/python2.7 -lpython2.7 -o link.o link.c
	gcc -shared -o algo.so $(INCLUDES) -L"`ocamlc -where`" -L/usr/lib/python2.7 -lpython2.7 link.o ml_part_obj.o -lunix -lnums -lcamlstr -lbigarray -lasmrun_pic -lm -ldl
#	gcc -dynamiclib -I/usr/include/python2.7/ -L/usr/lib/python2.7 -lpython2.7 -o $(NAME) $(SRC)
#	mv $(NAME) $(NAME_SO)

fclean:
#	rm -f $(NAME)
	rm -f $(NAME_SO)
	rm -f *.cm*
	rm -f *.o

re: fclean all
