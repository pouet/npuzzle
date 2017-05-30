# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: grisbour <marvin@42.fr>                    +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2017/05/24 15:35:30 by grisbour          #+#    #+#              #
#    Updated: 2017/05/30 09:53:28 by grisbour         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

NAME_SO = algo.so

SRC = link.c

INCLUDES = -I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/swift-migrator/sdks/MacOSX.sdk/System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7/ -I"`ocamlc -where`"
LIB_PATH = -L"`ocamlc -where`" -L/usr/lib/python2.7
LIBS = -lpython2.7 -lunix -lnums -lcamlstr -lbigarray -lasmrun_pic -lm -ldl

all: $(NAME_SO)

$(NAME_SO):
	ocamlfind ocamlopt -fPIC -output-obj -package batteries -linkpkg ml_part.ml -o ml_part_obj.o 
	gcc -fPIC -c $(INCLUDES) -o link.o link.c
	gcc -dynamic -Wl,-no_compact_unwind -o $(NAME_SO) $(INCLUDES) $(LIB_PATH) $(LIBS) link.o ml_part_obj.o

clean:
	rm -f *.cm*
	rm -f *.o

fclean: clean
	rm -f $(NAME_SO)

re: fclean all

.PHONY: all clean fclean re
