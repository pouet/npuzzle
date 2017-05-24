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

all: $(NAME)

$(NAME):
	gcc -dynamiclib -I/usr/include/python2.7/ -L/usr/lib/python2.7 -lpython2.7 -o $(NAME) $(SRC)
	mv $(NAME) $(NAME_SO)

fclean:
	rm -f $(NAME)
	rm -f $(NAME_SO)

re: fclean all
