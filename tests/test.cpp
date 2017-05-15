/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   test.cpp                                           :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: grisbour <marvin@42.fr>                    +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2017/05/15 09:41:27 by grisbour          #+#    #+#             */
/*   Updated: 2017/05/15 10:14:13 by grisbour         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include <queue>
#include <vector>
#include <iostream>
#include <functional>

#include "Person.hpp"

int		main(void) {
	std::priority_queue<Person, std::vector<Person>, Compare > pq;

	pq.push(Person(10));

	std::cout << "Top element : " << pq.top()._age << std::endl;
	return 0;
}
