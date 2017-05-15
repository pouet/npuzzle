/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   Person.hpp                                         :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: grisbour <marvin@42.fr>                    +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2017/05/15 09:49:07 by grisbour          #+#    #+#             */
/*   Updated: 2017/05/15 10:14:05 by grisbour         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

class Person {
	public:
		Person(int age) : _age(age) {}
		int		_age;
		bool operator<(Person &rhs) {
			return this->_age < rhs._age;
		}
};

class Compare {
	public:
		bool operator() (Person & lhs, Person & rhs) {
			return lhs._age < rhs._age;
		}
};
