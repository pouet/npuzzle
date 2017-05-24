/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   link.c                                             :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: grisbour <marvin@42.fr>                    +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2017/05/24 15:22:44 by grisbour          #+#    #+#             */
/*   Updated: 2017/05/24 15:34:28 by grisbour         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include <Python.h>

/*
 * Function to be called from Python
 */
static PyObject* py_algo(PyObject* self, PyObject* args)
{
	char *s = "BDDBBGHHDHGBDDBBGGHDDBGGGHHDBBGHHHDBGHDDDBGGBBGHHH";
	return Py_BuildValue("s", s);
}


/*
 * Bind Python function names to our C functions
 */
static PyMethodDef myModule_methods[] = {
	{"py_algo", py_algo, METH_VARARGS},
	{NULL, NULL}
};

/*
 * Python calls this to let us initialize our module
 */
void initalgo()
{
	(void) Py_InitModule("algo", myModule_methods);
}

