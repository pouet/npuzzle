/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   link.c                                             :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: grisbour <marvin@42.fr>                    +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2017/05/24 15:22:44 by grisbour          #+#    #+#             */
/*   Updated: 2017/05/24 16:45:37 by grisbour         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include <Python.h>
#include <stdio.h>

/*
 * Function to be called from Python
 */
static PyObject* py_algo(PyObject* self, PyObject* args)
{
	char *s = "BDDBBGHHDHGBDDBBGGHDDBGGGHHDBBGHHHDBGHDDDBGGBBGHHH";
	int *tab;
	int choice = -1;
	long size;
	PyObject *lst;

	PyArg_ParseTuple(args, "Oi", &lst, &choice);
	size = PyList_Size(lst);
	tab = (int *)malloc(sizeof(int) * size);
	for (long i = 0 ; i < size; i++)
	{
		tab[i] = (int)PyLong_AsLong(PyList_GetItem(lst, i));
	}
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

