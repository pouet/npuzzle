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

#include <stdio.h>
#include <Python.h>

#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/printexc.h>

char *hello_closure(int *tab, int size, int choice)
{
	char * args = NULL;
	static value * closure_f = NULL;

	caml_startup(&args);

	if (closure_f == NULL) {
		closure_f = caml_named_value("Hello callback");
	}
//	caml_callback(*closure_f, Val_unit);
//  caml_callback(*closure_f, Val_int(42));
	value toto = caml_alloc(size, size);
	for (int i = 0; i < size; i++) {
		Store_field(toto, i, Val_int(tab[i]));
	}
	value res = caml_callback2(*closure_f, toto, Val_int(choice));

	puts(String_val(res));
	return String_val(res);
}

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
	hello_closure(tab, (int)size, choice);
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

