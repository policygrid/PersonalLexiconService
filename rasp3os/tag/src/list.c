/******************************************************************************
 * Copyright 1995, 2002, 2006, 2011 John Carroll, Guido Minnen,               *
 *                                  Larry Piano, David Elworthy               *
 *                                                                            *
 * This file is part of RASP.                                                 *
 *                                                                            *
 * RASP is free software: you can redistribute it and/or modify it            *
 * under the terms of the GNU Lesser General Public License as published      *
 * by the Free Software Foundation, either version 3 of the License, or       *
 * (at your option) any later version.                                        *
 *                                                                            *
 * RASP is distributed in the hope that it will be useful,                    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *
 * GNU Lesser General Public License for more details.                        *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public License   *
 * along with RASP.  If not, see <http://www.gnu.org/licenses/>.              *
 ******************************************************************************

   Statistical labeller: sorted list handler.

   2-1-92	Created

   Principal external functions:
	list_search
	list_add
	list_search_and_add
	list_search_and_check

   Functions for handling sorted linked lists. The basic idea is to adopt
   data structures to have a next pointer as their first field, and to
   compare on the object that follows according to a user supplied ordering
   function, which is passed pointers to such objects and returns <= 0, 0
   or >= 0 is the same way that strcmp etc. would.

   All objects and entries in the list are passed as pointers, cast to
   (void *). It is assumed that the "next" pointer is the first element of a
   member of a list, and the comparison field follows. Compare functions are
   called with pointers to two comparison fields.
*/

#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "list.h"

/* The archetypal list structure. NB! Code here must never find the address of
the key field by saying &(l->key) because if the actual key field is larger
than a standard Key it does not work with some compilers. Instead use l + 1.
*/
typedef struct list_st ListSt, *List;
struct list_st
{
    List next;
    Key  key;    /* Hook for start of key object */
};

/*---------------------------------------------------------------------------
    list_search

    Search the list for the given object. head is the location of the head
    of the list. compar is called with the a pointer to key and a pointer
    to a key field from the list, and returns -1, 0 or 1 in the manner of
    strcmp. If the object is found, a pointer to it is returned and the
    found flag is set to TRUE. Otherwise, a pointer to the preceding member
    of the list is returned, with a flag of FALSE. In either case, a pointer
    to the next element is also returned. For an insert at the head of the
    list, the pointer to the preceding member is NULL.
----------------------------------------------------------------------------*/

void *list_search(void *head, Key *key, List_Cmp *compar, void **next,
		  BOOL *match)
{
    List l, prev;

    *match = FALSE; /* Default */

    /* Scan the list */
    for (prev = NULL, l = (List)head ; l != NULL ; l = l->next)
    {
	/* Test for match */
	int cmp;
	cmp = compar(key, (Key *)(((char *)l) + sizeof(List)));
	if (cmp == 0)  /* Exact match */
	{
	    *match = TRUE;
	    if (next) *next = l->next;
	    return l;
	}
	else if (cmp < 0) /* Element at l is greater than key */
	{
	    if (next) *next = l;
	    return prev;
	}

	/* Maintain prev pointer */
	prev = l;
    }

    /* End of list reached */
    if (next) *next = NULL;
    return prev;
}

/*---------------------------------------------------------------------------
    list_add

    Called with a pointer to the next field of the previous element (or a
    pointer to a variable containing the address of the list), and the next
    field, this allocates space and inserts a new member into the list. The
    remaining field must be set up by the client. "text" is used for errors.
    Returns a pointer to the object created.
----------------------------------------------------------------------------*/

void *list_add(void **prev, void *next, int size, char *text)
{
    List new;

    /* Create new entry */
    Allocate(new, size, text);

    *prev = new;
    new->next = next;
    return new;
}

/*---------------------------------------------------------------------------
    list_search_and_add

    Combines the effect of the previous functions. Returns a pointer to the
    object created.
----------------------------------------------------------------------------*/

void *list_search_and_add(void **head, Key *key, List_Cmp *compar, int size,
			  char *text, BOOL *match)
{
    List found, next, new;

    /* Look for the object */
    found = list_search(*head, key, compar, (void *)&next, match);
    if (*match) return found;

    /* Create the new space */
    if (found == NULL)
	new = list_add(head, next, size, text);
    else
	new = list_add((void **)(&(found->next)), next, size, text);

    return new;
}

/*---------------------------------------------------------------------------
    list_search_and_check

    Searches, adds and checks for duplicates.
----------------------------------------------------------------------------*/

void *list_search_and_check(void **head, Key *key, List_Cmp *compar, int size,
			    char *text, char *text1)
{
    BOOL match;
    void *new;

    new = list_search_and_add(head, key, compar, size, text, &match);

    if (match)
    {
	fprintf(stderr, "Duplicate entry %s in %s\n", 
				(text1 != NULL) ? text1 : "", text);
	return NULL;
    }
    else
	return new;
}
