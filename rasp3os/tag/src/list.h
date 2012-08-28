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
   4-2-92	Key types changed.

   See list.c for the sort of structures that must be used with the list
   handler.
*/

/* Type for a key - cast the actual key to this. In a list structure, the
second field has type Key. The comparison function is called with a pointer to
this field and with the object passed as "key" to the list_ functions. The
first argument is always the given key, the second the one from the list. */
typedef int *Key;

/* Type for a comparison function on list members. */
typedef int (List_Cmp)(Key *, Key *);

/* Find a member in a list */
extern void *list_search(void *head, Key *key, List_Cmp *compar, void **next,
		BOOL *match);

/* Add a member to a list */
extern void *list_add(void **prev, void *next, int size, char *text);

/* Find and add if necessary */
extern void *list_search_and_add(void **head, Key *key, List_Cmp *compar,
				 int size, char *text, BOOL *match);

/* Find and add, giving error on duplicate. Return NULL on error */
extern void *list_search_and_check(void **head, Key *key, List_Cmp *compar,
				   int size, char *text, char *text1);
