#ifndef TYPES_H
#define TYPES_H

		/* interface for derived type definitions */

/* Implementation allows type numbers to be 12 bits. Hence 2^12 = 4096 */
#define MAX_DTYPES 4096


typedef struct DerivedTypeComponent {
  int id;
  char *name;			/* not needed for derived types */
  Type type;
  unsigned long array_dim;	/* array size and no. of dims */
  long size;			/* Size of object in bytes */
  	/* flags */
  unsigned
  	private: 1,		/* has PRIVATE attribute */
	pointer: 1;		/* has POINTER attribute */
} DtypeComponent;

typedef struct DtypeTableEntry {
  /* fields for root node of derived type */
  char *name;			/* name of derived type */
  int num_components;
  DtypeComponent *components;
  /* end of fields for root node */
  LINENO_t line_declared;
  short file_declared;
} Dtype;

Dtype *dtype_table[MAX_DTYPES];	/* stores derived type defs */

/* not needed */
typedef struct DtypeScopeEntry {
  int Dtypetab_index;		/* array index in Dtype_table */
  int hash_num;			/* hash number for current scoping unit */
} Dtypescope;

Dtypescope dtype_scope[MAXSCOPES]; /* stores local Dtypes info */
int curr_dtype_bottom;		/* first Dtype entry of current scope */
int dtype_scope_top;		/* next slot in Dtype scope stack */
/**************/

		/* routines for derived type definitions */
/* 0 <= dtype_start < loc_symtab_top
 * dtype_start is the index where local symbol entry for derived type
 * is entered. */
void new_Dtype(int dtype_start); /* create a derived type definition */
int find_Dtype(char *name);	/* linearly search for derived type 
				   definition with same name */
void get_dtype_components(char *name);
void def_dtype(Token *id);

#endif
