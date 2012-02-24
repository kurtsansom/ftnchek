#ifndef TYPES_H
#define TYPES_H

#ifdef DTYPE
#define DTYPE_SHARED
#else
#define DTYPE_SHARED extern
#endif

		/* interface for derived type definitions */

/* Implementation allows type numbers to be 12 bits. Hence 2^12 = 4096 */
#define MAX_DTYPES 4096

int dtype_table_top; 	/* starting index in Dtype_table */

typedef struct DerivedTypeComponent {
  int id;
  char *name;			/* useless for derived types */
  InfoUnion info;
  type_t type;
  unsigned long array_dim;	/* array size and no. of dims */
  long size;			/* Size of object in bytes */
  	/* flags */
  unsigned
	array: 1,		/* is an array */
	pointer: 1,		/* has POINTER attribute */
  	private: 1;		/* has PRIVATE attribute */
} DtypeComponent;

typedef struct DtypeTableEntry {
  /* fields for root node of derived type */
  char *name;			/* name of derived type */
  int num_components;
  DtypeComponent *components;
  /* end of fields for root node */
  LINENO_t line_declared;
  short file_declared;
  unsigned
  	private: 1,		/* has PRIVATE statement */
	sequence: 1;		/* has SEQUENCE statement */
} Dtype;

DTYPE_SHARED
Dtype *dtype_table[MAX_DTYPES];	/* stores derived type defs */

int find_Dtype(char *name);	/* linearly search for derived type 
				   definition with same name */
void process_dtype_components(char *name);
void def_dtype(Token *id);

int print_dtypes();

#endif
