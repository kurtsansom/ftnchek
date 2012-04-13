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



DTYPE_SHARED
int dtype_table_top 	/* starting index in Dtype_table */
#ifdef DTYPE
=MIN_DTYPE_ID	/* initialize to first slot after elem types */
#endif
;

typedef struct DerivedTypeComponent {
  char *name;			/* component name (not in symtab) */
  type_t type;			
  unsigned long array_dim;	/* array size and no. of dims */
  long size;			/* Size of object in bytes */
  	/* flags */
  unsigned
	array: 1,		/* is an array */
	pointer: 1,		/* has POINTER attribute */
  	private: 1,		/* has PRIVATE attribute */
	allocatable: 1;		/* has ALLOCATABLE attribute */
} DtypeComponent;

typedef struct DtypeTableEntry {
  /* fields for root node of derived type */
  char *name;			/* name of derived type */
  char *module_name;		/* name of module declaring it, NULL if none */
  int num_components;
  DtypeComponent *components;
  /* end of fields for root node */
  LINENO_t line_declared;
  char *filename;
  Lsymtab *symt;		/* current symbol table entry */
  unsigned
  	public: 1,		/* is PUBLIC type */
  	private: 1,		/* is PRIVATE type */
	private_components: 1,  /* has PRIVATE stmt within def */
	sequence: 1;		/* has SEQUENCE statement */
} Dtype;

DTYPE_SHARED
Dtype *dtype_table[MAX_DTYPES];	/* stores derived type defs */

int find_dtype(Token *t, int in_dtype_def);
Lsymtab * def_dtype(int h, int tok_line_num, int tok_col_num, int access_spec, int dtype_def);  /* store derived type definition name in local
    symbol table */
void print_dtypes(Lsymtab *sym_list[], int n, int *need_key);  /* print names of derived type definitions from 
    dtype_table */
void privatize_components(const char *name);
void process_dtype_components(const char *name);
void ref_component(Token *comp, Token *result, int lvalue);
void ref_component_tree(Token *comp, Token *result, int lvalue);
const Token *ultimate_component(const Token *t);
int find_type_use_assoc(const char *name, const char *module_name);

#endif
