#include <stdio.h>
#include <unistd.h>
#include "y.tab.h"


#define bool int
#define true 1
#define false 0
//TODO CHANGE NODE NAMES


//File pointers 
extern FILE *yyin;
extern FILE *lexFile;
extern FILE *parseFile;
extern FILE *symbolTableFile;
extern FILE *astFile;
extern FILE * threeAddressCodeFile;

//Debug flags
extern bool parseDebug;
extern bool lexDebug ;
extern bool symbolTableDebug;
extern bool threeAddressCodeDebug;
extern bool astDebug;

//Debug info
extern int rowNum;
extern int tabSize;
extern int column;
extern char * buffer;
extern long bufferSize;

//For symbol table and parser
extern bool lookUpMode;
extern bool scanner_error;

extern int variable_offset;

extern int level;










