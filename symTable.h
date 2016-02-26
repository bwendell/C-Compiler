#ifndef SYMTAB_H
#define SYMTAB_H

//Any type definitions needed
typedef struct node
{
    int size;
    int level;
    struct treeNode *treePtr;
    struct node *ptr;
} node;

typedef struct FunctionInfo{

    int number_arguments;


}functionInfo;
//Flag Container
typedef struct FlagTypes
{

    //Type Flags
    bool void_flag;
    bool short_flag;    
    bool bool_flag;
    bool long_flag;
    bool float_flag;
    bool double_flag;  
    bool signed_flag;
    bool unsigned_flag;   
    bool struct_flag;
    bool enum_flag;
    bool typedef_flag;
    bool char_flag;
    bool int_flag;
    bool volatile_flag;


    //Storage Flag	
    bool auto_flag;
    bool register_flag;
    bool static_flag;
    bool extern_flag;
    bool const_flag;   

        
}flagContainer;

//BST Tree Structure
typedef struct treeNode
{
    //Type and storage information
    flagContainer  flags;
    bool function_flag;
    functionInfo * functionInfo;      
    //Data Containers
    int * dataI;
    float dataD;
    char * dataC;


    int array_size;
    int array_size2;
    bool array_flag;
    int offset;
    
    int declerationLineNumber; 
    int id;
    char * name;
    
    
    struct treeNode *left;
    struct treeNode *right;
    
}treeNode;

//Sybmol Table
extern node * symbolTable;
extern treeNode* currentIdentifier;
extern flagContainer flags;
extern flagContainer reset;


/* Symbol Table Function Headers */

node * CreateTable(node * statck);

node * PushLevel(node * statck, int data);

node * PopLevel(node * statck);

node * DestroyTable(node * statck);

node * PrintTable(node * top);



/* IdentifierTree Function Headers */

void PrintIdentifiers(treeNode *leaf);

treeNode * InsertIdentifier(treeNode *leaf,int data, flagContainer flagInfo);

treeNode * DeleteIdentifier(treeNode *leaf,int data);

treeNode * FindIdentifier(treeNode *leaf,int data);

treeNode * DeleteTree(treeNode *leaf);

treeNode* Shadows(int id, node * top);

treeNode* FindIDTableScope(int id, node * top);

#endif
