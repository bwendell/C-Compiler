#include "driver.h"
#include <stdlib.h>


//Creates symbol table (stack) by setting the top to null
node * CreateTable(node * top){

  top = NULL;
  return top;

}

//Pushes level onto top of symbol table
node * PushLevel(node * top, int data){

  //If this table has no entries
  if (top == NULL){   
    //Allocate Memory for new node
    top =(struct node *)malloc(1*sizeof(struct node));
    
    //Set top's pointer to NULL as there are no other entries in the list      
    top->ptr = NULL;

    //Set the depth of the level
    top->level = data;
    
    //Initializes identifier table by setting treePtr to null
    top->treePtr = NULL;
    
    //Initializes the number of levels currently in symbol table
    top->size = 0;
      
  //There already exists node(s) in the table  
  }else {
    //Create a pointer to the node to be inserted
    node * new_node;
    
    //Allocate memory for new node
    new_node =(struct node *)malloc(1*sizeof(struct node));
    
    //Link the list to the top of symbol table
    new_node->ptr = top;
    
    //Set level value
    new_node->level = data;
    
    //Set top as the newly inserted node
    top = new_node;
  }
  //Increment size by 1
  top->size++;
  
  return top;
   
}

//Pops off the top node of symbol table
node * PopLevel(node * top){

  //Initialize our cursor
  node * stack_holder;
  
  //Set our cursor to top node
  stack_holder = top;

  //Nothing to pop
  if (stack_holder == NULL){
      return NULL;
  }else {
  //Set stack holder to the soon to be top node
  stack_holder = stack_holder->ptr;
  
  //Delete the identifiers of the tree #TODO
  
  //Deallocate memory for top
  free(top);
  
  //Set the new top to stack_holder
  top = stack_holder;
  
  //Decremenet size
  top->size--;
  
  }
  
  //Return the modified table
  return top;
  
}

/***************************************/


//Prints identifiers of a level in order
void PrintIdentifiers(treeNode *leaf){

  //Nothing to print
  if(leaf==NULL){
    return;
  }

  //Print prior identifiers
  PrintIdentifiers(leaf->left);

  //Check Flags and print proper types/storage containers
  if(leaf->flags.const_flag){fprintf(symbolTableFile,"const ");}
  if(leaf->flags.static_flag){fprintf(symbolTableFile,"static ");}
  if(leaf->flags.short_flag){fprintf(symbolTableFile,"short ");}
  if(leaf->flags.long_flag){fprintf(symbolTableFile,"long ");}
  if(leaf->flags.float_flag){fprintf(symbolTableFile,"float ");}
  if(leaf->flags.double_flag){fprintf(symbolTableFile,"double ");}
  if(leaf->flags.signed_flag){fprintf(symbolTableFile,"signed ");}
  if(leaf->flags.unsigned_flag){fprintf(symbolTableFile,"unsigned ");}
  if(leaf->flags.struct_flag){fprintf(symbolTableFile,"struct ");}
  if(leaf->flags.enum_flag){fprintf(symbolTableFile,"enum ");}
  if(leaf->flags.typedef_flag){fprintf(symbolTableFile,"typedef ");}
  if(leaf->flags.char_flag){fprintf(symbolTableFile,"char ");}
  if(leaf->flags.int_flag){fprintf(symbolTableFile,"int ");}
  if(leaf->flags.volatile_flag){fprintf(symbolTableFile,"volatile ");}
  if(leaf->flags.void_flag){fprintf(symbolTableFile,"void ");}

  //Print the identifier
  fprintf(symbolTableFile,"%s Declared on Line # %d ",leaf->name, leaf->declerationLineNumber);

  //Print character value if avaliable
  if(leaf->array_flag){
      fprintf(symbolTableFile,"Array Size Of: %d\n",leaf->array_size);
  }
  else{
      fprintf(symbolTableFile,"Non Array\n");
  }

  //Print the follwing identifiers
  PrintIdentifiers(leaf->right);
}

treeNode * insertIdentifier(treeNode *leaf,int data, flagContainer flag_info){
      
//We have arrivied in the proper location or first identifer to be inserted
if(leaf==NULL){
  //Create our new id poniter
  treeNode *new_id;

  //Allocate memory for new id
  new_id = (treeNode *)malloc(sizeof(treeNode));

  //Set the Key
  new_id -> id = data;

  //Set the flags according to flag container object
  new_id -> left = new_id -> right = NULL;
  new_id->flags.const_flag = flag_info.const_flag;
  new_id->flags.static_flag = flag_info.static_flag;
  new_id->flags.short_flag = flag_info.short_flag;
  new_id->flags.long_flag = flag_info.long_flag;
  new_id->flags.float_flag = flag_info.float_flag;
  new_id->flags.double_flag = flag_info.double_flag;
  new_id->flags.signed_flag = flag_info.signed_flag;
  new_id->flags.unsigned_flag = flag_info.unsigned_flag;
  new_id->flags.struct_flag = flag_info.struct_flag;
  new_id->flags.enum_flag = flag_info.enum_flag;
  new_id->flags.typedef_flag = flag_info.typedef_flag;
  new_id->flags.char_flag = flag_info.char_flag;
  new_id->flags.int_flag = flag_info.int_flag;
  new_id->flags.auto_flag = flag_info.auto_flag;
  new_id->flags.register_flag = flag_info.register_flag;
  new_id->flags.extern_flag = flag_info.extern_flag;
  return new_id;
}
  //Go right for proper location
  if(data >(leaf->id)){
      leaf->right = insertIdentifier(leaf->right,data, flag_info);
  //Go left for proper location        
  } else if(data < (leaf->id)){
      leaf->left = insertIdentifier(leaf->left,data, flag_info);
  }
      
    //Return the identifier tree
    return leaf;

}

/*
//Delete the specified element in the identifier tree
treeNode * DeleteIdentifier(treeNode *leaf, int data){
      //Create variable for delete node
      treeNode *delete_id;
      
      //Element does not exist in the tree
      if(leaf==NULL){
              printf("Element Not Found");
      //Go left for proper location
      }else if(data < leaf->id){
              leaf->left = deleteIdentifier(leaf->left, data);
      //Go right for proper location
      }else if(data > leaf->id) {
              leaf->right = deleteIdentifier(leaf->right, data);
      }
      //We have found the element
      else{
              //The taboo case, id node has 2 children
              if(leaf->right && leaf->left){
                      //Set the delete_id
                      delete_id = leaf->right;
                      //Go left untill null
                      while(delete_id->left!= NULL){
                          delete_id = leaf->left;
                      }
                      //swap every thang :( TODO 
                      leaf -> id = delete_id->id;
                      
                      //Go right and call delete to swapped node 
                     leaf -> right = deleteIdentifier(leaf->right,delete_id->id);
              //The unsucky cases
              }else {
                      //set the delete identifier to hold place
                      delete_id = leaf;
                      
                      //Connect to right child
                      if(leaf->left == NULL){
                              leaf = leaf->right;
                      }
                      //Connect to left
                      else if(leaf->right == NULL){
                              leaf = leaf->left;
                      }
                      
                      //Delete id
                      //Delete every thing proper :(
                      free(delete_id); 
              }
      }
      return leaf;

}
*/

//Finds and returns specific identifier
treeNode * FindIdentifier(treeNode *leaf, int data){
  //Id not found 
  if(leaf==NULL){
    return NULL;
  }
  
  //Go Right
  if(data > leaf->id){
    return FindIdentifier(leaf->right,data);
  //Go Left
  } else if(data < leaf->id) {
    return FindIdentifier(leaf->left,data);
  //We have found and are returning the id
  } else{ 
    return leaf;
  }
}

//Prints out our symbol table
node * PrintTable(node * top){
  
  //Create cursor variable and set to top
  node * cursor;
  cursor = top;

  //No Table to Print
  if (cursor == NULL){
    return;     
  }
  //Print out contents of the table
  while (cursor != NULL){
    //Print out level
    fprintf(symbolTableFile,"%s\n", "Level:");
    fprintf(symbolTableFile,"%d\n", cursor->level);
    //Print out ids
    fprintf(symbolTableFile,"%s\n", "IDS at that Level:");
    PrintIdentifiers(cursor->treePtr);
    //Iterate to next level
    cursor = cursor->ptr;
  }
  
  
}

//Check the table when inserting 
treeNode* Shadows(int id, node * top){
  
  //Create and set cursor to top
  node * cursor;
  cursor = top->ptr;
  
  //Nothing to search for
  if(cursor == NULL){
      return NULL;     
  }
  
 //Check every level's identifiers to make sure id is
 //not already declared 
 while (cursor != NULL){
    //If the id is found to shadow, print a warning
    //and return the identifier pointer
    if( FindIdentifier(cursor->treePtr, id ) != NULL){
      printf("Herfeee");
      treeNode * temp = FindIdentifier(cursor->treePtr, id );
      if(!temp->function_flag && top->level == top->size){
        printf("\t Warning: Variable shadows %s in level %d\n", temp->name, cursor->level );
      }

      return cursor->treePtr;
    }
    //Continue through levels
    cursor = cursor->ptr;

  }
  //Id does not shadow, return null
  return NULL;

}

//Deletes an id tree
treeNode * DeleteTree(treeNode *leaf){

  //Check to make sure not already empty
  if(leaf == NULL){
    return NULL;
  }
  //Post order delete so children are deleted before parents
  if(leaf->left !=NULL) {
    return DeleteTree(leaf->right);
  }
  if(leaf->right !=NULL){
    return DeleteTree(leaf->right);
  }

  //Deallocate the memory
  // TODO Deallocate all other pointers in the node prior
  free(leaf);
  leaf = NULL;
  
  return NULL;

}

//Check the table when inserting 
treeNode* FindIDTableScope(int id, node * top){
  
  //Create and set cursor to top
  node * cursor;
  cursor = top;
  

  
 //Check every level's identifiers to make sure id is
 //not already declared 
 while (cursor != NULL){
    //If the id is found, print found
    //and return the identifier pointer
    if( FindIdentifier(cursor->treePtr, id ) != NULL && cursor->level == 0){
      treeNode * temp = FindIdentifier(cursor->treePtr, id);
      return temp;
    }
    //Continue through levels
    cursor = cursor->ptr;

  }
  //Id does not shadow, return null
  return NULL;

}

//Destroys the symbol table
node * DestroyTable(node * top){
  //Set cursor to go through and delete nodes
  node * cursor;
  
  //Make sure there are nodes to delete
  if(top != NULL){
      //Set cursor to top
      cursor = top;
      
      //Deallocated memory for every
      while (cursor != NULL){
        DeleteTree(cursor->treePtr);
        cursor = cursor->ptr;
        free(top);
        top = cursor;
    
      }
        free(cursor);
        top = NULL; 
          
  }
  //Return top
  return top; 
}

