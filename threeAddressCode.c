#include "driver.h"
#include "nodes.h"
#include <stdlib.h>

// global counters
int level_counter = 0;
int label_counter = 0;

/*
	Generate 3AC code for root node. This function will go recursively through
	the tree and generate the 3AC for each node. This function is called 
	in driver.c, and it is passed the root node directly after it has been
	constructed.

	The c code is printed out as each node is reached if there is source code
	to output. The source code is held in a buffer, and the line number is
	incremented when each line is output.
*/
void GenerateThreeAddressCode(TranslationUnitNode * ast){

  // check to see if there is self recursion necessary
  if(ast->external_declaration != NULL && ast->translation_unit != NULL ){
  
      GenerateThreeAddressCode(ast->translation_unit);
      ThreeAddrExternalDeclaration(ast->external_declaration);
      
  } 
  // else generate code recursively
  else if(ast->external_declaration != NULL){
  
      ThreeAddrExternalDeclaration(ast->external_declaration);
  
  } 



}

// Generates 3AC for External Declaration
void ThreeAddrExternalDeclaration(ExternalDeclarationNode * ast){
    
  // check to see if there is function definitions
  if(ast->function_definition != NULL){
  
  ThreeAddrFunctionDefinition(ast->function_definition);

  } 
  // else move directly to declarations
  else if(ast->declaration != NULL){
    
    ThreeAddrDeclaration(ast->declaration);
    
  } 


}

// Generates 3AC for Declaration
ThreeAddrDeclaration(DeclarationNode * ast){

  
  // check to see if there are declarations
  if(ast->declaration_specifiers != NULL && ast->init_declarator_list != NULL){
      ThreeAddrDeclarationSpecifiers(ast->declaration_specifiers);
      ThreeAddrInitDeclaratorList(ast->init_declarator_list);
      
  }
    

}
// Generates 3AC for Declaration Specifiers
ThreeAddrDeclarationSpecifiers(DeclarationSpecifiersNode * ast){



	// check to see if there is a type specifier
    if(ast->type_specifier != NULL){
     
      TypeSpecifierNode * x;
      x = ast->type_specifier;
      
    }
  

}

// Generates 3AC for Init Declarator List
ThreeAddrInitDeclaratorList(InitDeclaratorListNode * ast){


	// check to see if there is multiple declarators
    if(ast-> init_declarator != NULL && ast-> init_declarator_list != NULL){
    
      ThreeAddrInitDeclaratorList(ast-> init_declarator_list);
      ThreeAddrInitDeclarator(ast->init_declarator);
    }
	// else there is only one
	// end self recursion
    else if(ast-> init_declarator != NULL){
    
      ThreeAddrInitDeclarator(ast->init_declarator);
    }
    
    

}

// Generates 3AC for Init Declarator
ThreeAddrInitDeclarator(InitDeclaratorNode * ast){

	// check to see if there is declarator and initializer
    if(ast-> declarator != NULL && ast-> initializer!= NULL ){
      printSource(ast->line);
      ThreeAddrDeclarator(ast->declarator);
      ThreeAddrInitializer(ast-> initializer);

      //Get Identifier Name
      DeclaratorNode *  declarator_temp;
      declarator_temp = ast->declarator;

      //Get the value or variable declarator is being initialized too
      InitializerNode  * initializer_temp = ast->initializer;
      printf("%s(%d) = _R%d\n", declarator_temp->identifier->name,declarator_temp->identifier->offset, register_count-1);
    }
    else if(ast-> declarator != NULL){
    
      ThreeAddrDeclarator(ast->declarator);
      
    }
    

} 

// Generates 3AC for Declarator
ThreeAddrDeclarator(DeclaratorNode * ast){

	// check to see if there is a direct declarator
  if(ast->direct_declarator != NULL){
  
   ThreeAddrDirectDeclarator(ast->direct_declarator);

  }
    



}

// Generates 3AC for Direct Declarator
void ThreeAddrDirectDeclarator(DirectDeclaratorNode * ast){


 
	// else there is an expression
  if(ast->constant_expression != NULL && ast->direct_declarator != NULL){

    
        ThreeAddrDirectDeclarator(ast->direct_declarator);

		// handle it if it's an array
        if(!ast->array_flag){
                ThreeAddrConstantExpression(ast->constant_expression);

          }

     
        
        
        
    
    }

    else if(ast->direct_declarator != NULL && ast->parameter_type_list != NULL){
          ThreeAddrDeclarator(ast->direct_declarator);


    }


}

// Generates 3AC for Function Definition
void ThreeAddrFunctionDefinition(FunctionDefinitionNode * ast){


	// check to see if there is a declaration specifiers, declarator, and compound statement
  if(ast-> declaration_specifiers != NULL && ast->declarator != NULL && ast-> compound_statement != NULL ){
    DeclaratorNode * temp = ast->declarator;
    printf("%s: \n", temp->identifier->name);
    printf("Stack Pointer : %d\n", variable_offset - 4);
	// do each production recursively
    ThreeAddrDeclarationSpecifiers(ast-> declaration_specifiers);
    ThreeAddrDeclarator(ast->declarator );
    ThreeAddrCompoundStatement(ast->compound_statement);

  }



}

// Generates 3AC for Compound Statement
void ThreeAddrCompoundStatement(CompoundStatementNode * ast){

	// check to see if there is a declaration list and statement list
   if(ast-> declaration_list != NULL && ast->statement_list != NULL){
        ThreeAddrDeclarationList(ast->declaration_list );
        ThreeAddrStatementList(ast->statement_list);
  
    }
   // else if there is a declaration list
   else if(ast-> declaration_list != NULL){
    
    ThreeAddrDeclarationList(ast->declaration_list );
  
   }
   // else if there is a statement list
   else if(ast-> statement_list != NULL){
    
    ThreeAddrStatementList(ast->statement_list );
  
   }


}

// Generates 3AC for Statement List
void ThreeAddrStatementList(StatementListNode * ast){
  
	// check to see if there is a statement list and statement list
  if(ast->statement_list != NULL && ast->statement_list != NULL){

    ThreeAddrStatementList(ast->statement_list);
    ThreeAddrStatement(ast->statement);

  }else if(ast->statement != NULL){
  
    ThreeAddrStatement(ast->statement);
  
  }




}

// Generates 3AC for Statement
void ThreeAddrStatement(StatementNode * ast){

	// check to see if there is a selection statement
  if(ast-> selection_statement != NULL){
  
    ThreeAddrSelectionStatement(ast->selection_statement);
    
  
  }
  // check to see if there is a expression statement
  else if(ast-> expression_statement != NULL){
  
    ThreeAddrExpressionStatement(ast->expression_statement);
    
  
  }
  // check to see if there is a compound statement
  else if(ast->compound_statement != NULL){
  

    ThreeAddrCompoundStatement(ast->compound_statement);
  
  }
  // check to see if there is a iteration statement
  else if(ast->iteration_statement != NULL){
  
    ThreeAddrIterationStatement(ast->iteration_statement);
  
  }



}

// Generates 3AC for Expression Statement
void ThreeAddrExpressionStatement(ExpressionStatementNode * ast){

	// check to see if there is a expression
  if(ast-> expression != NULL){
  
    ThreeAddrExpression(ast-> expression);
    
  
  }
  
 


}

// Generates 3AC for Declaration List
void ThreeAddrDeclarationList(DeclarationListNode * ast){


	// check to see if there is a declaration and declaration list
  if(ast-> declaration != NULL && ast->declaration_list != NULL){
  
    ThreeAddrDeclarationList(ast->declaration_list);
    ThreeAddrDeclaration(ast->declaration);
  
  
  }
  // else no self recursion
  else if(ast-> declaration != NULL){
  
    ThreeAddrDeclaration(ast->declaration);
  
  
  }
  
  
  
   

}

// Generates 3AC for Constant Expression
void ThreeAddrConstantExpression(ConstantExpressionNode * ast){

	// check to see if there is a conditional expression
  if(ast->conditional_expression != NULL){
  
    ThreeAddrConditionalExpression(ast->conditional_expression);
  
  }








}

// Generates 3AC for Selection Statement
void ThreeAddrSelectionStatement(SelectionStatementNode *ast){
  



  //selection_statement <- IF '(' expression ')' statement
  if(ast->expression != NULL && ast->statement != NULL){
    ExpressionNode * temp = ast->expression;
    printSource(temp->line);
    label_counter++;
    int label_holder;

    ThreeAddrExpression(ast->expression);
    
    printf("_L%d:\n", label_counter++ );
    label_holder = label_counter;
    printf("Branch if not R%d: _L%d \n", register_count-1, label_counter++);
    ThreeAddrStatement(ast->statement);
    printf("_L%d:\n", label_holder);
   
  }
  
 

}

// Generates 3AC for Expression
void ThreeAddrExpression(ExpressionNode * ast){

	// check to see if there is a assignment expression
  if(ast-> assignment_expression != NULL){
  
    ThreeAddrAssignmentExpression(ast-> assignment_expression);
  
  }
  
  


}

// Generates 3AC for Assignment Expression
void ThreeAddrAssignmentExpression(AssignmentExpressionNode * ast){


	// check to see if there is a unary expression and assignment expression
  if(ast-> unary_expression != NULL  && ast->assignment_expression != NULL){
    printSource(ast->line);
    ThreeAddrUnaryExpressionNode(ast->unary_expression);
    int register_holder = register_count;
    ThreeAddrAssignmentExpression(ast->assignment_expression);
    UnaryExpressionNode * unary_expression_temp = ast->unary_expression;
    AssignmentExpressionNode * assignment_expression_temp = ast->assignment_expression;
    UnaryExpressionNode  * unary_temp = ast-> unary_expression ;
    static int first = 1;

    // edit
    PostfixExpressionNode * postfix_expression_temp;
    postfix_expression_temp = unary_expression_temp->postfix_expression;

	// constant in left side
    if( assignment_expression_temp->constant != NULL ){
      if (postfix_expression_temp->primary_expression == NULL){
        // printf("arrays\n");
        printf("&(_R%d) = _R%d\n", register_holder-1 , register_count-1);
      }
      else if (postfix_expression_temp->primary_expression != NULL ){
        // printf("not arrays, regular assignment\n");
        printf("%s ( %d ) = _R%d\n", unary_temp ->identifier->name , unary_temp->identifier->offset,register_count-1);
      }
    }
	// variable in left side
    else if(assignment_expression_temp->constant == NULL) {
      if (postfix_expression_temp->primary_expression == NULL){
        // printf("arrays\n");
        if(assignment_expression_temp->identifier != NULL) {
          if(assignment_expression_temp-> identifier->array_flag) {
            printf("&(_R%d) = _R%d\n", register_holder-1 , register_count-1);
            first = 0;
          }
          else {
            printf("&(_R%d) = %s(%d)\n", register_holder-1 , assignment_expression_temp->identifier->name,assignment_expression_temp->identifier->offset );
            first = 1;
          }
        }
	  }
      else if (postfix_expression_temp->primary_expression != NULL ){
        // printf("not arrays, regular assignment\n");
        printf("%s(%d) =  R%d\n", unary_temp ->identifier->name ,unary_temp ->identifier->offset, register_count-1);
      }
    }
  }
  else if(ast-> conditional_expression  != NULL){
  
    ThreeAddrConditionalExpression(ast->conditional_expression);
  
  }

}

// Generates 3AC for Conditional Expression
void ThreeAddrConditionalExpression(ConditionalExpressionNode * ast){

	// check to see if there is a logical or expression
  if(ast-> logical_or_expression  != NULL){
  
    ThreeAddrLogicalOrExpression(ast->logical_or_expression);
   
  }
    


}

// Generates 3AC for Logoical Or Expression
void ThreeAddrLogicalOrExpression(LogicalOrExpressionNode * ast){

	// check to see if there is a logical and expression
  if(ast-> logical_and_expression  != NULL){
  
    ThreeAddrLogicalAndExpression(ast->logical_and_expression);
   
  }
    

}

// Generates 3AC for Logical And Expression
void ThreeAddrLogicalAndExpression(LogicalAndExpressionNode * ast){

	// check to see if there is a inclusive or expression
  if(ast-> inclusive_or_expression  != NULL){
  
    ThreeAddrInclusiveOrExpression(ast->inclusive_or_expression);
   
  }
    

}

// Generates 3AC for Inclusive Or Expression
void ThreeAddrInclusiveOrExpression(InclusiveOrExpressionNode * ast){

	// check to see if there is a exclusive or expression
  if(ast-> exclusive_or_expression  != NULL){
  
    ThreeAddrExclusiveOrExpression(ast->exclusive_or_expression);
   
  }
    

}

// Generates 3AC for Exclusive Or Expression
void ThreeAddrExclusiveOrExpression(ExclusiveOrExpressionNode * ast){

	// check to see if there is an and expression
  if(ast-> and_expression  != NULL){
  
    ThreeAddrAndExpression(ast->and_expression);
   
  }
    

}

void ThreeAddrInitializer( InitializerNode * ast){
   

	// check to see if there is a initializer list
  if(ast->initializer_list != NULL){
    
    ThreeAddrInitializerList(ast-> initializer_list);
  
  }
  // check to see if there is a assignment expression
  else if(ast-> assignment_expression != NULL){
    
    ThreeAddrAssignmentExpression(ast-> assignment_expression);
  
  }
    

}


void ThreeAddrInitializerList(InitializerListNode * ast){



  //initializer_list <- initializer_list ',' initializer
  if(ast->initializer_list != NULL && ast->initializer != NULL){
    
    ThreeAddrInitializerList(ast-> initializer_list);
    ThreeAddrInitializer(ast-> initializer);
  
  }
  else if(ast->initializer != NULL){

    ThreeAddrInitializer(ast-> initializer);
  
  }
 

}



void ThreeAddrAndExpression(AndExpressionNode * ast){

	// check to see if there is a equality expression
  if(ast->equality_expression != NULL){
    
    ThreeAddrEqualityExpression(ast-> equality_expression);
  
  }
    

  
}

void ThreeAddrEqualityExpression(EqualityExpressionNode *ast){

	// check to see if there is a relational expression
  if(ast->relational_expression != NULL){
    
    ThreeAddrRelationalExpression(ast-> relational_expression);
  
  }
    

}

void ThreeAddrRelationalExpression(RelationalExpressionNode *ast){

  int register_holder;
  
  if(ast->shift_expression != NULL && ast->relational_expression != NULL){
  
    RelationalExpressionNode * relational_temp = ast->relational_expression;
    ThreeAddrRelationalExpression(ast-> relational_expression);
    register_holder = register_count;
    ThreeAddrShiftExpression(ast-> shift_expression);
    
    ShiftExpressionNode * shift_expr = ast->shift_expression;

    ShiftExpressionNode * shift_expression_temp = relational_temp->shift_expression;
    AdditiveExpressionNode * additive_expression_temp = shift_expression_temp->additive_expression;
    MultiplicativeExpressionNode * multiplicative_expression_temp = additive_expression_temp->multiplicative_expression;
    CastExpressionNode * cast_expression_temp = multiplicative_expression_temp->cast_expression;
    UnaryExpressionNode * unary_expression_temp = cast_expression_temp->unary_expression;
    PostfixExpressionNode * postfix_expression_temp = unary_expression_temp->postfix_expression;
    PrimaryExpressionNode * primary_expression_temp = postfix_expression_temp->primary_expression;


    if( primary_expression_temp == NULL ) {
      printf("_R%d = _R%d %s _R%d\n",register_count++,register_holder-1 ,ast->expression,register_count-1);
    }
    else if( primary_expression_temp != NULL) {
    
      if(relational_temp->identifier != NULL && shift_expr->identifier !=NULL ) {

         printf("_R%d = %s(%d) %s %s(%d)\n",register_count++,relational_temp->identifier->name ,relational_temp->identifier->offset, ast->expression, shift_expr->identifier->name, shift_expr->identifier->offset);
      }
      else if(relational_temp != NULL ) {
         printf("_R%d = %s(%d) %s _R%d\n",register_count++,relational_temp->identifier->name ,relational_temp->identifier->offset,ast->expression,register_count-1);
      }

   }



  }

  else if(ast->shift_expression != NULL){
    ThreeAddrShiftExpression(ast-> shift_expression);
  }
    

}

void ThreeAddrShiftExpression(ShiftExpressionNode * ast){

  if(ast->additive_expression != NULL){
    
    ThreeAddrAdditiveExpressionNode(ast-> additive_expression);
  
  }
    

}

void ThreeAddrAdditiveExpressionNode(AdditiveExpressionNode * ast){


  if(ast->multiplicative_expression != NULL && ast->additive_expression != NULL){

      printSource(ast->line);


      char operator;

      if(ast->addition) {
        operator = '+';
      }
      else {
        operator = '-';
      }
        AdditiveExpressionNode * additive_temp = ast->additive_expression;
        MultiplicativeExpressionNode * multiplicative_temp = ast->multiplicative_expression;

      int multiplicative_holder, additive_holder;

      if(additive_temp->identifier != NULL  && multiplicative_temp->identifier != NULL){

        typeCheckIdId(additive_temp->identifier,multiplicative_temp->identifier );
        ThreeAddrAdditiveExpressionNode(ast->additive_expression);
        //printf("_R%d = %s\n",register_count++, additive_temp->identifier->name );
        additive_holder = register_count;
        ThreeAddrMultiplicativeExpressionNode(ast-> multiplicative_expression);
        //printf("_R%d = %s\n",register_count++, multiplicative_temp->identifier->name );
        multiplicative_holder = register_count;

        if(additive_temp->identifier->array_flag) {
          printf("_R%d = _R%d %c _R%d\n",register_count++, additive_holder-1, operator, multiplicative_holder-1);
        }
        else {
          printf("_R%d = %s(%d) %c %s(%d)\n",register_count++, additive_temp->identifier->name,additive_temp->identifier->offset, operator, multiplicative_temp->identifier->name, multiplicative_temp->identifier->offset);
        }
      
      }else if(additive_temp->identifier != NULL){

        //typeCheck()

        ThreeAddrAdditiveExpressionNode(ast->additive_expression);
        //printf("_R%d = %s\n",register_count++, additive_temp->identifier->name );
        additive_holder = register_count;
        ThreeAddrMultiplicativeExpressionNode(ast-> multiplicative_expression);
        multiplicative_holder = register_count;


        
        if(additive_temp->identifier->array_flag) {
          printf("_R%d = _R%d %c _R%d\n",register_count++, additive_holder-1, operator, multiplicative_holder-1);
        }
        else {
          printf("_R%d = %s(%d) %c _R%d\n",register_count++, additive_temp->identifier->name, additive_temp->identifier->offset, operator, multiplicative_holder-1);
        }
      }else if(multiplicative_temp->identifier != NULL){

        ThreeAddrAdditiveExpressionNode(ast->additive_expression);
        additive_holder = register_count;
        ThreeAddrMultiplicativeExpressionNode(ast-> multiplicative_expression);
        //printf("_R%d = %s\n",register_count++, multiplicative_temp->identifier->name );
        multiplicative_holder = register_count;


        

        if(multiplicative_temp->identifier->array_flag) {
          printf("_R%d = _R%d %c _R%d\n",register_count++, additive_holder-1, operator, multiplicative_holder-1);
        }
        else {
          printf("_R%d = _R%d %c %s(%d)\n",register_count++, additive_holder-1, operator, multiplicative_temp->identifier->name, multiplicative_temp->identifier->offset);
        }
      }else{


        ThreeAddrAdditiveExpressionNode(ast->additive_expression);
        additive_holder = register_count;
        ThreeAddrMultiplicativeExpressionNode(ast-> multiplicative_expression);
        multiplicative_holder = register_count;

        printf("_R%d = _R%d %c _R%d\n",register_count++, additive_holder-1, operator, multiplicative_holder-1);
      }






  }else if(ast->multiplicative_expression != NULL){
    
    ThreeAddrMultiplicativeExpressionNode(ast-> multiplicative_expression);
  
  }
    

}

void ThreeAddrMultiplicativeExpressionNode(MultiplicativeExpressionNode * ast){

  char operator;

  if(ast->mult) {
    operator = '*';
  }
  else {
    operator = '/';
  }

  if(ast->cast_expression != NULL && ast->multiplicative_expression != NULL){

        MultiplicativeExpressionNode* multiplicative_temp = ast->multiplicative_expression;
        CastExpressionNode * cast_temp = ast->cast_expression;
        int cast_holder, multiplicative_holder;
    

        if(multiplicative_temp->identifier != NULL  && cast_temp->identifier != NULL){

          //typeCheck()
          ThreeAddrMultiplicativeExpressionNode(ast->multiplicative_expression);
          //printf("_R%d = %s\n",register_count++, multiplicative_temp->identifier->name );
          multiplicative_holder = register_count;
          ThreeAddrCastExpressionNode(ast->cast_expression);
          //printf("_R%d = %s\n",register_count++, cast_temp->identifier->name );
          cast_holder = register_count;

          

          if(multiplicative_temp->identifier->array_flag) {
            printf("_R%d = _R%d %c _R%d\n",register_count++, multiplicative_holder-1, operator, cast_holder-1);
          }
          else {
            printf("_R%d = %s(%d) %c %s(%d)\n",register_count++, multiplicative_temp->identifier->name,multiplicative_temp->identifier->offset, operator, cast_temp->identifier->name,cast_temp->identifier->offset);
          }

        //printf("_R%d = %s %c %s\n",register_count++, additive_temp->identifier->name, operator, multiplicative_temp->identifier->name);
        }else if(multiplicative_temp->identifier != NULL){

          //typeCheck()
          ThreeAddrMultiplicativeExpressionNode(ast->multiplicative_expression);
          //printf("_R%d = %s\n",register_count++,  multiplicative_temp->identifier->name );
          multiplicative_holder = register_count;
          ThreeAddrCastExpressionNode(ast->cast_expression);
          cast_holder = register_count;
           

          if(multiplicative_temp->identifier->array_flag) {
            printf("_R%d = _R%d %c _R%d\n",register_count++, multiplicative_holder-1, operator, cast_holder-1);
          }
          else {
            printf("_R%d = %s(%d) %c _R%d\n",register_count++, multiplicative_temp->identifier->name, multiplicative_temp->identifier->offset, operator, cast_holder-1);
          }


        }else if(cast_temp->identifier != NULL){

          ThreeAddrMultiplicativeExpressionNode(ast->multiplicative_expression);
          multiplicative_holder = register_count;
          ThreeAddrCastExpressionNode(ast->cast_expression);
          //printf("_R%d = %s\n",register_count++, cast_temp->identifier->name );
          cast_holder = register_count; 

          

          if(cast_temp->identifier->array_flag) {
          printf("_R%d = _R%d %c _R%d\n",register_count++, multiplicative_holder-1, operator, cast_holder-1);
        }
        else {
          printf("_R%d = _R%d %c %s(%d)\n",register_count++, multiplicative_holder-1, operator, cast_temp->identifier->name,cast_temp->identifier->offset);
        }

        }else{

          ThreeAddrMultiplicativeExpressionNode(ast->multiplicative_expression);
          multiplicative_holder = register_count;
          ThreeAddrCastExpressionNode(ast->cast_expression);
          cast_holder = register_count;

          printf("_R%d = _R%d %c _R%d\n",register_count++, multiplicative_holder-1, operator, cast_holder-1);
        }


  }else if(ast->cast_expression != NULL){
    
    ThreeAddrCastExpressionNode(ast-> cast_expression);
  
  }
    

}

void ThreeAddrCastExpressionNode(CastExpressionNode * ast){


  if(ast->unary_expression!= NULL){
    
    ThreeAddrUnaryExpressionNode(ast-> unary_expression);
  
  }
    


}
void ThreeAddrUnaryExpressionNode(UnaryExpressionNode * ast){


  if(ast->postfix_expression  != NULL){
    
    ThreeAddrPostfixExpressionNode(ast-> postfix_expression );
  
  }
    

}

void ThreeAddrPostfixExpressionNode(PostfixExpressionNode * ast){


  //postfix_expression <- postfix_expression '[' expression ']' 
  if(ast->postfix_expression != NULL && ast->expression != NULL){
    
    //ThreeAddrPostfixExpressionNode(ast->postfix_expression );
    //ThreeAddrExpression(ast->expression);
    PostfixExpressionNode * postfix_temp = ast->postfix_expression;
    ExpressionNode * expression_temp = ast->expression;
    ConstantNode * constant_temp = expression_temp->constant;
    treeNode * id = postfix_temp->identifier;

    treeNode * identifier_temp_left;
    treeNode * identifier_temp_right = expression_temp-> identifier;


    int oneD_flag = 1;
    
    if(constant_temp != NULL ) {
      while(postfix_temp->postfix_expression != NULL){
        oneD_flag = 0;
        ExpressionNode * expression_temp = postfix_temp->expression;
        postfix_temp= postfix_temp->postfix_expression;
        ConstantNode * row_offset = expression_temp->constant;

        printf("_R%d = %d\n", register_count++,id->array_size );
        
        printf("_R%d = %d\n", register_count++,row_offset->int_constant );

        printf("_R%d = _R%d * _R%d \n", register_count++,register_count-1, register_count-2);
        printf("_R%d = %d\n", register_count++, constant_temp->int_constant);

      }
          if (oneD_flag){
          printf("_R%d = %d\n", register_count++, constant_temp->int_constant);
          printf("_R%d = 4\n", register_count++);
          printf("_R%d = _R%d * _R%d ^ \n", register_count++, register_count-1, register_count-2);
          printf("_R%d = %s(%d) + _R%d ^\n",register_count, id->name ,id->offset, register_count-1);
          register_count++;
      }
      else{
          printf("_R%d = _R%d + _R%d\n", register_count++, register_count-1, register_count-2);
          printf("_R%d = 4\n", register_count++);
          printf("_R%d = _R%d * _R%d ^\n", register_count++, register_count-1, register_count-2);
          printf("_R%d = %s(%d) + _R%d ^\n",register_count, id->name ,id->offset, register_count-1);
          register_count++;
      }
    }
    else if( constant_temp == NULL ) {
      while(postfix_temp->postfix_expression != NULL){
        oneD_flag = 0;
        ExpressionNode * expression_temp = postfix_temp->expression;
        postfix_temp= postfix_temp->postfix_expression;
        ConstantNode * row_offset = expression_temp->constant;
        identifier_temp_left = expression_temp-> identifier;


        printf("_R%d = %d\n", register_count++,id->array_size );
        
        printf("_R%d = %s(%d)\n", register_count++,identifier_temp_left->name,identifier_temp_left->offset);
        printf("_R%d = _R%d * _R%d \n", register_count++,register_count-1, register_count-2);
        printf("_R%d = %s(%d)\n", register_count++, identifier_temp_right->name,identifier_temp_right->offset);
        
      }

          if (oneD_flag){
          printf("_R%d = %s(%d)\n", register_count++, identifier_temp_right->name,identifier_temp_right->offset);
          printf("_R%d = 4\n", register_count++);
          printf("_R%d = _R%d * _R%d ^ \n", register_count++, register_count-1, register_count-2);
          printf("&_R%d = %s(%d) + _R%d ^ \n",register_count, id->name ,id->offset, register_count-1);
          register_count++;
      }
      else{
          printf("_R%d = _R%d + _R%d\n", register_count++, register_count-1, register_count-2);
          printf("_R%d = 4\n", register_count++);
          printf("_R%d = _R%d * _R%d ^\n", register_count++, register_count-1, register_count-2);
          printf("_R%d = %s(%d) + _R%d ^ \n",register_count, id->name ,id->offset, register_count-1);
          register_count++;
      }
    }
  }
  else if(ast->argument_expression_list != NULL && ast-> postfix_expression != NULL){
    ArgumentExpressionListNode * argument_temp = ast->argument_expression_list;
    ConstantNode * constant_temp = argument_temp->constant;
    printf("Push Param(%d)\n", constant_temp->int_constant);
    printf("Val Out(_R%d)\n", register_count++);
    printf("Call %s\n", ast->identifier->name);

  }
  else if(ast->primary_expression != NULL){
    
    ThreeAddrPrimaryExpressionNode(ast-> primary_expression);
  
  }

}


void ThreeAddrPrimaryExpressionNode(PrimaryExpressionNode * ast){

  
  if(ast->constant != NULL){
  
    ThreeAddrConstant(ast->constant);
    
  }else if(ast->identifier != NULL){
    

  
  }




}

void ThreeAddrConstant(ConstantNode * ast){


  if(ast->int_flag){
      printf("_R%d = %d \n", register_count++, ast->int_constant);
  }
  else if( ast->char_flag) {
      printf("_R%d = %c\n", register_count++, ast->char_constant);
  }
  else if( ast->float_flag) {

     printf("_R%d = %f\n", register_count++, ast->float_constant);

  }
}


void ThreeAddrIterationStatement(IterationStatementNode * ast){



  
  //For Loop
  if(ast->expression != NULL && ast->expression1 != NULL && ast->expression2 != NULL && ast->statement != NULL){
    ExpressionNode * temp = ast->expression;
    printSource(temp->line);
    label_counter++;
    int label_holder;
    ThreeAddrExpression(ast->expression);
    printf("_L%d:\n", label_counter++ );
    label_holder = label_counter;
    ThreeAddrExpression(ast->expression1);
    printf("Branch if not R%d: _L%d \n", register_count-1, label_counter++);
    ThreeAddrExpression(ast->expression2);
    ThreeAddrStatement(ast->statement);
    printf("goto _L%d\n", label_holder - 1);
    printf("_L%d:\n", label_holder);




  }
  //While Loop
  else if(ast->expression != NULL && ast->statement != NULL){

    ExpressionNode * temp = ast->expression;
    printSource(temp->line);
    label_counter++;
    int label_holder;
    printf("_L%d:\n", label_counter++ );
    ThreeAddrExpression(ast->expression);
    label_holder = label_counter;
    printf("Branch if not R%d: _L%d \n", register_count-1, label_counter++);
    ThreeAddrStatement(ast->statement);
    printf("goto _L%d\n", label_holder - 1);
    printf("_L%d:\n", label_holder);




  }


}

void typeCheckIdId(treeNode * x, treeNode * y){

  if(x->flags.int_flag && y->flags.float_flag){
      printf("\t Type Mismatch: int and float!\n");

  }else if(x->flags.int_flag && y->flags.char_flag){
      printf("\t Type Mismatch: int and char!\n");

  }else if(x->flags.char_flag && y->flags.float_flag){
      printf("\t Type Mismatch: char and float!\n");

  }else if(x->flags.char_flag && y->flags.int_flag){
      printf("\t Type Mismatch: char and int!\n");

  }else if(x->flags.float_flag && y->flags.int_flag){
      printf("\t Type Mismatch: float and int!\n");

  }else if(x->flags.float_flag && y->flags.char_flag){
      printf("\t Type Mismatch: float and char\n");


  }

}

void typeCheckIdCons(treeNode * x, ConstantNode * y){

  if(x->flags.int_flag && y->float_flag){


  }else if(x->flags.int_flag && y->char_flag){

  }else if(x->flags.char_flag && y->float_flag){

  }else if(x->flags.char_flag && y->int_flag){

  }else if(x->flags.float_flag && y->int_flag){

  }else if(x->flags.float_flag && y->char_flag){


  }

}

void typeCheckConsCons(ConstantNode * x, ConstantNode * y){

  if(x->int_flag && y->float_flag){

  }else if(x->int_flag && y->char_flag){

  }else if(x->char_flag && y->float_flag){

  }else if(x->char_flag && y->int_flag){

  }else if(x->float_flag && y->int_flag){

  }else if(x->float_flag && y->char_flag){


  }

}
void printSource(int line){

  int index, lineTracker = 0, lineCount = 0;
  static currentLine = 0;
  if(currentLine < line){
    printf("\t\t");
    //Go to appropriate line
    for(index = 0 ; index < line - 1; index++){
     //Track our current location in the file
     while(buffer[lineTracker] != '\n'){         
        lineTracker++;
      }
      lineTracker++;
      lineCount++;
    }
    //Check that we still have file to parse
    if(lineCount < rowNum  ){
      //Print the line with the error
      while(buffer[lineTracker] != '\n'){
      
        //Be wary to avoid printing garbage
        if(lineTracker < bufferSize && buffer[lineTracker] != '\t'){
          printf("%c", buffer[lineTracker]);
       
        }
        lineTracker++;
      
      }
      printf("\n");

    }
    currentLine = line;

  }

}
