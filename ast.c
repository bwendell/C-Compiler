#include "driver.h"
#include "nodes.h"
#include <stdlib.h>

void ReportAST(TranslationUnitNode * ast){


  fprintf(astFile,"[.{Translation Node} ");
  
  if(ast->external_declaration != NULL && ast->translation_unit != NULL ){
  
      ReportAST(ast->translation_unit);
      PrintExternalDeclaration(ast->external_declaration);
      
  } 
  else if(ast->external_declaration != NULL){
  
      PrintExternalDeclaration(ast->external_declaration);
  
  } 
  fprintf(astFile," ] ");


}



void PrintExternalDeclaration(ExternalDeclarationNode * ast){
    

  fprintf(astFile," [.{External Declaration} ");
  
  if(ast->function_definition != NULL){
  
    PrintFunctionDefinition(ast->function_definition);
    ast->function_definition != NULL;
  } 
  
  else if(ast->declaration != NULL){
    
    PrintDeclaration(ast->declaration);
    
  } 

  fprintf(astFile," ] ");

}


PrintDeclaration(DeclarationNode * ast){

  fprintf(astFile,"[ .{Declaration} ");
  
  
  if(ast->declaration_specifiers != NULL && ast->init_declarator_list != NULL){
      PrintDeclarationSpecifiers(ast->declaration_specifiers);
      PrintInitDeclaratorList(ast->init_declarator_list);
      
  }
    
  fprintf(astFile," ] ");

}

PrintDeclarationSpecifiers(DeclarationSpecifiersNode * ast){

    fprintf(astFile," [.{Declaration Specifiers} ");

    //fprintf(astFile,"Declaration Specifiers %d %d\n", level++, ast_id++);
    if(ast->type_specifier != NULL){
     
      TypeSpecifierNode * x;
      x = ast->type_specifier;
      fprintf(astFile," [ %s ] ", x->type);
      
    }
    
    fprintf(astFile," ] ");

}

PrintInitDeclaratorList(InitDeclaratorListNode * ast){

    fprintf(astFile," [.{Init Declarator List} ");
    
    if(ast-> init_declarator != NULL && ast-> init_declarator_list != NULL){
    
      PrintInitDeclaratorList(ast-> init_declarator_list);
      PrintInitDeclarator(ast->init_declarator);
    }
    
    else if(ast-> init_declarator != NULL){
    
      PrintInitDeclarator(ast->init_declarator);
    }
    
    

    fprintf(astFile," ] ");
}

PrintInitDeclarator(InitDeclaratorNode * ast){

    fprintf(astFile," [.{Init Declarator} ");
    
    if(ast-> declarator != NULL && ast-> initializer!= NULL ){
    PrintDeclarator(ast->declarator);
    fprintf(astFile," = ");
    PrintInitializer(ast-> initializer);
    }
    else if(ast-> declarator != NULL){
    
      PrintDeclarator(ast->declarator);
      
    }
    
   fprintf(astFile," ] ");
} 

PrintDeclarator(DeclaratorNode * ast){

  fprintf(astFile," [.Declarator ");
  if(ast->direct_declarator != NULL){
  
    PrintDirectDeclarator(ast->direct_declarator);

  }
    
  fprintf(astFile," ] ");


}

void PrintDirectDeclarator(DirectDeclaratorNode * ast){

  fprintf(astFile," [.{Direct Declarator} ");

    if(ast->identifier != NULL){
  
        fprintf(astFile," {%s} ", ast->identifier->name);
    
    }
    else if(ast->constant_expression != NULL && ast->direct_declarator != NULL){
    
        PrintDirectDeclarator(ast->direct_declarator);
        fprintf(astFile," '[' ");
        PrintConstantExpression(ast->constant_expression);
        fprintf(astFile," ']' ");
        
        
        
        
    
    }

  fprintf(astFile," ] ");
}

void PrintFunctionDefinition(FunctionDefinitionNode * ast){

  fprintf(astFile," [.{Function Definition} ");
  if(ast-> declaration_specifiers != NULL && ast->declarator != NULL && ast-> compound_statement != NULL ){
    
    PrintDeclarationSpecifiers(ast-> declaration_specifiers);
    PrintDeclarator(ast->declarator );
    PrintCompoundStatement(ast->compound_statement);

  }

   fprintf(astFile," ] ");


}
void PrintCompoundStatement(CompoundStatementNode * ast){


  fprintf(astFile," [.{Compound Statement} {Open Bracket} ");
  
   if(ast-> declaration_list != NULL && ast->statement_list != NULL){
        PrintDeclarationList(ast->declaration_list );
        PrintStatementList(ast->statement_list);
  
    }
   
   else if(ast-> declaration_list != NULL){
    
    PrintDeclarationList(ast->declaration_list );
  
   }
   else if(ast-> statement_list != NULL){
    
    PrintStatementList(ast->statement_list );
  
   }
  fprintf(astFile," {Closed Bracket} ] ");

}

void PrintStatementList(StatementListNode * ast){
  fprintf(astFile," [.{Statement List} ");

  if(ast->statement_list != NULL && ast->statement_list != NULL){

    PrintStatementList(ast->statement_list);
    PrintStatement(ast->statement);

  }else if(ast->statement != NULL){
  
    PrintStatement(ast->statement);
  
  }
  fprintf(astFile," ] ");


}

void PrintStatement(StatementNode * ast){

  fprintf(astFile," [.{Statement} ");
  if(ast-> selection_statement != NULL){
  
    PrintSelectionStatement(ast->selection_statement);
    
  
  }
  
  else if(ast-> expression_statement != NULL){
  
    PrintExpressionStatement(ast->expression_statement);
    
  
  }
  else if(ast->compound_statement != NULL){
  

    PrintCompoundStatement(ast->compound_statement);
  
  }
  else if(ast->iteration_statement != NULL){
  
    PrintIterationStatement(ast->iteration_statement);
  
  }
  fprintf(astFile," ] ");


}

void PrintExpressionStatement(ExpressionStatementNode * ast){


  fprintf(astFile," [.{Expression Statement} ");
  if(ast-> expression != NULL){
  
    PrintExpression(ast-> expression);
    
  
  }
  
  fprintf(astFile," ] ");


}


void PrintDeclarationList(DeclarationListNode * ast){


  fprintf(astFile," [.{Declaration List} ");
  if(ast-> declaration != NULL && ast->declaration_list != NULL){
  
    PrintDeclarationList(ast->declaration_list);
    PrintDeclaration(ast->declaration);
  
  
  }
  else if(ast-> declaration != NULL){
  
    PrintDeclaration(ast->declaration);
  
  
  }
  
  
  
   fprintf(astFile," ] ");

}

void PrintConstantExpression(ConstantExpressionNode * ast){

  fprintf(astFile," [.{Constant Expression} ");
  if(ast->conditional_expression != NULL){
  
    PrintConditionalExpression(ast->conditional_expression);
  
  }



  fprintf(astFile," ] ");





}

void PrintSelectionStatement(SelectionStatementNode *ast){
  
  
  fprintf(astFile," [.{Selection Statement} ");
  //selection_statement <- IF '(' expression ')' statement
  if(ast->expression != NULL && ast->statement != NULL){
    fprintf(astFile," [.{if} ] [ ( ] ");
    PrintExpression(ast->expression);
    fprintf(astFile," [ ) ] ");
    PrintStatement(ast->statement);
   
  }
  
   fprintf(astFile," ] ");

}

void PrintExpression(ExpressionNode * ast){

  fprintf(astFile," [.{Expression} ");
  if(ast-> assignment_expression != NULL){
  
    PrintAssignmentExpression(ast-> assignment_expression);
  
  }
  
  
  fprintf(astFile," ] ");

}

void PrintAssignmentExpression(AssignmentExpressionNode * ast){

  fprintf(astFile," [.{Assignment Expression} ");

  if(ast-> unary_expression != NULL  && ast->assignment_expression != NULL){
  
    PrintUnaryExpressionNode(ast-> unary_expression);
    fprintf(astFile," [ = ] ");
    PrintAssignmentExpression(ast->assignment_expression);
  
  }
  else if(ast-> conditional_expression  != NULL){
  
    PrintConditionalExpression(ast->conditional_expression);
  
  }
  
  
  
  
  fprintf(astFile," ] ");

}

void PrintConditionalExpression(ConditionalExpressionNode * ast){

  fprintf(astFile," [.{Conditional Expression} ");
  if(ast-> logical_or_expression  != NULL){
  
    PrintLogicalOrExpression(ast->logical_or_expression);
   
  }
    
  fprintf(astFile," ] ");

}

void PrintLogicalOrExpression(LogicalOrExpressionNode * ast){

  fprintf(astFile," [.{Logical Or Expression} ");
  if(ast-> logical_and_expression  != NULL){
  
    PrintLogicalAndExpression(ast->logical_and_expression);
   
  }
    
  fprintf(astFile," ] ");
}

void PrintLogicalAndExpression(LogicalAndExpressionNode * ast){

  fprintf(astFile," [.{ Logical And Expression} ");
  if(ast-> inclusive_or_expression  != NULL){
  
    PrintInclusiveOrExpression(ast->inclusive_or_expression);
   
  }
    
  fprintf(astFile," ] ");
}

void PrintInclusiveOrExpression(InclusiveOrExpressionNode * ast){

  fprintf(astFile," [.{ Inclusive Or Expression} ");
  if(ast-> exclusive_or_expression  != NULL){
  
    PrintExclusiveOrExpression(ast->exclusive_or_expression);
   
  }
    
  fprintf(astFile," ] ");
}

void PrintExclusiveOrExpression(ExclusiveOrExpressionNode * ast){

  fprintf(astFile," [.{ Exclusive Or Expression} ");
  if(ast-> and_expression  != NULL){
  
    PrintAndExpression(ast->and_expression);
   
  }
    
  fprintf(astFile," ] ");
}

void PrintInitializer( InitializerNode * ast){
   
  fprintf(astFile," [.{ Initializer} ");
  
  if(ast->initializer_list != NULL){
    
    PrintInitializerList(ast-> initializer_list);
  
  }
  
  else if(ast-> assignment_expression != NULL){
    
    PrintAssignmentExpression(ast-> assignment_expression);
  
  }
    
  fprintf(astFile," ] ");
}


void PrintInitializerList(InitializerListNode * ast){

  fprintf(astFile," [.{ Initializer List } ");

  //initializer_list <- initializer_list ',' initializer
  if(ast->initializer_list != NULL && ast->initializer != NULL){
    
    PrintInitializerList(ast-> initializer_list);
    fprintf(astFile," [.{ , } ] ");
    PrintInitializer(ast-> initializer);
  
  }
  else if(ast->initializer != NULL){

    PrintInitializer(ast-> initializer);
  
  }
 
  fprintf(astFile," ] ");
}



void PrintAndExpression(AndExpressionNode * ast){
  fprintf(astFile," [.{ And Expression} ");
  if(ast->equality_expression != NULL){
    
    PrintEqualityExpression(ast-> equality_expression);
  
  }
    
  fprintf(astFile," ] ");
  
}

void PrintEqualityExpression(EqualityExpressionNode *ast){
  fprintf(astFile," [.{ Equality Expression} ");
  if(ast->relational_expression != NULL){
    
    PrintRelationalExpression(ast-> relational_expression);
  
  }
    
  fprintf(astFile," ] ");
}

void PrintRelationalExpression(RelationalExpressionNode *ast){
  fprintf(astFile," [.{ Relational Expression} ");
  
  if(ast->shift_expression != NULL && ast->relational_expression != NULL){
    PrintRelationalExpression(ast-> relational_expression);
    fprintf(astFile, " [ GT  ] " );
    PrintShiftExpression(ast-> shift_expression);
  }

  else if(ast->shift_expression != NULL){
    PrintShiftExpression(ast-> shift_expression);
  }
    
  fprintf(astFile," ] ");
}

void PrintShiftExpression(ShiftExpressionNode * ast){
  fprintf(astFile," [.{ Shift Expression} ");

    
  if(ast->additive_expression != NULL){
    
    PrintAdditiveExpressionNode(ast-> additive_expression);
  
  }


    
  fprintf(astFile," ] ");
}

void PrintAdditiveExpressionNode(AdditiveExpressionNode * ast){
  fprintf(astFile," [.{ Additive Expression} ");
  
   if(ast->multiplicative_expression != NULL && ast->additive_expression != NULL){
    
    PrintAdditiveExpressionNode(ast->additive_expression);
    fprintf(astFile," [ + ] ");
    PrintMultiplicativeExpressionNode(ast-> multiplicative_expression);

  
  }
  
  else if(ast->multiplicative_expression != NULL){
    
    PrintMultiplicativeExpressionNode(ast-> multiplicative_expression);
  
  }
    
  fprintf(astFile," ] ");
}

void PrintMultiplicativeExpressionNode(MultiplicativeExpressionNode * ast){

  fprintf(astFile," [.{ Multiplicative Expression} ");
  if(ast->cast_expression != NULL && ast->multiplicative_expression != NULL){

    PrintMultiplicativeExpressionNode(ast->multiplicative_expression);
    fprintf(astFile, " [ {*} ]");
    PrintCastExpressionNode(ast->cast_expression);


  }
  else if(ast->cast_expression != NULL){
    
    PrintCastExpressionNode(ast-> cast_expression);
  
  }
    
  fprintf(astFile," ] ");
}

void PrintCastExpressionNode(CastExpressionNode * ast){

  fprintf(astFile," [.{ Cast Expression} ");
  if(ast->unary_expression!= NULL){
    
    PrintUnaryExpressionNode(ast-> unary_expression);
  
  }
    
  fprintf(astFile," ] ");

}
void PrintUnaryExpressionNode(UnaryExpressionNode * ast){

  fprintf(astFile," [.{ Unary Expression} ");
  if(ast->postfix_expression  != NULL){
    
    PrintPostfixExpressionNode(ast-> postfix_expression );
  
  }
    
  fprintf(astFile," ] ");
}

void PrintPostfixExpressionNode(PostfixExpressionNode * ast){

  fprintf(astFile," [.{ Postfix Expression} ");
  //postfix_expression <- postfix_expression '[' expression ']' 
  if(ast->postfix_expression != NULL && ast->expression != NULL){
    
    PrintPostfixExpressionNode(ast->postfix_expression );
    fprintf(astFile," [ '[' ] ");
    PrintExpression(ast->expression);
    fprintf(astFile," [ ']' ] ");
  }
  else if(ast->primary_expression != NULL){
    
    PrintPrimaryExpressionNode(ast-> primary_expression);
  
  }
    
  fprintf(astFile," ] ");
}


void PrintPrimaryExpressionNode(PrimaryExpressionNode * ast){

  fprintf(astFile," [.{ Primary Expression} ");
  
  if(ast->constant != NULL){
  
    PrintConstant(ast->constant);
    
  }else if(ast->identifier != NULL){
    
    fprintf(astFile," [.{%s} ] ", ast->identifier->name);
  
  }
    
  fprintf(astFile," ] ");



}

void PrintConstant(ConstantNode * ast){


  if(ast->int_flag){
    fprintf(astFile," [.{%d} ] ", ast -> int_constant);
  }
  else if( ast->char_flag) {
    fprintf(astFile," [.{%c} ] ", ast -> char_constant);
  }
  else if( ast->float_flag) {
    fprintf(astFile," [.{%f} ] ", ast -> float_constant);
  }
}


void PrintIterationStatement(IterationStatementNode * ast){


  fprintf(astFile," [.{ Iteration Statement} ");
  
  //While Loop
  if(ast->expression != NULL && ast->statement != NULL){
  
    fprintf(astFile," [ while ] [ ( ] ");
    PrintExpression(ast->expression);
    fprintf(astFile," [  ) ]");
    PrintStatement(ast->statement);
  }
  
  fprintf(astFile," ] ");


}


bool CheckAdditiveExpressionTypes(AdditiveExpressionNode * exp1, MultiplicativeExpressionNode  * exp2){

   AdditiveExpressionNode * additive_expression;
   MultiplicativeExpressionNode  * multiplicative_expression;
   CastExpressionNode * cast_expression;
   UnaryExpressionNode * unary_expression;
   PostfixExpressionNode * postfix_expression;
   PrimaryExpressionNode * primary_expression;
   ConstantNode * constant;
   treeNode * identifier;
     
  //Get the value of the additive expression
  multiplicative_expression = exp1->multiplicative_expression;
  cast_expression = multiplicative_expression->cast_expression;
  unary_expression = cast_expression->unary_expression;
  postfix_expression = unary_expression->postfix_expression;
  primary_expression = postfix_expression->primary_expression;
  identifier = primary_expression->identifier;
  
  
  //Get the value of the multiplicative expression
  cast_expression = exp2->cast_expression;
  unary_expression = cast_expression->unary_expression;
  postfix_expression = unary_expression->postfix_expression;
  primary_expression = postfix_expression->primary_expression;
  constant = primary_expression->constant;
  
  //Check Flags
  if(constant->float_flag && identifier->flags.int_flag){
        printf("Warning: Addition of type int and float, type conversion to occur\n");
    
  }else if(constant->float_flag && identifier->flags.char_flag){
        printf("Warning: Addition of type char and float, type conversion to occur\n");
  
}
}

