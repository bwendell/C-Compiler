#include "driver.h"
#include "symTable.h"
#include <stdlib.h>

#ifndef AST_H
#define AST_H

/*
  Each struct corresponds to a production in the grammar.
  The data members of each stuct are the right hand side of it's production.
  The print function is used to print out the corresponding production.
  Each print function will print the node into a file, and then Qtree, 
  a LaTeX library, will parse the data and create the tree.
*/

// translation unit struct
typedef struct translation_unit_node
{
    struct TranslationUnitNode *translation_unit;
    struct ExternalDeclarationNode *external_declaration;
} TranslationUnitNode;


// external declaration struct
typedef struct external_declaration_node
{
    struct FunctionDefinitionNode *function_definition;
    struct DeclarationNode * declaration;
} ExternalDeclarationNode;

// function definition struct
typedef struct function_definition_node
{
    struct DeclaratorCompoundStatementNode *declarator_compound_statment;
    struct DeclaratorDeclartionListNode * declarator_declaration_list;
    struct CompoundStatementNode * compound_statement; 
    struct DeclarationSpecifiersNode * declaration_specifiers;
    struct DeclaratorNode * declarator; 
    char * function_name;
} FunctionDefinitionNode;


// declaration struct
typedef struct declaration_node
{
    struct DeclarationSpecifiersNode * declaration_specifiers;
    struct InitDeclaratorListNode * init_declarator_list;
    struct node *ptr;
} DeclarationNode;

// declaration list struct
typedef struct declaration_list_node
{
  struct DeclarationNode * declaration;
  struct DeclarationList * declaration_list;
    
} DeclarationListNode;

// declaration specifier struct
typedef struct declaration_specifiers_node
{
    struct StorageClassSpecifierNode * storage_class_specifier;
    struct DeclartionSpecifiersNodes * declaration_specifiers;
    struct TypeSpecifierNode * type_specifier;
    struct TypeQualifier * type_qualifier;
} DeclarationSpecifiersNode;

// storage class specifier struct
typedef struct storage_class_specifier_node
{

  char * SPECIFIER;
  
} StorageClassSpecifierNode;

// type specifier struct
typedef struct type_specifier_node
{

    struct StructOrUnionSpecifierNode * struct_or_union_specifier;
    struct EnumSpecifier * enum_specifier; 
    char * type;
    
} TypeSpecifierNode;

// type qualifier struct
typedef struct type_qualifier_node
{
    char * type;
} TypeQualifierNode;

// struct or union specifier struct
typedef struct struct_or_union_specifier_node
{

    struct StructOrUnionNode * struct_or_union;
    struct Identifier * identifier;
    struct StructDeclarationList * struct_declartion_list;


} StructOrUnionSpecifierNode;

// struct or union struct
typedef struct struct_or_union_node
{
    char * type; 

}StructOrUnionNode;

// struct declaration list struct
typedef struct struct_declaration_list_node
{

    struct StructDeclarationNode * struct_declaration;
    struct StructDeclarationListNode * struct_declaration_list;


}StructDeclarationListNode; 

// init declarator list struct
typedef struct init_declarator_list_node
{

   struct InitDeclaratorListNode * init_declarator_list;
   struct InitDeclaratorNode * init_declarator; 
   


}InitDeclaratorListNode; 

// init declarator struct
typedef struct init_declarator_node
{

   struct InitializerNode* initializer;
   struct DeclaratorNode * declarator; 

   struct treeNode * identifier;
   struct ConstantNode * constant;
   int  line;
   


}InitDeclaratorNode;

// struct declaration struct
typedef struct struct_declaration_node
{

   struct SpecifierQualifierListNode* specifier_qualifier_list;     
   struct StructDeclaratorListNode * struct_declarator_list; 
   
}StructDeclarationNode;

// specifier qualifier list struct
typedef struct specifier_qualifier_list_node
{

   struct TypeSpecifierNode * type_specifier; 
   struct TypeQualifierNode * type_qualifier;    
   struct SpecifierQualifierListNode * specifier_qualifier_list; 
  
}SpecifierQualifierListNode;

// struct declarator list struct
typedef struct struct_declarator_list_node
{

   struct StructDeclaratorListNode * struct_declarator_list; 
   struct StructDeclaratorNode * struct_declarator;    
  
}StructDeclaratorListNode;

// struct declarator struct
typedef struct struct_declarator_node
{

   struct DeclaratorNode * declarator; 
   struct StructDeclaratorNode * struct_declarator;  
   struct ConstantExpressionNode * constant_expression;  
  
}StructDeclaratorNode;

// enum specifier struct
typedef struct enum_specifier_node
{

   struct EnumeratorListNode * enumerator_list; 
   struct IdentifierNode * identifier; 
   char * ENUMt; 
}EnumSpecifierNode;


typedef struct enumerator_list_node
{

   struct EnumeratorListNode * enumerator_list; 
   struct EnumeratorNode * enumerator; 
  
}EnumeratorListNode;


typedef struct enumerator_node
{
   //TODO go back and add characters being returned as characters i.e. =, +, - etc
   struct ConstantExpressionNode * constant_expression;
   struct IdentifierNode * identifier; 
  
}EnumeratorNode;


typedef struct declarator_node
{

   struct DirectDeclaratorNode * direct_declarator; 
   struct PointerNode * pointer;
   struct treeNode * identifier;
  
}DeclaratorNode;


typedef struct direct_declarator_node
{
   bool array_flag; 
   bool function_flag;
   struct treeNode * identifier;
   struct DeclaratorNode * declarator;
   struct DirectDeclaratorNode * direct_declarator; 
   struct ConstantExpressionNode * constant_expression;
   struct PointerNode * pointer;
   struct ParameterTypeListNode * parameter_type_list;
   struct IdentifierListNode * identifier_list; 
  
}DirectDeclaratorNode;


typedef struct pointer_node
{

    struct TypeQualiferListNode * type_qualifier_list;
    struct PointerNode * pointer;
}PointerNode;

typedef struct type_qualifier_list_node
{

    struct TypeQualifierNode * type_qualifier;
    struct TypeQualifierListNode * type_qualifier_list;
}TypeQualiferListNode;

typedef struct parameter_type_list_node{

    struct ParameterListNode * parameter_list;
    treeNode * identifier;
    char * ELIPSISt; 
}ParameterTypeListNode;

typedef struct parameter_list_node{

    struct ParameterListNode * parameter_declaration;
    treeNode * identifier;

}ParameterListNode;


typedef struct parameter_declaration_node
{

    struct DeclarationSpecifiersNode * declaration_specifiers;
    struct DeclaratorNode * declarator;
    struct AbstractDeclaratorNode * abstract_declarator; 
    treeNode * identifier;
    
}ParameterDeclarationNode;

typedef struct identifier_list_node
{

   struct IdentifierNode * identifier;
   struct IdentifieListNode * identifier_list;
    
}IdentifierListNode;

typedef struct initializer_node
{

   struct InitializerListNode * initializer_list;
   struct AssignmentExpressionNode * assignment_expression;
  
   struct treeNode * identifier;
   struct ConstantNode * constant;   
    
}InitializerNode;

typedef struct initializer_list_node
{

   struct InitializerNode * initializer;
   struct InitializerListNode * initializer_list;
    
}InitializerListNode;

typedef struct type_name_node
{

   struct AbstractDeclaratorNode * abstract_declarator;
   struct SpecifierQualifierListNode * specifier_qualifier_list; 
    
}TypeNameNode;

typedef struct abstract_declarator_node
{

  struct PointerNode * pointer;
  struct DirectAbstractDeclaratorNode * direct_abstract_declarator;
  struct ConstantExpressionNode * constant_expression;
    
}AbstractDeclaratorNode;

typedef struct direct_abstract_declarator_node
{

  struct PointerNode * pointer;
  struct DirectAbstractDeclaratorNode * direct_abstract_declarator;
  struct AbstractDeclaratorNode * abstract_declarator;
  struct ConstantExpressionNode * constant_expression;
  struct ParameterListNode * parameter_list;
    
}DirectAbstractDeclaratorNode;

typedef struct statement_node
{

  struct LabeledStatementNode * labeled_statement;
  struct CompoundStatementNode * compound_statement;
  struct ExpressionStatementNode * expression_statement;
  struct SelectionStatementNode * selection_statement;
  struct IterationStatementNode * iteration_statement;
  struct JumpStatementNode * jump_statement;
  int line;
  struct treeNode * identifier;
  struct ConstantNode * constant;
    
}StatementNode;

typedef struct labeled_statement_node
{
    
  struct IdentifierNode * identifier;
  struct ConstantExpressionNode * constant_expression;
  struct StatementNode * statement;
  char * CASEt; 
  char * COLONt;
  char * DEFAULTt;


}LabeledStatementNode;

typedef struct expression_statement_node
{

  char * SEMICOLONt;
  struct ExpressionNode * expression;
  
    
}ExpressionStatementNode;

typedef struct compound_statement_node
{

  char * LEFTBRACKETt;
  char * RIGHTBRACKETt;
  struct StatementListNode * statement_list;
  struct DeclarationListNode * declaration_list;
  struct treeNode * identifier;
  struct ConstantNode * constant;
    
}CompoundStatementNode;


typedef struct statement_list_node
{
  struct StatementListNode * statement_list;
  struct StatementNode * statement;
  struct treeNode * identifier;
  struct ConstantNode * constant;

}StatementListNode;



typedef struct selection_statement_node
{

  struct ExpressionNode * expression;
  struct StatementNode * statement;
  struct StatementNode * statement2;
  char * IFt;
  char * ELSEt;
  char * SWITCHt;
  char * LEFTPARENt;
  char * RIGHTPARENt;

}SelectionStatementNode;

typedef struct iteration_statement_node
{

  struct ExpressionNode * expression;
  struct ExpressionNode * expression1;
  struct ExpressionNode * expression2;
  struct ExpressionStatementNode * expression_statement;
  struct StatementNode * statement;
  char * WHILEt;
  char * DOt;
  char * FORt;
  char * LEFTPARENt;
  char * RIGHTPARENt;
  char * SEMICOLONt;
  int line;

}IterationStatementNode;

typedef struct jump_statement_node
{

  struct treeNode * identifier;
  struct ConstantNode * constant;
  struct ExpressionNode * expression;
  char * RETURNt;
  char * BREAKt;
  char * SEMICOLONt;

}JumpStatementNode;

typedef struct expression_node
{

  struct ExpressionNode * expression;
  struct AssignmentExpressionNode * assignment_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  int line;

}ExpressionNode;

typedef struct assignment_expression_node
{
  struct ConditionalExpressionNode * conditional_expression;
  struct AssignmentExpressionNode * assignment_expression;
  struct AssignmentOperatorNode * assignment_operator;
  struct UnaryExpressionNode * unary_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  int line;


}AssignmentExpressionNode;

typedef struct assignment_operator_node
{
  char * MUL_ASSIGNt;
  char * DIV_ASSIGNt;
  char * MOD_ASSIGNt;
  char * ADD_ASSIGNt;
  char * SUB_ASSIGNt;
  char * LEFT_ASSIGNt;
  char * RIGHT_ASSIGNt;
  char * AND_ASSIGNt;
  char * XOR_ASSIGNt;


}AssignmentOperatorNode;

typedef struct conditional_expression_node
{

  struct ExpressionNode * expressionNode;
  struct LogicalOrExpressionNode * logical_or_expression;
  struct ConditionalExpressionNode * conditional_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  int line;


}ConditionalExpressionNode;

typedef struct constant_expression_node
{
  struct ConditionalExpressionNode * conditional_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  
}ConstantExpressionNode;

typedef struct logical_or_expression_node
{
  struct LogicalAndExpressionNode * logical_and_expression;
  struct LogicalOrExpressionNode * logical_or_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  
}LogicalOrExpressionNode;

typedef struct logical_and_expression_node
{
  struct LogicalAndExpressionNode * logical_and_expression;
  struct InclusiveOrExpressionNode * inclusive_or_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  
}LogicalAndExpressionNode;

typedef struct inclusive_or_expression_node
{
  struct ExclusiveOrExpressionNode * exclusive_or_expression;
  struct InclusiveOrExpressionNode * inclusive_or_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  
}InclusiveOrExpressionNode;

typedef struct exclusive_or_expression_node
{
  struct ExclusiveOrExpressionNode * exclusive_or_expression;
  struct AndExpressionNode * and_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  
}ExclusiveOrExpressionNode;

typedef struct and_expression_node
{

  struct EqualitiyExpressionNode * equality_expression;
  struct AndExpressionNode * and_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  
}AndExpressionNode;

typedef struct equality_expression_node
{

  struct RelationalExpressionNode * relational_expression;
  struct EqualityExpressionNode * equality_expression; 
  struct AndExpressionNode * and_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  
}EqualityExpressionNode;


typedef struct relational_expression_node
{

  struct ShiftExpressionNode * shift_expression;
  struct RelationalExpressionNode * relational_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  char * expression;

  
}RelationalExpressionNode;

typedef struct shift_expression_node
{

  struct AdditiveExpressionNode * additive_expression;
  struct ShiftExpressionNode * shift_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;

  
} ShiftExpressionNode;


typedef struct additive_expression_node
{

  int temporary_register;
  struct AdditiveExpressionNode * additive_expression;
  struct MultiplicativeExpressionNode * multiplicative_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  bool addition;
  int line;

  
} AdditiveExpressionNode;


typedef struct multiplicative_expression_node
{

  struct CastExpressionNode * cast_expression;
  struct MultiplicativeExpressionNode * multiplicative_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  bool mult;
  int line;

} MultiplicativeExpressionNode;


typedef struct cast_expression_node
{

  struct CastExpressionNode * cast_expression;
  struct UnaryExpressionNode * unary_expression;
  struct TypeNameNode * type_name;
  struct treeNode * identifier;
  struct ConstantNode * constant;

  
} CastExpressionNode;

typedef struct unary_expression_node
{

  struct CastExpressionNode * cast_expression;
  struct UnaryExpressionNode * unary_expression;
  struct UnaryOperatorNode * unary_operator;
  struct TypeNameNode * type_name;
  struct PostfixExpressionNode * postfix_expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;

  
} UnaryExpressionNode;

typedef struct unary_operator_node
{

  struct TypeNameNode * type_name;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  
} UnaryOperatorNode;



typedef struct postfix_expression_node
{
  struct PrimaryExpressionNode * primary_expression;
  struct PostfixExpressionNode * postfix_expression;
  struct ExpressionNode * expression;
  struct ArgumentExpressionListNode * argument_expression_list;



  struct treeNode * identifier;
  struct ConstantNode * constant;
  

}PostfixExpressionNode;

typedef struct primary_expression_node
{
  struct ExpressionNode * expression;
  struct treeNode * identifier;
  struct ConstantNode * constant;
  struct StringNode * string;

  

}PrimaryExpressionNode;

typedef struct argument_expression_list_node
{

  struct AssignmentExpressionNode * assignment_expression;
  struct ArgumentExpressionListNode * argument_expression_list;
  struct treeNode * identifier;
  struct ConstantNode * constant;


}ArgumentExpressionListNode;

typedef struct constant_node
{

  int  int_constant;
  bool int_flag;
  char   char_constant;
  bool char_flag;
  float  float_constant;
  bool float_flag;

  int temporary_register;

}ConstantNode;

typedef struct string_node
{
  char * charConstant;
  
}StringNode;

//Pointers used in our compiler
// Each pointer is used to hold a production
extern TranslationUnitNode * translation_unit_node;

//Pointers used in our compiler

extern StorageClassSpecifierNode * storage_class_specifier_node;

extern FunctionDefinitionNode * function_definition_node;

extern DeclarationSpecifiersNode * declaration_specifiers_node;

extern DirectDeclaratorNode * direct_declarator_node;

extern InitDeclaratorNode * init_declarator_node;

extern InitializerNode * initializer_node;

extern DeclaratorNode * declarator_node;

extern InitDeclaratorListNode * init_declarator_list_node;

extern DeclarationNode * declaration_node;

extern ExternalDeclarationNode * external_declaration_node;

extern TranslationUnitNode * translation_unit_node;

extern CompoundStatementNode * compound_statement_node;

extern DeclarationListNode * declaration_list_node;

extern LogicalOrExpressionNode * logical_or_expression_node;

extern LogicalAndExpressionNode * logical_and_expression_node;

extern InclusiveOrExpressionNode * incluseive_or_expresiion_node;

extern ConstantExpressionNode * constant_expression_node;

extern ConditionalExpressionNode * conditional_expression_node;

extern ExpressionNode * expression_node; 

extern StatementNode * statement_node;

extern StatementListNode * statement_list_node;

extern SelectionStatementNode * selection_statement_node;

extern IterationStatementNode * iteration_statement_node;

extern AssignmentExpressionNode * assignment_expression_node;

extern LogicalAndExpressionNode * logical_and_expression_node;

extern InclusiveOrExpressionNode * inclusive_or_expression_node;

extern AndExpressionNode * and_expression_node;

extern EqualityExpressionNode * equality_expression_node;

extern RelationalExpressionNode * relational_expression_node;

extern PrimaryExpressionNode * primary_expression_node;

extern PostfixExpressionNode * postfix_expression_node;

extern UnaryExpressionNode * unary_expression_node;

extern CastExpressionNode * cast_expression_node;

extern MultiplicativeExpressionNode * multiplicative_expression_node;

extern ShiftExpressionNode * shift_expression_node;

extern ExclusiveOrExpressionNode * exclusive_or_expression_node;

extern AdditiveExpressionNode * additive_expression_node;

extern ConstantNode * constant_node;

extern ExpressionStatementNode * expression_statement_node;

extern AssignmentOperatorNode * assignment_operator_node;

extern InitializerListNode * initializer_list_node;

extern TypeSpecifierNode * type_specifier_node;

extern ParameterTypeListNode * parameter_type_list_node;

extern ParameterListNode * parameter_list_node;

extern ParameterDeclarationNode * parameter_declaration_node;

extern ArgumentExpressionListNode * argument_expression_list_node;

//All the functions to print our AST because objects are for the weak
void ReportAST(TranslationUnitNode * ast);
void PrintTranslationUnit(TranslationUnitNode * ast);
void PrintExternalDeclaration(ExternalDeclarationNode * ast);
void PrintFunctionDefinition(FunctionDefinitionNode * ast);
void PrintDeclaration(DeclarationNode * ast);
void PrintDeclarationSpecifiers(DeclarationSpecifiersNode * ast);
void PrintInitDeclaratorList(InitDeclaratorListNode * ast);
void PrintInitDeclarator(InitDeclaratorNode * ast);
void PrintDeclarator(DeclaratorNode * ast);
void PrintDirectDeclarator(DirectDeclaratorNode * ast);
void PrintCompoundStatement(CompoundStatementNode * ast);
void PrintDeclarationList(DeclarationListNode * ast);
void PrintTranslationUnit(TranslationUnitNode * ast);
void PrintConstantExpression(ConstantExpressionNode * ast);
void PrintConditionalExpression(ConditionalExpressionNode * ast);
void PrintStatementList(StatementListNode * ast);
void PrintStatement(StatementNode * ast);
void PrintSelectionStatement(SelectionStatementNode * ast);
void PrintIterationStatement(IterationStatementNode * ast);
void PrintExpression(ExpressionNode * ast); 
void PrintAssignmentExpression(AssignmentExpressionNode * ast);
void PrintConditional(ConditionalExpressionNode * ast);
void PrintAndExpression(AndExpressionNode * ast);
void PrintEqualityExpression(EqualityExpressionNode *ast);
void PrintRelationalExpression(RelationalExpressionNode *ast);
void PrintShiftExpression(ShiftExpressionNode * ast);
void PrintAdditiveExpressionNode(AdditiveExpressionNode * ast);
void PrintMultiplicativeExpressionNode(MultiplicativeExpressionNode * ast);
void PrintCastExpressionNode(CastExpressionNode * ast);
void PrintUnaryExpressionNode(UnaryExpressionNode * ast);
void PrintPostfixExpressionNode(PostfixExpressionNode * ast);
void PrintPrimaryExpressionNOde(PrimaryExpressionNode * ast);
void PrintInitializer(InitializerNode * ast);
void PrintConstant(ConstantNode * ast);
void PrintExpressionStatement(ExpressionStatementNode * ast);
void PrintAssignmentOperator(AssignmentOperatorNode * ast);
void PrintIterationStatement(IterationStatementNode * ast);
void PrintInitializerList(InitializerListNode  * ast);
bool CheckAdditiveExpressionTypes(AdditiveExpressionNode * exp1, MultiplicativeExpressionNode  * exp2);


//Three Address Code
extern int register_count;
extern int label_count;

//All the functions to print our Three Address Code because objects are for the weak
void GenerateThreeAddressCode(TranslationUnitNode * ast);

#endif


