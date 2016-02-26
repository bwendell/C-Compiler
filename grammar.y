%{
#include "driver.h"
#include "symTable.h"
#include "nodes.h"
#include <stdio.h>

void yyerror(char *);

%}

%union {
	int iVal;
	float dVal;
	char * sVal;
	char * identifierName;
	
	
  struct TranslationUnitNode * translation_unit_node;
  struct ExternalDeclarationNode * external_declaration_node;
  struct FunctionDefinitionNode * function_definition_node;
  struct CompoundStatementNode * compound_statement_node;
  struct DeclarationListNode * declaration_list_node;
  struct StatementListNode * statement_list_node;
  struct DeclarationNode * declaration_node;
  struct DeclarationSpecifiersNode * declaration_specifier_node;
  struct TypeSpecifierNode * type_specifier_node;
  struct StorageClassSpecifierNode * storage_class_specifier_node;
  struct DeclaratorNode * declarator_node;
  struct InitDeclaratorNode * init_declarator_node;
  struct InitDeclaratorListNode * init_declarator_list_node;
  struct DirectDeclaratorNode * direct_declarator_node;
  struct treeNode * indentifier_node;
  struct ConstantExpressionNode * constant_expression_node;
  struct ConditionalExpressionNode * conditional_expression_node;
  struct StatementNode * statement_node;
  struct SelectionStatementNode * selection_statement_node;
  struct IterationStatementNode * iteration_statement_node;
  struct ExpressionNode * expression_node;
  struct AssignmentExpressionNode * assignment_expression_node;
  struct InitializerNode * initializer_node;
  struct LogicalOrExpression * logical_or_expression_node;
  struct LogicalAndExpression * logical_and_expression_node;
  struct InclusiveOrExpression * inclusive_or_expression_node;
  struct AndExpressionNode * and_expression_node;
  struct EqualityExpressionNode * equality_expression_node;
  struct RelationalExpressionNode * relational_expression_node;
  struct PrimaryExpressionNode * primary_expression_node;
  struct PostfixExpressionNode * postfix_expression_node;
  struct UnaryExpressionNode * unary_expression_node;
  struct CastExpressionNode * cast_expression_node;
  struct MultiplicativeExpressionNode * multiplicative_expression_node; 
  struct ShiftExpressionNode * shift_expression_node;
  struct AdditiveExpressionNode * additive_expression_node;
  struct ExclusiveOrExpressionNode * exclusive_or_expression_node; 
  struct ExpressionStatementNode * expression_statement_node;
  struct ConstantNode * constant_node;
  struct AssignmentOperatorNode * assignment_operator_node;
  struct InitializerListNode * initializer_list_node; 
  struct ParameterTypeListNode * parameter_type_list_node;
  struct ParameterListNode * parameter_list_node;
  struct ParameterDeclarationNode * parameter_declaration_node;
  struct ArgumentExpressionList * argument_expression_list_node;
  struct JumpStatementNode * jump_statement_node;
}

%token IDENTIFIER 
%token INTEGER_CONSTANT FLOATING_CONSTANT CHARACTER_CONSTANT ENUMERATION_CONSTANT 
%token STRING_LITERAL 
%token SIZEOF
%token PTR_OP 
%token INC_OP DEC_OP 
%token LEFT_OP RIGHT_OP 
%token LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP 
%token MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN 
%token LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN 
%token TYPEDEF_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELIPSIS RANGE

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

//Ast token declarations

%type <translation_unit_node> translation_unit
%type <external_declaration_node> external_declaration
%type <function_definition_node> function_definition	
%type <compound_statement_node> compound_statement
%type <declaration_list_node> declaration_list
%type <statement_list_node> statement_list
%type <declaration_node> declaration
%type <declaration_specifier_node> declaration_specifiers
%type <type_specifier_node> type_specifier
%type <storage_class_specifier_node> storage_class_specifier
%type <declarator_node> declarator
%type <init_declarator_node> init_declarator
%type <init_declarator_list_node> init_declarator_list
%type <direct_declarator_node> direct_declarator
%type <constant_expression_node> constant_expression
%type <conditional_expression_node> conditional_expression
%type <indentifier_node> identifier
%type <statement_node> statement
%type <selection_statement_node> selection_statement
%type <iteration_statement_node> iteration_statement
%type <expression_node> expression;
%type <assignment_expression_node> assignment_expression;
%type <initializer_node> initializer;
%type < logical_or_expression_node> logical_or_expression;
%type < logical_and_expression_node> logical_and_expression;
%type < inclusive_or_expression_node> inclusive_or_expression;
%type < exclusive_or_expression_node> exclusive_or_expression;
%type < and_expression_node > and_expression;
%type < equality_expression_node > equality_expression;
%type < relational_expression_node > relational_expression;
%type < primary_expression_node > primary_expression;
%type < postfix_expression_node > postfix_expression;
%type < unary_expression_node > unary_expression;
%type < cast_expression_node > cast_expression;
%type < multiplicative_expression_node > multiplicative_expression;
%type < shift_expression_node > shift_expression;
%type < additive_expression_node > additive_expression;
%type <constant_node> constant;
%type <expression_statement_node> expression_statement;
%type <assignment_operator_node> assignment_operator;
%type <initializer_list_node> initializer_list;
%type <parameter_type_list_node> parameter_type_list;
%type <parameter_list_node> parameter_list;
%type <parameter_declaration_node> parameter_declaration;
%type <argument_expression_list_node> argument_expression_list;
%type<jump_statement_node> jump_statement;
%start translation_unit


%%

translation_unit
	: external_declaration
		{if (parseDebug){
   			fprintf(parseFile,"translation_unit <- external_declaration \n\n");
   			}
   			
     	  //AST 
		    translation_unit_node = (TranslationUnitNode *)malloc(1*sizeof(TranslationUnitNode));
		    translation_unit_node -> translation_unit = NULL;
		    translation_unit_node -> external_declaration = $1;		   
        $$ = translation_unit_node;
	      
		
   		}
	| translation_unit external_declaration
		{if (parseDebug){
			fprintf(parseFile,"translation_unit <- translation_unit external_declaration \n\n");
			}
     	  //AST 
		    translation_unit_node = (TranslationUnitNode *)malloc(1*sizeof(TranslationUnitNode));
		    translation_unit_node -> translation_unit = $1;
		    translation_unit_node -> external_declaration = $2;	
		    $$ = translation_unit_node;
	      		
			

		}
	;

external_declaration
	: function_definition
		{if (parseDebug){

			fprintf(parseFile,"external_declaration <- function_definition \n\n");
			}

		 	
			 //AST
			 external_declaration_node = (ExternalDeclarationNode *)malloc(1*sizeof(ExternalDeclarationNode));
			 external_declaration_node -> function_definition = $1;
			 $$ = external_declaration_node;

			
		}
	| declaration
		{if (parseDebug){
			fprintf(parseFile,"external_declaration <- declarationn \n\n");
			}

			 //AST
			 external_declaration_node = (ExternalDeclarationNode *)malloc(1*sizeof(ExternalDeclarationNode));
			 external_declaration_node -> declaration = $1;
			 $$ = external_declaration_node;
			
		}
		
	;

function_definition
	: declarator compound_statement
		{if (parseDebug){
			fprintf(parseFile,"function_definition <- declarator compound_statement \n\n");
			}		
		}
	| declarator declaration_list compound_statement
		{if(parseDebug){
			fprintf(parseFile,"function_definition <- declarator declaration_list compound_statement \n\n");
			}
						
						 
		}
	| declaration_specifiers declarator compound_statement
		{if(parseDebug){
			fprintf(parseFile,"function_definition <- declaration_specifiers declarator compound_statement \n\n");
			}
						
		 //AST
		 function_definition_node = (FunctionDefinitionNode *)malloc(1*sizeof(FunctionDefinitionNode));
		 function_definition_node -> declaration_specifiers = $1;
 		 function_definition_node -> compound_statement = $3;
		 function_definition_node -> declarator = $2;
		 $$ = function_definition_node;
		}
	| declaration_specifiers declarator declaration_list compound_statement
		{if(parseDebug){
			fprintf(parseFile,"function_definition <- declaration_specifiers declaration compound_statement \n\n");
			}

		}
	;

declaration
	: declaration_specifiers ';'
		{if(parseDebug){
			fprintf(parseFile,"declaration <- declaration_specifiers \n\n");
			}
		}
	| declaration_specifiers init_declarator_list ';'
		{if(parseDebug){
			fprintf(parseFile,"declaration <- declaration_specifiers init_declarator_list \n\n");
			}		
			 //AST
			 declaration_node = (DeclarationNode *)malloc(1*sizeof(DeclarationNode));
			 declaration_node -> declaration_specifiers  = $1;
			 declaration_node -> init_declarator_list = $2;
			 $$ = declaration_node;
		}
	;

declaration_list
	: declaration
		{if(parseDebug){
			fprintf(parseFile,"declaration_list <- declaration \n\n");
			fprintf(parseFile,"Look up is true \n\n");
			
			}
			lookUpMode = true;
			
				//AST
			 declaration_list_node = (DeclarationListNode *)malloc(1*sizeof(DeclarationListNode));
			 declaration_list_node -> declaration  = $1;
			 $$ = declaration_list_node;
			
			
	    //Reset flags for following declerations
      flags = reset;
		}
	| declaration_list declaration
		{if(parseDebug){
			fprintf(parseFile,"declaration_list <- declaration_list declaration \n\n");
			fprintf(parseFile,"Look up is true \n\n");
			}
			
				//AST
			 declaration_list_node = (DeclarationListNode *)malloc(1*sizeof(DeclarationListNode));
 			 declaration_list_node -> declaration_list = $1;
			 declaration_list_node -> declaration  = $2;
			 $$ = declaration_list_node;
		}
	;

declaration_specifiers
	: storage_class_specifier
		{if(parseDebug){
			fprintf(parseFile,"declaration_specifiers <- storage_class_specifier \n\n");
			}
		}
	| storage_class_specifier declaration_specifiers
		{if(parseDebug){
			fprintf(parseFile,"declaration_specifiers <- storage_class_specifier declaration_specifiers \n\n");
			}
		}
	| type_specifier
		{if(parseDebug){
			fprintf(parseFile,"declaration_specifiers <- type_specifier \n\n");
			fprintf(parseFile,"Insert Mode \n\n");
			}
        //AST
        declaration_specifiers_node = (DeclarationSpecifiersNode *)malloc(1*sizeof(DeclarationSpecifiersNode));
        declaration_specifiers_node->type_specifier = $1;
        $$ = declaration_specifiers_node;
        //Set Lookup mode to false
        lookUpMode = false;
        
			 
		}
	| type_specifier declaration_specifiers
		{if(parseDebug){
			fprintf(parseFile,"declaration_specifiers <- type_specifier declaration_specifiers \n\n");
			}
		}
	| type_qualifier 
		{if(parseDebug){
			fprintf(parseFile,"declaration_specifiers <- type_qualifier \n\n");
			}
		}
	| type_qualifier declaration_specifiers
		{if(parseDebug){
			fprintf(parseFile,"declaration_specifiers <- declaration_specifiers \n\n");
			}
		}
	;

storage_class_specifier
	: AUTO
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- AUTO \n\n");
			}
			
			//Set Flag
			flags.auto_flag = true;
		}	
	| REGISTER
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- REGISTER  \n\n");
			}
			
			//Set Flag
			flags.register_flag = true;
		}
	| STATIC
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- STATIC \n\n");
			}
			
						
			//Set Flag
			flags.static_flag = true;

		}
	| EXTERN
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- EXTERN \n\n");
			}
			
			//Set Flag
			flags.extern_flag = true;
		}
	| TYPEDEF
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- TYPEDEF \n\n");
			}
			
			
			//Set Flag
			flags.typedef_flag = true;
		}
	;

type_specifier
	: VOID
		{if(parseDebug){
			fprintf(parseFile,"type_specifier <- VOID \n\n");
			}
			
			//Set Flag
			flags.void_flag = true;
		}
	| CHAR
		{if(parseDebug){
			fprintf(parseFile,"type_specifier <- CHAR \n\n");
			}
			
						//Ast
			type_specifier_node = (TypeSpecifierNode * )malloc(1 * sizeof(TypeSpecifierNode));
			type_specifier_node -> type = "char";
			$$ = type_specifier_node;
			
			//Set Flag
			flags.char_flag = true;
		}
	| SHORT
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- SHORT \n\n");
			}
			
			//Set Flag
			flags.short_flag = true;
		}
	| INT
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- INT \n\n");
			}
			
			//Ast
			type_specifier_node = (TypeSpecifierNode * )malloc(1 * sizeof(TypeSpecifierNode));
			type_specifier_node -> type = "int";
			$$ = type_specifier_node;
			
			//Set Flag
			flags.int_flag = true;
		}
	| LONG
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- LONG \n\n");
			}
			
			//Set Flag
			flags.long_flag = true;
		}
	| FLOAT 
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- FLOAT \n\n");
			}
			
			//Ast
			type_specifier_node = (TypeSpecifierNode * )malloc(1 * sizeof(TypeSpecifierNode));
			type_specifier_node -> type = "float";
			$$ = type_specifier_node;
			
			//Set Flag
			flags.float_flag = true;
		}
	| DOUBLE
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- DOUBLE \n\n");
			}
			
			//Set Flag
			flags.double_flag = true;
		}
	| SIGNED
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- SIGNED \n\n");
			}
			
			//Set Flag
			flags.signed_flag = true;
		}
	| UNSIGNED
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- UNSIGNED \n\n");
			}
			
			//Set Flag
			flags.unsigned_flag = true;
		}
	| struct_or_union_specifier
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- struct_or_union_specifier \n\n");
			}
			
			//Set Flag
			flags.struct_flag = true;
		}
	| enum_specifier
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- enum_specifier \n\n");
			}
			
			//Set Flag
			flags.enum_flag = true;
		}
	| TYPEDEF_NAME
		{if(parseDebug){
			fprintf(parseFile,"storage_class_specifier <- TYPEDEF_NAME \n\n");
			}
			
			//Set Flag
			flags.typedef_flag = true;
		}
	;

type_qualifier
	: CONST
		{if(parseDebug){
			fprintf(parseFile,"type_qualifier <- CONST \n\n");
			}
			
			//Set Flag
			flags.const_flag = true;
		}
	
	| VOLATILE
		{if(parseDebug){
			fprintf(parseFile,"type_qualifier <- VOLATILE \n\n");
			}
			
			//Set Flag
			flags.volatile_flag = true;
		}
	;

struct_or_union_specifier
	: struct_or_union identifier '{' struct_declaration_list '}'
		{if(parseDebug){
			fprintf(parseFile,"struct_or_union_specifier <- struct_or_union identifier '{' struct_declaration_list '}' \n\n");
			}
		}
	| struct_or_union '{' struct_declaration_list '}'
		{if(parseDebug){
			fprintf(parseFile,"struct_or_union_specifier <- struct_or_union '{' struct_declaration_list '}' \n\n");
			}
		}
	| struct_or_union identifier
		{if(parseDebug){
			fprintf(parseFile,"struct_or_union_specifier <- struct_or_union identifier \n\n");
			}
		}
	;

struct_or_union
	: STRUCT
		{if(parseDebug){
			    fprintf(parseFile,"STRUCT \n\n");
			}
		}
	| UNION
		{if(parseDebug){
			fprintf(parseFile,"UNION \n\n");
		}
		}
	;

struct_declaration_list
	: struct_declaration
		{if(parseDebug){
			fprintf(parseFile,"struct_declaration_list <- struct_declaration \n\n");
			}
		}
	| struct_declaration_list struct_declaration
		{if(parseDebug){
			fprintf(parseFile,"struct_declaration_list <- struct_declaration_list struct_declaration \n\n");
			}
		}
	;

init_declarator_list
	: init_declarator
		{if(parseDebug){
			fprintf(parseFile,"init_declarator_list <- init_declarator \n\n");
			}
			 
			 //AST
			 init_declarator_list_node = (InitDeclaratorNode *)malloc(1*sizeof(InitDeclaratorNode));
			 init_declarator_list_node -> init_declarator = $1;
			 $$ = init_declarator_list_node;
		}
	| init_declarator_list ',' init_declarator
		{if(parseDebug){
			fprintf(parseFile,"init_declarator_list <- init_declarator_list ',' init_declarator \n\n");
			}
			
			 //AST
			 init_declarator_list_node = (InitDeclaratorNode *)malloc(1*sizeof(InitDeclaratorNode));
			 init_declarator_list_node -> init_declarator_list = $1;
			 init_declarator_list_node -> init_declarator = $3;
			 $$ = init_declarator_list_node;
		}
	;

init_declarator
	: declarator
		{if(parseDebug){
			fprintf(parseFile,"init_declarator <- declarator \n\n");
			}
			
			 //AST
			 init_declarator_node = (InitDeclaratorNode *)malloc(1*sizeof(InitDeclaratorNode));
			 init_declarator_node -> declarator = $1;
			 $$ = init_declarator_node;
		}
	| declarator '=' initializer
		{if(parseDebug){
			fprintf(parseFile,"init_declarator <- declarator '=' initializer \n\n");
			}
			 //AST
			 init_declarator_node = (InitDeclaratorNode *)malloc(1*sizeof(InitDeclaratorNode));
			 init_declarator_node -> declarator = $1;
			 init_declarator_node -> initializer = $3;
			 init_declarator_node->line = rowNum;
			 $$ = init_declarator_node;

		}
	;

struct_declaration
	: specifier_qualifier_list struct_declarator_list ';'
		{if(parseDebug){
			fprintf(parseFile,"struct_declaration <- specifier_qualifier_list struct_declarator_list ';' \n\n");
			}
		}
	;

specifier_qualifier_list
	: type_specifier
		{if(parseDebug){
			fprintf(parseFile,"specifier_qualifier_list <- type_specifier \n\n");
			}
		}
	| type_specifier specifier_qualifier_list
		{if(parseDebug){
			fprintf(parseFile,"specifier_qualifier_list <- type_specifier specifier_qualifier_list \n\n");
			}
		}
	| type_qualifier
		{if(parseDebug){
			fprintf(parseFile,"specifier_qualifier_list <- type_qualifier \n\n");
			}
		}
	| type_qualifier specifier_qualifier_list
		{if(parseDebug){
			fprintf(parseFile,"specifier_qualifier_list <- type_qualifier specifier_qualifier_list \n\n");
			}
		}
	;

struct_declarator_list
	: struct_declarator
		{if(parseDebug){
			fprintf(parseFile,"struct_declarator_list <- struct_declarator \n\n");
			}
		}
	| struct_declarator_list ',' struct_declarator
		{if(parseDebug){
			fprintf(parseFile,"struct_declarator_list <- struct_declarator_list ',' struct_declarator \n\n");
			}
		}
	;

struct_declarator
	: declarator
		{if(parseDebug){
			fprintf(parseFile,"struct_declarator <- declarator \n\n");
			}
		}
	| ':' constant_expression
		{if(parseDebug){
			fprintf(parseFile,"struct_declarator <- ':' constant_expression \n\n");
			}
		}
	| declarator ':' constant_expression
		{if(parseDebug){
			fprintf(parseFile,"struct_declarator <- declarator ':' constant_expression \n\n");
			}
		}
	;

enum_specifier
	: ENUM '{' enumerator_list '}'
		{if(parseDebug){
			fprintf(parseFile,"enum_specifier <- ENUM '{' enumerator_list '}' \n\n");
			}
		}
	| ENUM identifier '{' enumerator_list '}'
		{if(parseDebug){
			fprintf(parseFile,"enum_specifier <- ENUM identifier '{' enumerator_list '}' \n\n");
			}
		}
	| ENUM identifier
		{if(parseDebug){
			fprintf(parseFile,"enum_specifier <- ENUM identifier \n\n");
			}
		}
	;

enumerator_list
	: enumerator
		{if(parseDebug){
			fprintf(parseFile,"enumerator_list <- enumerator \n\n");
			}
		}
	| enumerator_list ',' enumerator
		{if(parseDebug){
			fprintf(parseFile,"enumerator_list <- enumerator_list ',' enumerator \n\n");
			}
		}
	;

enumerator
	: identifier
		{if(parseDebug){
			fprintf(parseFile,"enumerator <- identifier \n\n");
			}
		}
	| identifier '=' constant_expression
		{if(parseDebug){
			fprintf(parseFile,"enumerator <- identifier '=' constant_expression \n\n");
			}
		}
	;

declarator
	: direct_declarator
		{if(parseDebug){
			fprintf(parseFile,"declarator <- direct_declarator \n\n");
			}
			lookUpMode = true;
			 //AST
			 declarator_node = (DeclaratorNode *)malloc(1*sizeof(DeclaratorNode));
			 declarator_node -> direct_declarator = $1;

			 DirectDeclaratorNode * direct_declarator;
			 direct_declarator = $1;
			 declarator_node -> identifier = direct_declarator->identifier;
			 $$ = declarator_node;
			
		}
	| pointer direct_declarator
		{if(parseDebug){
			fprintf(parseFile,"declarator <- pointer direct_declarator \n\n");
			}
		}
	;

direct_declarator
	: identifier
		{if(parseDebug){
			fprintf(parseFile,"direct_declarator <- identifier \n\n");
			}
			 
			 //AST
			 direct_declarator_node = (DirectDeclaratorNode *)malloc(1*sizeof(DirectDeclaratorNode));
			 direct_declarator_node->identifier = $1;
			 $$ = direct_declarator_node;
			 
		}
	| '(' declarator ')'
		{if(parseDebug){
			fprintf(parseFile,"direct_declarator <- '(' declarator ')' \n\n");
			}
		}
	| direct_declarator '[' ']'
		{if(parseDebug){
			fprintf(parseFile,"direct_declarator <- direct_declarator '[' ']' \n\n");
			}
		}
	| direct_declarator '[' constant_expression ']'
		{if(parseDebug){
			fprintf(parseFile,"direct_declarator <- direct_declarator '[' constant_expression ']' \n\n");
			}
			
			 //AST
			 direct_declarator_node = (DirectDeclaratorNode *)malloc(1*sizeof(DirectDeclaratorNode));
			 direct_declarator_node->direct_declarator = $1;
			 direct_declarator_node->constant_expression = $3;
			 direct_declarator_node->array_flag = true;
			 DirectDeclaratorNode * direct_delcarator_temp = $1;
			 direct_declarator_node->identifier = direct_delcarator_temp->identifier;
			 $$ = direct_declarator_node;
		 	 if(currentIdentifier->array_size){
		 	 	printf("2d Array\n");
				 currentIdentifier->array_size2 = currentIdentifier->dataI;
				 int arraySize = currentIdentifier->dataI;
				 variable_offset = (variable_offset - (4 * arraySize));



				 variable_offset = (variable_offset)  + (currentIdentifier->array_size2 * currentIdentifier->array_size * 4);
		 	 }else{

				 currentIdentifier->array_flag = true;	
				 currentIdentifier->array_size = currentIdentifier->dataI;
				 int arraySize = currentIdentifier->dataI;
				 variable_offset = variable_offset -4;
				 variable_offset = (variable_offset)  + ((arraySize) * 4);


		 	 }


		}
	| direct_declarator '(' ')' 
		{if(parseDebug){
			fprintf(parseFile,"direct_declarator <- direct_declarator '(' ')' \n\n");
			}
		}
	| direct_declarator '(' parameter_type_list ')'
		{if(parseDebug){
			fprintf(parseFile,"direct_declarator <- direct_declarator '(' parameter_type_list ')' \n\n");
			}


			 //AST
			 direct_declarator_node = (DirectDeclaratorNode *)malloc(1*sizeof(DirectDeclaratorNode));
			 direct_declarator_node->direct_declarator = $1;
			 direct_declarator_node->parameter_type_list= $3;
			 DirectDeclaratorNode * direct_delcarator_temp = $1;
			 direct_declarator_node->identifier = direct_delcarator_temp->identifier;
			 treeNode * x = direct_declarator_node->identifier;
			 x->function_flag = true;

			 $$ = direct_declarator_node;
			 
		}
	| direct_declarator '(' identifier_list ')'
		{if(parseDebug){
			fprintf(parseFile,"direct_declarator <- direct_declarator '(' identifier_list ')' \n\n");
			}
		}
	;

pointer
	: '*'
		{if(parseDebug){
			fprintf(parseFile,"pointer <- '*' \n\n");
			}
		}
	| '*' type_qualifier_list
		{if(parseDebug){
			fprintf(parseFile,"pointer <- '*' type_qualifier_list \n\n");
			}
		}
	| '*' pointer
		{if(parseDebug){
			fprintf(parseFile,"pointer <- '*' pointer \n\n");
			}
		}
	| '*' type_qualifier_list pointer
		{if(parseDebug){
			fprintf(parseFile,"pointer <- '*' type_qualifier_list pointer \n\n");
			}
		}
	;

type_qualifier_list
	: type_qualifier
		{if(parseDebug){
			fprintf(parseFile,"type_qualifier_list <- type_qualifier \n\n");
			}
		}
	| type_qualifier_list type_qualifier
		{if(parseDebug){
			fprintf(parseFile,"type_qualifier_list <- type_qualifier_list type_qualifier_list \n\n");
			}
		}
	;

parameter_type_list
	: parameter_list
		{if(parseDebug){
			fprintf(parseFile,"parameter_type_list <- parameter_list \n\n");
			}


			 //AST
			 parameter_type_list_node = (ParameterTypeListNode *)malloc(1*sizeof(ParameterTypeListNode));
			 parameter_type_list_node->parameter_list = $1;
			 ParameterListNode * parameter_list_temp = $1;
			 parameter_type_list_node->identifier = parameter_list_temp->identifier;
			 $$ = parameter_type_list_node;
	
		}
	| parameter_list ',' ELIPSIS
		{if(parseDebug){
			fprintf(parseFile,"parameter_type_list <- parameter_list ',' ELIPSIS \n\n");
			}
		}
	;

parameter_list
	: parameter_declaration
		{if(parseDebug){
			fprintf(parseFile,"parameter_list <- parameter_declaration \n\n");
			}
		}
	| parameter_list ',' parameter_declaration
		{if(parseDebug){
			fprintf(parseFile,"parameter_list <- parameter_list ',' parameter_declaration \n\n");
			}
		}
	;

parameter_declaration
	: declaration_specifiers declarator
		{if(parseDebug){
			fprintf(parseFile,"parameter_declaration <- declaration_specifiers declarator \n\n");
			}

			 parameter_declaration_node = (ParameterDeclarationNode *)malloc(1*sizeof(ParameterDeclarationNode));
			 parameter_declaration_node->declaration_specifiers = $1;
			 parameter_declaration_node->declarator = $2;
			 DeclaratorNode * declarator_temp = $2;
			 parameter_declaration_node->identifier = declarator_temp->identifier;
			 $$ = parameter_declaration_node;
		}
	| declaration_specifiers
		{if(parseDebug){
			fprintf(parseFile,"parameter_declaration <- declaration_specifiers \n\n");
			}
		}
	| declaration_specifiers abstract_declarator
		{if(parseDebug){
			fprintf(parseFile,"parameter_declaration <- declaration_specifiers abstract_declarator \n\n");
			}
		}
	;

identifier_list
	: identifier
		{if(parseDebug){
			fprintf(parseFile,"identifier_list <- identifier \n\n");
			}
		}
	| identifier_list ',' identifier
		{if(parseDebug){
			fprintf(parseFile,"identifier_list <- identifier_list ',' identifier \n\n");
			}
		}
	;

initializer
	: assignment_expression
		{if(parseDebug){
			fprintf(parseFile,"initializer <- assignment_expression \n\n");
			}
			
			 //AST
			 initializer_node = (InitializerNode *)malloc(1*sizeof(InitializerNode));
			 initializer_node->assignment_expression = $1;
			AssignmentExpressionNode * temp;
			temp = $1;
			initializer_node->identifier = temp->identifier;
			initializer_node->constant = temp->constant;
			$$ = initializer_node;

		}
	| '{' initializer_list '}'
		{if(parseDebug){
			fprintf(parseFile,"initializer <- '{' initializer_list '}' \n\n");
			}
			
			//AST
			 initializer_node = (InitializerNode *)malloc(1*sizeof(InitializerNode));
			 initializer_node->initializer_list = $2;
			 $$ = initializer_node;
		}
	| '{' initializer_list ',' '}'
		{if(parseDebug){
			fprintf(parseFile,"initializer <- '{' initializer_list ',' '}' \n\n");
			}
			
				//AST
			 initializer_node = (InitializerNode *)malloc(1*sizeof(InitializerNode));
			 initializer_node->initializer_list = $2;
			 $$ = initializer_node;
		}
	;

initializer_list
	: initializer
		{if(parseDebug){
			fprintf(parseFile,"initializer_list <- initializer \n\n");
			}
			
			 //AST
			 initializer_list_node = (InitializerNode *)malloc(1*sizeof(InitializerNode));
			 initializer_list_node->initializer= $1;
			 $$ = initializer_list_node;
		}
	| initializer_list ',' initializer
		{
		if(parseDebug){
			fprintf(parseFile,"initializer_list <- initializer_list ',' initializer \n\n");
			}
			
			 //AST
			 initializer_list_node = (InitializerNode *)malloc(1*sizeof(InitializerNode));
			 initializer_list_node->initializer_list= $1;
			 initializer_list_node->initializer= $3;
			 $$ = initializer_list_node;
		}
	;

type_name
	: specifier_qualifier_list
		{if(parseDebug){
			fprintf(parseFile,"type_name <- specifier_qualifier_list \n\n");
			}
		}
	| specifier_qualifier_list abstract_declarator
		{if(parseDebug){
			fprintf(parseFile,"type_name <- specifier_qualifier_list abstract_declarator \n\n");
			}
		}
	;

abstract_declarator
	: pointer
		{if(parseDebug){
			fprintf(parseFile,"abstract_declarator <- pointer \n\n");
			}
		}
	| direct_abstract_declarator
		{if(parseDebug){
			fprintf(parseFile,"abstract_declarator <- direct_abstract_declarator \n\n");
			}
		}
	| pointer direct_abstract_declarator
		{if(parseDebug){
			fprintf(parseFile,"abstract_declarator <- pointer direct_abstract_declarator \n\n");
			}
		}
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
		{if(parseDebug){
			fprintf(parseFile,"direct_abstract_declarator <- '(' abstract_declarator ')' \n\n");
			}
		}
	| '[' ']'
		{if(parseDebug){
			fprintf(parseFile,"direct_abstract_declarator <- '[' ']' \n\n");
			}
		}
	| '[' constant_expression ']'
		{if(parseDebug){
			fprintf(parseFile,"direct_abstract_declarator <- '[' constant_expression ']' \n\n");
			}
		}
	| direct_abstract_declarator '[' ']'
		{if(parseDebug){
			fprintf(parseFile,"direct_abstract_declarator <- direct_abstract_declarator '[' ']' \n\n");
			}
		}
	| direct_abstract_declarator '[' constant_expression ']'
		{if(parseDebug){
			fprintf(parseFile,"direct_abstract_declarator <- direct_abstract_declarator '[' constant_expression ']' \n\n");
			}
		}
	| '(' ')'
		{if(parseDebug){
			fprintf(parseFile,"direct_abstract_declarator <- '(' ')' \n\n");
			}
		}
	| '(' parameter_type_list ')'
		{if(parseDebug){
			fprintf(parseFile,"direct_abstract_declarator <- '(' parameter_type_list ')' \n\n");
			}
		}
	| direct_abstract_declarator '(' ')'
		{if(parseDebug){
			fprintf(parseFile,"direct_abstract_declarator <- direct_abstract_declarator '(' ')' \n\n");
			}
		}
	| direct_abstract_declarator '(' parameter_type_list ')'
		{if(parseDebug){
			fprintf(parseFile,"direct_abstract_declarator <- direct_abstract_declarator '(' parameter_type_list ')' \n\n");
			}
		}
	;

statement
	: labeled_statement
		{if(parseDebug){
			fprintf(parseFile,"statement <- labeled_statement \n\n");
			}

		}
	| compound_statement
		{if(parseDebug){
			fprintf(parseFile,"statement <- compound_statement \n\n");
			}
			 //AST
			 statement_node = (StatementNode *)malloc(1*sizeof(StatementNode));
			 statement_node->compound_statement = $1;
			 $$ = statement_node;
			
		}
	| expression_statement
		{if(parseDebug){
			fprintf(parseFile,"statement <- expression_statement \n\n");
			}
			
				//AST 
		    statement_node = (StatementNode *)malloc(1*sizeof(StatementNode));
		    statement_node -> expression_statement = $1;
		    $$ = statement_node;
		}
	| selection_statement
		{if(parseDebug){
			fprintf(parseFile,"statement <- selection_statement \n\n");
			}
			

		    statement_node = (StatementNode *)malloc(1*sizeof(StatementNode));
		    statement_node->selection_statement = $1;
		    $$ = statement_node;

		}
	| iteration_statement
		{if(parseDebug){
			fprintf(parseFile,"statement <- iteration_statement \n\n");
			}
			
				//AST 
		    statement_node = (StatementNode *)malloc(1*sizeof(StatementNode));
		    statement_node -> iteration_statement = $1;
		    statement_node->line = rowNum;
		    $$ = statement_node;
	
			
		}
	| jump_statement
		{if(parseDebug){
			fprintf(parseFile,"statement <- jump_statement \n\n");
			}

			//AST 
		    statement_node = (StatementNode *)malloc(1*sizeof(StatementNode));
		    statement_node -> jump_statement = $1;
		    statement_node->line = rowNum;
		    $$ = statement_node;

		}
	;

labeled_statement
	: identifier ':' statement
		{if(parseDebug){
			fprintf(parseFile,"labeled_statement <- identifier ':' \n\n");
			}
		}
	| CASE constant_expression ':' statement
		{if(parseDebug){
			fprintf(parseFile,"labeled_statement <- CASE constant_expression ':' statement \n\n");
			}
		}
	| DEFAULT ':' statement
		{if(parseDebug){
			fprintf(parseFile,"DEFAULT ':' statement \n\n");
			}
		}
	;

expression_statement
	: ';'
		{if(parseDebug){
			fprintf(parseFile,"expression_statement <- ';' \n\n");
			}
		}
	| expression ';'
		{if(parseDebug){
			fprintf(parseFile,"expression_statement <- expression ';' \n\n");
			}
			
			 expression_statement_node = (ExpressionStatementNode *)malloc(1*sizeof(ExpressionStatementNode));
			 expression_statement_node->expression = $1;
			 $$ = expression_statement_node;
		}
	;

compound_statement
	: '{' '}'
		{if(parseDebug){
			fprintf(parseFile,"compound_statement <- '{' '}' \n\n");
			}
			 //Look Up Mode
			 lookUpMode = true;
			 
			 //AST TODO ADD {}
			 compound_statement_node = NULL;
			 $$ = compound_statement_node;


		}
	| '{' statement_list '}'
		{if(parseDebug){
			fprintf(parseFile,"compound_statement <- '{' statement_list '}' \n\n");
			}
			
			 //Look Up Mode
			 lookUpMode = true;
			  			
			 //AST TODO ADD {}
			 compound_statement_node = (CompoundStatementNode *)malloc(1*sizeof(CompoundStatementNode));
			 compound_statement_node->statement_list = $2;
			 $$ = compound_statement_node;
		}
	| '{' declaration_list '}'
		{if(parseDebug){
			fprintf(parseFile,"compound_statement <- '{' declaration_list '}' \n\n");
			}
			
			  //Look Up Mode
			  lookUpMode = true;
			   symbolTable = PushLevel(symbolTable,level++);	
			 //AST TODO ADD {}
			 compound_statement_node = (CompoundStatementNode *)malloc(1*sizeof(CompoundStatementNode));
			 compound_statement_node->declaration_list = $2;
			 $$ = compound_statement_node;
 			symbolTable->size = variable_offset;
			 symbolTable = PopLevel(symbolTable);	
		}
	| '{' declaration_list statement_list '}'
		{if(parseDebug){
			fprintf(parseFile,"compound_statement <- '{' declaration_list statement_list '}'  \n\n");
			}
			  //Look Up Mode
			  lookUpMode = true;
			 symbolTable = PushLevel(symbolTable,level++);	
				//AST TODO ADD {}
			 compound_statement_node = (CompoundStatementNode *)malloc(1*sizeof(CompoundStatementNode));
			 compound_statement_node->declaration_list = $2;
 			 compound_statement_node->statement_list = $3;
			 $$ = compound_statement_node;
			symbolTable->size = variable_offset;
			symbolTable = PopLevel(symbolTable);	
			
		}
	;

statement_list
	: statement
		{if(parseDebug){
			fprintf(parseFile,"statement_list <- statement  \n\n");
			}
			
			 statement_list_node = (StatementListNode *)malloc(1*sizeof(StatementListNode));
			 statement_list_node->statement = $1;
			 $$ = statement_list_node;
		

		}
	| statement_list statement
		{if(parseDebug){
			fprintf(parseFile,"statement_list <- statement_list statement \n\n");
			}

			 statement_list_node = (StatementListNode *)malloc(1*sizeof(StatementListNode));
			 statement_list_node->statement_list = $1;
	 	     statement_list_node->statement = $2;
			 $$ = statement_list_node;

		}
	;

selection_statement
	: IF '(' expression ')' statement
		{if(parseDebug){
			fprintf(parseFile,"selection_statement <- IF '(' expression ')' statement  \n\n");
			}
			
			 selection_statement_node = (SelectionStatementNode *)malloc(1*sizeof(SelectionStatementNode));
			 selection_statement_node -> expression = $3;
			 selection_statement_node -> statement = $5;
			 $$ = selection_statement_node;
		}
	| IF '(' expression ')' statement ELSE statement
		{if(parseDebug){
			fprintf(parseFile,"selection_statement <- IF '(' expression ')' statement ELSE statement \n\n");
			}
			
			 selection_statement_node = (SelectionStatementNode *)malloc(1*sizeof(SelectionStatementNode));
			 selection_statement_node -> expression = $3;
			 selection_statement_node -> statement = $5;
			 selection_statement_node -> statement2 = $7;
			 $$ = selection_statement_node;
		}
	| SWITCH '(' expression ')' statement
		{if(parseDebug){
			fprintf(parseFile,"selection_statement <- SWITCH '(' expression ')' statement \n\n");
			}
		}
	;

iteration_statement
	: WHILE '(' expression ')' statement
		{if(parseDebug){
			fprintf(parseFile,"iteration_statement <- WHILE '(' expression ')' statement \n\n");
			}			
			lookUpMode = true;
			iteration_statement_node = (IterationStatementNode *)malloc(1*sizeof(IterationStatementNode));
			iteration_statement_node -> expression= $3;
			iteration_statement_node -> statement = $5;
			$$ = iteration_statement_node;
			
		}
	| DO statement WHILE '(' expression ')' ';'
		{if(parseDebug){
			fprintf(parseFile,"iteration_statement <- DO statement WHILE '(' expression_statement ')'; \n\n");
			}
		}
	| FOR '(' ';' ';' ')' statement
		{if(parseDebug){
			fprintf(parseFile,"iteration_statement <- FOR '(' ';' ';' ')' statement \n\n");
			}
		}
	| FOR '(' ';' ';' expression ')' statement
		{if(parseDebug){
			fprintf(parseFile,"iteration_statement <- FOR '(' ';' ';' expression ')' statement \n\n");
			}
		}
	| FOR '(' ';' expression ';' ')' statement
		{if(parseDebug){
			fprintf(parseFile,"iteration_statement <- FOR '(' ';' expression ';' ')' statement \n\n");
			}
		}
	| FOR '(' ';' expression ';' expression ')' statement
		{if(parseDebug){
			fprintf(parseFile,"iteration_statement <- FOR '(' ';' expression ';' expression ')' statement \n\n");
			}
		}
	| FOR '(' expression ';' ';' ')' statement
		{if(parseDebug){
			fprintf(parseFile,"iteration_statement <- FOR '(' expression ';' ';' ')' statement \n\n");
			}
		}
	| FOR '(' expression ';' ';' expression ')' statement
		{if(parseDebug){
			fprintf(parseFile,"iteration_statement <- FOR '(' expression ';' ';' expression ')' statement \n\n");
			}
		}
	| FOR '(' expression ';' expression ';' ')' statement
		{if(parseDebug){
			fprintf(parseFile,"iteration_statement <- FOR '(' expression ';' expression ';' ')' statement \n\n");
			}
		}
	| FOR '(' expression ';' expression ';' expression ')' statement
		{if(parseDebug){
			fprintf(parseFile,"iteration_statement <- FOR '(' expression ';' expression ';' expression ')' statement \n\n");
			}

			lookUpMode = true;
			iteration_statement_node = (IterationStatementNode *)malloc(1*sizeof(IterationStatementNode));
			iteration_statement_node -> expression= $3;
			iteration_statement_node -> expression1= $5;
			iteration_statement_node -> expression2= $7;
			iteration_statement_node -> statement = $9;
			$$ = iteration_statement_node;


		}
	;

jump_statement
	: GOTO identifier ';'
		{if(parseDebug){
			fprintf(parseFile,"jump_statement <- GOTO identifier ';' \n\n");
			}
		}
	| CONTINUE ';'
		{if(parseDebug){
			fprintf(parseFile,"jump_statement <- CONTINUE ';' \n\n");
			}
		}
	| BREAK ';'
		{if(parseDebug){
			fprintf(parseFile,"jump_statement <- BREAK ';' \n\n");
			}
		}
	| RETURN ';'
		{if(parseDebug){
			fprintf(parseFile,"jump_statement <- RETURN ';' \n\n");
			}
		}
	| RETURN expression ';'
		{if(parseDebug){
			fprintf(parseFile,"jump_statement <- RETURN expression';' \n\n");
			}
		}
	;

expression
	: assignment_expression
		{if(parseDebug){
			fprintf(parseFile,"expression <- assignment_expression \n\n");
			}
			
			//AST
			expression_node = (ExpressionNode *)malloc(1*sizeof(ExpressionNode));
			expression_node->assignment_expression = $1;

			AssignmentExpressionNode * temp = $1;
			expression_node->identifier = temp->identifier; 
			expression_node->constant = temp->constant;
			expression_node->line = rowNum;
			$$ = expression_node; 
		}
	| expression ',' assignment_expression
		{if(parseDebug){
			fprintf(parseFile,"expression <- expression ',' assignment_expression \n\n");
			}
			
			
			//AST
			expression_node = (ExpressionNode *)malloc(1*sizeof(ExpressionNode));
			expression_node-> expression = $1;
			expression_node->assignment_expression = $3;
			$$ = expression_node; 
		}
	;

assignment_expression
	: conditional_expression
		{if(parseDebug){
			fprintf(parseFile,"assignment_expression <- conditional_expression \n\n");
			}
			
			//AST
			assignment_expression_node = (AssignmentExpressionNode *)malloc(1*sizeof(AssignmentExpressionNode));
			assignment_expression_node-> conditional_expression = $1;
			ConditionalExpressionNode * temp;
			temp = $1;
			assignment_expression_node->identifier = temp->identifier;
			assignment_expression_node->constant = temp->constant;
		    assignment_expression_node->line = rowNum;
			$$ = assignment_expression_node;
		}
	| unary_expression assignment_operator assignment_expression
		{if(parseDebug){
			fprintf(parseFile,"assignment_expression <- unary_expression assignment_operator assignment_expression \n\n");
			}
			
			assignment_expression_node = (AssignmentExpressionNode *)malloc(1*sizeof(AssignmentExpressionNode));
			assignment_expression_node-> unary_expression = $1;
			assignment_expression_node-> assignment_expression = $3;
			AssignmentExpressionNode * temp;
			temp = $3;
			assignment_expression_node->identifier = temp->identifier;
			assignment_expression_node->constant = temp->constant;
		    assignment_expression_node->line = rowNum;
			$$ = assignment_expression_node;
		}
	;

assignment_operator
	: '='
	{if(parseDebug){
			fprintf(parseFile,"assignment_operator <- = \n\n");
			}
	}
	
	| MUL_ASSIGN
	{if(parseDebug){
			fprintf(parseFile,"assignment_operator <- MUL_ASSIGN \n\n");
			}
		}
	| DIV_ASSIGN
	{if(parseDebug){
			fprintf(parseFile,"assignment_operator <- =  DIV_ASSIGN\n\n");
			}
		}
	| MOD_ASSIGN
	{if(parseDebug){
			fprintf(parseFile,"assignment_operator <- = MOD_ASSIGN\n\n");
			}
		}
	| ADD_ASSIGN
	{if(parseDebug){
			fprintf(parseFile,"assignment_operator <- = ADD_ASSIGN\n\n");
			}
		}
	| SUB_ASSIGN
	{if(parseDebug){
			fprintf(parseFile,"assignment_operator <- = SUB_ASSIGN \n\n");
			}
		}
	| LEFT_ASSIGN
	{if(parseDebug){
			fprintf(parseFile,"assignment_operator <- = LEFT_ASSIGNn\n");
			}
		}
	| RIGHT_ASSIGN
	{if(parseDebug){
			fprintf(parseFile,"assignment_operator <- = RIGHT_ASSIGN\n\n");
			}
		}
	| AND_ASSIGN
	{if(parseDebug){
			fprintf(parseFile,"assignment_operator <- = AND_ASSIGNn\n");
			}
		}
	| XOR_ASSIGN
	{if(parseDebug){
			fprintf(parseFile,"assignment_operator <- = XOR_ASSIGN\n\n");
			}
		}
	
	;

conditional_expression
	: logical_or_expression
		{if(parseDebug){
			fprintf(parseFile,"conditional_expression <- logical_or_expression \n \n");
			}
			 //AST TODO ADD {}
			 conditional_expression_node = (ConditionalExpressionNode *)malloc(1*sizeof(ConditionalExpressionNode));
			 conditional_expression_node->logical_or_expression= $1;
			LogicalOrExpressionNode * temp;
			temp = $1;
			conditional_expression_node->identifier = temp->identifier;
			conditional_expression_node->constant = temp->constant;
			conditional_expression_node->line = rowNum;
			 $$ = conditional_expression_node;
		}
	| logical_or_expression '?' expression ':' conditional_expression
		{if(parseDebug){
			fprintf(parseFile,"conditional_expression <- logical_or_expression '?' expression ':' conditional_expression \n\n");
			}
		}
	;

constant_expression
	: conditional_expression
		{if(parseDebug){
			fprintf(parseFile,"constant_expression <- conditional_expression \n\n");
			}
			
			//AST
			
			 //AST TODO ADD {}
			 constant_expression_node = (ConstantExpressionNode *)malloc(1*sizeof(ConstantExpressionNode));
			 constant_expression_node->conditional_expression= $1;
			ConditionalExpressionNode * temp;
			temp = $1;
			constant_expression_node->identifier = temp->identifier;
			constant_expression_node->constant = temp->constant;
			 $$ = constant_expression_node;
			
		}
	;

logical_or_expression
	: logical_and_expression
		{if(parseDebug){
			fprintf(parseFile,"logical_or_expression <- logical_and_expression \n\n");
			}
			
			 //AST TODO ADD {}
			logical_or_expression_node = (LogicalOrExpressionNode *)malloc(1*sizeof(LogicalOrExpressionNode));
			logical_or_expression_node->logical_and_expression = $1;
			LogicalAndExpressionNode * temp;
			temp = $1;
			logical_or_expression_node->identifier = temp->identifier;
			logical_or_expression_node->constant = temp->constant;
			$$ = logical_or_expression_node;
		}
	| logical_or_expression OR_OP logical_and_expression
		{if(parseDebug){
			fprintf(parseFile,"logical_or_expression <- logical_or_expression OR_OP logical_and_expression \n\n");
			}
		}
	;

logical_and_expression
	: inclusive_or_expression
		{if(parseDebug){
			fprintf(parseFile,"logical_and_expression <- inclusive_or_expression \n\n");
			}
			
			//AST TODO ADD {}
			logical_and_expression_node = (LogicalAndExpressionNode *)malloc(1*sizeof(LogicalAndExpressionNode));
			logical_and_expression_node->inclusive_or_expression = $1;
			InclusiveOrExpressionNode * temp;
			temp = $1;
			logical_and_expression_node->identifier = temp->identifier;
			logical_and_expression_node->constant = temp->constant;

			$$ = logical_and_expression_node;
		}
	| logical_and_expression AND_OP inclusive_or_expression
		{if(parseDebug){
			fprintf(parseFile,"logical_and_expression <- logical_and_expression AND_OP inclusive_or_expression  \n\n");
			}
		}
	;

inclusive_or_expression
	: exclusive_or_expression
		{if(parseDebug){
			fprintf(parseFile,"inclusive_or_expression <- exclusive_or_expression  \n\n");
			}
			
			 //AST TODO ADD {}
			inclusive_or_expression_node = (LogicalAndExpressionNode *)malloc(1*sizeof(LogicalAndExpressionNode));
			inclusive_or_expression_node->exclusive_or_expression = $1;
			ExclusiveOrExpressionNode * temp;
			temp = $1;
			inclusive_or_expression_node->identifier = temp->identifier;
			inclusive_or_expression_node->constant = temp->constant;
			 $$ = inclusive_or_expression_node;
		}
	| inclusive_or_expression '|' exclusive_or_expression
		{if(parseDebug){
			fprintf(parseFile,"inclusive_or_expression <- inclusive_or_expression '|' exclusive_or_expression \n");
			}
		}
	;

exclusive_or_expression
	: and_expression
		{if(parseDebug){
			fprintf(parseFile,"exclusive_or_expression <- and_expression \n\n");
			}
			exclusive_or_expression_node  = (ExclusiveOrExpressionNode *)malloc(1*sizeof(ExclusiveOrExpressionNode));
			exclusive_or_expression_node ->and_expression = $1;
			AndExpressionNode * temp;
			temp = $1;
			exclusive_or_expression_node->identifier = temp->identifier;
			exclusive_or_expression_node->constant = temp->constant;
			$$ = exclusive_or_expression_node ;

		}
	| exclusive_or_expression '^' and_expression
		{if(parseDebug){
			fprintf(parseFile,"exclusive_or_expression <- exclusive_or_expression '^' and_expression \n");
			}
		}
	;

and_expression
	: equality_expression
		{if(parseDebug){
			fprintf(parseFile,"and_expression <- equality_expression \n\n");
			}
			
			 //AST TODO ADD {}
			 and_expression_node = (AndExpressionNode *)malloc(1*sizeof(AndExpressionNode));
			and_expression_node->equality_expression = $1;
			EqualityExpressionNode * temp;
			temp = $1;
			and_expression_node->identifier = temp->identifier;
			and_expression_node->constant= temp->constant;

			$$ = and_expression_node;
		}
	| and_expression '&' equality_expression
		{if(parseDebug){
			fprintf(parseFile,"and_expression <- and_expression '&' equality_expression \n");
			}
		}
	;

equality_expression
	: relational_expression
		{if(parseDebug){
			fprintf(parseFile,"equality_expression <- relational_expression \n\n");
			}

			equality_expression_node = (EqualityExpressionNode *)malloc(1*sizeof(EqualityExpressionNode));
			equality_expression_node->relational_expression = $1;
			RelationalExpressionNode * temp;
			temp = $1;
			equality_expression_node->identifier = temp->identifier;
			equality_expression_node->constant = temp->constant;
			$$ = equality_expression_node;
		}
	| equality_expression EQ_OP relational_expression
		{if(parseDebug){
			fprintf(parseFile,"equality_expression <-  equality_expression EQ_OP relational_expression \n\n");
			}
		}
	| equality_expression NE_OP relational_expression
		{if(parseDebug){
			fprintf(parseFile,"equality_expression <- equality_expression NE_OP relational_expression \n\n");
			}
		}
	;

relational_expression
	: shift_expression
		{if(parseDebug){
			fprintf(parseFile,"relational_expression <- shift_expression \n\n");
			}
			relational_expression_node = (RelationalExpressionNode *)malloc(1*sizeof(RelationalExpressionNode));
			relational_expression_node->shift_expression = $1;
			ShiftExpressionNode * shift_temp;
			shift_temp = $1;
			relational_expression_node->identifier = shift_temp->identifier;
			relational_expression_node->constant = shift_temp->constant;
			$$ = relational_expression_node;
		}
	| relational_expression '<' shift_expression
		{if(parseDebug){
			fprintf(parseFile,"relational_expression <- relational_expression '<' shift_expression \n\n");
			}

			relational_expression_node = (RelationalExpressionNode *)malloc(1*sizeof(RelationalExpressionNode));
			relational_expression_node->relational_expression = $1;
			relational_expression_node->shift_expression = $3;
			relational_expression_node->expression = "<";
			$$ = relational_expression_node;
		}
	| relational_expression '>' shift_expression
		{if(parseDebug){
			fprintf(parseFile,"relational_expression <- relational_expression '>' shift_expression \n\n");
			}
			relational_expression_node = (RelationalExpressionNode *)malloc(1*sizeof(RelationalExpressionNode));
			relational_expression_node-> relational_expression = $1;
			relational_expression_node-> shift_expression = $3;
			relational_expression_node->expression = ">";
			$$ = relational_expression_node;
		}
	| relational_expression LE_OP shift_expression
		{if(parseDebug){
			fprintf(parseFile,"relational_expression <- relational_expression LE_OP shift_expression \n\n");
			}
			relational_expression_node = (RelationalExpressionNode *)malloc(1*sizeof(RelationalExpressionNode));
			relational_expression_node-> relational_expression = $1;
			relational_expression_node-> shift_expression = $3;
			relational_expression_node->expression = "<=";
			$$ = relational_expression_node;
		}
	| relational_expression GE_OP shift_expression
		{if(parseDebug){
			fprintf(parseFile,"relational_expression <- relational_expression GE_OP shift_expression \n\n");
			}

			relational_expression_node = (RelationalExpressionNode *)malloc(1*sizeof(RelationalExpressionNode));
			relational_expression_node-> relational_expression = $1;
			relational_expression_node-> shift_expression = $3;
			relational_expression_node->expression = ">=";
			$$ = relational_expression_node;
		}

	;

shift_expression
	: additive_expression
		{if(parseDebug){
			fprintf(parseFile,"shift_expression <- additive_expression \n\n");
			}
			
			shift_expression_node = (ShiftExpressionNode *)malloc(1*sizeof(ShiftExpressionNode));
			shift_expression_node->additive_expression = $1;
			AdditiveExpressionNode * temp;
			temp = $1;
			shift_expression_node->identifier = temp->identifier;
			shift_expression_node->constant = temp->constant;
			$$ = shift_expression_node;
		}
	| shift_expression LEFT_OP additive_expression
		{if(parseDebug){
			fprintf(parseFile,"shift_expression <- shift_expression LEFT_OP additive_expression \n\n");
			}
		}
	| shift_expression RIGHT_OP additive_expression
		{if(parseDebug){
			fprintf(parseFile,"shift_expression <- shift_expression RIGHT_OP additive_expression \n\n");
			}
		}
	;

additive_expression
	: multiplicative_expression
		{if(parseDebug){
			fprintf(parseFile,"additive_expression <- multiplicative_expression \n\n");
			}

			additive_expression_node = (AdditiveExpressionNode *)malloc(1*sizeof(AdditiveExpressionNode));
			additive_expression_node->multiplicative_expression = $1;
			MultiplicativeExpressionNode * temp;
			temp = $1;
			additive_expression_node->identifier = temp->identifier;
			additive_expression_node->constant = temp->constant;
			$$ = additive_expression_node;
		}
	| additive_expression '+' multiplicative_expression
		{if(parseDebug){
			fprintf(parseFile,"additive_expression <- additive_expression '+' multiplicative_expression \n\n");
			}
			
			additive_expression_node = (AdditiveExpressionNode *)malloc(1*sizeof(AdditiveExpressionNode));
			additive_expression_node->multiplicative_expression = $3;
			additive_expression_node->additive_expression = $1;
			additive_expression_node->addition = true;
			additive_expression_node->line = rowNum;
			$$ = additive_expression_node;

		}
	| additive_expression '-' multiplicative_expression
		{if(parseDebug){
			fprintf(parseFile,"additive_expression <- additive_expression '-' multiplicative_expression \n\n");
			}

			additive_expression_node = (AdditiveExpressionNode *)malloc(1*sizeof(AdditiveExpressionNode));
			additive_expression_node->multiplicative_expression = $3;
			additive_expression_node->additive_expression = $1;
			additive_expression_node->addition = false;
			additive_expression_node->line = rowNum;
			$$ = additive_expression_node;
		}
	;

multiplicative_expression
	: cast_expression
		{if(parseDebug){
			fprintf(parseFile,"multiplicative_expression <- cast_expression \n\n");
			}
			multiplicative_expression_node = (MultiplicativeExpressionNode *)malloc(1*sizeof(MultiplicativeExpressionNode));
			multiplicative_expression_node->cast_expression = $1;
			CastExpressionNode * temp;
			temp = $1;
			multiplicative_expression_node->identifier = temp->identifier;
			multiplicative_expression_node->constant = temp->constant;
			$$ = multiplicative_expression_node;
		}
	| multiplicative_expression '*' cast_expression
		{if(parseDebug){
			fprintf(parseFile,"multiplicative_expression <- multiplicative_expression '*' cast_expression \n\n");
			}


			multiplicative_expression_node = (MultiplicativeExpressionNode *)malloc(1*sizeof(MultiplicativeExpressionNode));
			multiplicative_expression_node ->multiplicative_expression = $1;
			multiplicative_expression_node ->cast_expression = $3;
			multiplicative_expression_node->mult = true;
			multiplicative_expression_node->line = rowNum;
			$$ = multiplicative_expression_node;

		}
	| multiplicative_expression '/' cast_expression
		{if(parseDebug){
			fprintf(parseFile,"multiplicative_expression <- multiplicative_expression '/' cast_expression \n\n");
			}
			multiplicative_expression_node = (MultiplicativeExpressionNode *)malloc(1*sizeof(MultiplicativeExpressionNode));
			multiplicative_expression_node ->multiplicative_expression = $1;
			multiplicative_expression_node ->cast_expression = $3;
			multiplicative_expression_node->mult = false;
			multiplicative_expression_node->line = rowNum;
			$$ = multiplicative_expression_node;
		}
	| multiplicative_expression '%' cast_expression
		{if(parseDebug){
			fprintf(parseFile,"multiplicative_expression <- multiplicative_expression ''AND'' cast_expression \n\n");
			}
		}
	;

cast_expression
	: unary_expression
		{if(parseDebug){
			fprintf(parseFile,"cast_expression <- unary_expression \n\n");
			}
			cast_expression_node = (CastExpressionNode *)malloc(1*sizeof(CastExpressionNode));
			cast_expression_node->unary_expression = $1;
			UnaryExpressionNode * temp;
			temp = $1;
			cast_expression_node->identifier = temp->identifier;
			cast_expression_node->constant = temp->constant;
			$$ = cast_expression_node;
		}
	| '(' type_name ')' cast_expression
		{if(parseDebug){
			fprintf(parseFile,"cast_expression <-  '(' type_name ')' cast_expression \n\n");
			}
		}
	;

unary_expression
	: postfix_expression
		{if(parseDebug){
			fprintf(parseFile,"unary_expression <- postfix_expression \n\n");
			}
		
		unary_expression_node = (UnaryExpressionNode *)malloc(1*sizeof(UnaryExpressionNode));
		unary_expression_node->postfix_expression = $1;
		PostfixExpressionNode * temp;
		temp = $1;
		unary_expression_node->identifier = temp->identifier;
		unary_expression_node->constant = temp->constant;
		$$ = unary_expression_node;

		}
	| INC_OP unary_expression
		{if(parseDebug){
			fprintf(parseFile,"unary_expression <- INC_OP unary_expression \n\n");
			}
		}
	| DEC_OP unary_expression
		{if(parseDebug){
			fprintf(parseFile,"unary_expression <- DEC_OP unary_expression \n\n");
			}
		}
	| unary_operator cast_expression
		{if(parseDebug){
			fprintf(parseFile,"unary_expression <- unary_operator cast_expression \n\n");
			}
		}
	| SIZEOF unary_expression
		{if(parseDebug){
			fprintf(parseFile,"unary_expression <- SIZEOF unary_expression \n\n");
			}
		}
	| SIZEOF '(' type_name ')'
		{if(parseDebug){
			fprintf(parseFile,"unary_expression <- SIZEOF '(' type_name ')' \n\n");
			}
		}
	;

unary_operator
	: '&'
		{if(parseDebug){
			fprintf(parseFile,"unary_operator <- & '(' type_name ')' \n\n");
			}
		}
	| '*'
		{if(parseDebug){
			fprintf(parseFile,"unary_operator <- * '(' type_name ')' \n\n");
			}
		}
	| '+'
		{if(parseDebug){
			fprintf(parseFile,"unary_operator <- + '(' type_name ')' \n\n");
			}
		}
	| '-'
		{if(parseDebug){
			fprintf(parseFile,"unary_operator <- - '(' type_name ')' \n\n");
			}
		}
	| '~'
		{if(parseDebug){
			fprintf(parseFile,"unary_operator <- ~ '(' type_name ')' \n\n");
			}
		}
	| '!'
		{if(parseDebug){
			fprintf(parseFile,"unary_operator <- ! '(' type_name ')' \n\n");
			}
		}
	;

postfix_expression
	: primary_expression
		{if(parseDebug){
			fprintf(parseFile,"postfix_expression <- primary_expression \n\n");
			}

			postfix_expression_node = (PostfixExpressionNode *)malloc(1*sizeof(PostfixExpressionNode));
			postfix_expression_node->primary_expression = $1;
			PrimaryExpressionNode * temp = $1;
			postfix_expression_node->identifier = temp->identifier;
			postfix_expression_node->constant = temp->constant;
			$$ = postfix_expression_node;
		}
	| postfix_expression '[' expression ']'
		{if(parseDebug){
			fprintf(parseFile,"postfix_expression <- postfix_expression '[' expression ']' \n\n");
			}
			
			postfix_expression_node = (PostfixExpressionNode *)malloc(1*sizeof(PostfixExpressionNode));
			postfix_expression_node->postfix_expression = $1;
			postfix_expression_node->expression = $3;
			PostfixExpressionNode * temp = $1;
			postfix_expression_node->identifier = temp->identifier;
			postfix_expression_node->constant = temp->constant;
			if(!lookUpMode){
				ConstantNode * constant_temp = temp->constant;
				variable_offset = currentIdentifier->offset *= constant_temp->int_constant;
			}	
			$$ = postfix_expression_node;


		

		}
	| postfix_expression '(' ')'
		{if(parseDebug){
			fprintf(parseFile,"postfix_expression <- postfix_expression '(' ')' \n\n");
			}
		}
	| postfix_expression '(' argument_expression_list ')'
		{if(parseDebug){
			fprintf(parseFile,"postfix_expression <- postfix_expression '(' argument_expression_list ')' \n\n");
			}

			postfix_expression_node = (PostfixExpressionNode *)malloc(1*sizeof(PostfixExpressionNode));
			postfix_expression_node->postfix_expression = $1;
			postfix_expression_node->argument_expression_list = $3;
			PostfixExpressionNode * temp = $1;
			postfix_expression_node->identifier = temp->identifier;
			postfix_expression_node->constant = temp->constant;
			$$ = postfix_expression_node;



		}
	| postfix_expression '.' identifier
		{if(parseDebug){
			fprintf(parseFile,"postfix_expression <- postfix_expression '.' identifier \n\n");
			}
		}
	| postfix_expression PTR_OP identifier
		{if(parseDebug){
			fprintf(parseFile,"postfix_expression <- postfix_expression PTR_OP identifier \n\n");
			}
		}
	| postfix_expression INC_OP
		{if(parseDebug){
			fprintf(parseFile,"postfix_expression <- postfix_expression INC_OP \n\n");
			}
		}
	| postfix_expression DEC_OP
		{if(parseDebug){
			fprintf(parseFile,"postfix_expression <- postfix_expression DEC_OP \n\n");
			}
		}
	;

primary_expression
	: identifier
		{if(parseDebug){
			fprintf(parseFile,"primary_expression <- identifier \n\n");
			}
			primary_expression_node = (PrimaryExpressionNode *)malloc(1*sizeof( PrimaryExpressionNode));
			primary_expression_node->identifier = $1;
			$$ = primary_expression_node;		

		}
	| constant
		{if(parseDebug){
			fprintf(parseFile,"primary_expression <- constant \n\n");
			}
			
			primary_expression_node = (PrimaryExpressionNode *)malloc(1*sizeof( PrimaryExpressionNode));
			primary_expression_node->constant = $1;
			$$ = primary_expression_node;	

		}
	| string
		{if(parseDebug){
			fprintf(parseFile,"primary_expression <- string \n\n");
			}
		}
	| '(' expression ')'
		{if(parseDebug){
			fprintf(parseFile,"primary_expression <- '(' expression ')' \n\n");
			}
		}
	;

argument_expression_list
	: assignment_expression
		{if(parseDebug){
			fprintf(parseFile,"argument_expression_list <- assignment_expression \n\n");
			}

			argument_expression_list_node = (ArgumentExpressionListNode *)malloc(1*sizeof( ArgumentExpressionListNode));
			argument_expression_list_node->assignment_expression = $1;
			AssignmentExpressionNode *assignment_temp = $1;
			argument_expression_list_node->identifier = assignment_temp->identifier;
			argument_expression_list_node->constant = assignment_temp->constant;
			$$ = argument_expression_list_node;	
		}
	| argument_expression_list ',' assignment_expression
		{if(parseDebug){
			fprintf(parseFile,"argument_expression_list <- argument_expression_list ',' assignment_expression \n\n");
			}
		}
	;

constant
	: INTEGER_CONSTANT  {
	            if(currentIdentifier != NULL){		
	                currentIdentifier->dataI = (int *) malloc(sizeof(int));
	                currentIdentifier->dataI = yylval.iVal;
	                currentIdentifier->flags.int_flag = true;
	               
                }

	                constant_node = (ConstantNode *)malloc(1*sizeof( ConstantNode ));
			            constant_node->int_flag = true;
			            constant_node->int_constant = yylval.iVal; 
			            $$ = constant_node;	
                
   
	            if(parseDebug){
			        fprintf(parseFile,"CONSTANT <- INTEGER_CONSTANT \n\n");
			    }
	         }
	| CHARACTER_CONSTANT {
	
	            if(currentIdentifier != NULL){
	                
	                currentIdentifier->dataC = (char *) malloc(sizeof(char));
	                currentIdentifier->dataC = yylval.sVal;
	                currentIdentifier->flags.char_flag = true;
                }
                
	                constant_node = (ConstantNode *)malloc(1*sizeof( ConstantNode ));
			            constant_node->char_flag = true;
			            constant_node->char_constant = yylval.sVal[1];
			            $$ = constant_node;	
	            if(parseDebug){
			        fprintf(parseFile,"CONSTANT <- CHARACTER_CONSTANT \n\n");
			    }


	         }
	| FLOATING_CONSTANT {	        
	
	       
	            if(parseDebug){
			        fprintf(parseFile,"CONSTANT ->FLOATING_CONSTANT \n\n");
			    }
			    
			    	    constant_node = (ConstantNode *)malloc(1*sizeof( ConstantNode ));
			            constant_node->float_flag = true;
			            constant_node->float_constant = yylval.dVal; 
			            $$ = constant_node;	
			    
	                
	               
	         }
	| ENUMERATION_CONSTANT {	        
	        
	            if(parseDebug){
			        fprintf(parseFile,"CONSTANT ->ENUMERATION_CONSTANT\n\n");
			    }
	         }
	;

string
	: STRING_LITERAL
	    {
	            if(currentIdentifier != NULL){
	                
	                currentIdentifier->dataC = (char *) malloc(sizeof(yylval.sVal));
	                currentIdentifier->dataC = yylval.sVal;
                }
	            if(parseDebug){
			        fprintf(parseFile,"string ->STRING_LITERAL \n\n");
			    }
        }
	;

identifier
	: IDENTIFIER { 
       
  int i;
  int n;
  treeNode * shadowCheck;
  node * shadow_level;
  //Cipher id name to numberical value
  for(i = 0, n = 0; i < sizeof(yylval.identifierName) ; i++){
    n += yylval.identifierName[i];       
  }
  //We are in decleration mode  
	if(!lookUpMode){
    //Check and make sure variable is not already declared in the level        
    currentIdentifier = FindIdentifier(symbolTable->treePtr, n);
    if(currentIdentifier != NULL && !currentIdentifier->function_flag ){
      yyerror("Error: ");
      printf("\t%s already declared on line number %d\n", yylval.identifierName, currentIdentifier->declerationLineNumber);
    }

    //Insert the id into the identifer tree
    symbolTable->treePtr = insertIdentifier(symbolTable->treePtr, n, flags);

    //Save the id name into the identifier tree 
    currentIdentifier = FindIdentifier(symbolTable->treePtr, n);
    currentIdentifier->name = yylval.identifierName;
    currentIdentifier->declerationLineNumber = rowNum;
    int temp = variable_offset;
    currentIdentifier->offset = temp-4;
    $$ = currentIdentifier;
    if(currentIdentifier->flags.int_flag){

    	variable_offset+= 4;

    }
    else if(currentIdentifier->flags.char_flag){

    	variable_offset+= 4;
    }
    else if(currentIdentifier->flags.float_flag){

    	variable_offset+= 32;
    }
			 
    //Check and see if it shadows 
   
    Shadows(n, symbolTable);
    flags = reset;
  //We are in look up mode
  }else {
     //Locate the identifier
     $$ = currentIdentifier = FindIdentifier(symbolTable->treePtr, n);
     //If not found, search the entire table
     if(currentIdentifier == NULL){
        $$ = currentIdentifier = FindIDTableScope(n, symbolTable);
        if(currentIdentifier == NULL){
          yyerror("Error: ");
          printf("\tVariable %s Not Declared ", yylval.identifierName);
        }
      }
	 }      
      if(parseDebug){
        fprintf(parseFile,"identifier ->IDENTIFIER (%s)\n", yylval.identifierName);
	    }        
  }
	;
%%

//TODO Fix up this dood with fancier error messages
void yyerror(char *msg)
{
  
  int index, lineTracker = 0, lineCount = 0;
  char * temp = yyget_text();
 
  //Go to appropriate line
  for(index = 0 ; index < rowNum - 1; index++){
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
      if(lineTracker < bufferSize){
        printf("%c", buffer[lineTracker]);
     
      }
      lineTracker++;
    
    }
    printf("\n");
  }
  
  if(scanner_error){
  
      for(index = 0; index < column ; index++){
      printf(" ");
      }
      printf("^\n");    
      scanner_error = false;
      printf("\t%s\n", msg);
      exit(0);
  
  }else {
    for(index = 0; index < column - 1; index++){
        printf(" ");
    }
      printf("^\n"); 
  }
   
  printf("\t%s line %d and column: %d\n", msg, rowNum, column);
  translation_unit_node = NULL;

  //exit(-1);
}


