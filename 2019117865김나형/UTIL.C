/****************************************************/
/* File: util.c                                     */
/* Utility function implementation                  */
/* for the C minus compiler                         */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/
#define _CRT_SECURE_NO_WARNINGS
#include "globals.h"
#include "util.h"

char* typeName(ExpType type) {
	static char i[] = "int";
	static char v[] = "void";

	switch (type) {
		case Integer: return i;
		case Void: return v;
	}
}

/* Procedure printToken prints a token 
 * and its lexeme to the listing file
 */
void printToken( TokenType token, const char* tokenString )
{ switch (token)
  { case IF: fprintf(listing, "Reserved Word: if\n"); break;
    case ELSE: fprintf(listing, "Reserved Word: else\n"); break;

    case ASSIGN: fprintf(listing,"=\n"); break;
    case LT: fprintf(listing,"<\n"); break;
	case LE: fprintf(listing, "<=\n"); break;
	case GT: fprintf(listing, ">\n"); break;
	case GE: fprintf(listing, ">=\n"); break;
    case EQ: fprintf(listing,"==\n"); break;
	
    case LPAREN: fprintf(listing,"(\n"); break;
    case RPAREN: fprintf(listing,")\n"); break;
	case LCURLY: fprintf(listing, "{\n"); break;
	case RCURLY: fprintf(listing, "}\n"); break;
	case LBRACE: fprintf(listing, "[\n"); break;
	case RBRACE: fprintf(listing, "]\n"); break;
    case SEMI: fprintf(listing,";\n"); break;
    case PLUS: fprintf(listing,"+\n"); break;
    case MINUS: fprintf(listing,"-\n"); break;
    case TIMES: fprintf(listing,"*\n"); break;
    case OVER: fprintf(listing,"/\n"); break;
	case NE: fprintf(listing, "!=\n"); break;
    case ENDFILE: fprintf(listing,"EOF\n"); break;

	case COMMA: fprintf(listing, ",\n"); break;
	case INT: fprintf(listing, "Reserved Word: int\n"); break;
	case VOID: fprintf(listing, "Reserved Word: void\n"); break;
	case RETURN: fprintf(listing, "Reserved Word: return\n"); break;
	case WHILE: fprintf(listing, "Reserved Word: while\n"); break;
    case NUM:
		fprintf(listing, "NUM, val= %s\n",tokenString);
      break;
    case ID:
		fprintf(listing, "ID, name= %s\n",tokenString);
      break;
    case ERROR:
		fprintf(listing, "ERROR: %s\n",tokenString);
		//fprintf(listing, "ERROR\n");
      break;
    default: /* should never happen */
		fprintf(listing,"Unknown token: %d\n",token);
  }
}

/* Function newStmtNode creates a new statement
 * node for syntax tree construction
 */
TreeNode * newStmtNode(StmtKind kind)
{ TreeNode * t = (TreeNode *) malloc(sizeof(TreeNode));
  int i;
  if (t==NULL)
	  fputc(listing,"Out of memory error at line %d\n",lineno);
  else {
    for (i=0;i<MAXCHILDREN;i++) t->child[i] = NULL;
    t->sibling = NULL;
    t->nodekind = StmtK; //노드가 문장이라는 것을 나타낸다
    t->kind.stmt = kind; //복합문, if문, while문, return중 어떤것인지 삽입
    t->lineno = lineno;
  }
  return t;
}

/* Function newExpNode creates a new expression 
 * node for syntax tree construction
 */
TreeNode * newExpNode(ExpKind kind)
{ TreeNode * t = (TreeNode *) malloc(sizeof(TreeNode));
  int i;
  if (t==NULL)
	  fprintf(listing,"Out of memory error at line %d\n",lineno);
  else {
    for (i=0;i<MAXCHILDREN;i++) t->child[i] = NULL;
    t->sibling = NULL;
    t->nodekind = ExpK;
    t->kind.exp = kind;
    t->lineno = lineno;
    t->type = Void; //?
  }
  return t;
}

/* Function copyString allocates and makes a new
 * copy of an existing string
 */
char * copyString(char * s)
{ int n;
  char * t;
  if (s==NULL) return NULL;
  n = strlen(s)+1;
  t = malloc(n);
  if (t==NULL)
	  fprintf(listing,"Out of memory error at line %d\n",lineno);
  else strcpy(t,s);
  return t;
}

/* Variable indentno is used by printTree to
 * store current number of spaces to indent
 */
static indentno = 0;

/* macros to increase/decrease indentation */
#define INDENT indentno+=2
#define UNINDENT indentno-=2

/* printSpaces indents by printing spaces */
static void printSpaces(void)
{ int i;
  for (i=0;i<indentno;i++)
    fprintf(listing," ");
}

/* procedure printTree prints a syntax tree to the
* listing file using indentation to indicate subtrees
*/
void printTree(TreeNode* tree)
{
	int i;
	INDENT; //printTree함수호출할 수록 커서를 시작지점에서 2칸씩 공백을 주어 포함관계를 표현하기 위함
	while (tree != NULL) {
		printSpaces();
		//노드가 문장일 경우
		if (tree->nodekind == StmtK)
		{
			switch (tree->kind.stmt) {
			case CompStmtK:
				fprintf(listing, "Compound Statement :\n");
				break;
			case SelStmtK:
				fprintf(listing, "If\n");
				break;
			case IterStmtK:
				fprintf(listing, "While\n");
				break;
			case RetStmtK:;
				fprintf(listing, "return\n");
				break;
			case CallK:
				fprintf(listing, "Call : %s\n", tree->attr.name);
				break;
			default:
				fprintf(listing, "Unknown ExpNode kind\n");
				break;
			}
		}
		else if (tree->nodekind == ExpK)
		{
			switch (tree->kind.exp) {
			case VarDeclK:
				fprintf(listing, "Parameter : %s, %s\n", typeName(tree->type), tree->attr.name);
				break;
			case VarArrayDeclK:

				if (tree->isParam == TRUE)
					fprintf(listing, "Array Parameter: %s, %s\n", typeName(tree->type), tree->attr.name);
				else
					fprintf(listing, "Array : %s, %s, size : %d\n", typeName(tree->type), tree->attr.name, tree->arraysize);
				break;
			case FuncDeclK:
				fprintf(listing, "Function: %s, type : %s\n", tree->attr.name, typeName(tree->type));
				break;
			case AssignK:
				fprintf(listing, "Assign : \n");
				break;
			case OpK:
				fprintf(listing, "Op : ");
				printToken(tree->attr.op, "\0");
				break;
			case IdK:
				fprintf(listing, "Id : %s\n", tree->attr.name);
				break;
			case ConstK:
				fprintf(listing, "Const : %d\n", tree->attr.val);
				break;
			default:
				fprintf(listing, "Unknown ExpNode kind\n");
				break;
			}
		}
		else fprintf(listing, "Unknown node kind\n");
		for (i = 0; i < MAXCHILDREN; i++)
			printTree(tree->child[i]);
		tree = tree->sibling;
	}
	UNINDENT;
}