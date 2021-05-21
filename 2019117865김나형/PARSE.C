/****************************************************/
/* File: parse.c                                    */
/* The parser implementation for the TINY compiler  */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

static TokenType token; /* holds current token */

/* function prototypes for recursive calls */
static TreeNode* decl_list(void);
static TreeNode* decl(void);
static TreeNode* var_decl(void);
static ExpType type_spec(void);
//static TreeNode* fun_decl(void);
static TreeNode* params(void);
static TreeNode* param_list(ExpType type);
static TreeNode* param(ExpType type);
static TreeNode* comp_stmt(void);
static TreeNode* local_decl(void);
static TreeNode* stmt_list(void);
static TreeNode* stmt(void);
static TreeNode* expr_stmt(void);
static TreeNode* sel_stmt(void);
static TreeNode* iter_stmt(void);
static TreeNode* ret_stmt(void);
static TreeNode* expr(void);
//var
static TreeNode* simp_expr(TreeNode* f);
//relop
static TreeNode* add_expr(TreeNode* f);
//addop
static TreeNode* term(TreeNode* f);
//mulop
static TreeNode* factor(TreeNode* f);
static TreeNode* call(void);
static TreeNode* args(void);
static TreeNode* args_list(void);

static void syntaxError(char* message)
{
	fprintf(listing, "\n>>> ");
	fprintf(listing, "Syntax error at line %d: %s", lineno, message);
	Error = TRUE;
}

static void match(TokenType expected)
{
	if (token == expected) {
		token = getToken();
	}
	else {
		syntaxError("unexpected token -> ");
		printToken(token, tokenString);
		fprintf(listing, "      ");
	}
}

TreeNode* decl_list(void) //decl_list -> decl_list | decl
{
	TreeNode* t = decl();
	TreeNode* p = t;
	while (token != ENDFILE)
	{
		TreeNode* q;
		q = decl();
		if (q != NULL) {
			if (t == NULL) {
				t = p = q; // decl
			}
			else /* now p cannot be NULL either */
			{
				p->sibling = q;
				p = q; //decl_list
			}
		}
	}
	return t;
}

TreeNode* decl(void) // decl -> var-decl | fun-decl
{
	TreeNode* t = NULL;
	ExpType type; // 예상 값
	char* name;

	type = type_spec();
	name = copyString(tokenString);
	match(ID);

	switch (token)
	{
	case SEMI:
		t = newExpNode(VarDeclK);
		if (t != NULL)
		{
			t->attr.name = name;
			t->type = type;
		}
		match(SEMI);
		break;
	case LBRACE: // [ var-declaration
		t = newExpNode(VarArrayDeclK);
		if (t != NULL)
		{
			t->attr.name = name;
			t->type = type;
		}
		match(LBRACE);
		if (t != NULL)
			t->arraysize = atoi(tokenString);
		match(NUM);
		match(RBRACE);
		match(SEMI);
		break;
	case LPAREN: // ( fun-declaration
		t = newExpNode(FuncDeclK);
		if (t != NULL)
		{
			t->attr.name = name;
			t->type = type;
		}
		match(LPAREN);
		if (t != NULL) {
			t->child[0] = params();
		}
		match(RPAREN);
		if (t != NULL) {
			t->child[1] = comp_stmt();
		}
		break;
	default: syntaxError("unexpected token (decl) -> ");
		printToken(token, tokenString);
		token = getToken();
		break;
	}
	return t;
}
// var_decl -> type_spec id ; | type_spec id [ num ]
TreeNode* var_decl(void)
{
	TreeNode* t = NULL;
	ExpType type;
	char* name;

	type = type_spec();
	name = copyString(tokenString);
	match(ID);
	switch (token)
	{
	case SEMI:
		t = newExpNode(VarDeclK);
		if (t != NULL)
		{
			t->attr.name = name;
			t->type = type;
		}
		match(SEMI);
		break;
	case LBRACE: // [ -> var-declaration
		t = newExpNode(VarArrayDeclK);
		if (t != NULL)
		{
			t->attr.name = name;
			t->type = type;
		}
		match(LBRACE);
		if (t != NULL)
			t->arraysize = atoi(tokenString);
		match(NUM);
		match(RBRACE);
		match(SEMI);
		break;
	default: syntaxError("unexpected token(var_decl) -> ");
		printToken(token, tokenString);
		token = getToken();
		break;
	}
	return t;
}

ExpType type_spec(void) // type_spec -> int | void
{
	if (token == INT) {
		fprintf(listing, "");
	} //??

	switch (token)
	{
	case INT:
		token = getToken();
		return Integer;
	case VOID:
		token = getToken();
		return Void;
	default: syntaxError("unexpected token(type_spec) -> ");
		printToken(token, tokenString);
		token = getToken();
		return Void;
	}
}

//fun_decl -> type_spec id ; | type_spec id ( num );

TreeNode* params(void) // params -> param_list | void
{
	ExpType type;
	TreeNode* t;
	type = type_spec();

	if (type == Void && token == RPAREN) // void 단독 사용 불가
	{
		t = newExpNode(VarDeclK);
		t->isParam = TRUE;
		t->type = Void;
	}
	else {
		t = param_list(type);
	}

	return t;
}

TreeNode* param_list(ExpType type) // param_list -> param_list , param | param => param { , param }
{
	TreeNode* t = param(type);
	TreeNode* p = t;
	TreeNode* q;
	while (token == COMMA)
	{
		match(COMMA);
		q = param(type_spec()); //이게 문제...

		if (q != NULL) {
			if (t == NULL) {
				t = p = q;
			}
			else /* now p cannot be NULL either */
			{
				p->sibling = q;
				p = q;

			}
		}
	}
	return t;
}

TreeNode* param(ExpType type) // param -> type_spec id | type_spec [ ] 
{
	TreeNode* t;
	char* name;

	name = copyString(tokenString);
	match(ID);
	if (token == LBRACE) // [
	{
		match(LBRACE);
		match(RBRACE);
		t = newExpNode(VarArrayDeclK); // array
	}
	else {
		t = newExpNode(VarDeclK);
	}

	if (t != NULL)
	{
		t->attr.name = name;
		t->type = type;
		t->isParam = TRUE;
	}
	return t;
}

TreeNode* comp_stmt(void) // comp_stmt -> { local_decl stmt_list }
{
	TreeNode* t = newStmtNode(CompStmtK);
	match(LCURLY);
	t->child[0] = local_decl();
	t->child[1] = stmt_list();
	match(RCURLY);
	return t;
}

TreeNode* local_decl(void) // local_decl -> local_decl var_decl | empty
{
	TreeNode* t = NULL;
	TreeNode* p;

	if (token == INT || token == VOID)
		t = var_decl();
	p = t;
	if (t != NULL)
	{
		while (token == INT || token == VOID)
		{
			TreeNode* q;
			q = var_decl();
			if (q != NULL) {
				if (t == NULL) t = p = q;
				else /* now p cannot be NULL either */
				{
					p->sibling = q;
					p = q;
				}
			}
		}
	}
	return t;
}

TreeNode* stmt_list(void) // stmt_list -> stmt_list stmt | empty
{
	TreeNode* t;
	TreeNode* p;

	if (token == RCURLY)
		return NULL;
	t = stmt();
	p = t;
	while (token != RCURLY)
	{
		TreeNode* q;
		q = stmt();
		if (q != NULL) {
			if (t == NULL) t = p = q;
			else /* now p cannot be NULL either */
			{
				p->sibling = q;
				p = q;
			}
		}
	}

	return t;
}

TreeNode* stmt(void) //stmt -> expr_stmt stmt | empty
{
	TreeNode* t;
	switch (token)
	{
	case LCURLY:
		t = comp_stmt();
		break;
	case IF:
		t = sel_stmt();
		break;
	case WHILE:
		t = iter_stmt();
		break;
	case RETURN:
		t = ret_stmt();
		break;
	case ID:
	case LPAREN:
	case NUM:
	case SEMI:
		t = expr_stmt();
		break;
	default: syntaxError("unexpected token(stmt) -> ");
		printToken(token, tokenString);
		token = getToken();
		return Void;
	}
	return t;
}

TreeNode* expr_stmt(void) //expr_stmt -> expr ; | ;
{
	TreeNode* t = NULL;

	if (token == SEMI)
		match(SEMI);
	else if (token != RCURLY)
	{
		t = expr();
		match(SEMI);
	}
	return t;
}

TreeNode* sel_stmt(void) //sel_stmt -> if ( expression ) stmt | if ( expression ) stmt else stmt
{
	TreeNode* t = newStmtNode(SelStmtK);

	match(IF);
	match(LPAREN);
	if (t != NULL)
		t->child[0] = expr();
	match(RPAREN);
	if (t != NULL)
		t->child[1] = stmt();
	if (token == ELSE)
	{
		match(ELSE);
		if (t != NULL)
			t->child[2] = stmt();
	}

	return t;
}

TreeNode* iter_stmt(void) //iter_stmt -> while ( expression ) stmt else stmt
{
	TreeNode* t = newStmtNode(IterStmtK);

	match(WHILE);
	match(LPAREN);
	if (t != NULL)
		t->child[0] = expr();
	match(RPAREN);
	if (t != NULL)
		t->child[1] = stmt();
	return t;
}

TreeNode* ret_stmt(void) //return ; | return expr ;
{
	TreeNode* t = newStmtNode(RetStmtK);

	match(RETURN);
	if (token != SEMI && t != NULL)
		t->child[0] = expr();
	match(SEMI);
	return t;
}

TreeNode* expr(void) //var = expr | simple_expr
{
	TreeNode* t = NULL;
	TreeNode* q = NULL;
	int flag = FALSE;

	if (token == ID)
	{
		q = call();
		flag = TRUE;
	}

	if (flag == TRUE && token == ASSIGN)
	{
		if (q != NULL && q->nodekind == ExpK && q->kind.exp == IdK)
		{
			match(ASSIGN);
			t = newExpNode(AssignK);
			if (t != NULL)
			{
				t->child[0] = q;
				t->child[1] = expr();
			}
		}
		else
		{
			syntaxError("attempt to assign to something not an lvalue\n");
			token = getToken();
		}
	}
	else
		t = simp_expr(q);
	return t;
}

//var -> id | id [ expr ]

TreeNode* simp_expr(TreeNode* f) //add_expr relop add_expr | add_expr
{
	TreeNode* t, * q;
	TokenType oper;
	q = add_expr(f);
	if (token == LT || token == LE || token == GT || token == GE || token == EQ || token == NE)
	{
		oper = token;
		match(token);
		t = newExpNode(OpK);
		if (t != NULL)
		{
			t->child[0] = q;
			t->child[1] = add_expr(NULL);
			t->attr.op = oper;
		}
	}
	else
		t = q;
	return t;
}

//relop -> <= | < | > | >= | == | !=

TreeNode* add_expr(TreeNode* f) //add_expr addop term | term
{
	TreeNode* t;
	TreeNode* q;

	t = term(f);
	if (t != NULL)
	{
		while (token == PLUS || token == MINUS)
		{
			q = newExpNode(OpK);
			if (q != NULL) {
				q->child[0] = t;
				q->attr.op = token;
				t = q;
				match(token);
				t->child[1] = term(NULL);

			}
		}
	}
	return t;
}

//addop -> +|-

TreeNode* term(TreeNode* f)//term mulop factor | factor
{
	TreeNode* t;
	TreeNode* q;

	t = factor(f);
	if (t != NULL)
	{
		while (token == TIMES || token == OVER)
		{
			q = newExpNode(OpK);
			if (q != NULL) {
				q->child[0] = t;
				q->attr.op = token;
				t = q;
				match(token);
				t->child[1] = factor(NULL);

			}
		}
	}
	return t;
}

//mulop -> * | /

TreeNode* factor(TreeNode* f) //( expr ) | var | call | NUM
{
	TreeNode* t;

	if (f != NULL)
		return f;

	switch (token)
	{
	case LPAREN:
		match(LPAREN);
		t = expr();
		match(RPAREN);
		break;
	case ID:
		t = call();
		break;
	case NUM:
		t = newExpNode(ConstK);
		if (t != NULL)
		{
			t->attr.val = atoi(tokenString);
			t->type = Integer;
		}
		match(NUM);
		break;
	default: syntaxError("unexpected token(factor) -> ");
		printToken(token, tokenString);
		token = getToken();
		return Void;
	}
	return t;
}

TreeNode* call(void) // ID ( args )
{
	TreeNode* t;
	char* name = NULL;

	if (token == ID)
		name = copyString(tokenString);
	match(ID);

	if (token == LPAREN)
	{
		match(LPAREN);
		t = newStmtNode(CallK);
		if (t != NULL)
		{
			t->attr.name = name;
			t->child[0] = args();
		}
		match(RPAREN);
	}
	else if (token == LBRACE)
	{
		t = newExpNode(IdK);
		if (t != NULL)
		{
			t->attr.name = name;
			t->type = Integer;
			match(LBRACE);
			t->child[0] = expr();
			match(RBRACE);
		}
	}
	else
	{
		t = newExpNode(IdK);
		if (t != NULL)
		{
			t->attr.name = name;
			t->type = Integer;
		}
	}
	return t;
}

TreeNode* args(void) // arg_list | empty
{
	if (token == RPAREN)
		return NULL;
	else
		return args_list();
}

TreeNode* args_list(void) // arg_list , expression | expression 
{
	TreeNode* t;
	TreeNode* p;

	t = expr();
	p = t;
	if (t != NULL)
	{
		while (token == COMMA)
		{
			match(COMMA);
			TreeNode* q = expr();
			if (q != NULL) {
				if (t == NULL) t = p = q;
				else /* now p cannot be NULL either */
				{
					p->sibling = q;
					p = q;
				}
			}
		}
	}
	return t;
}

/****************************************/
/* the primary function of the parser   */
/****************************************/
/* Function parse returns the newly
 * constructed syntax tree
 */
TreeNode* parse(void)
{
	TreeNode* t;
	token = getToken();
	t = decl_list();
	if (token != ENDFILE)
		syntaxError("Code ends before file\n");
	return t;
}