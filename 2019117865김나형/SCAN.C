/****************************************************/
/* File: scan.c                                     */
/* The scanner implementation for the C minus compiler */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"

/* states in scanner DFA */
typedef enum
{
	START, INEQ, INCOMMENT, INNUM, INID, DONE, INLT, INGT, INNE, INOVER
} // 필요 상태(state) 수정
StateType;

/* lexeme of identifier or reserved word */
char tokenString[MAXTOKENLEN + 1];

/* BUFLEN = length of the input buffer for
source code lines */
#define BUFLEN 256

static char lineBuf[BUFLEN]; /* holds the current line */
static int linepos = 0; /* current position in LineBuf */
static int bufsize = 0; /* current size of buffer string */
static int EOF_flag = FALSE; /* corrects ungetNextChar behavior on EOF */

							 /* getNextChar fetches the next non-blank character
							 from lineBuf, reading in a new line if lineBuf is
							 exhausted */

static int getNextChar(void)
{
	if (!(linepos < bufsize))
	{
		lineno++;
		if (fgets(lineBuf, BUFLEN - 1, source))
		{
			if (EchoSource) fprintf(listing, "%4d: %s", lineno, lineBuf);
			bufsize = strlen(lineBuf);
			linepos = 0;
			return lineBuf[linepos++];
		}
		else
		{
			EOF_flag = TRUE;
			return EOF;
		}
	}
	else return lineBuf[linepos++];
}

/* ungetNextChar backtracks one character
in lineBuf */
static void ungetNextChar(void)
{
	if (!EOF_flag) linepos--;
}

/* lookup table of reserved words */
static struct
{
	char* str;
	TokenType tok;
} reservedWords[MAXRESERVED]
= { // RESERVED WORDS(예약어) 수정
	{ "if",IF },{ "else",ELSE },{ "while",WHILE },
{ "return",RETURN },{ "int",INT },{ "void",VOID } };

/* lookup an identifier to see if it is a reserved word */
/* uses linear search */
static TokenType reservedLookup(char* s)
{
	int i;
	for (i = 0; i < MAXRESERVED; i++)
		if (!strcmp(s, reservedWords[i].str))
			return reservedWords[i].tok;
	return ID;
}

/****************************************/
/* the primary function of the scanner  */
/****************************************/
/* function getToken returns the
* next token in source file
*/
TokenType getToken(void)
{  /* index for storing into tokenString */
	int tokenStringIndex = 0;
	/* holds current token to be returned */
	TokenType currentToken;
	/* current state - always begins at START */
	StateType state = START;
	/* flag to indicate save to tokenString */

	int save;
	while (state != DONE)
	{
		int c = getNextChar();
		save = TRUE;
		switch (state)
		{
		case START:
			if (isdigit(c))
				state = INNUM;
			else if (isalpha(c))
				state = INID;
			else if ((c == ' ') || (c == '\t') || (c == '\n'))
				save = FALSE;

			else if (c == '/')
			{
				save = FALSE;
				state = INOVER;
			}
			else if (c == '=')
			{
				save = FALSE;
				state = INEQ;
			}
			else if (c == '<')
			{
				save = FALSE;
				state = INLT;
			}
			else if (c == '>')
			{
				save = FALSE;
				state = INGT;
			}
			else if (c == '!')
			{
				save = FALSE;
				state = INNE;
			}
			else
			{
				state = DONE;
				switch (c)
				{
				case EOF:
					save = FALSE;
					currentToken = ENDFILE;
					break;
				case '+':
					currentToken = PLUS;
					break;
				case '-':
					currentToken = MINUS;
					break;
				case '*':
					currentToken = TIMES;
					break;
				case '(':
					currentToken = LPAREN;
					break;
				case ')':
					currentToken = RPAREN;
					break;
				case '{':
					currentToken = LCURLY;
					break;
				case '}':
					currentToken = RCURLY;
					break;
				case '[':
					currentToken = LBRACE;
					break;
				case ']':
					currentToken = RBRACE;
					break;
				case ';':
					currentToken = SEMI;
					break;
				case ',':
					currentToken = COMMA;
					break;

				case 'if':
					currentToken = IF;
					break;
				case 'int':
					currentToken = INT;
					break;
				case 'retu':
					currentToken = RETURN;
					break;
				case 'void':
					currentToken = VOID;
					break;
				case 'whil':
					currentToken = WHILE;
					break;

				default:
					currentToken = ERROR;
					break;
				}
			}
			break;

			// COMMENTS 와 DIVISION(/) 분류
		case INOVER:
			if (c == '*') // COMMENTS(/* */)
			{
				state = INCOMMENT;
				save = FALSE;
			}
			else // DIVISION(/)
			{
				ungetNextChar();
				state = DONE;
				currentToken = OVER;
			}
			break;

			// ASSIGN(=) 과 EQUALITY(==) 분류
		case INEQ:
			state = DONE;
			if (c == '=') // EQUALITY(==)
				currentToken = EQ;
			else // ASSIGN(=)
			{
				currentToken = ASSIGN;
				ungetNextChar();
			}
			break;

			// LESS THAN(<) 과 LESS EQUAL(<=) 분류
		case INLT:
			state = DONE;
			if (c == '=') // LESS EQUAL(<=)
				currentToken = LE;
			else // LESS THAN(<)
			{
				currentToken = LT;
				ungetNextChar();
			}
			break;

			// GREATER THAN(>) 과 GREATER EQUAL(>=) 구분
		case INGT:
			state = DONE;
			if (c == '=') // GREATER EQUAL(>=)
				currentToken = GE;
			else // GREATER THAN(>)
			{
				currentToken = GT;
				ungetNextChar();
			}
			break;

			// NOT EQUAL(!=) 과 ERROR 구분
		case INNE:
			state = DONE;
			if (c == '=') // NOT EQUAL(!=)
				currentToken = NE;
			else // ERROR
				currentToken = ERROR;
			break;


			// 주석인 경우 체크 및 무시
		case INCOMMENT:
			save = FALSE;
			if (c == '*')
			{
				c = getNextChar();
				if (c == '/') // END OF COMMENTS
					state = START;
				else if (c == EOF) // END OF SOURCE CODES
				{
					state = DONE;
					currentToken = ENDFILE;
				}
				else // NOT END OF COMMENTS
					ungetNextChar();
			}
			else if (c == EOF) // END OF COMMENTS
			{
				state = DONE;
				currentToken = ENDFILE;
			}
			break;

		case INNUM:
			if (!isdigit(c))
			{ /* backup in the input */
				if (isalpha(c)) {
					currentToken = ERROR;
					break;
				} // NUM과 ID 섞일 경우  ERROR로 체크
				else if(currentToken != ERROR){
					currentToken = NUM;
				}
				ungetNextChar();
				save = FALSE;
				state = DONE;
			}
			break;
		case INID:
			if (!isalpha(c))
			{ /* backup in the input */
				if (isdigit(c)) {
					currentToken = ERROR;
					break;
				} // NUM과 ID 섞일 경우  ERROR로 체크
				else if(currentToken != ERROR){
					currentToken = ID;
				}
				ungetNextChar();
				save = FALSE;
				state = DONE;
			}
			break;
		case DONE:
		default: /* should never happen */
			fputc(listing, "Scanner Bug: state= %d\n", state);
			state = DONE;
			currentToken = ERROR;
			break;
		}
		if ((save) && (tokenStringIndex <= MAXTOKENLEN))
			tokenString[tokenStringIndex++] = (char)c;
		if (state == DONE)
		{
			tokenString[tokenStringIndex] = '\0';
			if (currentToken == ID)
				currentToken = reservedLookup(tokenString);
		}
	}
	if (TraceScan) {
		fprintf(listing, "\t%d: ", lineno);
		printToken(currentToken, tokenString);
	}
	return currentToken;
} /* end getToken */