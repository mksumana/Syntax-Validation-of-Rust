import ply.yacc as yacc
# Get the token list from the lexer
from rlex import tokens
# Grammar rules
def p_program(p):
    '''program : statements'''
    p[0] = p[1]

def p_statements(p):
    '''statements : statement
                  | statement statements'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_statement(p):
    '''statement : ID ASSIGN expression SEMICOLON
                 | IF expression LBRACE statements RBRACE
                 | IF expression LBRACE statements RBRACE ELSE LBRACE statements RBRACE
                 | WHILE expression LBRACE statements RBRACE
                 | FOR expression IN RANGE LBRACE statements RBRACE
                 | expression SEMICOLON'''
    p[0] = p[1]

def p_expression(p):
    '''expression : INTEGER
                  | ID
                  | ID PLUS ID
                  | ID MINUS ID
                  | ID MULT ID
                  | ID DIV ID
                  | LPAREN expression RPAREN
                  | expression EQ expression
                  | expression LT expression
                  | expression GT expression'''
    p[0] = p[1]

# Error rule for syntax errors
def p_error(p):
    print(f"Syntax error at line {p.lineno}: Unexpected token '{p.value}'")

# Build the parser
parser = yacc.yacc()

if __name__ == "__main__":
    # Test the parser
    input_code = """
        if x<y {}
        {
            y = x +z;
        } 
    """
    result = parser.parse(input_code)
    if result:
        print("Accepted!")
    else:
        print("Rejected!")
