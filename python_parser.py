import ply.lex as lex
import ply.yacc as yacc

tokens = (
    'NUMBER',
    'STRING',
    'PLUS',
    'MINUS',
    'MUL',
    'DIVIDE',
    'LPAREN',
    'RPAREN',
    'WHILE',
    'COLON',
    'IF',
    'ELSE',
    'FOR',
    'LBRACE',
    'RBRACE',
    'ID',
    'EQUALS',
    'DEQUALS',  # means !=
    'LESSTHAN',
    'GREATERTHAN',
    'LESSTHANEQ',  # <=
    'GREATERTHANEQ',
    'COMMA',
    'INDENT',
    'DEDENT',
    'IN',
    'FLOAT',
    'RANGE',
    'SQBR',
    'SQBL',
    'NEQUALS',
    'DEF',
    'NEWLINE'
)

# Regular expression rules for simple tokens
# (Updated the regular expression for COMMA)

# this is declaration
t_COLON = r':'
t_PLUS = r'\+'
t_MINUS = r'-'
t_MUL = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_EQUALS = r'='
t_DEQUALS = r'=='
t_LESSTHAN = r'<'
t_GREATERTHAN = r'>'
t_LESSTHANEQ = r'<='
t_GREATERTHANEQ = r'>='
t_NEQUALS = r'!='
t_COMMA = r','
t_SQBR = r'\]'
t_SQBL = r'\['

reserved = {
    'while': 'WHILE',
    'if': 'IF',
    'else': 'ELSE',
    'for': 'FOR',
    'in': 'IN',
    'def': 'DEF'
}

indentation_stack = [0]

def t_INDENT(t):
    r'\n[ \t]*'
    t.lexer.lineno += 1
    new_indentation = len(t.value) - 1
    if new_indentation > indentation_stack[-1]:
        t.type = 'INDENT'
        indentation_stack.append(new_indentation)
        return t
    elif new_indentation < indentation_stack[-1]:
        indentation_stack.pop()
        t.type = 'DEDENT'
        return t

def t_RANGE(t):
    r'range'
    return t

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_STRING(t):
    r'\".*?\"'
    t.type = 'STRING'
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # Check for reserved words
    return t

# A regular expression rule with some action code
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'
t_ignore_COMMENT = r'\#.*'

def t_DEF(t):
    r'def'
    return t

def t_RETURN(t):
    r'return'
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MUL', 'DIVIDE'),
)

def p_program(p):
    '''program : statements
    '''
    p[0] = p[1]

def p_statements(p):
    '''statements : statement
                  | statement statements
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_statement(p):
    '''statement : IF LPAREN expression RPAREN COLON INDENT program DEDENT ELSE COLON INDENT program DEDENT
                 | IF LPAREN expression RPAREN COLON  INDENT program DEDENT
                 | FOR ID IN ID COLON INDENT program DEDENT
                 | WHILE LPAREN expression RPAREN COLON INDENT program DEDENT
                 | WHILE expression COLON INDENT program DEDENT
                 | expression
                 | ID EQUALS NUMBER
                 | ID EQUALS STRING
                 | ID EQUALS FLOAT
                 | tuple_declaration
                 | FOR ID IN STRING COLON INDENT program DEDENT
                 | FOR ID IN RANGE LPAREN expression RPAREN COLON INDENT program DEDENT
                 | FOR ID IN RANGE LPAREN ID COMMA ID COMMA ID RPAREN INDENT program DEDENT
                 | FOR ID IN RANGE LPAREN ID COMMA ID RPAREN INDENT program DEDENT
                 | FOR ID IN RANGE SQBL expression SQBR COLON INDENT program DEDENT 
                 | function_declaration
    '''
    if len(p) == 2 and p[1] == 'expression':
        p[0] = ('expression', p[1])
    elif len(p) == 9:
        p[0] = ('for', p[3], p[7])
    elif len(p) == 14:
        p[0] = ('if-else', p[3], p[7], p[12])
    elif len(p) == 8:
        p[0] = ('for', p[2], p[4], p[7])
    elif p[1] == "WHILE" and len(p) == 6:
        p[0] = ('while', p[2], p[5])
    elif p[1] == "WHILE" and len(p) == 8:
        p[0] = ('while', p[3], p[7])
    elif len(p) == 2:
        p[0] = (p[1])
    elif p[1] == "ID":
        p[0] = ("declaration", p[1], p[3])

# added here function declaration:
def p_function_declaration(p):
    '''
    function_declaration : DEF ID LPAREN ID COMMA ID RPAREN COLON INDENT program
    '''
    print("Found function declaration")
    pass

# ##

def p_tuple_declaration(p):
    '''tuple_declaration : ID EQUALS LPAREN element RPAREN
                         | ID EQUALS SQBL element SQBR
                         | ID EQUALS LBRACE element RBRACE'''
    p[0] = (p[1], p[4])

def p_element(p):
    '''element : STRING COMMA element
                 | STRING
                 | NUMBER COMMA element
                 | NUMBER
                 | FLOAT COMMA element
                 | FLOAT'''
    if len(p) > 2:
        p[0] = (p[1], p[3])
    else:
        p[0] = (p[1])

def p_expression(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression MUL expression
                  | expression DIVIDE expression
                  | LPAREN expression RPAREN
                  | expression LESSTHAN expression
                  | expression GREATERTHAN expression
                  | expression LESSTHANEQ expression
                  | expression EQUALS expression
                  | expression GREATERTHANEQ expression
                  | expression DEQUALS expression
                  | expression NEQUALS expression
                  | expression EQUALS expression PLUS expression
                  | ID
                  | NUMBER
                  | expression COMMA expression
    '''
    if len(p) == 4:
        p[0] = (p[2], p[1], p[3])
    else:
        p[0] = p[1]

def p_error(p):
    print(f"Syntax error at line {p.lineno}, position {p.lexpos}: Unexpected token '{p.value}'")
    # You can choose to handle the error gracefully or raise an exception
    # raise SyntaxError("Syntax error")


parser = yacc.yacc()

print("Enter the input code press enter on a new line to stop")
lines = []
while True:
    line = input()
    if line:
        lines.append(line)
    else:
        lines.append('\n')
        break
input_string = '\n'.join(lines)
print(input_string)
lexer.input(input_string)

# Tokenize
while True:
    tok = lexer.token()
    if not tok:
        break  # No more input
    print(tok)
print("\n")

# result = parser.parse(input_code)
result = parser.parse(input_string)

# print(result)
# print("\n")
if result:
    print("Accepted! ")
else:
    print("Rejected !")
print("_________\n")