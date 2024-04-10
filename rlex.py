import ply.lex as lex
tokens = [
    'ID',
    'INTEGER',
    'SEMICOLON',
    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'IF',
    'ELSE',
    'WHILE',
    'PLUS',
    'MINUS',
    'MULT',
    'DIV',
    'EQ',
    'LT',
    'GT',
    'ASSIGN',
    'COMMA',
    'FOR',
    'IN',
    'RANGE'
]

# Regular expression rules for simple tokens
t_SEMICOLON = r';'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULT = r'\*'
t_DIV = r'/'
t_EQ = r'=='
t_LT = r'<'
t_GT = r'>'
t_ASSIGN = r'='
t_COMMA = r','

# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    if t.value == 'if':
        t.type = 'IF'
    elif t.value == 'else':
        t.type = 'ELSE'
    elif t.value == 'while':
        t.type = 'WHILE'
    return t

def t_INTEGER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Error handling rule
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

input_code = '''
if x 
{
    y = x + z;
}

lexer.input(input_code)
for token in lexer:
    print(token)'''
