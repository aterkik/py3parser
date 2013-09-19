""" Constants used by the lexer. """
import re

EOF = -1

KEYWORDS = ['False', 'None', 'True',
            'and', 'as', 'assert',
            'break', 'class', 'continue',
            'def', 'del', 'elif', 'else',
            'except', 'finally', 'for',
            'from', 'global', 'if', 'import',
            'in', 'is', 'lambda', 'nonlocal',
            'not', 'or', 'pass',
            'raise', 'return', 'try',
            'while', 'with', 'yield']

OPERATORS = {'//=': 'AUGINTDIV', # Augmented integer division
             '>>=': 'AUGRSHIFT',
             '<<=': 'AUGLSHIFT',
             '//': 'INTDIV',
             '<<': 'LSHIFT',
             '>>': 'RSHIFT',
             '<=': 'LTEQ',
             '>=': 'GTEQ',
             '==': 'EQ',
             '!=': 'NEQ',
             '+=': 'AUGPLUS',
             '-=': 'AUGMINUS',
             '*=': 'AUGTIMES',
             '**=': 'AUGEXP',
             '/=': 'AUGDIV',
             '%=': 'AUGMOD',
             '&=': 'AUGBITAND',
             '|=': 'AUGBITOR',
             '^=': 'AUGXOR',
             '+': 'PLUS',
             '-': 'MINUS',
             '*': 'TIMES',
            '**': 'EXP',
             '/': 'DIV',
             '%': 'MOD',
             '&': 'BITAND',
             '|': 'BITOR',
             '^': 'XOR',
             '~': 'NEG',
             '<': 'LT',
             '>': 'GT',
             '(': 'LPAR',
             ')': 'RPAR',
             '[': 'LBRACKET',
             ']': 'RBRACKET',
             '{': 'LBRACE',
             '}': 'RBRACE',
             ',': 'COMMA',
             ':': 'COLON',
             '.': 'PERIOD',
             ';': 'SEMICOLON',
             '@': 'AT',
             '=': 'ASSIGN',
             '->': 'RETSYM', # Used for function annotations
             '...': 'ELLIPSIS',
             # Not a valid op in Python. In here because of PEP 401.
             '<>': 'PEP401'} 

JOINERS = {'[' : ']', '{' : '}', '(' : ')'}

WHITESP = re.compile(r'^[ \t\r\f\v]')
WS_LINE = re.compile(r'^[\s]*[\n]')
KEYWORD_IDENT = re.compile('^[_a-zA-Z][_a-zA-Z0-9]*')
INTEGER = re.compile(r'^(0[oO][0-7]+)|(0[xX][0-9a-fA-F]+)'
                     '|(0[bB][01]+)|([1-9][0-9]*|0+)')
FLOAT_ = re.compile(r'^(([0-9]+|([0-9]*\.[0-9]+)|([0-9]+\.))'
                    r'[eE][+-]?[0-9]+)|(([0-9]*\.[0-9]+)|([0-9]+\.))')
IMAGINARY = re.compile(r'^(((([0-9]+|([0-9]*\.[0-9]+)|([0-9]+\.))'
                       r'[eE][+-]?[0-9]+)|(([0-9]*\.[0-9]+)|([0-9]'
                       r'+\.)))|([0-9]+))[jJ]')
SHORTB_DBLQ = re.compile(r'^(b|B|br|Br|bR|BR)?"(\\.|[^"\\])*"')
SHORTB_SINGLEQ = re.compile(r"^(b|B|br|Br|bR|BR)?'(\\.|[^'\\])*'")
LONGB_DBLQ = re.compile(r'^(b|B|br|Br|bR|BR)?"""(\\.|[^"\\]|"(?!""))*"""')
LONGB_SINGLEQ = re.compile(r"^(b|B|br|Br|bR|BR)?'''(\\.|[^'\\]|'(?!''))*'''")
SHORT_DBLQ = re.compile(r'^[rR]?"(\\.|[^"\\])*"')
SHORT_SINGLEQ = re.compile(r"^[rR]?'(\\.|[^'\\])*'")
LONG_DBLQ = re.compile(r'^[rR]?"""(\\.|[^"\\]|"(?!""))*"""')
LONG_SINGLEQ = re.compile(r"^[rR]?'''(\\.|[^'\\]|'(?!''))*'''")
