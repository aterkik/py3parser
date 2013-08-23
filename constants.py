import re

EOF = -1

KEYWORDS = ['False', 'None', 'True',
            'and', 'as', 'assert',
            'break', 'class', 'continue',
            'def', 'del', 'elif',
            'else', 'except', 'finally',
            'for', 'from', 'global',
            'if', 'import', 'in',
            'is', 'lambda', 'nonlocal',
            'not', 'or', 'pass',
            'raise', 'return', 'try',
            'while', 'with', 'yield']

OPERATORS = ['//=', '>>=', '<<=', '%%=',
             '**', '//', '<<', '>>',
             '<=', '>=', '==', '!=',
             '+=', '-=', '*=', '/=',
             '%=', '&=', '|=', '^=',
             '+', '-', '*', '**',
             '/', '%', '&', '|',
             '^', '~', '<', '>',
             '(', ')', '[', ']',
             '{', '}', ',', ':',
             '.', ';', '@', '=']

whitesp = re.compile(r'^[ \t\r\f\v]')
keyword_ident = re.compile('^[_a-zA-Z][_a-zA-Z0-9]*')
integer = re.compile(r'^(0[oO][0-7]+)|(0[xX][0-9a-fA-F]+)'
                      '|(0[bB][01]+)|([1-9][0-9]*|0+)')
float_ = re.compile(r'^(([0-9]+|([0-9]*\.[0-9]+)|([0-9]+\.))[eE][+-]?[0-9]+)|(([0-9]*\.[0-9]+)|([0-9]+\.))')
imaginary = re.compile(r'^(((([0-9]+|([0-9]*\.[0-9]+)|([0-9]+\.))[eE][+-]?[0-9]+)|(([0-9]*\.[0-9]+)|([0-9]+\.)))|([0-9]+))[jJ]')
shortb_dblq = re.compile(r'^(b|B|br|Br|bR|BR)?"(\\.|[^"\\])*"')
shortb_singleq = re.compile(r"^(b|B|br|Br|bR|BR)?'(\\.|[^'\\])*'")
longb_dblq = re.compile(r'^(b|B|br|Br|bR|BR)?"""(\\.|[^"\\]|"(?!""))*"""')
longb_singleq = re.compile(r"^(b|B|br|Br|bR|BR)?'''(\\.|[^'\\]|'(?!''))*'''")
short_dblq = re.compile(r'^[rR]?"(\\.|[^"\\])*"')
short_singleq = re.compile(r"^[rR]?'(\\.|[^'\\])*'")
long_dblq = re.compile(r'^[rR]?"""(\\.|[^"\\]|"(?!""))*"""')
long_singleq = re.compile(r"^[rR]?'''(\\.|[^'\\]|'(?!''))*'''")
