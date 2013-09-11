"""Parser for a large subset of Python 3"""
from lexer import Lexer
from ply import yacc

from const import OPERATORS, KEYWORDS

TEST_INPUT = open('test.py.txt').read()

tokens = [
    'NEWLINE',
    'INDENT',
    'DEDENT',
    'ID',
    'LIT',
    'ENDMARKER'
    ]
tokens += map(lambda x: x.upper(), KEYWORDS)
tokens += OPERATORS.values()

"""EBNF production:
   file_input : (NEWLINE | stmt)* ENDMARKER"""
def p_file_input(p):
    """file_input : newlines_or_stmts ENDMARKER"""
    pass

def p_newlines_or_stmts(p):
    """newlines_or_stmts : empty
                | newlines 
                | stmts"""
    pass

def p_empty(p):
    """empty : """
    pass

def p_newlines(p):
    """newlines : NEWLINE
                | newlines NEWLINE"""
    pass

def p_stmts(p):
    """stmts : stmt
             | stmts stmt"""
    pass



"""EBNF production:
   decorator : '@' dotted_name [ '(' [arglist] ')' ] NEWLINE"""
def p_decorator(p):
    """decorator : AT dotted_name LPAR arglist RPAR NEWLINE
                 | AT dotted_name LPAR RPAR NEWLINE
                 | AT dotted_name NEWLINE"""
    pass

"""EBNF production:
   decorators : decorator+"""
def p_decorators(p):
    """decorators : decorator decorators_1"""
    pass

def p_decorators_1(p):
    """decorators_1 : decorator decorators_1
                    | empty"""
    pass

"""EBNF production:
   decorated: decorators (classdef | funcdef)"""
def p_decorated(p):
    """decorated : decorators classdef
                 | decorators funcdef"""
    pass


"""EBNF production:
   funcdef: 'def' NAME parameters ['->' test] ':' suite"""
def p_funcdef(p):
    """funcdef : DEF ID parameters RETSYM test COLON suite
               | DEF ID parameters COLON suite"""
    pass


# For parameters list, the productions used are
# found at docs.python.org/3.3/reference/compound_stmts.html#function
# not at the default grammar file.
def p_parameters(p):
    """parameters : LPAR parameter_list RPAR
                  | LPAR RPAR"""
    pass

def p_parameter_list(p):
    """parameter_list : defparameters_1 keyparams
                      | keyparams"""
    pass

def p_defparameters_1(p):
    """defparameters_1 : defparameters_2 COMMA
                       | defparameter COMMA defparameters_1"""
    pass

def p_keyparams(p):
    """keyparams : TIMES typedparameter defparameters_2 COMMA EXP typedparameter
                 | TIMES typedparameter COMMA EXP typedparameter
                 | TIMES typedparameter defparameters_2
                 | TIMES defparameters_2 COMMA EXP typedparameter
                 | TIMES COMMA EXP typedparameter
                 | TIMES defparameters_2
                 | TIMES typedparameter
                 | defparameter COMMA
                 | defparameter"""
    pass

def p_defparameters_2(p):
    """defparameters_2 : COMMA defparameter
                       | COMMA defparameter defparameters_2"""
    pass

def p_defparameter(p):
    """defparameter : typedparameter
                    | typedparameter ASSIGN test"""
    pass

def p_typedparameter(p):
    """typedparameter : ID
                      | ID COLON test"""
    pass





def p_varargslist(p):
    """varargslist : LPAR args_list RPAR
                   | LPAR RPAR"""
    pass

def p_args_list(p):
    """args_list : defargs_1 keyargs
                 | keyargs"""
    pass

def p_defargs_1(p):
    """defargs_1 : defargs_2 COMMA
                 | defarg COMMA defargs_1"""
    pass

def p_keyargs(p):
    """keyargs : TIMES untypedarg defargs_2 COMMA EXP untypedarg
               | TIMES untypedarg COMMA EXP untypedarg
               | TIMES untypedarg defargs_2 
               | TIMES defargs_2 COMMA EXP untypedarg
               | TIMES COMMA EXP untypedarg
               | TIMES defargs_2
               | TIMES untypedarg
               | defarg COMMA
               | defarg"""
    pass

def p_defargs_2(p):
    """defargs_2 : COMMA defarg 
                 | COMMA defarg defargs_2"""
    pass

def p_defarg(p):
    """defarg : untypedarg
              | untypedarg ASSIGN test"""
    pass
def p_untypedarg(p):
    """untypedarg : ID"""
    pass

"""EBNF production:
stmt : simple_stmt | compound_stmt"""
def p_stmt(p):
    """stmt : simple_stmt
            | compound_stmt"""
    pass

"""EBNF production:
simple_stmt : small_stmt (';' small_stmt)* [';'] NEWLINE"""
def p_simple_stmt(p):
    """simple_stmt : small_stmt small_stmts SEMICOLON NEWLINE
                   | small_stmt SEMICOLON NEWLINE
                   | small_stmt small_stmts NEWLINE
                   | small_stmt NEWLINE"""
    pass

def p_small_stmts(p):
    """small_stmts : SEMICOLON small_stmt
                   | SEMICOLON small_stmt small_stmts"""
    pass


"""EBNF production:
small_stmt : expr_stmt
           | del_stmt
           | pass_stmt
           | flow_stmt
           | import_stmt
           | global_stmt
           | nonlocal_stmt
           | assert_stmt"""
def p_small_stmt(p):
    """small_stmt : expr_stmt
                  | del_stmt
                  | pass_stmt
                  | flow_stmt
                  | import_stmt
                  | global_stmt
                  | nonlocal_stmt
                  | assert_stmt"""
    pass

"""EBNF production:
expr_stmt : testlist_star_expr (augassign (yield_expr|testlist) |
                        ('=' (yield_expr|testlist_star_expr))*)"""
def p_expr_stmt(p):
    """expr_stmt : testlist_star_expr rexpr_stmt_1"""
    pass

def p_rexpr_stmt_1(p):
    """rexpr_stmt_1 : augassign yield_expr
                  | augassign testlist
                  | rexpr_stmt_2"""
    pass

def p_rexpr_stmt_2(p):
    """rexpr_stmt_2 : ASSIGN yield_testlist_star
                    | ASSIGN yield_testlist_star rexpr_stmt_2"""
    pass

def p_yield_testlist_star(p):
    """yield_testlist_star : yield_expr
                           | testlist_star_expr"""
    pass


"""EBNF production:
   testlist_star_expr : (test|star_expr) (',' (test|star_expr))* [',']"""
def p_testlist_star_expr(p):
    """testlist_star_expr : test testlist_right
                          | star_expr testlist_right"""
    pass

def p_testlist_right(p):
    """testlist_right : test_star_exprs COMMA
                      | COMMA
                      | empty"""
    pass

def p_test_star_exprs(p):
    """test_star_exprs : COMMA test
                       | COMMA star_expr
                       | COMMA test test_star_exprs
                       | COMMA star_expr test_star_exprs"""
    pass


"""EBNF production:
augassign : ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' |
            '<<=' | '>>=' | '**=' | '//=')"""
def p_augassign(p):
    """augassign : AUGPLUS
                 | AUGMINUS
                 | AUGTIMES
                 | AUGDIV
                 | AUGMOD
                 | AUGBITAND
                 | AUGBITOR
                 | AUGXOR
                 | AUGLSHIFT
                 | AUGRSHIFT
                 | AUGEXP
                 | AUGINTDIV"""
    pass


"""EBNF production:
del_stmt : 'del' exprlist
"""
def p_del_stmt(p):
    """del_stmt : DEL exprlist"""
    pass

"""EBNF production:
pass_stmt : 'pass'"""
def p_pass_stmt(p):
    """pass_stmt : PASS"""
    pass

"""EBNF production:
flow_stmt : break_stmt
          | continue_stmt
          | return_stmt
          | raise_stmt
          | yield_stmt"""
def p_flow_stmt(p):
    """flow_stmt : break_stmt
                 | continue_stmt
                 | return_stmt
                 | raise_stmt
                 | yield_stmt"""
    pass


"""EBNF production:
break_stmt : 'break'"""
def p_break_stmt(p):
    """break_stmt : BREAK"""
    pass


"""EBNF production:
continue_stmt: 'continue'"""
def p_continue_stmt(p):
    """continue_stmt : CONTINUE"""
    pass


"""EBNF production:
return_stmt : 'return' [testlist]"""
def p_return_stmt(p):
    """return_stmt : RETURN
                   | RETURN testlist"""
    pass

"""EBNF production:
yield_stmt : yield_expr"""
def p_yield_stmt(p):
    """yield_stmt : yield_expr"""
    pass

"""EBNF production:
raise_stmt : 'raise' [test ['from' test]]"""
def p_raise_stmt(p):
    """raise_stmt : RAISE test FROM test
                  | RAISE test
                  | RAISE"""
    pass

"""EBNF production:
import_stmt : import_name | import_from"""
def p_import_stmt(p):
    """import_stmt : import_name
                   | import_from"""
    pass

"""EBNF production:
import_name : 'import' dotted_as_names"""
def p_import_name(p):
    """import_name : IMPORT dotted_as_names"""
    pass

"""EBNF production:
import_from : ('from' (('.' | '...')* dotted_name | ('.' | '...')+)
              'import' ('*' | '(' import_as_names ')' | import_as_names))"""
def p_import_from(p):
    """import_from : FROM dotted_path IMPORT import_module"""
    pass

def p_dotted_path(p):
    """dotted_path : dots dotted_name
                   | dotted_name
                   | dots"""
    pass

def p_import_module(p):
    """import_module : TIMES
                     | LPAR import_as_names RPAR
                     | import_as_names"""

# From the grammer reference:
# 'note below: the ('.' | '...') is necessary because '...' is tokenized
# as ELLIPSIS'
def p_dots(p):
    """dots : PERIOD 
            | ELLIPSIS 
            | PERIOD dots
            | ELLIPSIS dots"""
    pass


"""EBNF production:
import_as_name : NAME ['as' NAME]"""
def p_import_as_name(p):
    """import_as_name : ID
                      | ID AS ID"""
    pass

"""EBNF production:
dotted_as_name : dotted_name ['as' NAME]"""
def p_dotted_as_name(p):
    """dotted_as_name : dotted_name
                      | dotted_name AS ID"""
    pass


"""EBNF production:
import_as_names : import_as_name (',' import_as_name)* [',']"""
def p_import_as_names(p):
    """import_as_names : import_as_name import_as_name_commas COMMA
                       | import_as_name import_as_name_commas
                       | import_as_name COMMA
                       | import_as_name"""
    pass

def p_import_as_name_commas(p):
    """import_as_name_commas : COMMA import_as_name
                             | COMMA import_as_name import_as_name_commas"""
    pass


"""EBNF production:
dotted_as_names : dotted_as_name (',' dotted_as_name)*"""
def p_dotted_as_names(p):
    """dotted_as_names : dotted_as_name dotted_as_name_commas
                       | dotted_as_name"""
    pass

def p_dotted_as_name_commas(p):
    """dotted_as_name_commas : COMMA dotted_as_name
                             | COMMA dotted_as_name dotted_as_name_commas"""
    pass



"""EBNF production:
dotted_name : NAME ('.' NAME)*"""
def p_dotted_name(p):
    """dotted_name : ID
                   | ID dotted_name"""
    pass


"""EBNF production:
global_stmt : 'global' NAME (',' NAME)*"""
def p_global_stmt(p):
    """global_stmt : GLOBAL ID
                   | GLOBAL ID ident_commas"""
    pass

def p_ident_commas(p):
    """ident_commas : COMMA ID
                    | COMMA ID ident_commas"""
    pass



"""EBNF production:
nonlocal_stmt : 'nonlocal' NAME (',' NAME)*"""
def p_nonlocal_stmt(p):
    """nonlocal_stmt : NONLOCAL ID
                     | NONLOCAL ID ident_commas"""
    pass


"""EBNF production:
assert_stmt : 'assert' test [',' test]"""
def p_assert_stmt(p):
    """assert_stmt : ASSERT test
                   | ASSERT test test_commas"""
    pass

def p_test_commas(p):
    """test_commas : COMMA test
                   | COMMA test test_commas"""
    pass
 


"""EBNF production:
compound_stmt : if_stmt | while_stmt | for_stmt | try_stmt | with_stmt
                | funcdef | classdef | decorated"""
def p_compound_stmt(p):
    """compound_stmt : if_stmt
                     | while_stmt
                     | for_stmt
                     | try_stmt
                     | with_stmt
                     | funcdef
                     | classdef
                     | decorated"""
    pass

"""EBNF production:
if_stmt : 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]"""
def p_if_stmt(p):
    """if_stmt : IF test COLON suite elifs ELSE COLON suite
               | IF test COLON suite ELSE COLON suite
               | IF test COLON suite elifs
               | IF test COLON suite"""
    pass

def p_elifs(p):
    """elifs : ELIF test COLON suite
             | ELIF test COLON suite elifs"""
    pass


"""EBNF production:
while_stmt : 'while' test ':' suite ['else' ':' suite]"""
def p_while_stmt(p):
    """while_stmt : WHILE test COLON suite ELSE COLON suite
                  | WHILE test COLON suite"""
    pass


"""EBNF production:
for_stmt : 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]"""
def p_for_stmt(p):
    """for_stmt : FOR exprlist IN testlist COLON suite ELSE COLON suite
                | FOR exprlist IN testlist COLON suite"""
    pass

"""EBNF production:
 try_stmt : ('try' ':' suite
             ((except_clause ':' suite)+
              ['else' ':' suite]
              ['finally' ':' suite] |
              'finally' ':' suite))"""
def p_try_stmt(p):
    """try_stmt : TRY COLON suite excepts ELSE COLON suite FINALLY COLON suite
                | TRY COLON suite excepts FINALLY COLON suite
                | TRY COLON suite excepts ELSE COLON suite
                | TRY COLON suite excepts
                | TRY COLON suite FINALLY COLON suite"""
    pass

def p_excepts(p):
    """excepts : except_clause COLON suite
               | except_clause COLON suite excepts"""
    pass


"""EBNF production:
with_stmt : 'with' with_item (',' with_item)* ':' suite"""
def p_with_stmt(p):
    """with_stmt : WITH with_item with_items COLON suite
                 | WITH with_item COLON suite
                 | WITH with_item"""
    pass

def p_with_items(p):
    """with_items : COMMA with_item
                  | COMMA with_item with_items"""
    pass


"""EBNF production:
with_item : test ['as' expr]"""
def p_with_item(p):
    """with_item : test AS expr
                 | test"""
    pass

"""EBNF production:
except_clause: 'except' [test ['as' NAME]]"""
def p_except_clause(p):
    """except_clause : EXCEPT test AS ID
                     | EXCEPT test
                     | EXCEPT"""
    pass

"""EBNF production:
suite : simple_stmt | NEWLINE INDENT stmt+ DEDENT"""
def p_suite(p):
    """suite : simple_stmt
             | NEWLINE INDENT stmts DEDENT"""
    pass

"""EBNF production:
test : or_test ['if' or_test 'else' test] | lambdef"""
def p_test(p):
    """test : or_test IF or_test ELSE test
            | or_test
            | lambdef"""
    pass

"""EBNF production:
test_nocond : or_test | lambdef_nocond"""
def p_test_nocond(p):
    """test_nocond : or_test
                   | lambdef_nocond"""
    pass

"""EBNF production:
lambdef : 'lambda' [varargslist] ':' test"""
def p_lambdef(p):
    """lambdef : LAMBDA varargslist COLON test
               | LAMBDA COLON test"""
    pass

"""EBNF production:
lambdef_nocond: 'lambda' [varargslist] ':' test_nocond"""
def p_lambdef_nocond(p):
    """lambdef_nocond : LAMBDA varargslist COLON test_nocond
                      | LAMBDA COLON test_nocond"""
    pass

"""EBNF production:
or_test : and_test ('or' and_test)*"""
def p_or_test(p):
    """or_test : and_test or_tests
               | and_test"""
    pass

def p_or_tests(p):
    """or_tests : OR and_test
                | OR and_test or_tests"""
    pass

                

"""EBNF production:
and_test : not_test ('and' not_test)*"""
def p_and_test(p):
    """and_test : not_test and_tests
                | not_test"""
    pass

def p_and_tests(p):
    """and_tests : AND not_test
                 | AND not_test and_tests"""
    pass


"""EBNF production:
not_test : 'not' not_test | comparison"""
def p_not_test(p):
    """not_test : NOT not_test
                | comparison"""
    pass

"""EBNF production:
comparison : expr (comp_op expr)*"""
def p_comparison(p):
    """comparison : expr comps
                  | expr"""
    pass

def p_comps(p):
    """comps : comp_op expr
             | comp_op expr comps"""
    pass



"""EBNF production:
comp_op : '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'"""
def p_comp_op(p):
    """comp_op : LT 
               | GT
               | EQ
               | LTEQ
               | GTEQ
               | PEP401
               | NEQ
               | IN
               | NOT IN
               | IS
               | IS NOT"""
    pass


"""EBNF production:
star_expr : '*' expr"""
def p_star_expr(p):
    """star_expr : '*' expr"""
    pass

"""EBNF production:
expr : xor_expr ('|' xor_expr)*"""
def p_expr(p):
    """expr : xor_expr xor_exprs
            | xor_expr"""
    pass

def p_xor_exprs(p):
    """xor_exprs : BITOR xor_expr
                 | BITOR xor_expr xor_exprs"""
    pass


"""EBNF proudction:
xor_expr : and_expr ('^' and_expr)*"""
def p_xor_expr(p):
    """xor_expr : and_expr and_exprs 
                | and_expr"""
    pass

def p_and_exprs(p):
    """and_exprs : XOR and_expr
                 | XOR and_expr and_exprs"""
    pass



"""EBNF production:
and_expr : shift_expr ('&' shift_expr)*"""
def p_and_expr(p):
    """and_expr : shift_expr shift_exprs
                | shift_expr"""
    pass

def p_shift_exprs(p):
    """shift_exprs : BITAND shift_expr
                   | BITAND shift_expr shift_exprs"""
    pass


"""EBNF production:
shift_expr : arith_expr (('<<'|'>>') arith_expr)*"""
def p_shift_expr(p):
    """shift_expr : arith_expr arith_exprs
                  | arith_expr"""
    pass

def p_arith_exprs(p):
    """arith_exprs : LSHIFT arith_expr
                   | RSHIFT arith_expr
                   | LSHIFT arith_expr arith_exprs
                   | RSHIFT arith_expr arith_exprs"""
    pass


"""EBNF production
arith_expr : term (('+'|'-') term)*"""
def p_arith_expr(p):
    """arith_expr : term terms
                  | term"""
    pass

def p_terms(p):
    """terms : PLUS term
             | MINUS term
             | PLUS term terms
             | MINUS term terms"""
    pass


"""EBNF production:
term : factor (('*'|'/'|'%'|'//') factor)*"""
def p_term(p):
    """term : factor factors
            | factor"""
    pass

def p_factors(p):
    """factors : TIMES factor
               | DIV factor
               | MOD factor
               | INTDIV factor
               | TIMES factor factors
               | DIV factor factors
               | MOD factor factors
               | INTDIV factor factors"""
    pass



"""EBNF production:
factor : ('+'|'-'|'~') factor | power"""
def p_factor(p):
    """factor : PLUS factor
              | MINUS factor
              | NEG factor
              | power"""
    pass


"""EBNF production:
power : atom trailer* ['**' factor]"""
def p_power(p):
    """power : atom trailers powers
             | atom powers
             | atom trailers
             | atom"""
    pass

def p_trailers(p):
    """trailers : trailer
                | trailer trailers"""
    pass

def p_powers(p):
    """powers : EXP factor
              | EXP factor powers"""
    pass


"""EBNF production:
atom : ('(' [yield_expr|testlist_comp] ')' |
        '[' [testlist_comp] ']' |
        '{' [dictorsetmaker] '}' |
        NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')"""
def p_atom(p):
    """atom : atom_parent
            | atom_bracket
            | atom_brace
            | ID
            | LIT
            | ELLIPSIS 
            | NONE
            | TRUE
            | FALSE"""
    pass

def p_atom_parent(p):
    """atom_parent : LPAR yield_expr RPAR
                   | LPAR testlist_comp RPAR 
                   | LPAR RPAR"""
    pass

def p_atom_bracket(p):
    """atom_bracket : LBRACKET testlist_comp RBRACKET
                    | LBRACKET RBRACKET"""
    pass

def p_atom_brace(p):
    """atom_brace : LBRACE dictorsetmaker RBRACE
                  | LBRACE RBRACE"""
    pass


"""EBNF production:
testlist_comp : (test|star_expr) ( comp_for | (',' (test|star_expr))*
                [','] )"""
def p_testlist_comp(p):
    """testlist_comp : test_or_star comp_for
                     | test_or_star test_or_stars
                     | test_or_star"""
    pass

def p_test_or_star(p):
    """test_or_star : test
                    | star_expr"""
    pass

def p_test_or_stars(p):
    """test_or_stars : COMMA test_or_star COMMA
                     | COMMA test_or_star
                     | COMMA test_or_star test_or_stars"""
    pass


"""EBNF production:
trailer : '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME"""
def p_trailer(p):
    """trailer : LPAR arglist RPAR
               | LPAR RPAR
               | LBRACKET subscriptlist 
               | PERIOD ID"""
    pass


"""EBNF production:
subscriptlist : subscript (',' subscript)* [',']"""
def p_subscriptlist(p):
    """subscriptlist : subscript subscripts COMMA
                     | subscript COMMA
                     | subscript subscripts
                     | subscript"""
    pass

def p_subscripts(p):
    """subscripts : COMMA subscript
                  | COMMA subscript subscripts"""
    pass

"""EBNF production:
subscript : test | [test] ':' [test] [sliceop]"""
def p_subscript(p):
    """subscript : test
                 | test COLON test sliceop
                 | COLON test sliceop
                 | test COLON sliceop
                 | test COLON test
                 | COLON sliceop
                 | test COLON
                 | COLON test"""
    pass


"""EBNF production:
sliceop : ':' [test]"""
def p_sliceop(p):
    """sliceop : COLON test
               | COLON"""
    pass


"""EBNF production:
exprlist : (expr|star_expr) (',' (expr|star_expr))* [',']"""
def p_exprlist(p):
    """exprlist : expr_or_star expr_stars COMMA
                | expr_or_star COMMA
                | expr_or_star expr_stars
                | expr_or_star"""
    pass

def p_expr_or_star(p):
    """expr_or_star : expr
                    | star_expr"""
    pass

def p_expr_or_stars(p):
    """expr_stars : COMMA expr
                  | COMMA star_expr
                  | COMMA expr expr_stars
                  | COMMA star_expr expr_stars"""
    pass

"""EBNF production:
testlist : test (',' test)* [',']"""
def p_testlist(p):
    """testlist : test testlist_1 COMMA
                | test COMMA
                | test testlist_1
                | test"""
    pass

def p_testlist_1(p):
    """testlist_1 : COMMA test
                  | COMMA test testlist_1"""
    pass


"""EBNF production:
dictorsetmaker : ( (test ':' test (comp_for | (',' test ':' test)* [','])) |
                  (test (comp_for | (',' test)* [','])) )"""
def p_dictorsetmaker(p):
    """dictorsetmaker : test COLON test comp_for
                      | test COLON test coloned_tests COMMA
                      | test COLON test COMMA
                      | test COLON test coloned_tests 
                      | test COLON test
                      | test comp_for
                      | test testlist_1 COMMA
                      | test testlist_1
                      | test"""
    pass

def p_coloned_tests(p):
    """coloned_tests : COMMA test COLON test
                     | COMMA test COLON test coloned_tests"""
    pass


"""EBNF production:
classdef : 'class' NAME ['(' [arglist] ')'] ':' suite"""
def p_classdef(p):
    """classdef : CLASS ID LPAR arglist RPAR COLON suite
                | CLASS ID LPAR RPAR COLON suite
                | CLASS ID COLON suite"""
    pass


"""EBNF production:
arglist : (argument ',')* (argument [',']
                         |'*' test (',' argument)* [',' '**' test]
                         |'**' test)"""

def p_comma_arguments(p):
    """comma_arguments : ',' argument
                       | ',' argument comma_arguments"""
    pass

def p_argslist(p):
    """arglist : arguments argument COMMA
               | arguments argument
               | arguments TIMES test comma_arguments COMMA EXP test
               | arguments TIMES test COMMA EXP test
               | arguments TIMES test comma_arguments
               | arguments TIMES test
               | arguments EXP test

               | argument COMMA
               | argument
               | TIMES test comma_arguments COMMA EXP test
               | TIMES test COMMA EXP test
               | TIMES test comma_arguments
               | TIMES test
               | EXP test"""
    pass

def p_arguments(p):
    """arguments : argument COMMA
                 | argument COMMA arguments"""
    pass


"""EBNF production:
argument : test [comp_for] | test '=' test"""
def p_argument(p):
    """argument : test comp_for
                | test
                | test EQ test"""
    pass


"""EBNF production:
comp_iter : comp_for | comp_if"""
def p_comp_iter(p):
    """comp_iter : comp_for
                 | comp_if"""
    pass


"""EBNF production:
comp_for : 'for' exprlist 'in' or_test [comp_iter]"""
def p_comp_for(p):
    """comp_for : FOR exprlist IN or_test comp_iter
                | FOR exprlist IN or_test"""
    pass


"""EBNF production:
comp_if : 'if' test_nocond [comp_iter]"""
def p_comp_if(p):
    """comp_if : IF test_nocond comp_iter
               | IF test_nocond"""
    pass

"""EBNF production:
yield_expr : 'yield' [yield_arg]"""
def p_yield_expr(p):
    """yield_expr : YIELD yield_arg
                  | YIELD"""
    pass


"""EBNF production:
yield_arg : 'from' test | testlist"""
def p_yield_arg(p):
    """yield_arg : FROM test
                 | testlist"""
    pass


lexer = Lexer()
parser = yacc.yacc()

if __name__ == '__main__':
    
    def main():
        import sys

        s = sys.stdin.read()
        if not s:
            return
        result = parser.parse(s, lexer=lexer)

        print
        print result

    main()
