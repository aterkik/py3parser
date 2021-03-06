#!/usr/bin/env python

"""Unit tests for lexer.py"""
import unittest
from lexer import Lexer

TEST_INPUT = open('test.py.txt').read()

class LexerTest(unittest.TestCase):
    """Basic tokenization teser"""

    def test_tokens(self):
        """Tokens tester"""
        lexer = Lexer()
        lexer.input(TEST_INPUT)

        self.assertEquals(str(lexer.token()), '(DEF def)')
        self.assertEquals(str(lexer.token()), '(ID "fact")')
        self.assertEquals(str(lexer.token()), '(LPAR "(")')
        self.assertEquals(str(lexer.token()), '(ID "x")')
        self.assertEquals(str(lexer.token()), '(RPAR ")")')
        self.assertEquals(str(lexer.token()), '(COLON ":")')
        self.assertEquals(str(lexer.token()), '(NEWLINE)')
        self.assertEquals(str(lexer.token()), '(INDENT)')
        self.assertEquals(str(lexer.token()), '(IF if)')
        self.assertEquals(str(lexer.token()), '(ID "x")')
        self.assertEquals(str(lexer.token()), '(EQ "==")')
        self.assertEquals(str(lexer.token()), '(MINUS "-")')
        self.assertEquals(str(lexer.token()), '(LIT 1)')
        self.assertEquals(str(lexer.token()), '(COLON ":")')
        self.assertEquals(str(lexer.token()), '(NEWLINE)')
        self.assertEquals(str(lexer.token()), '(INDENT)')
        self.assertEquals(str(lexer.token()), '(RETURN return)')
        self.assertEquals(str(lexer.token()), '(LIT 1.j)')
        self.assertEquals(str(lexer.token()), '(NEWLINE)')
        self.assertEquals(str(lexer.token()), '(DEDENT)')
        self.assertEquals(str(lexer.token()), '(ELIF elif)')
        self.assertEquals(str(lexer.token()), '(ID "x")')
        self.assertEquals(str(lexer.token()), '(EQ "==")')
        self.assertEquals(str(lexer.token()), '(LIT 0)')
        self.assertEquals(str(lexer.token()), '(COLON ":")')
        self.assertEquals(str(lexer.token()), '(NEWLINE)')
        self.assertEquals(str(lexer.token()), '(INDENT)')
        self.assertEquals(str(lexer.token()), '(RETURN return)')
        self.assertEquals(str(lexer.token()), '(LIT 1)')
        self.assertEquals(str(lexer.token()), '(NEWLINE)')
        self.assertEquals(str(lexer.token()), '(DEDENT)')
        self.assertEquals(str(lexer.token()), '(ELSE else)')
        self.assertEquals(str(lexer.token()), '(COLON ":")')
        self.assertEquals(str(lexer.token()), '(NEWLINE)')
        self.assertEquals(str(lexer.token()), '(INDENT)')
        self.assertEquals(str(lexer.token()), '(RETURN return)')
        self.assertEquals(str(lexer.token()), '(ID "x")')
        self.assertEquals(str(lexer.token()), '(TIMES "*")')
        self.assertEquals(str(lexer.token()), '(ID "fact")')
        self.assertEquals(str(lexer.token()), '(LPAR "(")')
        self.assertEquals(str(lexer.token()), '(ID "x")')
        self.assertEquals(str(lexer.token()), '(MINUS "-")')
        self.assertEquals(str(lexer.token()), '(LIT 1)')
        self.assertEquals(str(lexer.token()), '(RPAR ")")')
        self.assertEquals(str(lexer.token()), '(NEWLINE)')
        self.assertEquals(str(lexer.token()), '(DEDENT)')
        self.assertEquals(str(lexer.token()), '(DEDENT)')
        self.assertEquals(str(lexer.token()), '(ID "s")')
        self.assertEquals(str(lexer.token()), '(ASSIGN "=")')
        self.assertEquals(str(lexer.token()), '(LIT "foo\\ \n\'\"")')
        self.assertEquals(str(lexer.token()), '(NEWLINE)')
        self.assertEquals(str(lexer.token()), '(ID "fact")')
        self.assertEquals(str(lexer.token()), '(LPAR "(")')
        self.assertEquals(str(lexer.token()), '(LIT 20)')
        self.assertEquals(str(lexer.token()), '(RPAR ")")')
        self.assertEquals(str(lexer.token()), '(NEWLINE)')
        self.assertEquals(str(lexer.token()), '(ENDMARKER)')

if __name__ == '__main__':
    unittest.main()
