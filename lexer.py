#!/usr/bin/env python

"""Handwritten lexer for Python 3"""
import const
import re

class Lexer(object):
    """A two-pass lexer. First pass transforms logical lines to physical ones
    and strips out comments. Second pass tokenizes"""
    def __init__(self):
        self.content = ''
        self.buf = ''
        self.pos = 0
        # Holds regexp for spaces or tabs, no mixing them in Python 3
        self.indentby = None

        # Identation marker list (used as a stack)
        # The first indent (i.e. 0) is initialized
        self.indents = [0]
        self.tokens = None
        
    def input(self, content):
        """Accepts parser input"""
        self.content = content
        # Turns logical lines to physical lines (first pass), writes
        # to buffer (self.buf)
        self.join_lines()
        # Trim trailing newlines
        self.content = self.buf.strip('\n')
        # Rearrange for the second pass
        self.pos = 0
        self.tokens = self.next_token()

    def join_lines(self):
        """Converts physical lines to logical lines"""
        implicit_joiners = []

        while self.lahead() != const.EOF:
            # Ignore comments
            if self.lahead() == '#':
                self.consume_comment()

            # Line-joining character
            elif self.lahead() == '\\' and self.peek(1) == '\n':
                self.adjust_pos('\\\n')

            # Implicit line-joiners ('{', '[', '(')
            elif self.lahead() in ['{', '[', '(']:
                implicit_joiners.append(const.JOINERS[self.lahead()])
                self.buf += self.lahead()
                self.adjust_pos(self.lahead())

            elif self.lahead() in ['}', ']', ')']:
                if len(implicit_joiners) > 0 and\
                implicit_joiners[-1] == self.lahead():
                   implicit_joiners.pop()
                self.buf += self.lahead()
                self.adjust_pos(self.lahead())

            # If there are unclosed line-joiners, discount the
            # newline character
            elif self.lahead() == '\n' and len(implicit_joiners) > 0:
                self.adjust_pos('\n')

            # Detect the indent character: spaces or tabs.
            # (Mixing spaces and tabs is not allowed in Python 3)
            elif re.match(r'^[\n]', self.next_input()) and not self.indentby:
                self.buf += '\n'
                self.adjust_pos('\n')

                # After the position is adjusted, detect the
                # indentation type (i.e. tabs or spaces)
                if re.match(r'^[ ]+', self.next_input()):
                    self.indentby = r'^[ \t]+'
                elif re.match(r'^[\t]+', self.next_input()):
                    self.indentby = r'^[\t ]+'

                # We've already adjusted the position so go back
                continue
            else:
                self.buf += self.lahead()
                self.adjust_pos(self.lahead())

    def lahead(self):
        """Look ahead"""
        return self.content[self.pos]\
               if self.pos < len(self.content) else const.EOF

    def consume_comment(self):
        """Ignores comments"""
        while self.content[self.pos] != '\n':
            self.pos += 1
        # Ignore the line-feed character too
        self.pos += 1

    def peek(self, step=1):
        """Peek ahead 'step' characters"""
        assert(step > 0)
        return self.content[self.pos + step]\
               if self.pos+step < len(self.content) else None

    def adjust_pos(self, text):
        """Move the position len(text) forward"""
        self.pos += len(text)

    def next_input(self):
        """Remaining string input"""
        return self.content[self.pos:]

    def token(self):
        for tok in self.tokens:
            return tok

        # Token generator ends
        return None

    def next_token(self):
        """Yields the next valid token"""
        # Logical-line start flag (used for indentation recognition)
        linestart = True

        while self.lahead() != const.EOF:
            ws_match = const.WS_LINE.match(self.next_input())
            if linestart and ws_match:
                # From the Python 3 spec:
                # "A logical line that contains only spaces, tabs, formfeeds
                # and possibly a comment, is ignored (i.e., no NEWLINE token
                # is generated)"
                self.adjust_pos(ws_match.group())
                # Leave the linestart switch at True
                linestart = True
                continue

            # Indent and/or Dedent tokens
            # self.indentby can legitimately be None (e.g. if the input program
            # is say a simple one-liner)
            if linestart and self.indentby:
                tokens = self.indentation()
                for token in tokens:
                    yield token
                linestart = False
                continue

            # Ignore whitespace not found at the start of line
            # (exclduing '\n').
            if const.WHITESP.match(self.lahead()):
                self.adjust_pos(self.lahead())
                continue

            # Logical, non-empty newline
            if self.lahead() == '\n':
                yield Token('NEWLINE')
                self.adjust_pos('\n')
                linestart = True
                continue

            token_string = self.string_literal()
            if token_string:
                yield token_string
                linestart = False
                continue

            token_imaginary = self.imaginary_literal()
            if token_imaginary:
                yield token_imaginary
                linestart = False
                continue

            token_float = self.float_literal()
            if token_float:
                yield token_float
                linestart = False
                continue

            token_int = self.int_literal()
            if token_int:
                yield token_int
                linestart = False
                continue

            token_ident_or_key = self.ident_or_keyword()
            if token_ident_or_key:
                yield token_ident_or_key
                linestart = False
                continue

            token_punct = self.punctuation()
            if token_punct:
                yield token_punct
                linestart = False
                continue

            raise Exception("Unrecognized token '{0}'".format(self.lahead()))

        # Out of the main loop!
        if self.lahead() == const.EOF:

            yield Token('NEWLINE')
            # Return and remaining DEDENT tokens
            while len(self.indents) > 1:
                yield Token('DEDENT')
                self.indents.pop()

            yield Token('ENDMARKER')
            raise StopIteration

    def indentation(self):
        """Returns a single 'indent' token in a list
            or a list of multiple 'dedent' tokens"""

        return_tokens = []
        matched = re.match(self.indentby, self.next_input())
        indent = matched.group() if matched else ''

        # Check if the current indent length is greater
        # (i.e. INDENT token)
        if len(indent) > self.indents[-1]:
            self.indents.append(len(indent))
            self.adjust_pos(indent)
            return_tokens.append(Token('INDENT'))

        # Check for DEDENT token
        elif len(indent) < self.indents[-1]:
        # For each indent at the end of the list not
        # not equal to the current indent, remove
        # from the list
            try:
                while self.indents[-1] != len(indent):
                    self.indents.pop()
                    return_tokens.append(Token('DEDENT'))

                self.adjust_pos(indent)
            except IndexError:
                raise Exception('Matching indent not found!')

        return return_tokens

    def string_literal(self):
        """String literals (Pattern test for long strings must come
        before the test for small strings)"""

        # Single-quoted long string (i.e. '''spam''')
        long_singleq_match = const.LONG_SINGLEQ.match(self.next_input())
        if long_singleq_match:
            text = long_singleq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text, decode=True)

        # Double-quoted long string (i.e. """spam""")
        long_dblq_match = const.LONG_DBLQ.match(self.next_input())
        if long_dblq_match:
            text = long_dblq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text, decode=True)

        # Single-quoted short string (i.e. 'spam')
        short_singleq_match = const.SHORT_SINGLEQ.match(self.next_input())
        if short_singleq_match:
            text = short_singleq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text, decode=True)

        # Double-quoted short string (i.e. "spam")
        short_dblq_match = const.SHORT_DBLQ.match(self.next_input())
        if short_dblq_match:
            #import pdb; pdb.set_trace()
            text = short_dblq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text, decode=True)

        # Byte literals. just like strings except
        # preceded by 'b|B|br|Br|bR|BR' and contain
        # ascii only.
        # long single-quoted byte strings
        longb_singleq_match = const.LONGB_SINGLEQ.match(self.next_input())
        if longb_singleq_match:
            text = longb_singleq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text, decode=True)

        # Long double-quoted byte string
        longb_dblq_match = const.LONGB_DBLQ.match(self.next_input())
        if longb_dblq_match:
            text = longb_dblq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text, decode=True)

        # Single-quoted short byte string
        shortb_singleq_match = const.SHORTB_SINGLEQ.match(self.next_input())
        if shortb_singleq_match:
            text = shortb_singleq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text, decode=True)

        # Double quoted short byte string
        shortb_dblq_match = const.SHORTB_DBLQ.match(self.next_input())

        if shortb_dblq_match:
            text = shortb_dblq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text, decode=True)

        return None

    def imaginary_literal(self):
        """Matches and returns a token for
        imaginary number literals or None"""
        match = const.IMAGINARY.match(self.next_input())
        if match:
            text = match.group()
            self.adjust_pos(text)
            return Token('LIT', text)
        return None

    def float_literal(self):
        """Token for floating point or None if match is not found"""
        match = const.FLOAT_.match(self.next_input())
        if match:
            text = match.group()
            self.adjust_pos(text)
            return Token('LIT', text)
        return None

    def int_literal(self):
        """Integer (in octal, hex, binary, decimal) or None"""
        match = const.INTEGER.match(self.next_input())
        if match:
            text = match.group()
            self.adjust_pos(text)
            return Token('LIT', text)
        return None

    def ident_or_keyword(self):
        """Identifiers or keywords or None"""
        match = const.KEYWORD_IDENT.match(self.next_input())
        if match:
            text = match.group()

            if text in const.KEYWORDS:
                self.adjust_pos(text)
                return Token(text.upper(), text)
            else:
                self.adjust_pos(text)
                return Token('ID', text, quote=True)
        return None

    def punctuation(self):
        """Returns punctuation token or None (if match is not found)"""
        # Punctuation (operators and delimiters are both found
        # in the constant 'const.operators'.)
        # Sorted in decreasing order of the operator's length
        # because we want the longest matching string.
        for operator in sorted(const.OPERATORS.keys(),
                               cmp=lambda x, y: -1 if len(x) < len(y) else 1,
                               reverse=True):
            if self.next_input().startswith(operator):
                self.adjust_pos(operator)
                return Token(const.OPERATORS[operator], operator, quote=True)
        return None

class Token(object):
    """Simple abstraction for a single token"""
    def __init__(self, type, text=None, quote=False, decode=False):
        self.type = type
        self.value = text
        self.quote = quote
        self.decode = decode

    def __unicode__(self):
        value = self.value.decode('string_escape')\
               if self.decode else self.value

        if value and self.quote:
            return '({0} "{1}")'.format(self.type, value)
        elif value:
            return '({0} {1})'.format(self.type, value)
        return '({0})'.format(self.type)

    def __str__(self):
        return self.__unicode__()

    def print_(self):
        """Escaping-aware print"""
        if self.decode:
            print self.__unicode__().encode('string_escape')
        else:
            print self.__unicode__()

if __name__ == '__main__':
    import sys

    def main():
        if len(sys.argv) < 2:
            print 'error: please specify a filename as the first argument.'
            exit(1)

        py3lexer = Lexer()
        py3lexer.input(open(sys.argv[1]).read())

        while True:
            token = py3lexer.token()
            if token:
                token.print_()
            else:
                break

    main()
