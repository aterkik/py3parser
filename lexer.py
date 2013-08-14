from constants import EOF, KEYWORDS, OPERATORS
import re

class Token:
    def __init__(self, ttype, text=None):
        self.ttype = ttype
        self.text = text 

    def __unicode__(self):
        if self.text:
            return '(%s %s)' % (self.ttype, self.text)
        return '(%s)' % self.ttype

    def __str__(self):
        return self.__unicode__()

class Lexer:
    # A two-pass lexer. First pass transforms logical lines to pyhsical ones,
    # strips comments. Second pass tokenizes.
    def __init__(self, fileinput):
        self.content = fileinput
        self.buf = ''
        self.pos = 0
        # Turns logical lines to physical lines
        self.join_lines()
        self.content = self.buf
        # Rearrange for the second pass
        self.pos = 0
        print self.buf

    def join_lines(self):
        implicit_joiners = []
        joiner = {'[' : ']', '{' : '}', '(' : ')'}

        while self.lahead() != EOF:
            # Ignore comments
            if self.lahead() == '#':
                self.consume_comment()
                continue

            # Line-joining character
            if self.lahead() == '\\' and self.peek(1) == '\n':
                self.adjust_pos('\\\n')
                continue

            # Implicit line-joiners ('{', '[', '(')
            if self.lahead() in ['{', '[', '(']:
                implicit_joiners.append(joiner[self.lahead()])
                self.buf += self.lahead()
                self.adjust_pos(self.lahead())
                continue

            if self.lahead() in ['}', ']', ')']:
                if self.lahead() == implicit_joiners[-1]:
                    implicit_joiners.pop()
                self.buf += self.lahead()
                self.adjust_pos(self.lahead())
                continue

            if self.lahead() == '\n' and len(implicit_joiners) > 0:
                self.adjust_pos('\n')
                continue

            # Replace multiple consecutive newlines by a single one
            mult_newlines = re.match(r'^[\n]+', self.content[self.pos:])
            if mult_newlines:
                matched = mult_newlines.string[:mult_newlines.end()]
                self.buf += '\n'
                self.adjust_pos(matched)
                continue

            self.buf += self.lahead()
            self.adjust_pos(self.lahead())

    def next_token(self):
        # Used as a flag to ignore all white-space lines
        empty_line = True

        while self.lahead() != EOF:
            # Ignore whitespace
            if self.lahead() in [' ', '\t']:
                self.adjust_pos(self.lahead())
                continue

            # NEWLINE
            if self.lahead() == '\n':
                if not empty_line:
                    yield Token('NEWLINE')
                    empty_line = True
                self.adjust_pos('\n')
                continue

            # Line-joining character
            if self.lahead() == '\\' and self.peek(1) == '\n':
                self.adjust_pos('\\\n')
                # TODO: ensure this works as intended
                empty_line = True
                continue

            # Identifiers and keywords
            if re.match('^[_a-zA-Z][_a-zA-Z0-9]*', self.content[self.pos:]):
                match = re.match('^[_a-zA-Z][_a-zA-Z0-9]*', self.content[self.pos:])
                text = match.string[:match.end()]

                if text in KEYWORDS:
                    yield self.keyword(text)
                else:
                    yield self.ident(text)

                empty_line = False
                continue

            # Literals

            # Strings
            # TODO: handle newlines in-between
            shortstring = "^[r|R]?[\'][^\']*[\']"
            match = re.match(shortstring, self.content[self.pos:])
            if match:
                text = match.string[:match.end()]
                self.adjust_pos(text)
                yield Token('LIT', text)
                empty_line = False
                continue
            # TODO: handle bytes


            # Floating point literals
            pointfloat = '^(([0-9]*\.[0-9]+)|([0-9]+\.))'
            match = re.match(pointfloat, self.content[self.pos:])
            if match:
                text = match.string[:match.end()]
                self.adjust_pos(text)
                yield Token('LIT', text)
                empty_line = False
                continue
            # TODO: handle exponential float
            # TODO: handle imaginary literals


            # Decimal integer
            decimal = '^([1-9][0-9]*|0+)'
            match = re.match(decimal, self.content[self.pos:])
            if match:
                text = match.string[:match.end()]
                self.adjust_pos(text)
                yield Token('LIT', text)
                empty_line = False
                continue

            # TODO: handle binary, octal, hex

            # Punctuation (operators and delimiters are both found
            # in the constant 'OPERATORS'.)
            found_op = False
            for op in OPERATORS:
                if self.content[self.pos:].startswith(op):
                    self.adjust_pos(op)
                    yield Token('PUNC', op)
                    empty_line = False
                    found_op = True
                    break

            if found_op:
                continue

            raise Exception("Unrecognized token '%s'." % self.lahead())

        if self.lahead() == EOF:
            yield Token('ENDMARKER')
        raise StopIteration



    def lahead(self):
        return self.content[self.pos] if self.pos < len(self.content) else EOF

    def peek(self, step=1):
        return self.content[self.pos + step] if self.pos+step < len(self.content) else None 

    def consume_comment(self):
        while self.content[self.pos] != '\n':
            self.pos += 1

        # Eat the new-line character too
        self.pos += 1

    def adjust_pos(self, text):
        self.pos += len(text)

    def keyword(self, text):
        self.adjust_pos(text)
        return Token('KEYWORD', text)
    
    def ident(self, text):
        self.adjust_pos(text)
        return Token('ID', text)

    

if __name__ == '__main__':
    import sys

    if len(sys.argv) > 1:
        l = Lexer(open(sys.argv[1]).read())
        #for t in l.next_token():
        #    print t






