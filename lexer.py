from constants import EOF, KEYWORDS, OPERATORS
import re

re.VERBOSE = True

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
        self.indentby = None # Holds regexp for spaces or tabs, no mixing in Python 3.

        # Turns logical lines to physical lines (first pass)
        self.join_lines()
        # Trim trailing newlines
        self.content = self.buf.strip('\n')

        # Rearrange for the second pass
        self.pos = 0
        print self.content

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

            # Line-joiner ends.
            if self.lahead() in ['}', ']', ')']:
                if self.lahead() == implicit_joiners[-1]:
                    implicit_joiners.pop()
                self.buf += self.lahead()
                self.adjust_pos(self.lahead())
                continue

            # If there are unclosed line-joiners, discount the
            # newline character
            if self.lahead() == '\n' and len(implicit_joiners) > 0:
                self.adjust_pos('\n')
                continue

            # Detect the indent character: spaces or tabs.
            # (Mixing spaces and tabs is not allowed in Python 3)
            newline = re.match(r'^[\n]', self.content[self.pos:])
            if newline and not self.indentby:
                self.buf += '\n'
                self.adjust_pos('\n')

                # After the position is adjusted, detect the 
                # indentation type (i.e. tabs or spaces)
                if re.match(r'^[ ]+', self.content[self.pos:]):
                    self.indentby = r'^[ \t]+'
                elif re.match(r'^[\t]+', self.content[self.pos:]):
                    self.indentby = r'^[\t ]+'

                # We've already adjusted the position so go back
                continue

            self.buf += self.lahead()
            self.adjust_pos(self.lahead())

    def next_token(self):
        # Identation marker list (used as a stack)
        indents = []

        # Ensure the first line has no indentation (This should be 
        # a parse error, not a lexical error)
        #assert(not re.match(r'^[ \t]+', self.content),
        #       "Error: indentation at first line.");

        # Initialize the first indent
        indents.append(0)

        # Logical-line start marker (used for indent recognition)
        linestart = True

        #XXX: Used match.group() instead of match.string[:match.end()] crap

        while self.lahead() != EOF:

            ws_line = re.match(r'^[\s]*[\n]', self.content[self.pos:])
            if linestart and ws_line:
               # From the Python 3 spec, 
               # "A logical line that contains only spaces, tabs, formfeeds
               # and possibly a comment, is ignored (i.e., no NEWLINE token
               # is generated)"
               self.adjust_pos(ws_line.group())
               linestart = True
               continue

            
            # Indent and/or Dedent tokens
            # self.indentby can legitimately be None (e.g. if the input program
            # is say a simple one-liner)
            if linestart and self.indentby:
                matcher = re.match(self.indentby, self.content[self.pos:])
                # TODO: raise an Exception if input program is mixing up
                # tabs and spaces

                if matcher:
                    indent = matcher.string[:matcher.end()]
                else:
                    indent = ''
                #assert(len(indents) > 0);

                # Check if the current indent length is greater
                # (i.e. INDENT token)
                if len(indent) > indents[-1]:
                    indents.append(len(indent))
                    yield Token('INDENT')
                    self.adjust_pos(indent)
                    

                # Check for DEDENT token
                elif len(indent) < indents[-1]:
                    # For each indent at the end of the list not
                    # not equal to the current indent, remove
                    # from the list
                    try:
                        while indents[-1] != len(indent):
                            yield Token('DEDENT')
                            indents.pop()
                    except IndexError:
                        print 'Matching indent not found!'
                        break

                    self.adjust_pos(indent)

                linestart = False
                continue

            # Ignore whitespace not at the start of line
            if re.match(r'^[ \t\r\f\v]', self.lahead()):
                self.adjust_pos(self.lahead())
                continue

            # Newline 
            if self.lahead() == '\n':
                yield Token('NEWLINE')
                self.adjust_pos('\n')
                linestart = True
                continue

            """
            # Line-joining character
            if self.lahead() == '\\' and self.peek(1) == '\n':
                self.adjust_pos('\\\n')
                # TODO: ensure this works as intended
                continue"""

            
            # String literals (Pattern test for long strings must come
            # before the test for small strings).
            # Single-quoted long string
            long_singleq = r"^[rR]?'''(\\.|[^'\\]|'(?!''))*'''"
            long_singleq_match = re.match(long_singleq,
                                        self.content[self.pos:])
            if long_singleq_match:
                text = long_singleq_match.group()
                self.adjust_pos(text)
                yield Token('LIT', text)
                continue

            # Double-quoted long string
            long_dblq = r'^[rR]?"""(\\.|[^"\\]|"(?!""))*"""'
            long_dblq_match = re.match(long_dblq,
                                       self.content[self.pos:])
            if long_dblq_match:
                text = long_dblq_match.group()
                self.adjust_pos(text)
                yield Token('LIT', text)
                continue


            # Single quoted short string
            short_singleq = r"^[rR]?'(\\.|[^'\\])*'"
            # Double quoted short string
            short_singleq_match = re.match(short_singleq,
                                        self.content[self.pos:])

            if short_singleq_match:
                text = short_singleq_match.group()
                self.adjust_pos(text)
                yield Token('LIT', text)
                continue

            short_dblq = r'^[rR]?"(\\.|[^"\\])*"'
            short_dblq_match = re.match(short_dblq,
                                        self.content[self.pos:])
            if short_dblq_match:
                text = short_dblq_match.group()
                self.adjust_pos(text)
                yield Token('LIT', text)
                continue

            # Byte literals. Just like strings except
            # preceded by 'b|B|br|Br|bR|BR' and contain
            # ASCII only.
            longb_singleq = r"^(b|B|br|Br|bR|BR)?'''(\\.|[^'\\]|'(?!''))*'''"
            longb_singleq_match = re.match(longb_singleq,
                                        self.content[self.pos:])
            if longb_singleq_match:
                text = longb_singleq_match.group()
                self.adjust_pos(text)
                yield Token('LIT', text)
                continue

            # Double-quoted long byte string
            longb_dblq = r'^(b|B|br|Br|bR|BR)?"""(\\.|[^"\\]|"(?!""))*"""'
            longb_dblq_match = re.match(longb_dblq,
                                       self.content[self.pos:])
            if longb_dblq_match:
                text = longb_dblq_match.group()
                self.adjust_pos(text)
                yield Token('LIT', text)
                continue


            # Single quoted short byte string
            shortb_singleq = r"^(b|B|br|Br|bR|BR)?'(\\.|[^'\\])*'"
            # Double quoted short byte string
            shortb_singleq_match = re.match(shortb_singleq,
                                        self.content[self.pos:])
            if shortb_singleq_match:
                text = shortb_singleq_match.group()
                self.adjust_pos(text)
                yield Token('LIT', text)
                continue

            shortb_dblq = r'^(b|B|br|Br|bR|BR)?"(\\.|[^"\\])*"'
            shortb_dblq_match = re.match(shortb_dblq,
                                        self.content[self.pos:])
            if shortb_dblq_match:
                text = shortb_dblq_match.group()
                self.adjust_pos(text)
                yield Token('LIT', text)
                continue

            # Imaginary number literals
            imaginary = r'^(((([0-9]+|([0-9]*\.[0-9]+)|([0-9]+\.))[eE][+-]?[0-9]+)|(([0-9]*\.[0-9]+)|([0-9]+\.)))|([0-9]+))[jJ]'
            match = re.match(imaginary, self.content[self.pos:])
            if match:
                text = match.group()
                self.adjust_pos(text)
                yield Token('LIT', text)
                continue

            # Floating point with exponent
            exponent = r'^(([0-9]+|([0-9]*\.[0-9]+)|([0-9]+\.))[eE][+-]?[0-9]+)|(([0-9]*\.[0-9]+)|([0-9]+\.))'
            match = re.match(exponent, self.content[self.pos:])
            if match:
                text = match.group()
                self.adjust_pos(text)
                yield Token('LIT', text)
                continue

                        
            # Identifiers and keywords
            if re.match('^[_a-zA-Z][_a-zA-Z0-9]*', self.content[self.pos:]):
                match = re.match('^[_a-zA-Z][_a-zA-Z0-9]*', self.content[self.pos:])
                text = match.string[:match.end()]

                if text in KEYWORDS:
                    yield self.keyword(text)
                else:
                    yield self.ident(text)

                continue

            # Integer (octal, hex, binary, decimal)
            integer = r'^(0[oO][0-7]+)|(0[xX][0-9a-fA-F]+)|(0[bB][01]+)|([1-9][0-9]*|0+)'
            match = re.match(integer, self.content[self.pos:])
            if match:
                text = match.group()
                self.adjust_pos(text)
                yield Token('LIT', text)
                continue

            # Punctuation (operators and delimiters are both found
            # in the constant 'OPERATORS'.)
            found_op = False
            for op in OPERATORS:
                if self.content[self.pos:].startswith(op):
                    yield Token('PUNC', op)
                    self.adjust_pos(op)
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
        # Ignore the line-feed character too
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
        for t in l.next_token():
            print t


