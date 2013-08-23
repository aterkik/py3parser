import const
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
    # A two-pass lexer. First pass transforms logical lines to physical ones
    # and strips out comments. Second pass tokenizes.
    def __init__(self, fileinput):
        self.content = fileinput
        self.buf = ''
        self.pos = 0
        # Holds regexp for spaces or tabs, no mixing them in Python 3.
        self.indentby = None 

        # Identation marker list (used as a stack)
        # The first indent (i.e. 0) is initialized
        indents = [0]

        # Turns logical lines to physical lines (first pass)
        self.join_lines()
        # Trim trailing newlines
        self.content = self.buf.strip('\n')

        # Rearrange for the second pass
        self.pos = 0

    def join_lines(self):
        implicit_joiners = []

        while self.lahead() != const.eof:
            # Ignore comments
            if self.lahead() == '#':
                self.consume_comment()

            # Line-joining character
            elif self.lahead() == '\\' and self.peek(1) == '\n':
                self.adjust_pos('\\\n')

            # Implicit line-joiners ('{', '[', '(')
            elif self.lahead() in ['{', '[', '(']:
                implicit_joiners.append(joiners[self.lahead()])
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
        return self.content[self.pos] if self.pos < len(self.content) else const.eof 

    def consume_comment(self):
        while self.content[self.pos] != '\n':
            self.pos += 1
        # Ignore the line-feed character too
        self.pos += 1

    def peek(self, step=1):
        return self.content[self.pos + step] if self.pos+step < len(self.content) else None 

    def adjust_pos(self, text):
        self.pos += len(text)

    def next_input(self):
        #TODO: assert(self.pos < len(self.content), 'Illegal position.')
        return self.content[self.pos:]

    def next_token(self):
        # Logical-line start flag (used for indent recognition)
        linestart = True

        while self.lahead() != const.eof:
            ws_line = re.match(r'^[\s]*[\n]', self.next_input())
            if linestart and ws_line:
               # From the Python 3 spec:
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
                token_dent = self.indentation()
                if token_dent:
                    yield token_dnt
                linestart = False
                continue

            # Ignore whitespace not found at the start of line
            if const.whitesp.match(self.lahead()):
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
                continue

            token_imaginary = self.imaginary_literal()
            if token_imaginary:
                yield token_imaginary
                continue

            token_float = self.float_literal()
            if token_float:
                yield token_float
                continue

            token_int = self.int_literal()
            if token_int:
                yield token_int
                continue

            token_ident_or_key = self.ident_or_keyword()
            if token_ident_or_key:
                yield token_ident_or_key
                continue

            token_punct = self.punctuation()
            if token_punct:
                yield token_punct 
                continue

            raise Exception("Unrecognized token '%s'." % self.lahead())

        # Out of the main loop!
        if self.lahead() == const.eof:
            yield Token('ENDMARKER')
            raise StopIteration

    def indentation(self):
        matched = re.match(self.indentby, self.next_input())
                
        if not matched:
            return None
                
        indent = matched.group()

        # TODO: assert(len(indents))

        # Check if the current indent length is greater
        # (i.e. INDENT token)
        if len(indent) > self.indents[-1]:
                indents.append(len(indent))
                self.adjust_pos(indent)
                return Token('INDENT')

        # Check for DEDENT token
        elif len(indent) < indents[-1]:
        # For each indent at the end of the list not
        # not equal to the current indent, remove
        # from the list
            try:
                while self.indents[-1] != len(indent):
                    self.indents.pop()
                    self.adjust_pos(indent)
                    return Token('DEDENT')
            except IndexError:
                raise Exception('Matching indent not found!')

        return None

    def string_literal(self):
        """String literals (Pattern test for long strings must come
        before the test for small strings).
        """
        long_singleq_match = const.long_singleq.match(self.next_input())
        # single-quoted long string
        if long_singleq_match:
            text = long_singleq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text)

        # double-quoted long string
        long_dblq_match = const.long_dblq.match(self.next_input())
        if long_dblq_match:
            text = long_dblq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text)

        # double quoted short string
        short_singleq_match = const.short_singleq.match(self.next_input())

        if short_singleq_match:
            text = short_singleq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text)

        # single quoted short string
        short_dblq_match = const.short_dblq.match(self.next_input())
        if short_dblq_match:
            text = short_dblq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text)

        # byte literals. just like strings except
        # preceded by 'b|B|br|Br|bR|BR' and contain
        # ascii only.
        # long single-quoted byte strings
        longb_singleq_match = const.longb_singleq.match(self.next_input())
        if longb_singleq_match:
            text = longb_singleq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text)

        # long double-quoted byte string
        longb_dblq_match = const.longb_dblq.match(self.next_input())
        if longb_dblq_match:
            text = longb_dblq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text)

        # single-quoted short byte string
        shortb_singleq_match = const.shortb_singleq.match(self.next_input())
        if shortb_singleq_match:
            text = shortb_singleq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text)

        # double quoted short byte string
        shortb_dblq_match = const.shortb_dblq.match(self.next_input())

        if shortb_dblq_match:
            text = shortb_dblq_match.group()
            self.adjust_pos(text)
            return Token('LIT', text)

        return None

    def imaginary_literal(self):
        """Matches and returns a token for 
        imaginary number literals or None."""
        match = const.imaginary.match(self.next_input())
        if match:
            text = match.group()
            self.adjust_pos(text)
            return Token('LIT', text)
        return None

    def float_literal(self):
        """Token for floating point or None if match is not found."""
        match = const.float_.match(self.next_input())
        if match:
            text = match.group()
            self.adjust_pos(text)
            return Token('LIT', text)
        return None

    def int_literal(self):
        """Integer (octal, hex, binary, decimal) or None."""
        match = const.integer.match(self.next_input())
        if match:
            text = match.group()
            self.adjust_pos(text)
            return Token('LIT', text)
        return None

    def ident_or_keyword(self):
        """Identifiers or keywords or None."""
        match = const.keyword_ident.match(self.next_input())
        if match:
            text = match.group()

            if text in const.keywords:
                self.adjust_pos(text)
                return Token('KEYWORD', text)
            else:
                self.adjust_pos(text)
                return Token('ID', text)
        return None
    
    def punctuation(self):
        """Returns punctuation token or None (if match is not found)."""
        # Punctuation (operators and delimiters are both found
        # in the constant 'OPERATORS'.)
        for op in const.operators:
            if self.next_input().startswith(op):
                self.adjust_pos(op)
                return Token('PUNC', op)
        return None

if __name__ == '__main__':
    import sys

    if len(sys.argv) > 1:
        l = Lexer(open(sys.argv[1]).read())
        for t in l.next_token():
            print t


