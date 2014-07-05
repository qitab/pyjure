
"
funcdef: 'def' NAME parameters ':' suite
parameters: '(' [varargslist] ')'
varargslist: ((fpdef ['=' test] ',')*
              ('*' NAME [',' '**' NAME] | '**' NAME) |
              fpdef ['=' test] (',' fpdef ['=' test])* [','])
fpdef: NAME | '(' fplist ')'
fplist: fpdef (',' fpdef)* [',']

statement: simple-statement | compound-statement
simple-statement: small-statement (';' small-statement)* [';'] NEWLINE
small-statement: (expr-statement | print-statement  | del-statement | pass-statement | flow-statement |
             import-statement | global-statement | exec-statement | assert-statement)
expr-statement: testlist (augassign (yield-expr|testlist) |
                     ('=' (yield-expr|testlist))*)
augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' |
            '<<=' | '>>=' | '**=' | '//=')
# For normal assignments, additional restrictions enforced by the interpreter
print-statement: 'print' ( [ test (',' test)* [','] ] |
                      '>>' test [ (',' test)+ [','] ] )
del-statement: 'del' exprlist
pass-statement: 'pass'
flow-statement: break-statement | continue-statement | return-statement | raise-statement | yield-statement
break-statement: 'break'
continue-statement: 'continue'
return-statement: 'return' [testlist]
yield-statement: yield-expr
raise-statement: 'raise' [test [',' test [',' test]]]
import-statement: import-name | import-from
import-name: 'import' dotted-as-names
import-from: ('from' ('.'* dotted-name | '.'+)
              'import' ('*' | '(' import-as-names ')' | import-as-names))
import-as-name: NAME ['as' NAME]
dotted-as-name: dotted-name ['as' NAME]
import-as-names: import-as-name (',' import-as-name)* [',']
dotted-as-names: dotted-as-name (',' dotted-as-name)*
dotted-name: NAME ('.' NAME)*
global-statement: 'global' NAME (',' NAME)*
exec-statement: 'exec' expr ['in' test [',' test]]
assert-statement: 'assert' test [',' test]

compound-statement: if-statement | while-statement | for-statement | try-statement | with-statement | funcdef | classdef | decorated
if-statement: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
while-statement: 'while' test ':' suite ['else' ':' suite]
for-statement: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
try-statement: ('try' ':' suite
           ((except-clause ':' suite)+
            ['else' ':' suite]
            ['finally' ':' suite] |
           'finally' ':' suite))
with-statement: 'with' with-item (',' with-item)*  ':' suite
with-item: test ['as' expr]
# NB compile.c makes sure that the default except clause is last
except-clause: 'except' [test [('as' | ',') test]]
suite: simple-statement | NEWLINE INDENT statement+ DEDENT

# Backward compatibility cruft to support:
# [ x for x in lambda: True, lambda: False if x() ]
# even while also allowing:
# lambda x: 5 if x else 2
# (But not a mix of the two)
testlist-safe: old-test [(',' old-test)+ [',']]
old-test: or-test | old-lambdef
old-lambdef: 'lambda' [varargslist] ':' old-test

test: or-test ['if' or-test 'else' test] | lambdef
or-test: and-test ('or' and-test)*
and-test: not-test ('and' not-test)*
not-test: 'not' not-test | comparison
comparison: expr (comp-op expr)*
comp-op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
expr: xor-expr ('|' xor-expr)*
xor-expr: and-expr ('^' and-expr)*
and-expr: shift-expr ('&' shift-expr)*
shift-expr: arith-expr (('<<'|'>>') arith-expr)*
arith-expr: term (('+'|'-') term)*
term: factor (('*'|'/'|'%'|'//') factor)*
factor: ('+'|'-'|'~') factor | power
power: atom trailer* ['**' factor]
atom: ('(' [yield-expr|testlist-comp] ')' |
       '[' [listmaker] ']' |
       '{' [dictorsetmaker] '}' |
       '`' testlist1 '`' |
       NAME | NUMBER | STRING+)
listmaker: test ( list-for | (',' test)* [','] )
testlist-comp: test ( comp-for | (',' test)* [','] )
lambdef: 'lambda' [varargslist] ':' test
trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
subscriptlist: subscript (',' subscript)* [',']
subscript: '.' '.' '.' | test | [test] ':' [test] [sliceop]
sliceop: ':' [test]
exprlist: expr (',' expr)* [',']
testlist: test (',' test)* [',']
dictorsetmaker: ( (test ':' test (comp-for | (',' test ':' test)* [','])) |
                  (test (comp-for | (',' test)* [','])) )

classdef: 'class' NAME ['(' [testlist] ')'] ':' suite

arglist: (argument ',')* (argument [',']
                         |'*' test (',' argument)* [',' '**' test] 
                         |'**' test)
# The reason that keywords are test nodes instead of NAME is that using NAME
# results in an ambiguity. ast.c makes sure it's a NAME.
argument: test [comp-for] | test '=' test

list-iter: list-for | list-if
list-for: 'for' exprlist 'in' testlist-safe [list-iter]
list-if: 'if' old-test [list-iter]

comp-iter: comp-for | comp-if
comp-for: 'for' exprlist 'in' or-test [comp-iter]
comp-if: 'if' old-test [comp-iter]

testlist1: test (',' test)*

# not used in grammar, but may appear in 'node' passed from Parser to Compiler
encoding-decl: NAME

yield-expr: 'yield' [testlist]
"

"
# PEP 3107: Function Annotations
# http://legacy.python.org/dev/peps/pep-3107/
decorator: '@' dotted-name [ '(' [arglist] ')' ] NEWLINE
decorators: decorator+
funcdef: [decorators] 'def' NAME parameters ['->' test] ':' suite
parameters: '(' [typedargslist] ')'
typedargslist: ((tfpdef ['=' test] ',')*
                ('*' [tname] (',' tname ['=' test])* [',' '**' tname]
                 | '**' tname)
                | tfpdef ['=' test] (',' tfpdef ['=' test])* [','])
tname: NAME [':' test]
tfpdef: tname | '(' tfplist ')'
tfplist: tfpdef (',' tfpdef)* [',']
"
