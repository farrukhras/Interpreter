import ply.lex as lex

reserved = {
  'IF' : 'IF',
  'ELSEIF' : 'ELSEIF',
  'ELSE' : 'ELSE',
  'NOT' : 'NOT',
  'AND' : 'AND',
  'OR': 'OR',
  'PRINT': 'PRINT',
  'STRUCT' : 'STRUCT'
}

tokens = [
  'TYPE',             # INT, DOUBLE, CHAR, STRING, BOOL
  'VAR',              # a
  'INT',              # 1
  'DOUBLE',           # 2.1
  'CHAR',             # 'c'
  'STRING',           # 'string'
  'BOOL',             # true|false
  'DOUBLEEQUAL',      # ==
  'PLUS',             # +
  'MINUS',            # -
  'DIVIDE',           # /
  'TIMES',            # *
  'POWER',            # ^
  'MODULUS',          # %
  'INCREMENT',        # ++
  'DECREMENT',        # --
  'LESSTHAN',         # <
  'GREATERTHAN',      # >
  'LESSTHANEQUAL',    # <=
  'GREATERTHANEQUAL', # >=
  'NOTEQUAL',         # !=
  'EQUAL',            # =
  'LBRACE',           # {
  'RBRACE',           # }
  'LPAREN',           # (
  'RPAREN',           # )
  'ACCESS',           # .
  'COMMA',            # ,
  'SEMICOLON',        # ;
] + list(reserved.values())

t_ignore = ' \n\t' # shortcut for whitespace

t_INCREMENT = r'\+\+'
t_DECREMENT = r'--'
t_DIVIDE =  r'/'
t_DOUBLEEQUAL = r'=='
t_EQUAL = r'='
t_GREATERTHANEQUAL =  r'>='
t_GREATERTHAN =   r'>'
t_LESSTHANEQUAL =  r'<='
t_LESSTHAN = r'<'
t_MINUS = r'-'
t_PLUS = r'\+'
t_TIMES = r'\*'
t_POWER = r'\^'
t_MODULUS = r'\%'
t_NOTEQUAL = r'!='
t_ACCESS = r'\.'
t_LBRACE = r'\{'
t_RBRACE =  r'\}'
t_LPAREN =   r'\('
t_RPAREN =  r'\)'
t_COMMA = r'\,'
t_SEMICOLON = r'\;'

def t_newline(t): #token for a newline
  r'"\n"+'
  t.lexer.lineno += 1
  pass

def t_TYPE(t):
  r'INT|STRING|BOOL|CHAR|DOUBLE|VAR|STRUCT'
  t.type = 'TYPE'
  return t

def t_DOUBLE(t): #token for a double
  r'[0-9]+\.[0-9]+' # one or more number followed by a . which followed by one or more numbers
  t.value = float(t.value) # double is the same as float in python
  return t

def t_INT(t): #token for an int
  r'[0-9]+' # matches a single digit number as a string
  t.value = int(t.value) # converts a string number to an int number
  return t

def t_BOOL(t):
  r'true|false'
  t.type = 'BOOL'
  return t

def t_IF(t):
  r'IF'
  t.type = reserved.get(t.value,'IF')
  return t 

def t_ELSEIF(t):
  r'ELSEIF'
  t.type = reserved.get(t.value,'ELSEIF')
  return t

def t_ELSE(t):
  r'ELSE'
  t.type = reserved.get(t.value,'ELSE')
  return t

def t_print(t):
  r'PRINT'
  t.type = reserved.get(t.value,'PRINT')
  return t 

def t_NOT(t):
  r'NOT'
  t.type = reserved.get(t.value,'NOT')
  return t

def t_OR(t):
  r'OR'
  t.type = reserved.get(t.value,'OR')
  return t

def t_AND(t):
  r'AND'
  t.type = reserved.get(t.value,'AND')
  return t

def t_VAR(t):
  r'[a-zA-Z_][a-zA-Z0-9_]*'
  t.type = 'VAR'
  return t

def t_STRING(t): #token for a string
  r'"[^"]*"'
  t.value = t.value[1:-1] # drop "surrounding quotes"
  return t

def t_CHAR(t): #token for an char
  r"'[a-zA-Z]'"
  t.value = t.value[1: -1] # just a single character
  return t

def t_error(t): # token for an error
  print("")
  print("Illegal Characters!")
  print("")
  # t.lexer.skip(1)

lex.lex() # build a lexer