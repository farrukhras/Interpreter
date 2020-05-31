import ply.lex as lex
import ply.yacc as yacc
import lexer

tokens = lexer.tokens

precedence = ( # precedence decreases from LEFT -> RIGHT AND TOP -> BOTTOM 
  ('left', 'OR', 'AND', 'NOT'),
  ('left', 'DOUBLEEQUAL', 'NOTEQUAL'),
  ('left', 'LESSTHAN', 'LESSTHANEQUAL', 'GREATERTHAN', 'GREATERTHANEQUAL'),
  # ('left', 'INCREMENT', 'DECREMENT'),
  ('left', 'MINUS', 'PLUS'),
  ('left', 'TIMES', 'DIVIDE'),
  ('left', 'MODULUS', 'POWER')
) 

def p_general(p):
  '''
  general : statements
            | empty
  '''
  p[0] =  p[1]

def p_statements(p):
  '''
  statements : statement statements
              | empty
  '''
  if len(p) == 2:
    p[0] = p[1]
  else:
    p[0] = [p[1]] + p[2]

def p_statement_exp(p): # A statement
  'statement : expression'
  p[0] = p[1]

def p_expression_paren(p): # An expression
  'expression : LPAREN expression RPAREN' 

  if len(p) == 4:
        p[0] = p[2]
  else:
    p[0] = p[1]

###############
#----EMPTY----#
###############

def p_empty(p):
  'empty : '
  p[0] = [ ]

##################################
#----DECLARATION & ASSIGNMENT----#
##################################

def p_variable_assignment(p):
  '''
  statement : TYPE VAR EQUAL expression
            | TYPE VAR LBRACE items RBRACE SEMICOLON
            | VAR EQUAL expression
  '''

  if len(p) == 5:
    p[0] = ('declare', p[1], p[2], p[4])
  elif len(p) == 7:
    p[0] = ('declare-struct', p[1], p[2], p[4])
  else:
    p[0] = ('assign', p[1], p[3])

###############
#----PRINT----#
###############

def p_print(p):
  '''
  statement : PRINT LPAREN expr RPAREN SEMICOLON
            | PRINT LPAREN statement RPAREN SEMICOLON
  '''
  p[0] = ('print', p[3])

def p_multiExpr(p):
  '''
  expr : expression comma expr
        | empty
  '''
  if len(p) == 4:
    p[0] = [p[1]] + p[3]
  else:
    p[0] = p[1]

def p_comma(p):
  '''
  comma : COMMA
        | empty
  '''

#################
#----STRUCTS----#
#################

def p_struct_items(p):
  '''
  items : TYPE VAR SEMICOLON items
        | empty
  '''
  if len(p) == 5:
    p[0] = [p[2]] + p[4]
  else:
    p[0] = p[1]

def p_initialize_struct(p):
  # STRUCT OBJ_NAME
  'statement : VAR VAR'
  p[0] = ("initialize-struct-obj", p[1], p[2])

def p_access_struct_variable(p):
  # object_name.var_name 
  'statement : VAR ACCESS VAR'
  p[0] = ('access-struct-var', p[1], p[3])

def p_struct_assignment(p):
  'statement : VAR ACCESS VAR EQUAL expression'
  p[0] = ('set-struct-var-value', p[1], p[3], p[5])

#############################
#----NUMERICAL OPERATORS----#
#############################

def p_expression_binop(p):
  ''' 
  expression : expression POWER expression
              | expression MODULUS expression
              | expression DIVIDE expression
              | expression TIMES expression
              | expression PLUS expression
              | expression MINUS expression
  '''
  p[0] = ('binop', p[1], p[2], p[3])

def p_expression_binop_paren(p):
  '''
  expression : LPAREN expression POWER expression RPAREN
              | LPAREN expression MODULUS expression RPAREN
              | LPAREN expression DIVIDE expression RPAREN
              | LPAREN expression TIMES expression RPAREN
              | LPAREN expression PLUS expression RPAREN
              | LPAREN expression MINUS expression RPAREN
  '''
  p[0] = ('binop', p[2], p[3], p[4])

def p_inc(p): #increment
  'statement : VAR INCREMENT'
  p[0] = ('inc', p[1])

def p_dec(p): #decrement
  'statement : VAR DECREMENT'
  p[0] = ('dec', p[1])

#############################
#-----LOGICAL OPERATORS-----#
#############################

def p_expression_comop(p):
    '''
    expression : expression DOUBLEEQUAL expression
              | expression LESSTHAN expression
              | expression GREATERTHAN expression
              | expression LESSTHANEQUAL expression
              | expression GREATERTHANEQUAL expression
              | expression NOTEQUAL expression
              | expression AND expression
              | expression OR expression
    '''
    p[0] = ('comop', p[1], p[2], p[3])

def p_expression_comop_paren(p):
  '''
  expression : LPAREN expression DOUBLEEQUAL expression RPAREN
            | LPAREN expression LESSTHAN expression RPAREN
            | LPAREN expression GREATERTHAN expression RPAREN
            | LPAREN expression LESSTHANEQUAL expression RPAREN
            | LPAREN expression GREATERTHANEQUAL expression RPAREN
            | LPAREN expression NOTEQUAL expression RPAREN
            | LPAREN expression AND expression RPAREN
            | LPAREN expression OR expression RPAREN  
  '''
  p[0] = ('comop', p[2], p[3], p[4])

def p_not(p):
  '''
  expression : NOT expression
              | LPAREN NOT expression RPAREN
  '''
  if len(p) == 5:
    p[0] = ("not_comop", p[3])
  else:
    p[0] = ('not_comop', p[2])

def p_negative_numbers(p):
  '''
  expression : MINUS INT
              | MINUS DOUBLE
              | MINUS VAR
  '''
  p[0] = ('neg_type', p[2])

#############################
#--------DATA TYPES---------#
#############################

def p_var(p):
  'expression : VAR'    
  p[0] = ('var', p[1])

def p_int(p):
  'expression : INT' 
  p[0] = ('num_int', p[1])

def p_double(p):
  'expression : DOUBLE'    
  p[0] = ('num_double', p[1])

def p_string(p):
  'expression : STRING'    
  p[0] = ('string', p[1])

def p_char(p):
  'expression : CHAR'    
  p[0] = ('char', p[1])

def p_bool(p):
  'expression : BOOL'    
  p[0] = ('bool', p[1])

def p_struct(p):
  'expression : STRUCT'    
  p[0] = ('struct', p[1])

def p_expression_var_access_var(p):
  'expression : VAR ACCESS VAR'
  p[0] = ("set_struct_item_to_struct_item", p[1], p[3]) 

########################
#----IF-ELSEIF-ELSE----#
########################

def p_if_statement(p): # if
  'statement : IF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON'
  p[0] = ('if', p[3], p[6])

def p_if_else_statement(p): # if-else
  '''
  statement : IF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSE LBRACE statement RBRACE SEMICOLON
            | IF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSE LBRACE empty RBRACE SEMICOLON
  '''
  p[0] = ('if-else', p[3], p[6], p[11])

def p_if_elseif_else_statement(p): # if-elseif-else
  '''
  statement : IF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSEIF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSE LBRACE statement RBRACE SEMICOLON
            | IF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSEIF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSE LBRACE empty RBRACE SEMICOLON
  '''
  p[0] = ('if-elseif-else', p[3], p[6], p[11], p[14], p[19])

def p_if_elseif_elseif_else_statement(p): # if-elseif-elseif-else
  '''
  statement : IF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSEIF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSEIF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSE LBRACE statement RBRACE SEMICOLON
            | IF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSEIF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSEIF LPAREN expression RPAREN LBRACE statement RBRACE SEMICOLON ELSE LBRACE empty RBRACE SEMICOLON
  '''
  p[0] = ('if-elseif-elseif-else', p[3], p[6],  p[11], p[14], p[19], p[22], p[27])

########################
#--------ERROR---------#
########################

def p_error(p):
  print ("ERROR ---> Syntax error in input! at line: ", str(p.lineno), " value: ", str(p.value))

# myLexer = lex.lex(module=lexer)
# myParser = yacc.yacc()

# while True:
#     try:
#         x = input('>>')
#     except EOFError:
#         break
#     tuple = myParser.parse(x, lexer=myLexer)
#     print(tuple)