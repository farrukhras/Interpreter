import ply.yacc as yacc
import myParser
import sys
import os

# search for an item in the environment
def env_lookup(var_name, environment):
  if var_name in environment[1]:
    return environment[1][var_name]
  else:
    return None

# look for a value when declaring a variable
def env_check(var_name, environment):
  if var_name in environment[1]:
    return True
  else:
    return False

# update an item in the environment 
def env_update(var_name, value, environment):
  if (type(environment[1][var_name]) is int and type(value) is int) or (type(environment[1][var_name]) is bool and type(value) is bool) or (type(environment[1][var_name]) is str and type(value) is str) or (type(environment[1][var_name]) is float and type(value) is float):
    if var_name in environment[1]:
        environment[1][var_name] = value
    elif not environment [0] == None:
      return env_update(var_name, value, environment)
  else:
    print("")
    print("ERROR ---> Type Mismatch")
    print("")
    exit(1)

# evaluate an expression
def evaluate_expression(tree, environment): # if the type of parsed input is a EXPRESSION
  nodeType = tree[0]

  # print(tree)

  if nodeType == "num_int" or nodeType == "num_double" or nodeType == "string" or nodeType == "char" or nodeType == "bool" or nodeType == "struct-item":
    return tree[1]

  elif nodeType == "var":
    var_name = tree[1]
    return env_lookup(var_name, environment)
  
  elif nodeType == "neg_type": # just print negative value at runtime, do not assign the value to the var
    if type(tree[1]) is str:
      if env_lookup(tree[1], environment) != None:
        new_value = -environment[1][tree[1]]
        return new_value
    else:
      return -tree[1]
  
  elif nodeType == "binop":
    left_child = tree[1]
    operator = tree[2]
    right_child = tree[3]
    left_val = evaluate_expression(left_child, environment)
    right_val = evaluate_expression(right_child, environment)
    
    if operator == "+":
      try:
        if (type(left_val) == str and type(right_val) == str):
          return left_val + " " + right_val
        else:
          return left_val + right_val
      except:
        print("")
        print("ERROR ----> Invalid operand types!!")
        print("")
        exit(1)

    elif operator == "-":
      try:
        return left_val - right_val
      except:
        print("")
        print("ERROR ----> Invalid operand types!!")
        print("")
        exit(1)
      
    elif operator == "*":
      try:
        return left_val * right_val
      except:
        print("")
        print("ERROR ----> Invalid operand types!!")
        print("")
        exit(1)
      
    elif operator == "/":
      try:
        if right_val == 0:
          print("")
          print("ERROR ---> Cannot Divide by 0.")
          print("")
          exit(1)
        else:
          return left_val / right_val
      except:
        print("")
        print("ERROR ----> Invalid operand types!!")
        print("")
        exit(1)
      
    elif operator == "^":
      try:
        return left_val ** right_val
      except:
        print("")
        print("ERROR ----> Invalid operand types!!")
        print("")
        exit(1)
      
    elif operator == "%":
      try:
        return left_val % right_val
      except:
        print("")
        print("ERROR ----> Invalid operand types!!")
        print("")
        exit(1)
    
    else:
      print("")
      print("ERROR -----> Internal Server Error!")
      print("")
      exit(1)
  
  elif nodeType == "not_comop": # just print negated value at runtime, do not assign the value to the var
    child = tree[1]
    val = evaluate_expression(child, environment)
    new_val = None

    if val != None: # if we want to update an existing variable
      if val == "true" or val == True:
        new_val = "false" 
      elif val == "false" or val == False:
        new_val = "true" 
      else:
        print("")
        print("ERROR ----> Bool Type Mismatch")
        print("")
        exit(1)
    else:
      print("")
      print("ERROR ----> Internal Server Error")
      print("")
      exit(1)

    return new_val

  elif nodeType == "comop":
    
    left_child = tree[1]
    operator = tree[2]
    right_child = tree[3]
    left_val = evaluate_expression(left_child, environment)
    right_val = evaluate_expression(right_child, environment)
    
    if operator == '>':
      print()
      try:
        if left_val > right_val:
          return "true"
        else:
          
          return "false"
      except:
        print("")
        print("ERROR ----> Operands Mismatch!!!")
        print("")
        exit(1)

    elif operator == '>=':
      try:
        if left_val >= right_val:
          return "true"
        else:
          return "false"
      except:
        print("")
        print("ERROR ----> Operands Mismatch!!!")
        print("")
        exit(1)

    elif operator == '<':
      try:
        if left_val < right_val:
          return "true"
        else:
          return "false"
      except:
        print("")
        print("ERROR ----> Operands Mismatch!!!")
        print("")
        exit(1)
      
    elif operator == '<=':
      try:
        if left_val <= right_val:
          return "true"
        else:
          return "false"
      except:
        print("")
        print("ERROR ----> Operands Mismatch!!!")
        print("")
        exit(1)
      
    elif operator == '==':
      try:
        if left_val == right_val:
          return "true"
        else:
          return "false"
      except:
        print("")
        print("ERROR ----> Operands Mismatch!!!")
        print("")
        exit(1)
      
    elif operator == '!=':
      try:
        if left_val != right_val:
          return "true"
        else:
          return "false"
      except:
        print("")
        print("ERROR ----> Operands Mismatch!!!")
        print("")
        exit(1)
      
    elif operator == 'AND':
      try:
        if left_val == "false" and right_val == "false":
          return "false"
        elif left_val == "false" and right_val == "true":
          return "false"
        elif left_val == "true" and right_val == "false":
          return "false"
        else:
          return "true"
      except:
        print("")
        print("ERROR ----> Operands Mismatch!!!")
        print("")
        exit(1)
      
    elif operator == 'OR':
      try:
        if left_val == "false" and right_val == "false":
          return "false"
        elif left_val == "false" and right_val == "true":
          return "true"
        elif left_val == "true" and right_val == "false":
          return "true"
        else:
          return "true"
      except:
        print("")
        print("ERROR ----> Operands Mismatch!!!")
        print("")
        exit(1)

    else:
      print("")
      print("ERROR -----> INVALID operator used!!!")
      print("")
      exit(1)

# evaluate a statement
def evaluate_statement(tree, environment): # if the type of parsed input is a STATEMEMT  
  stmtType = tree[0]

  if stmtType == "declare":
    # type_var = tree[1]
    var_name = tree[2]
    right_child = tree[3]
    new_value = evaluate_expression(right_child, environment) # find new value of the variable name
    
    if new_value == None:
      print("")
      print("ERROR --> Invalid Assignment!")
      print("")
      exit(1)
    else:
      if (not env_check(var_name, environment)): # if variable is not in the environment, then add it to the environment
        environment[1][var_name] = new_value # if not already in the table, then add it there
        # print(environment)
      else:
        print("")
        print("ERROR --> Variable Name Already Declared!!")
        print("")
        exit(1)

  elif stmtType == "assign":
    var_name = tree[1]
    right_child = tree[2]
    new_value = evaluate_expression(right_child, environment) # find new value of the variable name
    if (not env_lookup(var_name, environment)): # if variable is not in the environment, then print an error
      print("")
      print("ERROR --> Variable is not defined. Try again with a Variable that has been defined!!")
      print("")
      exit(1)
    else:
      env_update(var_name, new_value, environment)

  #################
  #----STRUCTS----#
  #################

  elif stmtType == "declare-struct":
    # type = tree[1]
    struct_name = tree[2] 
    children = tree[3] # e.g.('struct-item', 'a', ('struct-item', 'b', []))

    if (not env_lookup(struct_name, environment)): # if struct is not in the environment, then add it to the environment
      environment[1][struct_name] = {} # if not already in the table, then add it there
      for var_name in children:
        environment[1][struct_name][var_name] = -1
    else:
      print("")
      print("ERROR --> STRUCT Already Declared!!")
      print("Suggestion:  Choose a NEW name for your STRUCT and try AGAIN")
      print("")
      exit(1)

  elif stmtType == "initialize-struct-obj":
    struct_name = tree[1]
    struct_obj_name = tree[2]

    if (not env_lookup(struct_name, environment)): # if no STRUCT, with the given name, has been defined
      print("")
      print("ERROR --> STRUCT does not exists!!")
      print("Suggestion:  Kindly define the struct and try AGAIN!!!")
      print("")
      exit(1)
    elif (env_lookup(struct_obj_name, environment)): # if there already exists an object with the given name
      print("")
      print("ERROR --> Object already defined!!")
      print("Suggestion:  Kindly choose a new object name and try AGAIN!!!")
      print("")
      exit(1)
    else: # initialize a new object with the same parameters as that of the struct
      keys = environment[1][struct_name].keys()
      environment[1][struct_obj_name] = {}
      for var_name in keys:
        environment[1][struct_obj_name][var_name] = -1
      
  elif stmtType == "access-struct-var":
    obj_name = tree[1]
    var_to_access_name = tree[2]

    if (not env_lookup(obj_name, environment)): # if no Object, with the given name, has been defined
      print("")
      print("ERROR --> Struct Object does not exists!!")
      print("Suggestion:  Kindly define an object and try AGAIN!!!")
      print("")
      exit(1)
    else: # object found in the environment
      if var_to_access_name in environment[1][obj_name].keys(): # if the variable name, to be accessed, is present in the object
        return environment[1][obj_name][var_to_access_name]
      else: # if the variable name, to be accessed, is not present in the object
        print("")
        print("ERROR --> AttributeError")
        print("")
        exit(1)

  elif stmtType == "set-struct-var-value":
    obj_name = tree[1] # get the name of the object on RHS
    var_name = tree[2] # get the variable name on RHS
    val_name = tree[3] # get the term that needs to be assigned to the struct object item

    # RHS
    if (not env_lookup(obj_name, environment)): # if no Object, with the given name, has been defined
      print("")
      print("ERROR --> Struct Object does not exists!!")
      print("Suggestion:  Kindly define an object and try AGAIN!!!")
      print("")
      exit(1)
    else: # object found in the environment
      if not var_name in environment[1][obj_name].keys(): # if the variable name, to be accessed, is NOT present in the object
        print("")
        print("ERROR --> AttributeError")
        print("")
        exit(1)
      else: # if the variable name, to be accessed, IS present in the object
        # CHECK LHS of the equation
        if val_name[0] == "set_struct_item_to_struct_item": # if you want to set another structs item value to another structs item value
          tbs_obj_name = val_name[1] # name of the object whose value needs to be assigned to this structs' item
          tbs_item_name = val_name[2] # name of the objects' item that we need to assign to this structs' item

          if (not env_lookup(tbs_obj_name, environment)):
            print("")
            print("ERROR: ---> " + tbs_obj_name + " doesnot exists!!!")
            print("")
            exit(1)
          elif not tbs_item_name in environment[1][tbs_obj_name].keys(): # if the key does not exists in the object
            print("")
            print("ERROR: ---> " + tbs_item_name + " doesnot exists in " + tbs_obj_name + " !!!")
            print("")
            exit(1)
          else: # get the value of the LHS of the = sign and set it to the RHS
            val = environment[1][tbs_obj_name][tbs_item_name] # get the value
            environment[1][obj_name][var_name] = val # update the struct value
        
        else: # if the value to be set is just a normal Variable 
          val = evaluate_expression(tree[3], environment) # get the value
          environment[1][obj_name][var_name] = val # update the struct value

  ################
  #----OTHERS----#
  ################

  elif stmtType == "print":
    only_child = tree[1]
    output = ""

    if type(only_child) is list: # if the print has multiple expressions or statements to print (a list to be printed)
      for exp in only_child:
        if output != "": # if "output" is initially not empty
          output = output + " " + str(evaluate_expression(exp, environment))
        else:
          output = str(evaluate_expression(exp, environment))
      if output != None:
        print("yain>> ", output)
    else: 
      if len(only_child) > 2: # for structs
        if evaluate_statement(tree[1], environment) != None:
          print("yain>> ", evaluate_statement(tree[1], environment))
      else: # newline and single input
        if evaluate_expression(tree[1], environment) == "": # print new line
          print("")
        else:
          if evaluate_statement(tree[1], environment) != None:
            print("yain>> ", evaluate_statement(tree[1], environment))

  elif stmtType == "inc": # look for the variable in the environment, if present then update it, else return error ke bariable does not exists
    curr_value =  env_lookup(tree[1], environment)
    if curr_value != None:
      new_value = curr_value + 1
      env_update(tree[1], new_value, environment)
    else:
      print("")
      print("ERROR --> Variable does not exists!")
      print("")
      exit(1)

  elif stmtType == "dec": # look for the variable in the environment, if present then update it, else return error ke bariable does not exists
    curr_value =  env_lookup(tree[1], environment)
    if curr_value != None:
      new_value = curr_value - 1
      env_update(tree[1], new_value, environment)
    else:
      print("")
      print("ERROR --> Variable does not exists!")
      print("")
      exit(1)

  ########################
  #----IF-ELSEIF-ELSE----#
  ########################

  elif stmtType == "if":
    conditional_exp = tree[1] # x < 5
    then_stmt = tree[2] # A = B
    if evaluate_expression(conditional_exp, environment):
      evaluate_statement(then_stmt, environment)

  elif stmtType == "if-else":
    conditional_exp = tree[1]
    then_stmt = tree[2] 
    else_stmt = tree[3] 

    if evaluate_expression(conditional_exp, environment):
      evaluate_statement(then_stmt, environment)
    else:
      if else_stmt != []: # if we pass an empty else parameter, then do not render it
        evaluate_statement(else_stmt, environment)

  elif stmtType == "if-elseif-else":
    # exp -> stmt -> exp -> stmt -> stmt
    if_exp = tree[1]
    if_stmt = tree[2]
    elseif_exp = tree[3]
    elseif_stmt = tree[4]
    else_stmt = tree[5]

    if evaluate_expression(if_exp, environment) == "true":
      evaluate_statement(if_stmt, environment)
    elif evaluate_expression(elseif_exp, environment) == "true":
      evaluate_statement(elseif_stmt, environment)
    else:
      if else_stmt != []: # if we pass an empty else parameter, then do not render it
        evaluate_statement(else_stmt, environment)

  elif stmtType == "if-elseif-elseif-else":
    # exp -> stmt -> exp -> stmt -> exp -> stmt -> stmt
    if_exp = tree[1]
    if_stmt = tree[2]
    elseif1_exp = tree[3]
    elseif1_stmt = tree[4]
    elseif2_exp = tree[5]
    elseif2_stmt = tree[6]
    else_stmt = tree[7]

    if evaluate_expression(if_exp, environment):
      evaluate_statement(if_stmt, environment)
    elif evaluate_expression(elseif1_exp, environment):
      evaluate_statement(elseif1_stmt, environment)
    elif evaluate_expression(elseif2_exp, environment):
      evaluate_statement(elseif2_stmt, environment)
    else:
      if else_stmt != []: # if we pass an empty else parameter, then do not render it
        evaluate_statement(else_stmt, environment)

  else:
    return evaluate_expression(tree, environment)

yaplParser = yacc.yacc(module = myParser)

def main():
  environment = (None, {})
  try:
    with open(os.path.join("test_cases", sys.argv[1]), "r") as file:
      data = file.read()
  except EOFError:
    print("")
    print("Something extremely BAD happened when reading the File....Reopen the File, or just Relax and enjoy Holidays!!!!!!")
    print("")

  parse_data = yaplParser.parse(data) 

  res = None
  for x in parse_data:
    res = evaluate_statement(x, environment)
  if res != None:
    print(res)

main()