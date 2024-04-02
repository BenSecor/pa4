# Python starter code that covers the majority of the aspects of the
# PA4c OCaml video. 
import sys
from collections import namedtuple

global attr_count
attr_count = 0

# Where OCaml would use "algebraic datatypes", we use
# Python's "namedtuple", which is similar. 
CoolClass        = namedtuple("CoolClass", "Name Inherits Features")
Attribute        = namedtuple("Attribute", "Name Type Initializer") 
Method           = namedtuple("Method",    "Name Formals ReturnType Body")
Formal           = namedtuple("Formal",    "Name Type")

IMObject = namedtuple("IMObject", "Name NumMethods Methods")
# A Cool Identifier is a tuple (pair) of a location and a string.
ID               = namedtuple("ID",        "loc str") 
Expression = namedtuple("Expression", "loc ekind Type")
# Kinds of Expressions
Integer          = namedtuple("Integer",   "Integer") 
Plus             = namedtuple("Plus",      "Left Right") 
While            = namedtuple("While",     "Predicate Body") 
# Additional namedtuple structures for expressions
Assign = namedtuple("Assign", "Identifier Expression")
Minus = namedtuple("Minus", "Left Right")
Times = namedtuple("Times", "Left Right")
Divide = namedtuple("Divide", "Left Right")
LessThan = namedtuple("LessThan", "Left Right")
LessThanOrEqual = namedtuple("LessThanOrEqual", "Left Right")
Equal = namedtuple("Equal", "Left Right")
Not = namedtuple("Not", "Expression")
Negate = namedtuple("Negate", "Expression")
If = namedtuple("If", "Predicate Then Else")
Block = namedtuple("Block", "Expressions")
New = namedtuple("New", "Identifier")
IsVoid = namedtuple("IsVoid", "Expression")
TrueConstant = namedtuple("TrueConstant", "")
FalseConstant = namedtuple("FalseConstant", "")
Let = namedtuple("Let", "Bindings Expression")
Case = namedtuple("Case", "Expression Elements")
Tilde = namedtuple("Tilde", "Expression")
IntegerConstant = namedtuple("IntegerConstant", "Value")
StringConstant = namedtuple("StringConstant", "Value")
CaseBranch = namedtuple("CaseBranch", "Identifier Type Expression")
Binding = namedtuple("Binding", "Name Type Initializer")
StaticDispatch = namedtuple("StaticDispatch", "Object Type MethodName ArgsList")
SelfDispatch = namedtuple("SelfDispatch", "MethodName ArgsList")
DynamicDispatch = namedtuple("DynamicDispatch", "Object MethodName ArgsList")

# The lines in the CL-AST file that we read as input.
lines = [] 

# This follows a similar structure to the OCaml video code. 
def main(): 
  global lines
  fname = sys.argv[1] 
  with open(fname) as file:
    lines = [line.rstrip("\r\n") for line in file] 

  # Each call to read() returns the next line (as a string). 
  def read(): 
    global lines
    this_line = lines[0] 
    lines = lines[1:]
    return this_line 

  # read_list is a higher-order function. You pass in a worker
  # function (like "read_exp" or "read_feature"). This first
  # reads the number of elements in the list and then calls
  # the worker function that many times in a row. It returns
  # a list made of the returns of the sequential calls to the
  # worker function.
  def read_list(worker):
    k = int(read()) 
    return [ worker () for x in range(k) ] 

  def read_cool_program():
    return read_list(read_cool_class)

  def read_id():
    loc = read () 
    name = read ()
    return ID(loc, name) 

    #read thhe cool class
  def read_cool_class ():
    cname = read_id () 
    i = read()
    if i == "no_inherits": 
      inherits = None 
    elif i == "inherits": 
      inherits = read_id()
    # else:
    #   raise(Exception(f"read_cool_class: inherits {i}"))
    features = read_list(read_feature)
    return CoolClass(cname, inherits, features) 

    #feature is either attribute or method and where it has intitializers
  def read_feature (): 
    a = read()
    if a == "attribute_no_init":
      fname = read_id()
      ftype = read_id()
      return Attribute(fname, ftype, None) 
    elif a == "attribute_init":
      fname = read_id()
      ftype = read_id()
      finit = read_exp()
      return Attribute(fname, ftype, finit)
    elif a == "method":
      mname = read_id()
      formals = read_list(read_formal)
      mtype = read_id()
      mbody = read_exp()
      return Method(mname, formals, mtype, mbody)
    # else:
    #   raise(Exception(f"read_feature {a}"))

  def read_formal ():
    fname = read_id()
    ftype = read_id().str
    return Formal(fname, ftype) 

  def read_binding():
    b = read()
    if b == "let_binding_no_init":
        name = read_id()
        typex = read_id()
        return Binding(name, typex, None)
    elif b == "let_binding_init":
        name = read_id()
        typex = read_id()
        init = read_exp()
        return Binding(name, typex, init)
    # else:
    #     raise Exception(f"Unknown binding kind: {b}")
  # An expression starts with its location (line number) 
  # and then has a "kind" (like Plus or While). 
  def read_exp ():
    eloc = read() 
    ekind = read_ekind ()
    return Expression(eloc, ekind, None) 
    #reads a branch of a case expression o
  def read_case_exp():
    var = read_id()
    type_id = read_id()
    body_exp = read_exp()
    #since this method is caled by list, and we want to use tthe
    # type check exp later with this we are returning type expression 
    # with ekind = CaseBranch
    return Expression(var.loc, CaseBranch(var, type_id, body_exp), None)
  
  #read assign
  def read_assign():
    identifier = read_id()
    expression = read_exp()
    return Assign(identifier, expression)
  
    #read expression
  def read_ekind():
    ekind = read()
    if ekind == "integer":
        ival = read()
        return Integer(ival)
    elif ekind == "plus":
        left = read_exp()
        right = read_exp()
        return Plus(left, right)
    elif ekind == "while":
        predicate = read_exp()
        body = read_exp()
        return While(predicate, body)
    elif ekind == "minus":
        left = read_exp()
        right = read_exp()
        return Minus(left, right)
    elif ekind == "times":
        left = read_exp()
        right = read_exp()
        return Times(left, right)
    elif ekind == "divide":
        left = read_exp()
        right = read_exp()
        return Divide(left, right)
    elif ekind == "lt":
        left = read_exp()
        right = read_exp()
        return LessThan(left, right)
    elif ekind == "le":
        left = read_exp()
        right = read_exp()
        return LessThanOrEqual(left, right)
    elif ekind == "eq":
        left = read_exp()
        right = read_exp()
        return Equal(left, right)
    elif ekind == "not":
        expression = read_exp()
        return Not(expression)
    elif ekind == "negate":
        expression = read_exp()
        return Negate(expression)
    elif ekind == "if":
        predicate = read_exp()
        then_exp = read_exp()
        else_exp = read_exp()
        return If(predicate, then_exp, else_exp)
    elif ekind == "block":
        expressions = read_list(read_exp)
        return Block(expressions)
    elif ekind == "new":
        identifier = read_id()
        return New(identifier)
    elif ekind == "isvoid":
        expression = read_exp()
        return IsVoid(expression)
    elif ekind == "identifier":
        identifier = read_id()
        return identifier
    elif ekind == "true":
        return TrueConstant()
    elif ekind == "false":
        return FalseConstant()
    elif ekind == "let":
        bindings = read_list(read_binding)
        expression = read_exp()
        return Let(bindings, expression)
    elif ekind == "case":
        expression = read_exp()
        elements = read_list(read_case_exp)
        return Case(expression, elements)
    elif ekind == "tilde":
        expression = read_exp()
        return Tilde(expression)
    elif ekind == "string":
        value = read()
        return StringConstant(value)
    elif ekind == "assign":
        identifier = read_id()
        expression = read_exp()
        return Assign(identifier, expression)
    elif ekind == "dynamic_dispatch":
        object_exp = read_exp()
        method_name = read_id()
        args_list = read_list(read_exp)
        return DynamicDispatch(object_exp, method_name, args_list)
    elif ekind == "static_dispatch":
        object_exp = read_exp()
        type_name = read_id()
        method_name = read_id()
        args_list = read_list(read_exp)
        return StaticDispatch(object_exp, type_name, method_name, args_list)
    elif ekind == "self_dispatch":
        method_name = read_id()
        args_list = read_list(read_exp)
        return SelfDispatch(method_name, args_list)
    else:
        raise(Exception(f"Unhandled expression kind: {ekind}"))

    #creates the entire AST 
  ast = read_cool_program () 

    #contrsuts a method map for non exp type checking
  def construct_method_map():
    method_map = {}
    for cl in ast:
      for feature in cl.Features:
        if isinstance(feature, Method):
          if cl.Name.str not in method_map:
            method_map[cl.Name.str] = {}
            method_map[cl.Name.str][feature.Name.str] = (feature.Formals, feature.ReturnType)
          elif method_map[cl.Name.str].get(feature.Name.str) is not None:
            print(f"ERROR: {feature.Name.loc}: Type-Check: Method {feature.Name.str} redefined in class {cl.Name.str}")
            exit(1)
          else:
            method_map[cl.Name.str][feature.Name.str] = (feature.Formals, feature.ReturnType)
    return method_map
  
  M =  construct_method_map()
    #hardcoded classes for the ast so that other classes can acess object and IO
  IO = CoolClass(ID("0", "IO"), ID("0", "Object"), 
                 [Method(ID("0", "out_string"), [Formal(ID("0", "x"), "String")], ID("0", "SELF_TYPE"), Expression("0", "String", StringConstant(""))),
                    Method(ID("0", "out_int"), [Formal(ID("0", "x"), "Int")], ID("0", "SELF_TYPE"), Expression("0", "String", StringConstant(""))), 
                           Method(ID("0", "in_string"), [], ID("0", "String"), Expression("0", "String", StringConstant(""))), 
                                  Method(ID("0", "in_int"), [], ID("0", "Int"), Expression("0", "String", StringConstant(""))), 
                                         Method(ID("0", "abort"), [], ID("0", "Object"), Expression("0", "String", StringConstant(""))), 
                                                Method(ID("0", "type_name"), [], ID("0", "String"), Expression("0","String",  StringConstant(""))),
                                                        Method(ID("0", "copy"), [], ID("0", "SELF_TYPE"), Expression("0",  "String", StringConstant("")))])
  Object = CoolClass(ID("0", "Object"), None,
                        [Method(ID("0", "abort"), [], ID("0", "Object"), Expression("0","String", StringConstant(""))),
                        Method(ID("0", "type_name"), [], ID("0", "String"), Expression("0", "String", StringConstant(""))),
                        Method(ID("0", "copy"), [], ID("0", "SELF_TYPE"), Expression("0", "String", StringConstant("")))])

  # Add base classes to ast
  ast.extend([IO, Object])

    #use topsort to check for inheritance using dfs
  def topological_sort(classes):
    graph = {cls.Name.str: set() for cls in classes}
    for cls in classes:
        if cls.Inherits:
            graph[cls.Inherits.str].add(cls.Name.str)

    visited = set()
    result = []

    def dfs(node):
        visited.add(node)
        for child in graph[node]:
            if child not in visited:
                dfs(child)
        result.append(node)

    for cls in graph:
        if cls not in visited:
            dfs(cls)

    result.reverse()  # Reverse the result to get the correct order
    return result
  
  #defin differrtent sets of classes we can use later

  base_classes = [ "Int", "String", "Bool"]
  all_classes = ([c.Name.str for c in ast ] + base_classes)

  # Look for inheritance from Int, String and undeclared classes 
  for c in ast:
    if c.Inherits != None: 
      i = c.Inherits 
      if i.str in [ "Int", "String", "Bool" ]:
        #cannot inherit from Int, String, or Bool
        print(f"ERROR: {i.loc}: Type-Check: inheriting from forbidden class {i.str}")
        exit(1)
      elif i.str not in all_classes:
        #if i.str is not in all classes throw an error
        print(f"ERROR: {i.loc}: Type-Check: inheriting from undefined class {i.str}")
        exit(1)

   # Check for duplicate classes
  for i in range(len(ast)):
    c_i = ast[i]
    for j in range(i+1, len(ast)): 
      c_j = ast[j]
      if c_i.Name.str == c_j.Name.str:
        print(f"ERROR: {c_j.Name.loc}: Type-Check: class {c_i.Name.str} redefined") 
        exit(1) 

  # recursive method for finding if their are cycles in the class inheritance 
  def has_cycle(graph, start, visited, rec_stack):
    visited[start] = True
    rec_stack[start] = True

    for neighbor in graph[start]:
      if not visited[neighbor]:
        if has_cycle(graph, neighbor, visited, rec_stack):
          return True
      elif rec_stack[neighbor]:
        return True

    rec_stack[start] = False
    return False

  class_graph = {cls.Name.str: set() for cls in ast}
  for cls in ast:
    if cls.Inherits:
      class_graph[cls.Inherits.str].add(cls.Name.str)

  visited = {cls: False for cls in class_graph}
  rec_stack = {cls: False for cls in class_graph}

#look for cycle
  for cls in class_graph:
    if not visited[cls]:
      if has_cycle(class_graph, cls, visited, rec_stack):
        print(f"ERROR: 0: Type-Check: inheritance cycle: {cls}") 
        exit(1) 

    #create user classes of all the classes topsorted
  user_classes = topological_sort(ast)
  base_classes.reverse()  # Reverse the base classes to get the correct order of the base classes
  all_classes = (base_classes + user_classes)

    # go through classes and preform typecheck check to make surte attributes and methods are not redefined in classes 
  for cls in ast:
    attributes = set()
    methods = set()

    for feature in cls.Features:
      if isinstance(feature, Attribute):
        if feature.Name.str in attributes:
          print(f"ERROR: {feature.Name.loc}: Type-Check: Attribute {feature.Name.str} is redefined in class {cls.Name.str}")
          exit(1)
        else:
          attributes.add(feature.Name.str)
      elif isinstance(feature, Method):
        if feature.Name in methods:
          print(f"ERROR: {feature.Name.loc}: Type-Check: Method {feature.Name.str} is redefined in class {cls.Name.str}")
          exit(1)
        else:
          methods.add(feature.Name)

  # Check for a child class that redefines a parent method but changes the parameters
  def check_method_redefinition():
    for cls in ast:
      if cls.Inherits:
        parent_class = None
        for c in ast:
          if c.Name.str == cls.Inherits.str:
            parent_class = c
            break

        if not parent_class:
          print(f"ERROR: {cls.Inherits.loc}: Type-Check: Undefined parent class {cls.Inherits.str}")
          exit(1)

        for feature in cls.Features:
          if isinstance(feature, Method):
            for parent_feature in parent_class.Features:
              if isinstance(parent_feature, Method) and feature.Name == parent_feature.Name:
                if feature.Name == parent_feature.Name:
                  if len(feature.Formals) != len(parent_feature.Formals):
                      print(f"ERROR: {feature.Name.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} redefines parent method with different number of parameters")
                      exit(1)
                  else:
                      for formal_child, formal_parent in zip(feature.Formals, parent_feature.Formals):
                          if formal_child[1].str != formal_parent[1].str:
                              print(f"ERROR: {feature.Name.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} redefines parent method with different parameter types")
                              exit(1)

  check_method_redefinition()

  # Check for a missing method main in class Main
  def check_main_method():
    main_class = None
    for cls in ast:
        if cls.Name.str == "Main":
            main_class = cls
            break
    
    if not main_class:
        print("ERROR: 0: Type-Check: class Main not found")
        exit(1)

    main_method_found = False
    for feature in main_class.Features:
        #check that main is a method, this is one of our test cases from pa4t, we had an attribute called main
        if feature.Name.str == "main" and isinstance(feature, Method):
            main_method_found = True
            break
    #if no maine we throw an error
    if not main_method_found:
        print("ERROR: 0: Type-Check: class Main method main not found")
        exit(1)

  check_main_method()

  # Check for self and SELF_TYPE mistakes in classes and methods.
  def check_self_and_self_type():
    for cls in ast:
        for feature in cls.Features:
            if isinstance(feature, Method):
                for formal in feature.Formals:
                    if isinstance(formal,Formal):
                      if formal.Type == "SELF_TYPE":
                          print(f"ERROR: {formal.Name.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} has a formal parameter of type 'SELF_TYPE'")
                          exit(1)
    #check duplicate_attributes
  def check_duplicate_attributes():
      for cls in ast:
          attributes = set()
          for feature in cls.Features:
              if isinstance(feature, Attribute):
                  if feature.Name.str in attributes:
                      print(f"ERROR: {feature.Name.loc}: Type-Check: Attribute {feature.Name.str} is redefined in class {cls.Name.str}") 
                      exit(1) 
                  else:
                      attributes.add(feature.Name.str)

  # Check for inheriting an attribute with the same name.
  def check_inherit_duplicate_attributes():
      for cls in ast:
          if cls.Inherits:
              parent_class = None
              for c in ast:
                  if c.Name.str == cls.Inherits.str:
                      parent_class = c
                      break

              if not parent_class:
                  print(f"ERROR: {cls.Inherits.loc}: Type-Check: Undefined parent class {cls.Inherits.str}")
                  exit(1)

              for feature in parent_class.Features:
                  if isinstance(feature, Attribute):
                      for cls_feature in cls.Features:
                          if isinstance(cls_feature, Attribute) and cls_feature.Name.str == feature.Name.str:
                              print(f"ERROR: {cls_feature.Name.loc}: Type-Check: Attribute {cls_feature.Name.str} is inherited with the same name")
                              exit(1)

  # Check for redefining methods with different parameters.
  def check_method_parameter_redefinition():
      for cls in ast:
          if cls.Inherits:
              parent_class = None
              for c in ast:
                  if c.Name.str == cls.Inherits.str:
                      parent_class = c
                      break

              if not parent_class:
                  print(f"ERROR: {cls.Inherits.loc}: Type-Check: Undefined parent class {cls.Inherits.str}")
                  exit(1)

              for feature in cls.Features:
                  if isinstance(feature, Method):
                      for parent_feature in parent_class.Features:
                          if isinstance(parent_feature, Method) and feature.Name == parent_feature.Name:
                              if feature.Name.str == parent_feature.Name.str:
                                if len(feature.Formals) != len(parent_feature.Formals):
                                    print(f"ERROR: {feature.Name.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} redefines parent method with different number of parameters")
                                    exit(1)
                                else:
                                    for formal_child, formal_parent in zip(feature.Formals, parent_feature.Formals):
                                        if formal_child[1].str != formal_parent[1].str:
                                            print(f"ERROR: {feature.Name.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} redefines parent method with different parameter types")
                                            exit(1)

  # Check for undefined attribute types.
  def check_undefined_attribute_types():
      for cls in ast:
          for feature in cls.Features:
              if isinstance(feature, Attribute) and feature.Type.str not in all_classes+["SELF_TYPE"]:
                  print(f"ERROR: {feature.Type.loc}: Type-Check: Undefined type {feature.Type.str} for attribute {feature.Name.str} in class {cls.Name.str}")
                  exit(1)

  # Check for undefined parameter types in method definitions.
  def check_undefined_parameter_types():
      for cls in ast:
          for feature in cls.Features:
              if isinstance(feature, Method):
                  for formal in feature.Formals:
                    if isinstance(formal,Formal):
                      if formal.Type not in all_classes+["SELF_TYPE"]:
                          print(f"ERROR: {formal.Name.loc}: Type-Check: Undefined type {formal.Type.str} for parameter {formal.Name.str} in method {feature.Name.str} of class {cls.Name.str}")
                          exit(1)
  
  '''
   Output "parent_map \n"
   Then output the number of parent-child inheritance relations and then \n. This number is equal to the number of classes minus one (since Object has no parent).
   Then output each child class in turn (in ascending alphabetical order):
     - Output the name of the child class and then \n.
     - Output the name of the child class's parent and then \n.
  '''
  parent_map = {}

  def create_parent_map(parent_map):
    result = "parent_map\n"
    for cls in ast:
      if cls.Inherits:
        parent_map[cls.Name.str] = cls.Inherits.str
      elif cls.Name.str != "Object":
        parent_map[cls.Name.str] = "Object"
    for cls2 in base_classes:
       parent_map[cls2] = "Object"
    result += str(len(parent_map)) + "\n"
    for child, parent in sorted(parent_map.items()):
        result += child + "\n" + parent + "\n"
    return result

  parentMapString = create_parent_map(parent_map)

  def find_least_common_ancestor(type1, type2):
    if type1 == None:
        return type2
    if type2 == None:
        return type1
    parents1 = get_inheritances(type1)
    parents2 = get_inheritances(type2)
    # Traverse the ancestors of type1
    for parent in parents1:
        # If an ancestor of type1 is also an ancestor of type2, it's the LCA
        if parent in parents2:
            return parent
    # If no common ancestor is found, return the topmost ancestor (e.g., Object)
    return "Object"

  def get_inheritances(type):
    parents = []
    # Add the type itself to the list of ancestors
    parents.append(type)
    # Traverse the inheritance hierarchy upwards
    for cl in ast:
        if cl.Name.str == type:
            while cl.Inherits:
                # Get the parent class of the current type
                parents.append(cl.Inherits.str)
                for cl2 in ast:
                    if cl2.Name.str != cl.Inherits.str:
                        continue
                    else:
                        cl = cl2
                    break
            return parents
    return parents
  
  def type_check_exp(O, M, C, exp):
    if exp == []:
        return "", O, exp

    if isinstance(exp.ekind, Integer):
        return "Int", O, Expression(exp.loc, exp.ekind, "Int")
        
    elif isinstance(exp.ekind, Plus):
        left_type, O, left_exp = type_check_exp(O, M, C, exp.ekind.Left)
        right_type, O, right_exp = type_check_exp(O, M, C, exp.ekind.Right)
        if left_type != "Int" or right_type != "Int":
            print(f"ERROR: {exp.loc}: Type-Check: Plus operation with non-integer operands")
            exit(1)
        ekind = Plus(left_exp, right_exp)
        exp = Expression(exp.loc, ekind, "Int")
        return "Int", O, exp

    elif isinstance(exp.ekind, Minus):
        left_type, O, left_exp = type_check_exp(O, M, C, exp.ekind.Left)
        right_type, O, right_exp = type_check_exp(O, M, C, exp.ekind.Right)
        if left_type != "Int" or right_type != "Int":
            print(f"ERROR: {exp.loc}: Type-Check: Minus operation with non-integer operands")
            exit(1)
        ekind = Minus(left_exp, right_exp)
        exp = Expression(exp.loc, ekind, "Int")
        return "Int", O, exp
    elif isinstance(exp.ekind, Negate):
        operand_type, O, new_expression = type_check_exp(O, M, C, exp.ekind.Expression)
        if operand_type != "Int":
            print(f"ERROR: {exp.loc}: Type-Check: Negate operation with non-integer operand")
            exit(1)
        return "Int", O, Expression(exp.loc, Negate(new_expression), "Int")
    elif isinstance(exp.ekind, Times):
        left_type, O, left_exp = type_check_exp(O, M, C, exp.ekind.Left)
        right_type, O, right_exp = type_check_exp(O, M, C, exp.ekind.Right)
        if left_type != "Int" or right_type != "Int":
            print(f"ERROR: {exp.loc}: Type-Check: Times operation with non-integer operands")
            exit(1)
        ekind = Times(left_exp, right_exp)
        exp = Expression(exp.loc, ekind, "Int")
        return "Int", O, exp
    elif isinstance(exp.ekind, Divide):
        left_type, O_left, left_expr = type_check_exp(O, M, C, exp.ekind.Left)
        right_type, O_right, right_expr = type_check_exp(O_left.copy(), M, C, exp.ekind.Right)
        if left_type != "Int" or right_type != "Int":
            print(f"ERROR: {exp.loc}: Type-Check: Divide operation with non-integer operands")
            exit(1)
        new_expr = Expression(exp.loc, Divide(Expression(left_expr.loc, left_expr.ekind, left_type),
                        Expression(right_expr.loc, right_expr.ekind, right_type)), "Int")
        return "Int", O_right, new_expr
    elif isinstance(exp.ekind, IsVoid):
        expression_type, O, expression_exp = type_check_exp(O, M, C, exp.ekind.Expression)
        exp = Expression(exp.loc, expression_exp, "Bool")
        return "Bool", O, exp

    elif isinstance(exp.ekind, Attribute):
        # Check M so that the exp matches the corresponding attribute
        if exp.Name not in M[C.Name.str]:
            print(f"ERROR: {exp.ekind.Name.loc}: Type-Check: Attribute {exp.ekind.Name.str} not defined in class {C.Name.str}")
            exit(1)
        attribute_type = M[C.Name.str][exp.ekind.Name.str]
        if exp.ekind.Initializer:
            initializer_type, O, initializer_exp = type_check_exp(O, M, C, exp.ekind.Initializer)
            if initializer_type != attribute_type:
                print(f"ERROR: {exp.ekind.Initializer.loc}: Type-Check: Attribute {exp.Name.str} has wrong type")
                exit(1)
        # print(f"\nassigned {exp.ekind.Name.str} = {exp.ekind.Type.str}\n")
        O[exp.ekind.Name.str] = exp.ekind.Type.str
        exp = Expression(exp.loc,  Attribute(exp.ekind.Name, attribute_type, initializer_exp), attribute_type)
        return attribute_type, O, exp

    elif isinstance(exp.ekind, Method):
        # Check M so that the exp matches the data
        if exp.Name not in M[C.Name.str]:
            print(f"ERROR: {exp.ekind.Name.loc}: Type-Check: Method {exp.ekind.Name.str} not defined in class {C.Name.str}")
            exit(1)
        method_formals, method_return_type = M[C.Name.str][exp.ekind.Name.str]
        if len(exp.ekind.Formals) != len(method_formals):
            print(f"ERROR: {exp.loc}: Type-Check: Method {exp.ekind.Name.str} has wrong number of parameters")
            exit(1)
        for formal, method_formal in zip(exp.ekind.Formals, method_formals):
            if formal[1].str != method_formal[1].str:
                print(f"ERROR: {formal[1].loc}: Type-Check: Method {exp.ekind.Name.str} has wrong parameter type")
                exit(1)
        body_type, O, body_exp = type_check_exp(O, M, C, exp.ekind.Body)
        if body_type != method_return_type:
            print(f"ERROR: {exp.ekind.Body.loc}: Type-Check: Method {exp.ekind.Name.str} has wrong return type")
            exit(1)
        body = Method(exp.ekind.Name, exp.ekind.Formals, body_type, body_exp)
        exp = Expression(exp.loc, body, method_return_type)
        return method_return_type, O, exp
    elif isinstance(exp.ekind, While):
      # Type check the predicate expression
        predicate_type, O_predicate, predicate_exp = type_check_exp(O, M, C, exp.ekind.Predicate)
        
        # Check if the predicate expression is of type Bool
        if predicate_type != "Bool":
            print(f"ERROR: {exp.ekind.Predicate.loc}: Type-Check: While predicate is not of type Bool it is of type {predicate_type}")
            exit(1)
        
        # Type check the body expression
        body_type, O_body, body_exp = type_check_exp(O_predicate, M, C, exp.ekind.Body)
        
        # Return the type "Object" (as while loops in Cool always return Object), updated environment, and new expression
        return "Object", O_body, Expression(exp.loc, While(predicate_exp, body_exp), "Object")
    elif isinstance(exp.ekind, Block):
        final_type = exp.Type
        expressions = []
        for expression in exp.ekind.Expressions:
            expression_type, O, expression_exp = type_check_exp(O, M, C, expression)
            block_type = expression_type
            final_type = find_least_common_ancestor(block_type, final_type)
            expressions.append(expression_exp)
        return block_type, O, Expression(exp.loc, Block(expressions.copy()), block_type)

    elif isinstance(exp.ekind, New):
        if exp.ekind.Identifier.str =="SELF_TYPE":
            return C.Name.str , O, Expression(exp.loc, New(exp.ekind.Identifier), C.Name.str)
        if exp.ekind.Identifier.str not in all_classes:
            print(f"ERROR: {exp.ekind.Identifier.loc}: Type-Check: Undefined type {exp.ekind.Identifier.str}")
            exit(1)
        return exp.ekind.Identifier.str, O, Expression(exp.loc, New(exp.ekind.Identifier), exp.ekind.Identifier.str)
    elif isinstance(exp.ekind, CaseBranch):
        new_O = O.copy()
        new_O[exp.ekind.Identifier.str] = exp.ekind.Type.str
        case_type, O, case_exp = type_check_exp(new_O, M, C, exp.ekind.Expression)
        return case_type, O, Expression(exp.loc, CaseBranch(exp.ekind.Identifier, exp.ekind.Type, case_exp), exp.ekind.Identifier.str)
    elif isinstance(exp.ekind, ID):
        if exp.ekind.str == "self":
            return "SELF_TYPE", O, Expression(exp.loc, exp.ekind, "SELF_TYPE")
        if exp.ekind.str not in O:
            print(f"ERROR: {exp.ekind.loc}: Type-Check: Undefined attribute {exp.ekind.str}")
            exit(1)
        return O[exp.ekind.str], O, Expression(exp.loc, exp.ekind, O[exp.ekind.str])

    elif isinstance(exp.ekind, StringConstant):
        return "String", O, Expression(exp.loc, exp.ekind, "String")

    elif isinstance(exp.ekind, Tilde):
        expression_type, O, expression_exp = type_check_exp(O, M, C, exp.ekind.Expression)
        if expression_type != "Int":
            print(f"ERROR: {exp.ekind.Expression.loc}: Type-Check: Tilde operation with non-integer operand")
            exit(1)
        return "Int", O, Expression(exp.loc, expression_exp, "Int")
    elif isinstance(exp.ekind, SelfDispatch):
        if exp.ekind.MethodName.str not in M[C.Name.str]:
            print(f"ERROR: {exp.ekind.MethodName.loc}: Type-Check: Undefined method {exp.ekind.MethodName.str} in class {C.Name.str}")
            exit(1)
        method_formals, method_return_type = M[C.Name.str][exp.ekind.MethodName.str]
        if len(exp.ekind.ArgsList) != len(method_formals):
            print(f"ERROR: {exp.loc}: Type-Check: Method {exp.ekind.MethodName.str} called with wrong number of arguments")
            exit(1)
        new_args = []
        least_type = ""
        for arg, formal in zip(exp.ekind.ArgsList, method_formals):
            arg_type, O, new_arg_expression = type_check_exp(O, M, C, arg)
            if arg_type == "SELF_TYPE":
                arg_type = C.Name.str
            least_type = find_least_common_ancestor(arg_type, formal[1])
            if least_type != formal[1]:
                print(f"ERROR: {arg.loc}: Type-Check: Method {exp.ekind.MethodName.str} called with wrong argument type")
                exit(1)
            new_args.append(new_arg_expression)
        updated_expression = SelfDispatch(exp.ekind.MethodName, new_args)
        return method_return_type, O, Expression(exp.loc, updated_expression, method_return_type)
    elif isinstance(exp.ekind, Let):
        new_O = O.copy()
        bindings = []
        final_type = None
        for binding in exp.ekind.Bindings:
            # Add the binding to O
            new_O[binding.Name.str] = binding.Type.str
            final_type = find_least_common_ancestor(binding.Type.str, final_type)
            # Perform type checking for the initializer if present
            if binding.Initializer:
                initializer_type, O , new_initializer = type_check_exp(O, M, C, binding.Initializer)
                # Check if the initializer type matches the declared type
                if initializer_type != binding.Type.str:
                    print(f"ERROR: {binding.Initializer.loc}: Type-Check: {binding.Type.str} does not conform to {initializer_type} ")
                    exit(1)
                binding = Binding(binding.Name, binding.Type, new_initializer)
            bindings.append(binding)
        type, new_O, expression = type_check_exp(new_O, M, C, exp.ekind.Expression)
        # if type == "SELF_TYPE":
        #     type = C.Name.str
        return type, new_O, Expression(exp.loc, Let(bindings, expression), type)
    elif isinstance(exp.ekind, TrueConstant):
        # Return the type "Bool", the original environment, and the new expression
        return "Bool", O, Expression(exp.loc, TrueConstant(), "Bool")
    elif isinstance(exp.ekind, If):
        predicate_type, O_pred, pred_expr = type_check_exp(O, M, C, exp.ekind.Predicate)
        if predicate_type != "Bool":
            print(f"ERROR: {exp.ekind.Predicate.loc}: Type-Check: If predicate is not of type Bool")
            exit(1)
        then_type, O_then, then_expr = type_check_exp(O_pred.copy(), M, C, exp.ekind.Then)
        else_type, O_else, else_expr = type_check_exp(O_pred.copy(), M, C, exp.ekind.Else)
        if then_type == "SELF_TYPE" and else_type == "SELF_TYPE":
            new_type = "SELF_TYPE" 
        elif then_type == "SELF_TYPE":
            then_type = C.Name.str
        elif else_type == "SELF_TYPE":
            new_type = C.Name.str
        if then_type == else_type:
            new_type = then_type
        else:
            new_type = find_least_common_ancestor(then_type, else_type)
        new_expr = If(pred_expr,
                    then_expr,
                    else_expr)
        # print(f"IF TYPES, {new_type}, {then_type}, {else_type} \n")
        return new_type, O, Expression(exp.loc, new_expr, new_type)
    elif isinstance(exp.ekind, LessThan):
        left_type, O_left, left_expr = type_check_exp(O, M, C, exp.ekind.Left)
        right_type, O_right, right_expr = type_check_exp(O_left.copy(), M, C, exp.ekind.Right)
        if left_type != "Int" or right_type != "Int":
            print(f"ERROR: {exp.loc}: Type-Check: LessThan operation with non-integer operands")
            exit(1)
        new_expr = Expression(exp.loc,LessThan(Expression(left_expr.loc, left_expr.ekind, left_type),
                            Expression(right_expr.loc, right_expr.ekind, right_type)), "Bool")
        return "Bool", O_right, new_expr
    elif isinstance(exp.ekind, DynamicDispatch):
        object_type, O_obj, object_expr = type_check_exp(O, M, C, exp.ekind.Object)
        if object_type == "SELF_TYPE":
            object_type = C.Name.str
        if object_type not in all_classes:
            print(f"ERROR: {exp.ekind.Object.loc}: Type-Check: Undefined type {object_type}")
            # exit(1)
        if exp.ekind.MethodName.str not in M[object_type]:
            print(f"ERROR: {exp.ekind.MethodName.loc}: Type-Check: Undefined method {exp.ekind.MethodName.str} in class {object_type}")
            exit(1)
        method_formals, method_return_type = M[object_type][exp.ekind.MethodName.str]
        if method_return_type =="SELF_TYPE":
            method_return_type = object_type
        new_args_list = []
        O_arg = O_obj.copy()
        for arg in exp.ekind.ArgsList:
            arg_type, O_arg, arg_expr = type_check_exp(O_arg, M, C, arg)
            new_args_list.append(Expression(arg_expr.loc, arg_expr.ekind, arg_type))
        new_expr = Expression(exp.loc, DynamicDispatch(object_expr,
                                exp.ekind.MethodName,
                                new_args_list), method_return_type)
        return method_return_type, O, new_expr
    elif isinstance(exp.ekind, LessThanOrEqual):
        left_type, O_left, left_expr = type_check_exp(O, M, C, exp.ekind.Left)
        right_type, O_right, right_expr = type_check_exp(O_left.copy(), M, C, exp.ekind.Right)
        if left_type != "Int" or right_type != "Int":
            print(f"ERROR: {exp.loc}: Type-Check: LessThanOrEqual operation with non-integer operands")
            exit(1)
        new_expr = Expression(exp.loc, LessThanOrEqual(Expression(left_expr.loc, left_expr.ekind, left_type),
                                Expression(right_expr.loc, right_expr.ekind, right_type)), "Bool")
        return "Bool", O_right, new_expr
    elif isinstance(exp.ekind, Equal):
        left_type, O_left, left_expr = type_check_exp(O, M, C, exp.ekind.Left)
        right_type, O_right, right_expr = type_check_exp(O, M, C, exp.ekind.Right)
        if left_type != right_type:
            print(f"ERROR: {exp.loc}: Type-Check: Equal operation with different types {left_type}, {right_type}")
            exit(1)
        new_expr = Expression(exp.loc,Equal(Expression(left_expr.loc, left_expr.ekind, left_type),
                        Expression(right_expr.loc, right_expr.ekind, right_type)), "Bool")
        return "Bool", O_right, new_expr
    elif isinstance(exp.ekind, FalseConstant):
        # Return the type "Bool", the original environment, and the new expression
        return "Bool", O, Expression(exp.loc, FalseConstant(), "Bool")
    elif isinstance(exp.ekind, Case):
        # Type check the expression being matched
        case_type, _, case_exp = type_check_exp(O, M, C, exp.ekind.Expression)
        if case_type == "SELF_TYPE":
            case_type = C.Name.str  # Substitute SELF_TYPE with the current class name
        if case_type not in all_classes:
            print(f"ERROR: {exp.ekind.Expression.loc}: Type-Check: Undefined type {case_type}")
            exit(1)
        
        lowest_branch_type = None  # List to store the types of case branches
        branches = []
        # Iterate through case branches
        for branch in exp.ekind.Elements:
            new_O = O.copy()  # Create a new environment for each branch
            branch_type = None  # Variable to store the type of the branch body
            branch_type, new_O, branch_exp = type_check_exp(new_O, M, C, branch)
            branches.append(branch_exp)
            # Add the branch type to the list
            lowest_branch_type = find_least_common_ancestor(branch_type, lowest_branch_type )
        
        # Check if all branch types are consistent
        # if len(set(branch_types)) > 1:
            # print(f"ERROR: {exp.loc}: Type-Check: Case branches have different types")
            # exit(1)
        
        # The new expression will be the last expression in the case branch
        new_exp = Expression(exp.loc, Case(case_exp, branches), lowest_branch_type)
        
        return lowest_branch_type, new_O, new_exp 
    elif isinstance(exp.ekind, Assign):
        # Check if assigning to "self"
        if exp.ekind.Identifier.str == "self":
            print(f"ERROR: {exp.ekind.Identifier.loc}: Type-Check: Cannot assign to self")
            exit(1)
        
        # Check if the attribute is defined
        if exp.ekind.Identifier.str not in O:
            print(f"ERROR: {exp.ekind.Identifier.loc}: Type-Check: Undefined attribute {exp.ekind.Identifier.str}")
            exit(1)
        
        # Get the type of the attribute from the environment
        identifier_type = O[exp.ekind.Identifier.str]
        # Type check the assigned expression
        expression_type, O, new_exp = type_check_exp(O, M, C, exp.ekind.Expression)
        if expression_type == "SELF_TYPE":
            expression_type == C.Name.str
        if identifier_type != expression_type:
            least_type = find_least_common_ancestor(identifier_type, expression_type)
            # print(f"{least_type} is a subclass of id {identifier_type} that the expression type {expression_type} is a sub")
            # expression_type = least_type    
            # Check if the assigned expression type is a subtype of the identifier type
            if least_type != identifier_type:
                print(f"ERROR: {exp.loc}: Type-Check: Assigning expression of type {expression_type} to attribute of type {identifier_type}")
                exit(1)
        return expression_type, O, Expression(exp.loc, Assign(exp.ekind.Identifier, new_exp), expression_type)
    elif isinstance(exp.ekind, StaticDispatch):
        object_type, O, object_exp = type_check_exp(O, M, C, exp.ekind.Object)
        if object_type == "SELF_TYPE":
            object_type = C.Name.str
        if object_type not in all_classes:
            print(f"ERROR: {exp.ekind.Object.loc}: Type-Check: Undefined type {object_type}")
            exit(1)
        if exp.ekind.Type.str not in all_classes:
            print(f"ERROR: {exp.ekind.Type.loc}: Type-Check: Undefined type {exp.ekind.Type.str}")
            exit(1)
        if exp.ekind.MethodName.str not in M[exp.ekind.Type.str]:
            print(f"ERROR: {exp.ekind.MethodName.loc}: Type-Check: Undefined method {exp.ekind.MethodName.str} in class {exp.Type.str}")
            exit(1)
        method_formals, method_return_type = M[exp.ekind.Type.str][exp.ekind.MethodName.str]
        if len(exp.ekind.ArgsList) != len(method_formals):
            print(f"ERROR: {exp.loc}: Type-Check: Method {exp.ekind.MethodName.str} called with wrong number of arguments")
            exit(1)
        new_args = []
        least_type = ""
        for arg, formal in zip(exp.ekind.ArgsList, method_formals):
            arg_type, O, new_exp = type_check_exp(O, M, C, arg)
            if arg_type == "SELF_TYPE":
                arg_type = C.Name.str
            least_type = find_least_common_ancestor(arg_type, formal[1])
            if least_type != formal[1]:
                print(f"ERROR: {arg.loc}: Type-Check: Method {exp.ekind.MethodName.str} called with wrong argument type")
                exit(1)
            new_args.append(new_exp)
        if method_return_type == "SELF_TYPE":
            method_return_type = object_type
        # print( method_return_type + "METHOD RETURN TYPE \n")
        return method_return_type, O, Expression(exp.loc, StaticDispatch(object_exp, exp.ekind.Type, exp.ekind.MethodName, new_args.copy()), method_return_type)
    else:
        # print(f"UNKNOWN TYPE {exp} \n")
        return "", O, exp


  def generate_O(class_node):
    # Generates O to hold all instance variables in the class
    # O[name] = Type
    O = {}
    if class_node.Inherits and class_node.Inherits.str != class_node.Name.str :
        for cl in ast:
            if cl.Name.str == class_node.Inherits.str:
                O = generate_O(cl)
    for feature in class_node.Features:
        if isinstance(feature, Attribute):
            O[feature.Name.str] = feature.Type.str
    # print(O)
    return O

  def generate_M(ast):
    # Builds a M dictionary for the entire program to help type check method calls
    # M[Class][Method_Name] = (Args, ReturnType)
    M = {
        "Object": {
            "abort": ([], "Object"),
            "type_name": ([], "String"),
            "copy": ([], "SELF_TYPE")
        },
        "IO": {
            "out_string": ([(None, "String")], "SELF_TYPE"),
            "out_int": ([(None, "Int")], "SELF_TYPE"),
            "in_string": ([], "String"),
            "in_int": ([], "Int"),
            "abort": ([], "Object"),
            "type_name": ([], "String"),
            "copy": ([], "SELF_TYPE")
        },
        "String": {
            "length": ([], "Int"),
            "concat": ([(None, "String")], "String"),
            "substr": ([(None, "Int"), (None, "Int")], "String"),
            "abort": ([], "Object"),
            "type_name": ([], "String"),
            "copy": ([], "SELF_TYPE")
        },
        "Bool": {
            "abort": ([], "Object"),
            "type_name": ([], "String"),
            "copy": ([], "SELF_TYPE")},
        "Int": {
            "abort": ([], "Object"),
            "type_name": ([], "String"),
            "copy": ([], "SELF_TYPE")}
    }
    for cl in user_classes:
        for class_node in ast:
            if class_node.Name.str == cl:
                class_methods = {}
                if class_node.Inherits:
                    class_methods = M[class_node.Inherits.str]
                else:
                    class_methods = M["Object"]
                for feature in class_node.Features:
                    if isinstance(feature, Method):
                        arg_types = []
                        method_name = feature.Name.str
                        for formal in feature.Formals:
                            if formal != []:
                                # print(formal)
                                arg_types.append((formal.Name.str, formal.Type))
                        return_type = feature.ReturnType.str
                        #check to make sure we aren't overriding a curren tmethod incorectly
                        if method_name in class_methods:
                            if class_methods[method_name] != (arg_types, return_type):
                                print(f"ERROR: {feature.Name.loc}: Type-Check: Method {method_name} redefined with different signature")
                                exit(1)
                        class_methods[method_name] = (arg_types, return_type)
                M[class_node.Name.str] = class_methods
    return M
    
    
  
  def check_method_expressions(ast):
    M = generate_M(ast)
    new_ast = []
    for cl in ast:
        # Setup dictionary O
        O = generate_O(cl)
        new_features = []
        for feature in cl.Features:
            if isinstance(feature, Attribute) and feature.Initializer:
                expression_type, _, exp = type_check_exp(O, M, cl, feature.Initializer)
                if expression_type != feature.Type.str:
                    least_type = find_least_common_ancestor(feature.Type.str, expression_type)
                    if least_type != feature.Type.str:
                        print(f"ERROR: {exp.loc}: Type-Check: {feature.Type.str} does not conform to {expression_type}")
                        exit(1)
                new_feature = Attribute(feature.Name, feature.Type, exp)
                new_features.append(new_feature)
            elif isinstance(feature, Method):
                new_O = O.copy()
                for formal in feature.Formals:
                    O[formal[0].str] = formal[1]
                expression_type, O, exp = type_check_exp(O, M, cl, feature.Body)
                new_feature = Method(feature.Name, feature.Formals, feature.ReturnType, exp)
                new_features.append(new_feature)
                O = new_O
            else:
                new_features.append(feature)
        new_cl = CoolClass(cl.Name, cl.Inherits, new_features)
        new_ast.append(new_cl)
    return new_ast
  #Add checks for negative test cases
  check_self_and_self_type()
  check_duplicate_attributes()
  check_inherit_duplicate_attributes()
  check_method_parameter_redefinition()
  check_undefined_attribute_types()
  check_undefined_parameter_types()
  new_ast = check_method_expressions(ast)
  ast = new_ast

  def print_exp(exp):
    output_str = ""
    output_str += f"{exp[0]}\n"
    if exp.Type:
        output_str += exp.Type +"\n"
    else:
        print(f"ERROR: {exp.loc}: Does not have a type ;")
        exit(1)
    if isinstance(exp[1], Integer):
        output_str += "integer\n"
        output_str += f"{exp[1].Integer}\n"
        return output_str
    elif isinstance(exp[1], Plus):
        output_str += "plus\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
        return output_str
    elif isinstance(exp[1], Minus):
        output_str += "minus\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
        return output_str
    elif isinstance(exp[1], Times):
        output_str += "times\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
        return output_str
    elif isinstance(exp[1], Divide):
        output_str += "divide\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
        return output_str
    elif isinstance(exp[1], LessThan):
        output_str += "lt\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
        return output_str
    elif isinstance(exp[1], LessThanOrEqual):
        output_str += "le\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
        return output_str
    elif isinstance(exp[1], Equal):
        output_str += "eq\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
        return output_str
    elif isinstance(exp[1], Not):
        output_str += "not\n"
        output_str += print_exp(exp[1].Expression)
        return output_str
    elif isinstance(exp[1], Negate):
        # print("NEGATE FOUNDDDD" + exp.loc)
        output_str += "negate\n"
        output_str += print_exp(exp[1].Expression)
        return output_str
    elif isinstance(exp[1], If):
        output_str += "if\n"
        output_str += print_exp(exp[1].Predicate)
        # print("THEN BLOCK")
        # print(exp[1].Then)
        output_str += print_exp(exp[1].Then)
        output_str += print_exp(exp[1].Else)
        return output_str
    elif isinstance(exp[1], Block):
        output_str += "block\n"
        output_str += f"{len(exp[1].Expressions)}\n"
        for expression in exp[1].Expressions:
            output_str += print_exp(expression)
        return output_str
    elif isinstance(exp[1], New):
        output_str += "new\n"
        output_str += f"{exp[1].Identifier.loc}\n{exp[1].Identifier.str}\n"
        return output_str
    elif isinstance(exp[1], IsVoid):
        output_str += "isvoid\n"
        output_str += print_exp(exp[1].Expression)
        return output_str
    elif isinstance(exp[1], ID):
        output_str += "identifier\n"
        output_str += f"{exp[1].loc}\n{exp[1].str}\n"
        return output_str
    elif isinstance(exp[1], Let):
        output_str += "let\n"
        output_str += f"{len(exp[1].Bindings)}\n"
        for binding in exp.ekind.Bindings:
            if binding.Initializer:
                output_str += f"let_binding_init\n{binding.Name.loc}\n{binding.Name.str}\n{exp.loc}\n{binding.Type.str}\n{print_exp(binding.Initializer)}"
            else:
                output_str += f"let_binding_no_init\n{binding.Name.loc}\n{binding.Name.str}\n{exp.loc}\n{binding.Type.str}\n"
        output_str += print_exp(exp.ekind.Expression)
        return output_str
    elif isinstance(exp[1], Case):
        output_str += "case\n"
        output_str += print_exp(exp[1].Expression)
        output_str += f"{len(exp[1].Elements)}\n"
        for element in exp[1].Elements:
            output_str += print_exp(element)
        
        return output_str
    elif isinstance(exp[1], Tilde):
        output_str += "tilde\n"
        output_str += print_exp(exp[1].Expression)
        return output_str
    elif isinstance(exp[1], StringConstant):
        output_str += "string\n"
        output_str += f"{exp[1].Value}\n"
        return output_str
    elif isinstance(exp[1], Assign):
        output_str += "assign\n"
        output_str += f"{exp[1].Identifier.loc}\n{exp[1].Identifier.str}\n"
        output_str += print_exp(exp[1].Expression)
        return output_str
    elif isinstance(exp[1], DynamicDispatch):
        output_str += "dynamic_dispatch\n"
        output_str += print_exp(exp[1].Object)
        output_str += f"{exp[1].MethodName.loc}\n{exp[1].MethodName.str}\n"
        output_str += f"{len(exp[1].ArgsList)}\n"
        for arg in exp[1].ArgsList:
            output_str += print_exp(arg)
        return output_str
    elif isinstance(exp[1], StaticDispatch):
        output_str += "static_dispatch\n"
        output_str += print_exp(exp[1].Object)
        output_str += f"{exp[1].Type.loc}\n{exp[1].Type.str}\n"
        output_str += f"{exp[1].MethodName.loc}\n{exp[1].MethodName.str}\n"
        output_str += f"{len(exp[1].ArgsList)}\n"
        for arg in exp[1].ArgsList:
            output_str += print_exp(arg)
        return output_str
    elif isinstance(exp[1], SelfDispatch):
        output_str += "self_dispatch\n"
        output_str += f"{exp[1].MethodName.loc}\n{exp[1].MethodName.str}\n"
        output_str += f"{len(exp[1].ArgsList)}\n"
        for arg in exp[1].ArgsList:
            output_str += print_exp(arg)
        return output_str
    elif isinstance(exp[1], While):
      output_str += "while\n"
      output_str += print_exp(exp[1].Predicate)
      output_str += print_exp(exp[1].Body)
      return output_str
    elif isinstance(exp[1], TrueConstant):
      output_str += "true\n"
      return output_str
    elif isinstance(exp[1], FalseConstant):
      output_str += "false\n"
      return output_str
    elif isinstance(exp[1], CaseBranch ):
        output_str += f"{exp.ekind.Identifier.loc}\n"
        output_str += f"{exp.ekind.Type.str}\n"
        output_str += print_exp(exp.ekind.Expression)
    # else:
    #     print(f"UNKOWN TYPE printing {exp} \n")

    return output_str

  
  def print_attributes_str(class_obj, all_classes, ast):
    output_string = ""
    # Add inherited attributes first
    if class_obj.Inherits:
        parent_class = next((c for c in ast if c.Name.str == class_obj.Inherits.str), None)
        if parent_class:
            output_string += print_attributes_str(parent_class, all_classes, ast)

    for feature in class_obj.Features:
        if isinstance(feature, Attribute):
            global attr_count
            attr_count += 1
            if feature.Initializer:
                output_string += f"initializer\n{feature.Name.str}\n{feature.Type.str}\n{print_exp(feature.Initializer)}"
            else:
                output_string += f"no_initializer\n{feature.Name.str}\n{feature.Type.str}\n"
    return output_string
  
  
  def build_imp_map():
    im = {}
    im['IO'] = IMObject("IO", 7, ["abort\n0\nObject\n0\nObject\ninternal\nObject.abort\ncopy\n0\nObject\n0\nSELF_TYPE\ninternal\nObject.copy\ntype_name\n0\nObject\n0\nString\ninternal\nObject.type_name\nin_int\n0\nIO\n0\nInt\ninternal\nIO.in_int\nin_string\n0\nIO\n0\nString\ninternal\nIO.in_string\nout_int\n1\nx\nIO\n0\nSELF_TYPE\ninternal\nIO.out_int\nout_string\n1\nx\nIO\n0\nSELF_TYPE\ninternal\nIO.out_string\n"])
    im['Object'] = IMObject("Object", 3,  ["abort\n0\nObject\n0\nObject\ninternal\nObject.abort\ncopy\n0\nObject\n0\nSELF_TYPE\ninternal\nObject.copy\ntype_name\n0\nObject\n0\nString\ninternal\nObject.type_name\n"])

    im['Bool'] = IMObject("Bool", 3, ["abort\n0\nObject\n0\nObject\ninternal\nObject.abort\ncopy\n0\nObject\n0\nSELF_TYPE\ninternal\nObject.copy\ntype_name\n0\nObject\n0\nString\ninternal\nObject.type_name\n"])
    im['String'] = IMObject("String", 6, ["abort\n0\nObject\n0\nObject\ninternal\nObject.abort\ncopy\n0\nObject\n0\nSELF_TYPE\ninternal\nObject.copy\ntype_name\n0\nObject\n0\nString\ninternal\nObject.type_name\nconcat\n1\ns\nString\n0\nString\ninternal\nString.concat\nlength\n0\nString\n0\nInt\ninternal\nString.length\nsubstr\n2\ni\nl\nString\n0\nString\ninternal\nString.substr\n"])
    im['Int'] = IMObject("Int", 3, ["abort\n0\nObject\n0\nObject\ninternal\nObject.abort\ncopy\n0\nObject\n0\nSELF_TYPE\ninternal\nObject.copy\ntype_name\n0\nObject\n0\nString\ninternal\nObject.type_name\n"])
    #im [class name] = [number of methods (including inheritance)][methods in order of printing(order is inheritance first, ordered by appearance)]
    for c in user_classes:
      count = 0
      methods=[]
      new_methods = []
      new_features = []
      overwritten_methods = []
      if(c not in ['Object', 'IO']):
        output_string = ""
        for cl in ast:
            if cl.Name.str == c:
                class_obj = cl
                break
        # print(class_obj.Name.str)
        
        for feature in class_obj.Features:
            # print(cl.Name.str + feature.Name.str)
            if isinstance(feature, Method):
                output_string = ""
                count += 1
                new_features.append(feature.Name.str)
                output_string += feature.Name.str + "\n"
                output_string += print_formals(feature.Formals)
                output_string += class_obj.Name.str + "\n"
                output_string += print_exp(feature.Body)
                new_methods.append(output_string)
        if class_obj.Inherits:
            methods = []
            for method in im[class_obj.Inherits.str].Methods:
                overwritten =False
                for i in range(len(new_features)):
                    #something to fix... 
                    if method.startswith(new_features[i]+"\n"):
                        # print(f"{new_features[i]} is being overwritten by {method[:10]}")
                        overwritten = True
                        overwritten_methods.append(new_methods[i])
                        new_methods.pop(i)
                        count -=1
                if not overwritten:
                    methods.append(method)
            count += im[class_obj.Inherits.str].NumMethods
            methods += overwritten_methods.copy()
            methods += new_methods.copy()
        else:
            methods = []
            count += im["Object"].NumMethods
            methods = im["Object"].Methods.copy()
            methods += new_methods
        im[class_obj.Name.str] = IMObject(class_obj.Name.str, count, methods)
        # print(f"\n\n\nCLASS NAME{class_obj.Name}")
        # print(methods)
    return im
  
  def print_formals(formals):
    formals_str = ""
    formals_str += str(len(formals)) + "\n"  # Output number of formals
    for formal in formals:
        formals_str += formal[0].str + "\n"  # Output formal parameter name
    return formals_str
  
  def print_implementation_map():
      im = build_imp_map()
      implementation_map = "implementation_map\n"
      implementation_map += str(len(sorted_classes)) + "\n"
      for cls in sorted_classes:
          implementation_map += im[cls].Name + "\n" + str(im[cls].NumMethods) + "\n"
          for method in im[cls].Methods:
              implementation_map += method

          # class_obj = None
          # for c in ast:
          #     if c.Name.str == cls:
          #         class_obj = c
          #         break
          
          # if class_obj is None:
          #     implementation_map += ""  # Skip processing this class
          # else:
          #     # implementation_map += print_attributes_str(class_obj, all_classes, ast)
              
      return implementation_map 

# Class Map
  class_map = "class_map\n"
  # sort classes alphabetically
  sorted_classes = sorted(all_classes)
#   print(sorted_classes)
  class_map += str(len(sorted_classes)) + "\n"
  for cls in sorted_classes:
      global attr_count
      attr_count = 0
      class_map += cls + "\n"
      class_obj = None
      for c in ast:
          if c.Name.str == cls:
              class_obj = c
              break
      # If class_obj is not found, skip this class
      if class_obj is None:
        #   print(f"Class {cls} not found in the AST")
          class_map += "0\n"
          continue
      s = print_attributes_str(class_obj, all_classes, ast)
      class_map += str(attr_count) + "\n" + s
  
  implementation_map = print_implementation_map()

  # ANNOTATED AST CODE STARTS HERE

  global astString
  astString = ""

  # This function prints an identifier (name and location, each on seperate lines)
  def print_identifier(id):
      global astString
      astString += (id.loc + "\n" + id.str + "\n")
      

  # This function prints an expression, checking each expression type using different print methods when needed
  def print_expression(exp):
        global astString
        # check by ekind
        astString += exp[0] + "\n"
        # output the type of the expression by calling the type_check_exp function
        if exp.Type:
            astString += exp.Type + "\n"
        else :
            astString += "NONE VERY BAD\n"

        if isinstance(exp[1], Integer):
            astString += ("integer\n")
            astString += (exp[1].Integer + "\n")
        elif isinstance(exp[1], Plus):
            astString += ("plus\n")
            print_expression(exp[1].Left)
            print_expression(exp[1].Right)
        elif isinstance(exp[1], Minus):
            astString += ("minus\n")
            print_expression(exp[1].Left)
            print_expression(exp[1].Right)
        elif isinstance(exp[1], Times):
            astString += ("times\n")
            print_expression(exp[1].Left)
            print_expression(exp[1].Right)
        elif isinstance(exp[1], Divide):
            astString += ("divide\n")
            print_expression(exp[1].Left)
            print_expression(exp[1].Right)
        elif isinstance(exp[1], LessThan):
            astString += ("lt\n")
            print_expression(exp[1].Left)
            print_expression(exp[1].Right)
        elif isinstance(exp[1], LessThanOrEqual):
            astString += ("le\n")
            print_expression(exp[1].Left)
            print_expression(exp[1].Right)
        elif isinstance(exp[1], Equal):
            astString += ("eq\n")
            print_expression(exp[1].Left)
            print_expression(exp[1].Right)
        elif isinstance(exp[1], Not):
            astString += ("not\n")
            print_expression(exp[1].Expression)
        elif isinstance(exp[1], Negate):
            astString += ("negate\n")
            print_expression(exp[1].Expression)
        elif isinstance(exp[1], If):
            astString += ("if\n")
            print_expression(exp[1].Predicate)
            print_expression(exp[1].Then)
            print_expression(exp[1].Else)
        elif isinstance(exp[1], Block):
            astString += ("block\n")
            astString += (str(len(exp[1].Expressions)) + "\n")
            for expression in exp[1].Expressions:
                print_expression(expression)
        elif isinstance(exp[1], New):
            astString += ("new\n")
            print_identifier(exp[1].Identifier)
        elif isinstance(exp[1], IsVoid):
            astString += ("isvoid\n")
            print_expression(exp[1].Expression)
        elif isinstance(exp[1], ID):
            astString += ("identifier\n")
            print_identifier(exp[1])
        elif isinstance(exp[1], Let):
            astString += ("let\n")
            print_list(exp[1].Bindings, print_binding)
            print_expression(exp[1].Expression)
        elif isinstance(exp[1], Case):
            astString += ("case\n")
            print_expression(exp[1].Expression)
            astString += (str(len(exp[1].Elements)) + "\n")
            for element in exp[1].Elements:
                print_expression(element)
        elif isinstance(exp[1], Tilde):
            astString += ("tilde\n")
            print_expression(exp[1].Expression)
        elif isinstance(exp[1], StringConstant):
            astString += ("string\n")
            astString += (exp[1].Value + "\n")
        elif isinstance(exp[1], Assign):
            astString += ("assign\n")
            print_identifier(exp[1].Identifier)
            print_expression(exp[1].Expression)
        elif isinstance(exp[1], DynamicDispatch):
            astString += ("dynamic_dispatch\n")
            print_expression(exp[1].Object)
            print_identifier(exp[1].MethodName)
            astString += (str(len(exp[1].ArgsList)) + "\n")
            for arg in exp[1].ArgsList:
                print_expression(arg)
        elif isinstance(exp[1], StaticDispatch):
            astString += ("static_dispatch\n")
            print_expression(exp[1].Object)
            print_identifier(exp[1].Type)
            print_identifier(exp[1].MethodName)
            astString += (str(len(exp[1].ArgsList)) + "\n")
            for arg in exp[1].ArgsList:
                print_expression(arg)
        elif isinstance(exp[1], SelfDispatch):
            astString += ("self_dispatch\n")
            print_identifier(exp[1].MethodName)
            astString += (str(len(exp[1].ArgsList)) + "\n")
            for arg in exp[1].ArgsList:
                print_expression(arg)
        elif isinstance(exp[1], While):
            astString += ("while\n")
            print_expression(exp[1].Predicate)
            print_expression(exp[1].Body)
        elif isinstance(exp[1], TrueConstant):
            astString += ("true\n")
        elif isinstance(exp[1], FalseConstant):
            astString += ("false\n")
        elif isinstance(exp[1], CaseBranch):
            astString += f"{exp.ekind.Identifier.loc}\n"
            astString += f"{exp.ekind.Type.str}\n"
            print_expression(exp.ekind.Expression)
        # else:
        #     print(f"UNKOWN TYPE AST STUFF printing {exp} \n")

       

  # This function prints a binding either let_binding_no_init or let_binding_init
  def print_binding(attr):
      global astString
      # if let_binding_no_init
      if attr.Initializer == None:
          astString += ("let_binding_no_init\n")
          print_identifier(attr.Name)
          print_identifier(attr.Type)
      else:
          astString += ("let_binding_init\n")
          print_identifier(attr.Name)
          print_identifier(attr.Type)
          print_expression(attr.Initializer)


  # This function prints a feature, checking each feature type (attribute_no_init, attribute_init, method)
  def print_feature(feature):
      global astString
      if isinstance(feature, Attribute):
            if feature.Initializer:
                astString += ("attribute_init\n")
                print_identifier(feature.Name)
                print_identifier(feature.Type)
                print_expression(feature.Initializer)
            else:
                astString += ("attribute_no_init\n")
                print_identifier(feature.Name)
                print_identifier(feature.Type)
      elif isinstance(feature, Method):
            astString += ("method\n")
            print_identifier(feature.Name)
            # astString += (str(len(feature.Formals)) + "\n")
            # use print list
            print_list(feature.Formals, print_formal)
            astString += feature.Name.loc + "\n" + feature.ReturnType.str +"\n"
            print_expression(feature.Body)
    #   else:

            # print(f"UNKOWN TYPE AST STUFF printing {ast} \n")

  # This function prints a formal
  def print_formal(formal):
        global astString
        # print(formal)
        print_identifier(formal[0])
        astString += formal[0].loc + "\n"
        astString += formal[1]+"\n"


  # This function prints a list of elements starting with the number of elements
  def print_list(ast, print_element_function):
      global astString
      count = 0
      for cls in ast:
          if cls.Name.str != "Object" and cls.Name.str != "IO":
              count += 1
    
      astString += (str(count) + "\n")
    
      for element in ast:
            print_element_function(element)

  # This function prints a class whether it inherits or not
  def print_classes(ast):
      global astString
      count = 0
      for cls in ast:
          if cls.Name.str != "Object" and cls.Name.str != "IO":
              count += 1
      astString += (str(count) + "\n")
    #   for cl in user_classes:
    #       for cls in ast:
    #           if cls.Name.str == cl:
    #             #   print("CLASS NAME: " + cls.Name.str)
    #             if cls.Name.str != "IO" and cls.Name.str != "Object":
    #                 if cls.Inherits:
    #                         print_identifier(cls.Name)
    #                         astString += ("inherits\n")
    #                         print_identifier(cls.Inherits)
    #                         print_list(cls.Features, print_feature)
    #                 else:
    #                         print_identifier(cls.Name)
    #                         print_list(cls.Features, print_feature)
      for cls in ast:
              
        if cls.Name.str != "IO" and cls.Name.str != "Object":
            if cls.Inherits:
                    print_identifier(cls.Name)
                    astString += ("inherits\n")
                    print_identifier(cls.Inherits)
                    print_list(cls.Features, print_feature)
            else:
                    print_identifier(cls.Name)
                    astString += ("no_inherits\n")
                    print_list(cls.Features, print_feature)


  def print_program(ast):
      print_classes(ast)
    
  print_program(ast)

  # ANNOTATED AST CODE ENDS HERE
   
  with open(fname[:-4] + "-type", "w") as output_file:
      output_file.write(class_map + implementation_map + parentMapString + astString)

main() 

