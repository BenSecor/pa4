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

# A Cool Identifier is a tuple (pair) of a location and a string.
ID               = namedtuple("ID",        "loc str") 

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
Identifier = namedtuple("Identifier", "ID")
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

  def read_cool_class ():
    cname = read_id () 
    i = read()
    if i == "no_inherits": 
      inherits = None 
    elif i == "inherits": 
      inherits = read_id()
    else:
      raise(Exception(f"read_cool_class: inherits {i}"))
    features = read_list(read_feature)
    return CoolClass(cname, inherits, features) 

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
      mname = read_id ()
      formals = read_list(read_formal)
      mtype = read_id ()
      mbody = read_exp ()
      return Method(mname, formals, mtype, mbody)
    else:
      raise(Exception(f"read_feature {a}"))

  def read_formal ():
    fname = read_id()
    ftype = read_id()
    return (fname, ftype) 

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
    else:
        raise Exception(f"Unknown binding kind: {b}")
  # An expression starts with its location (line number) 
  # and then has a "kind" (like Plus or While). 
  def read_exp ():
    eloc = read() 
    ekind = read_ekind ()
    return (eloc, ekind) 

  def read_case_exp():
    var = read_id()
    type_id = read_id()
    body_exp = read_exp()
    return ( var, type_id, body_exp)
  
  
  def read_assign():
    identifier = read_id()
    expression = read_exp()
    return Assign(identifier, expression)

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
        return Identifier(identifier)
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

  ast = read_cool_program () 
  # Create namedtuple objects for base classes

  IO = CoolClass(ID("0", "IO"), ID("0", "Object"), [
      Method(ID("0", "in_int"), [], ID("0", "Int"), []),
      Method(ID("0", "in_string"), [], ID("0", "String"), []),
      Method(ID("0", "out_int"), [(ID("0", "x"),ID("0", "Int"))], ID("0", "SELF_TYPE"), []),
      Method(ID("0", "out_string"), [(ID("0", "x"),ID("0", "String"))], ID("0", "SELF_TYPE"), [])
  ])

  Object = CoolClass(ID("0", "Object"), None, [
      Method(ID("0", "abort"), [], ID("0", "Object"), []),
      Method(ID("0", "copy"), [], ID("0", "SELF_TYPE"), []),
      Method(ID("0", "type_name"), [], ID("0", "String"), [])
  ])
  # Add base classes to ast
  ast.extend([IO, Object])

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
  
  base_classes = [ "Int", "String", "Bool"]
  all_classes = ([c.Name.str for c in ast ] + base_classes)
  #print(f"DEBUG: all_classes = {all_classes}") 
  # print(f"DEBUG: ast = {ast}") 

  # Look for inheritance from Int, String and undeclared classes 
  for c in ast:
    if c.Inherits != None: 
      i = c.Inherits 
      if i.str in [ "Int", "String", "Bool" ]:
        print(f"ERROR: {i.loc}: Type-Check: inheriting from forbidden class {i.str}")
        exit(1)
      elif not i[1] in all_classes:
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
  
  user_classes = topological_sort(ast)
  base_classes.reverse()  # Reverse the base classes to get the correct order
  all_classes = (base_classes + user_classes)

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
        if feature.Name.str in methods:
          print(f"ERROR: {feature.Name.loc}: Type-Check: Method {feature.Name.str} is redefined in class {cls.Name.str}")
          exit(1)
        else:
          methods.add(feature.Name.str)

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
              if isinstance(parent_feature, Method) and feature.Name.str == parent_feature.Name.str:
                if feature.Name.str == parent_feature.Name.str:
                  if len(feature.Formals) != len(parent_feature.Formals):
                      print(f"ERROR: {feature.Name.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} redefines parent method with different number of parameters")
                      exit(1)
                  else:
                      for formal_child, formal_parent in zip(feature.Formals, parent_feature.Formals):
                          if formal_child[1].str != formal_parent[1].str:
                              print(f"ERROR: {feature.Name.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} redefines parent method with different parameter types")
                              exit(1)
                # if feature.Formals != parent_feature.Formals:
                #                   print(str(feature.Formals) + "\n" + str(parent_feature.Formals) + "\n")
                #                   print(f"ERROR: {feature.Name.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} redefines parent method with different parameters")
                #                   exit(1)

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
        if feature.Name.str == "main":
            main_method_found = True
            break

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
                    if formal[1].str == "SELF_TYPE":
                        print(f"ERROR: {formal[1].loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} has a formal parameter of type 'SELF_TYPE'")
                        exit(1)

                # if feature.ReturnType.str == "SELF_TYPE":
                #     # Allow SELF_TYPE as a return type
                #     if not any(isinstance(expr, ID) and expr.str == "self" for expr in feature.Body):
                #         print(f"ERROR: {feature.ReturnType.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} has return type 'SELF_TYPE' but does not refer to 'self' in its body")
                #         exit(1)

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
                          if isinstance(parent_feature, Method) and feature.Name.str == parent_feature.Name.str:
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
                      if formal[1].str not in all_classes+["SELF_TYPE"]:
                          print(f"ERROR: {formal[1].loc}: Type-Check: Undefined type {formal[1].str} for parameter {formal[0].str} in method {feature.Name.str} of class {cls.Name.str}")
                          exit(1)

  #Add checks for negative test cases
  check_self_and_self_type()
  check_duplicate_attributes()
  check_inherit_duplicate_attributes()
  check_method_parameter_redefinition()
  check_undefined_attribute_types()
  check_undefined_parameter_types()


  def print_exp(exp):
    output_str = ""
    if isinstance(exp[1], Integer):
        output_str += f"{exp[0]}\n"
        output_str += "integer\n"
        output_str += f"{exp[1].Integer}\n"
    elif isinstance(exp[1], Plus):
        output_str += f"{exp[0]}\n"
        output_str += "plus\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
    elif isinstance(exp[1], Minus):
        output_str += f"{exp[0]}\n"
        output_str += "minus\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
    elif isinstance(exp[1], Times):
        output_str += f"{exp[0]}\n"
        output_str += "times\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
    elif isinstance(exp[1], Divide):
        output_str += f"{exp[0]}\n"
        output_str += "divide\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
    elif isinstance(exp[1], LessThan):
        output_str += f"{exp[0]}\n"
        output_str += "lt\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
    elif isinstance(exp[1], LessThanOrEqual):
        output_str += f"{exp[0]}\n"
        output_str += "le\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
    elif isinstance(exp[1], Equal):
        output_str += f"{exp[0]}\n"
        output_str += "eq\n"
        output_str += print_exp(exp[1].Left)
        output_str += print_exp(exp[1].Right)
    elif isinstance(exp[1], Not):
        output_str += f"{exp[0]}\n"
        output_str += "not\n"
        output_str += print_exp(exp[1].Expression)
    elif isinstance(exp[1], Negate):
        output_str += f"{exp[0]}\n"
        output_str += "negate\n"
        output_str += print_exp(exp[1].Expression)
    elif isinstance(exp[1], If):
        output_str += f"{exp[0]}\n"
        output_str += "if\n"
        output_str += print_exp(exp[1].Predicate)
        output_str += print_exp(exp[1].Then)
        output_str += print_exp(exp[1].Else)
    elif isinstance(exp[1], Block):
        output_str += f"{exp[0]}\n"
        output_str += "block\n"
        output_str += f"{len(exp[1].Expressions)}\n"
        for expression in exp[1].Expressions:
            output_str += print_exp(expression)
    elif isinstance(exp[1], New):
        output_str += f"{exp[0]}\n"
        output_str += "new\n"
        output_str += f"{exp[1].Identifier.loc}\n{exp[1].Identifier.str}\n"
    elif isinstance(exp[1], IsVoid):
        output_str += f"{exp[0]}\n"
        output_str += "isvoid\n"
        output_str += print_exp(exp[1].Expression)
    elif isinstance(exp[1], Identifier):
        output_str += f"{exp[0]}\n"
        output_str += "identifier\n"
        output_str += f"{exp[1].ID.loc}\n{exp[1].ID.str}\n"
    elif isinstance(exp[1], Let):
        output_str += f"{exp[0]}\n"
        output_str += "let\n"
        # output_str += f"{len(exp[1].Bindings)}\n"
        for binding in exp[1].Bindings:
            output_str += print_binding(binding)
        output_str += print_exp(exp[1].Expression)
    elif isinstance(exp[1], Case):
        output_str += f"{exp[0]}\n"
        output_str += "case\n"
        output_str += print_exp(exp[1].Expression)
        output_str += f"{len(exp[1].Elements)}\n"
        for element in exp[1].Elements:
             output_str += f"{element[0].loc}\n{element[0].str}\n"
             output_str += f"{element[1].loc}\n{element[1].str}\n"
             output_str += print_exp(element[2])
    elif isinstance(exp[1], Tilde):
        output_str += f"{exp[0]}\n"
        output_str += "tilde\n"
        output_str += print_exp(exp[1].Expression)
    elif isinstance(exp[1], StringConstant):
        output_str += f"{exp[0]}\n"
        output_str += "string\n"
        output_str += f"{exp[1].Value}\n"
    elif isinstance(exp[1], Assign):
        output_str += f"{exp[0]}\n"
        output_str += "assign\n"
        output_str += f"{exp[1].Identifier.loc}\n{exp[1].Identifier.str}\n"
        output_str += print_exp(exp[1].Expression)
    elif isinstance(exp[1], DynamicDispatch):
        output_str += f"{exp[0]}\n"
        output_str += "dynamic_dispatch\n"
        output_str += print_exp(exp[1].Object)
        output_str += f"{exp[1].MethodName.loc}\n{exp[1].MethodName.str}\n"
        output_str += f"{len(exp[1].ArgsList)}\n"
        for arg in exp[1].ArgsList:
            output_str += print_exp(arg)
    elif isinstance(exp[1], StaticDispatch):
        output_str += f"{exp[0]}\n"
        output_str += "static_dispatch\n"
        output_str += print_exp(exp[1].Object)
        output_str += f"{exp[1].Type.loc}\n{exp[1].Type.str}\n"
        output_str += f"{exp[1].MethodName.loc}\n{exp[1].MethodName.str}\n"
        output_str += f"{len(exp[1].ArgsList)}\n"
        for arg in exp[1].ArgsList:
            output_str += print_exp(arg)
    elif isinstance(exp[1], SelfDispatch):
        output_str += f"{exp[0]}\n"
        output_str += "self_dispatch\n"
        output_str += f"{exp[1].MethodName.loc}\n{exp[1].MethodName.str}\n"
        output_str += f"{len(exp[1].ArgsList)}\n"
        for arg in exp[1].ArgsList:
            output_str += print_exp(arg)
    elif isinstance(exp[1], While):
      output_str += f"{exp[0]}\n"
      output_str += "while\n"
      output_str += print_exp(exp[1].Predicate)
      output_str += print_exp(exp[1].Body)
      return output_str
    elif isinstance(exp[1], TrueConstant):
      output_str += f"{exp[0]}\n"
      output_str += "true\n"
      return output_str
    elif isinstance(exp[1], FalseConstant):
      output_str += f"{exp[0]}\n"
      output_str += "false\n"
      return output_str
    else:
        raise ValueError(exp)

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

  # def print_formals(formals):
  #     output_string = ""
  #     formal_count = 0 
  #     for formal in formals:
  #        formal_count+=1
  #        output_string += formal + "\n"
  #     return str(formal_count) + "\n"+ output_string

  # def print_method_str(class_obj):
  #     output_string = ""
  #     method_count = 0 
  #     for method in class_obj.Features:
  #       if isinstance(method, Method):
  #           method_count+=1
  #           output_string += method.Name.str + "\n"
  #           output_string += print_formals(method.Formals)

  #           #if method was inherited (not overidden):
  #           #   output_string += f"initializer\n{attr.Name.str}\n{attr.Type.str}\n{print_int_initializer_str(attr.Initializer)}"
  #           #else:
  #           output_string += class_obj.Name.str + "\n"
  #           output_string += print_exp(method.Body) + "\n"
  #     return str(method_count)+"\n"+ output_string
  
  # def print_default_class_method(cls):
  #   if cls == "Bool":
  #       return "3\nabort\n0\nObject\n0\nObject\ninternal\nObject.abort\ncopy\n0\nObject\n0\nSELF_TYPE\ninternal\nObject.copy\ntype_name\n0\nObject\n0\nString\ninternal\nObject.type_name\n"
  #   elif cls == "Int":
  #       return "3\nabort\n0\nObject\n0\nObject\ninternal\nObject.abort\ncopy\n0\nObject\n0\nSELF_TYPE\ninternal\nObject.copy\ntype_name\n0\nObject\n0\nString\ninternal\nObject.type_name\n"
  #   elif cls == "String":
  #       return "6\nabort\n0\nObject\n0\nObject\ninternal\nObject.abort\ncopy\n0\nObject\n0\nSELF_TYPE\ninternal\nObject.copy\ntype_name\n0\nObject\n0\nString\ninternal\nObject.type_name\nconcat\n1\ns\nString\n0\nString\ninternal\nString.concat\nlength\n0\nString\n0\nInt\ninternal\nString.length\nsubstr\n2\ni\nl\nString\n0\nString\ninternal\nString.substr\n"
  #   elif cls == "IO":
  #       return "4\nabort\n0\nObject\n0\nObject\ninternal\nObject.abort\ncopy\n0\nObject\n0\nSELF_TYPE\ninternal\nObject.copy\ntype_name\n0\nObject\n0\nString\ninternal\nObject.type_name\n"
  #   elif cls == "Object":
  #       return "3\nabort\n0\nObject\n0\nObject\ninternal\nObject.abort\ncopy\n0\nObject\n0\nSELF_TYPE\ninternal\nObject.copy\ntype_name\n0\nObject\n0\nString\ninternal\nObject.type_name\n"
  #   else:
  #       return ""
    
# Class Map
  class_map = "class_map\n"
  # sort classes alphabetically
  sorted_classes = sorted(all_classes)
  class_map += str(len(sorted_classes)) + "\n"
  # print(f"DEBUG: class_map = {class_map}")
  # print(f"DEBUG: sorted_classes = {sorted_classes}")
  for cls in sorted_classes:
      global attr_count
      attr_count = 0
      class_map += cls + "\n"
      # print (f"DEBUG: cls = {cls}")
      # print (f"DEBUG: ast = {ast}")
      # Find the class object in ast
      class_obj = None
      for c in ast:
          if c.Name.str == cls:
              class_obj = c
              break
      # If class_obj is not found, skip this class
      if class_obj is None:
          # print(f"Class {cls} not found in the AST")
          class_map += "0\n"
          continue
      s = print_attributes_str(class_obj, all_classes, ast)
      class_map += str(attr_count) + "\n" + s
      

  # print (f"DEBUG: class_map = {class_map}")
  # # Implementation Map
  # implementation_map = "implementation_map\n"
  # implementation_map += str(len(sorted_classes)) + "\n"
  # for cls in sorted_classes:
  #     class_obj = None
  #     for c in ast:
  #         if c.Name.str == cls:
  #             class_obj = c
  #             break
      
  #     if class_obj is None:
  #         implementation_map += print_default_class_method(cls)  # Skip processing this class
  #     else:
  #         implementation_map += cls + "\n"
  #         implementation_map += print_method_str(class_obj)
        
  # print (f"DEBUG: imp_map = {implementation_map}")
  # # Parent Map
  # parent_map = "parent_map\n"
  # parent_map += str(len(sorted_classes) - 1) + "\n"
  # for cls in sorted_classes:
  #     class_obj = next(c for c in ast if c.Name.str == cls)
  #     if class_obj.Inherits:
  #         parent_map += cls + "\n" + class_obj.Inherits.str + "\n"

  # # Annotated AST
  # annotated_ast = ""
  # for cls in ast:
  #     annotated_ast += f"{cls.Name.loc}\n{cls.Name.str}\n"
  #     for feature in cls.Features:
  #         if isinstance(feature, Method):
  #             annotated_ast += f"{feature.Name.loc}\n{feature.Name.str}\n"
  #             annotated_ast += f"{len(feature.Formals)}\n"
  #             for formal in feature.Formals:
  #                 annotated_ast += f"{formal[0].loc}\n{formal[0].str}\n"
  #             annotated_ast += f"{cls.Name.str}\n" if cls.Name.str != cls.Inherits.str else "\n"
  #             annotated_ast += f"{feature.Body[0]}\n{feature.ReturnType.str}\n"
  #         elif isinstance(feature, Attribute):
  #             annotated_ast += f"{feature.Name.loc}\n{feature.Name.str}\n{feature.Type.str}\n"
  #             if feature.Initializer:
  #                 annotated_ast += f"{feature.Initializer[0]}\n{feature.Initializer[1].str}\n"
  #     annotated_ast += "\n"  # Empty line to separate class definitions

  # Write to .cl-type file
  with open(fname[:-4] + "-type", "w") as output_file:
      output_file.write(class_map ) #+ implementation_map + parent_map + annotated_ast)

main() 

