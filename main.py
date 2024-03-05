# Python starter code that covers the majority of the aspects of the
# PA4c OCaml video. 
import sys
from collections import namedtuple

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

  # An expression starts with its location (line number) 
  # and then has a "kind" (like Plus or While). 
  def read_exp ():
    eloc = read () 
    ekind = read_ekind ()
    return (eloc, ekind) 

  def read_ekind ():
    ekind = read () 
    if ekind == "integer":
      ival = read ()
      return Integer(ival)
    elif ekind == "plus": 
      left = read_exp ()
      right = read_exp ()
      return Plus(left, right) 
    elif ekind == "while":
      predicate = read_exp ()
      body = read_exp () 
      return While(predicate, body) 
    else: 
      raise (Exception(f"read_ekind: {ekind} unhandled"))

  ast = read_cool_program () 

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
  
  user_classes = topological_sort(ast)

  base_classes = [ "Int", "String", "Bool", "IO", "Object" ]
  base_classes.reverse()  # Reverse the base classes to get the correct order
  all_classes = (base_classes + user_classes)

  print(f"DEBUG: all_classes = {all_classes}") 
  print(f"DEBUG: ast = {ast}") 

  # Look for inheritance from Int, String and undeclared classes 
  for c in ast:
    if c.Inherits != None: 
      i = c.Inherits 
      if i.str in ["Int", "String"]:
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
        print("ERROR: CLASS Inheritance contains cycle: ") 
        exit(1) 
  
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
                if feature.Formals != parent_feature.Formals:
                  print(f"ERROR: {feature.Name.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} redefines parent method with different parameters")
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
      print("ERROR: Type-Check: Class Main is missing")
      exit(1)

    main_method = False
    for feature in main_class.Features:
      if isinstance(feature, Method) and feature.Name.str == "main":
        main_method = True
        break

    if not main_method:
      print("ERROR: Type-Check: Class Main is missing method 'main'")
      exit(1)

  check_main_method()

  # Check for self and SELF_TYPE mistakes in classes and methods.
  def check_self_and_self_type():
    for cls in ast:
      for feature in cls.Features:
        if isinstance(feature, Method):
          if any(formal[0].str == "self" for formal in feature.Formals):
            print(f"ERROR: {cls.Name.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} redefines 'self' parameter")
            exit(1)

          if "SELF_TYPE" in [formal[1].str for formal in feature.Formals]:
            print(f"ERROR: {cls.Name.loc}: Type-Check: Method {feature.Name.str} in class {cls.Name.str} uses 'SELF_TYPE' parameter")
            exit(1)

  check_self_and_self_type()

  def int_initializer_str(initializer):
    output_str = ""
    # take the first element of the initializer tuple
    output_str += f"{initializer[0]}\n"
    output_str += "Int\ninteger\n"
    output_str += f"{initializer[1].Integer}"
    return output_str

# Class Map
  class_map = "class_map\n"
  # sort classes alphabetically
  sorted_classes = sorted(all_classes)
  class_map += str(len(sorted_classes)) + "\n"
  print(f"DEBUG: class_map = {class_map}")
  print(f"DEBUG: sorted_classes = {sorted_classes}")
  for cls in sorted_classes:
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
      attributes = [f"{attr.Name.str}\n{attr.Type.str}\n{int_initializer_str(attr.Initializer)}" for attr in class_obj.Features if isinstance(attr, Attribute)]
      class_map += str(len(attributes)) + "\n"
      for attr in attributes:
          class_map += "initializer\n" + attr + "\n"
  
  print (f"DEBUG: class_map = {class_map}")
  # Implementation Map
  implementation_map = "implementation_map\n"
  implementation_map += str(len(sorted_classes)) + "\n"
  for cls in sorted_classes:
      class_obj = None
      for c in ast:
          if c.Name.str == cls:
              class_obj = c
              break

      if class_obj is None:
          print(f"Class {cls} not found in the AST")
          continue  # Skip processing this class
      implementation_map += cls + "\n"
      methods = [method for method in class_obj.Features if isinstance(method, Method)]
      implementation_map += str(len(methods)) + "\n"
      for method in methods:
          implementation_map += method.Name.str + "\n"
          implementation_map += str(len(method.Formals)) + "\n"
          for formal in method.Formals:
              implementation_map += formal[0].str + "\n"
          if cls != class_obj.Inherits.str:
              implementation_map += cls + "\n"
          else:
              implementation_map += "\n"  # No parent class if not overridden
          implementation_map += method.Body[0] + "\n"  # Line number
          implementation_map += method.ReturnType.str + "\n"  # Type

  # Parent Map
  parent_map = "parent_map\n"
  parent_map += str(len(sorted_classes) - 1) + "\n"
  for cls in sorted_classes:
      class_obj = next(c for c in ast if c.Name.str == cls)
      if class_obj.Inherits:
          parent_map += cls + "\n" + class_obj.Inherits.str + "\n"

  # Annotated AST
  annotated_ast = ""
  for cls in ast:
      annotated_ast += f"{cls.Name.loc}\n{cls.Name.str}\n"
      for feature in cls.Features:
          if isinstance(feature, Method):
              annotated_ast += f"{feature.Name.loc}\n{feature.Name.str}\n"
              annotated_ast += f"{len(feature.Formals)}\n"
              for formal in feature.Formals:
                  annotated_ast += f"{formal[0].loc}\n{formal[0].str}\n"
              annotated_ast += f"{cls.Name.str}\n" if cls.Name.str != cls.Inherits.str else "\n"
              annotated_ast += f"{feature.Body[0]}\n{feature.ReturnType.str}\n"
          elif isinstance(feature, Attribute):
              annotated_ast += f"{feature.Name.loc}\n{feature.Name.str}\n{feature.Type.str}\n"
              if feature.Initializer:
                  annotated_ast += f"{feature.Initializer[0]}\n{feature.Initializer[1].str}\n"
      annotated_ast += "\n"  # Empty line to separate class definitions

  # Write to .cl-type file
  with open(fname[:-4] + ".cl-type", "w") as output_file:
      output_file.write(class_map + implementation_map + parent_map + annotated_ast)


main() 

