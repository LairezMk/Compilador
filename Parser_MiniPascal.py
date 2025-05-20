import ply.yacc as yacc
import ply.lex as lex
import Analizador_Lexico_mini_pascal_arreglado as lex_rules  # Importar todo el módulo léxico
from Analizador_Lexico_mini_pascal_arreglado import tokens
import sys

VERBOSE = True # Cambia a False para menos información en la salida
hay_error = False  # Variable global para saber si hubo errores
symbol_table = {}

# Precedencia para resolver conflictos shift/reduce en if-then-else
precedence = (
    ('nonassoc', 'ELSE'),  # ELSE tiene menor precedencia
)

current_params = []  # Lista para almacenar los parámetros actuales
param_stack = []  # Pila para almacenar los parámetros de los procedimientos

# Reglas de la gramática
def p_program(p):
    'program : PROGRAM ID SEMICOLON declaration_sections block DOT'
    p[0] = ('program', p[2], p[4], p[6])  # Guardar el nombre del programa y la tabla de símbolos
    global symbol_table
    symbol_table[p[2]] = ('program')  # Guardar el nombre del programa en la tabla de símbolos

def p_declaration_sections(p):
    '''declaration_sections : declaration_sections declaration_section
                            | empty'''
    p[0] = p[1]  # Guardar la tabla de símbolos


def p_declaration_section(p):
    '''declaration_section : uses_opt
                           | constant_declaration
                           | type_declaration
                           | var_declaration
                           | procedure_declarations
                           | function_declaration
                           | constructor_declaration
                           | method_declaration'''
    p[0] = p[1]  # Guardar la sección de declaración en la tabla de símbolos

def p_method_declaration(p):
    '''method_declaration : CONSTRUCTOR ID DOT ID LPAREN parameter_list RPAREN SEMICOLON block 
                          | PROCEDURE ID DOT ID LPAREN parameter_list RPAREN SEMICOLON block 
                          | FUNCTION ID DOT ID LPAREN parameter_list RPAREN COLON type_specifier SEMICOLON block
                          | DESTRUCTOR ID DOT ID SEMICOLON block'''
    p[0] = ('method', p[2], p[4], p[6], p[8])  # Guardar el nombre del método y la tabla de símbolos
    global symbol_table
    symbol_table[p[2]] = ('method', p[4])  # Guardar el nombre del método en la tabla de símbolos


def p_constructor_declaration(p):
    '''constructor_declaration : CONSTRUCTOR ID LPAREN field_list RPAREN SEMICOLON block
                               | CONSTRUCTOR ID LPAREN RPAREN SEMICOLON block'''
    p[0] = ('constructor', p[2], p[4], p[6])  # Guardar el nombre del constructor y la tabla de símbolos
    global symbol_table
    symbol_table[p[2]] = ('constructor')  # Guardar el nombre del constructor en la tabla de símbolos

def p_uses_opt(p):
    '''uses_opt : USES id_list SEMICOLON'''
   #             | empty'''
    p[0] = p[2]  # Guardar la lista de identificadores en la tabla de símbolos
    global symbol_table
    for id in p[2]:
        if id not in symbol_table:
            lineno = p.lineno(1)
            print(f"Error semántico en la linea {lineno}: El módulo '{id}' no fue declarado.")
            global hay_error
            hay_error = True
        else:
            symbol_table[id] = ('module')  # Guardar el nombre del módulo en la tabla de símbolos

def p_id_list_single(p):
    'id_list : ID'
    p[0] = [p[1]]

def p_id_list_multiple(p):
    'id_list : id_list COMMA ID'
    p[0] = p[1] + [p[3]]    

def p_var_declaration(p):
    'var_declaration : VAR declaration_list'
    p[0] = ('var_declaration', p[2])

def p_declaration_list(p):
    '''declaration_list : declaration
                        | declaration_list declaration'''
    p[0] = p[1]  # Guardar la lista de declaraciones en la tabla de símbolos

def p_declaration(p):
    'declaration : id_list COLON type_specifier SEMICOLON'
    global symbol_table
    var_names = p[1]
    var_type = p[3]
    
    for name in var_names:
        if name in symbol_table:
            lineno = p.lineno(1)
            print(f"Error semántico en la linea {lineno}: La variable '{name}' ya fue declarada.")
            global hay_error
            hay_error = True
        else:
            p[0] = ('var', name, var_type)
            symbol_table[name] = ('var', var_type)


# El bloque se compone de declaraciones (variables y/o procedimientos) seguidas de una sentencia compuesta.
def p_block(p):
    'block : declaration_sections compound_statement'
    p[0] = ('block', p[1], p[2])  # Guardar la tabla de símbolos y la sentencia compuesta

def p_type_declaration(p):
    'type_declaration : TYPE type_list'
    p[0] = ('type_declaration', p[2])  # Guardar la lista de tipos en la tabla de símbolos

def p_type_list(p):
    '''type_list : type_definition
                 | type_list type_definition'''
    p[0] = p[1]  # Guardar la lista de tipos en la tabla de símbolos


def p_type_definition(p):
    'type_definition : ID EQUAL type_specifier SEMICOLON'
    global symbol_table
    type_name = p[1]
    type_value = p[3]

    if type_name in symbol_table:
        lineno = p.lineno(1)
        print(f"Error semántico en la linea {lineno}: El tipo '{type_name}' ya fue declarado.")
        global hay_error
        hay_error = True
    else:
        symbol_table[type_name] = ('type', type_value)
        p[0] = ('type', type_name, type_value)

def p_type_specifier(p):
    '''type_specifier : ARRAY LBRACKET type_expression RBRACKET OF type_specifier
                      | subrange
                      | FILE OF type_specifier
                      | SET OF type_specifier
                      | RECORD field_list case_part END
                      | OBJECT field_list method_list END                              
                      | LPAREN id_list RPAREN
                      | BOOLEAN_LITERAL
                      | INTEGER
                      | BYTE
                      | CHAR
                      | STRING
                      | STRING LBRACKET NUMBER RBRACKET
                      | ID'''
    p[0] = p[1]

def p_type_expression(p):
    '''type_expression : type_expression COMMA subrange
                       | subrange'''
    if len(p) == 2:
        # Solo un subrango, por ejemplo: 1..10
        p[0] = [p[1]]
    else:
        # Lista de subrangos, por ejemplo: 1..10, 2..5
        p[0] = p[1] + [p[3]]

def p_subrange(p):
    '''subrange : NUMBER DOTDOT NUMBER
                | NUMBER DOTDOT ID'''
    # Ejemplo: 1..10 o 1..N
    p[0] = ('subrange', p[1], p[3])

#RECORD -----------------------------------------------
def p_field_list(p):
    '''field_list : field_list field
                  | field
                  | empty'''
    if len(p) == 2:
        # field o empty
        if p[1] == None:
            p[0] = []
        else:
            p[0] = [p[1]]
    else:
        # field_list field
        p[0] = p[1] + [p[2]]

def p_field(p):
    '''field : id_list COLON type_specifier
             | id_list COLON type_specifier SEMICOLON
             | VAR id_list COLON type_specifier
             | VAR id_list COLON type_specifier SEMICOLON
             | id_list LPAREN STRING_LITERAL RPAREN
             | statement
             | if_statement
             | case_statement
             | assignment_statement'''
    # Para los campos de registro típicos
    if len(p) == 4 and isinstance(p[1], list):
        p[0] = ('field', p[1], p[3])
    elif len(p) == 5 and p[1] == 'VAR':
        p[0] = ('var_field', p[2], p[4])
    elif len(p) == 5 and isinstance(p[1], list):
        p[0] = ('field_with_string', p[1], p[3])
    else:
        # Para if_statement, case_statement, assignment_statement
        p[0] = p[1]

def p_case_part(p):
    '''case_part : CASE ID COLON type_specifier OF case_list SEMICOLON
                 | empty'''
    if len(p) == 2:
        # empty
        p[0] = []
    else:
        # CASE ID : type_specifier OF case_list ;
        p[0] = ('case_part', p[2], p[4], p[6])

def p_case_list(p):
    '''case_list : case_list SEMICOLON case_element
                 | case_element'''
    if len(p) == 2:
        # Solo un case_element
        p[0] = [p[1]]
    else:
        # case_list ; case_element
        p[0] = p[1] + [p[3]]

def p_case_list_opt_semicolon(p):
    '''case_list_opt_semicolon : case_list
                               | case_list SEMICOLON'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1]

def p_case_element(p):
    '''case_element : NUMBER COLON LPAREN field_list RPAREN
                    | NUMBER COLON field_list'''
    if len(p) == 6:
        # NUMBER : ( field_list )
        p[0] = ('case_element', p[1], p[4])
    else:
        # NUMBER : field_list
        p[0] = ('case_element', p[1], p[3])
#---------------------------------------------------

# OBJECT -----------------------------------------------

def p_method_list(p):
    '''method_list : method_list method
                   | method
                   | empty'''
    if len(p) == 2:
        if p[1] is None:
            p[0] = []
        else:
            p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

def p_method(p):
    '''method : CONSTRUCTOR ID LPAREN field_list RPAREN SEMICOLON
              | PROCEDURE ID LPAREN field_list RPAREN SEMICOLON  
              | FUNCTION ID LPAREN field_list RPAREN COLON type_specifier SEMICOLON
              | DESTRUCTOR ID SEMICOLON'''
    if p[1] == 'CONSTRUCTOR':
        p[0] = ('constructor', p[2], p[4])
    elif p[1] == 'PROCEDURE':
        p[0] = ('procedure', p[2], p[4])
    elif p[1] == 'FUNCTION':
        p[0] = ('function', p[2], p[4], p[7])
    elif p[1] == 'DESTRUCTOR':
        p[0] = ('destructor', p[2])

# ----------------------------------------------------

def p_case_statement(p):
    '''case_statement : CASE expression OF case_list_opt_semicolon END SEMICOLON
                      | CASE expression OF case_list_opt_semicolon ELSE statement_list END SEMICOLON
                      | CASE expression OF case_list_opt_semicolon END'''
    if len(p) == 6:
        # CASE expression OF case_list END
        p[0] = ('case', p[2], p[4])
    elif len(p) == 8:
        # CASE expression OF case_list END SEMICOLON
        p[0] = ('case', p[2], p[4])
    else:
        # CASE expression OF case_list ELSE statement_list END SEMICOLON
        p[0] = ('case_else', p[2], p[4], p[6])

def p_type_specifier_longint(p):
    'type_specifier : LONGINT'
    p[0] = 'LONGINT'

# Reglas para las declaraciones de procedimientos.
def p_procedure_declarations(p):
    '''procedure_declarations : procedure_declaration
                              | procedure_declarations procedure_declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

# Definición de un procedimiento.
# En esta gramática simplificada, un procedimiento tiene: 
# PROCEDURE id (lista de parámetros) ; block ;
def p_procedure_declaration(p):
    '''procedure_declaration : PROCEDURE ID LPAREN parameter_list RPAREN SEMICOLON block SEMICOLON
                             | PROCEDURE ID LPAREN RPAREN SEMICOLON block SEMICOLON
                             | PROCEDURE ID SEMICOLON block SEMICOLON
                             | PROCEDURE ID LPAREN parameter_list RPAREN SEMICOLON FORWARD SEMICOLON
                             | PROCEDURE ID LPAREN RPAREN SEMICOLON FORWARD SEMICOLON
                             | PROCEDURE ID SEMICOLON FORWARD SEMICOLON'''
    global param_stack
    proc_name = p[2]
    symbol_table[proc_name] = ('procedure')
    # Entra a un nuevo ámbito de parámetros
    if len(p) > 4 and isinstance(p[4], list):
        param_stack.append([param[1] for param in p[4]])
    else:
        param_stack.append([])
    # El parser analiza el bloque aquí
    # ...
    param_stack.pop()  # Sale del ámbito de parámetros



def p_function_declaration(p):
    '''function_declaration : FUNCTION ID LPAREN parameter_list RPAREN COLON type_specifier SEMICOLON block SEMICOLON
                            | FUNCTION ID LPAREN parameter_list RPAREN COLON type_specifier SEMICOLON FORWARD SEMICOLON'''
    func_name = p[2]
    symbol_table[func_name] = ('function')

    '''# Add parameters to symbol table (if any)
    if len(p) > 4 and p[4]:
        for param in p[4]:
            param_name = param[1]
            param_type = param[2]
            symbol_table[param_name] = ('parameter', param_type)'''

def p_function_call(p):
    'function_call : ID LPAREN expression_list RPAREN'
    p[0] = ('function_call', p[1], p[3])


def p_factor_function_call(p):
    'factor : function_call'
    p[0] = p[1]
    
# Lista de parámetros: en este ejemplo se permite únicamente una declaración de parámetros.
def p_parameter_list(p):
    '''parameter_list : parameter
                      | parameter_list SEMICOLON parameter
                      | empty'''
    if len(p) == 2 and p[1] != 'empty':
        p[0] = [p[1]]
    elif len(p) == 4:
        p[0] = p[1] + [p[3]]
    else:
        p[0] = []

def p_parameter(p):
    '''parameter : ID COLON type_specifier'''
    p[0] = ('parameter', p[1], p[3])  # Store parameter info for use in p_procedure_declaration

# El bloque compuesto se encierra entre BEGIN y END.
def p_compound_statement(p):
    '''compound_statement : BEGIN statement_list END
                            | BEGIN local_var_declarations statement_list END
                            ''' 
    
    p[0] = ('compound_statement', p[2], p[3])  # Guardar la lista de sentencias y la tabla de símbolos

def p_local_var_declarations(p):
    '''local_var_declarations : var_declaration
                             | empty'''
    p[0] = p[1]

# Una lista de sentencias: una o varias separadas por punto y coma.
def p_statement_list_multi(p):
    '''statement_list : statement_list statement SEMICOLON
                      | statement_list statement'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = p[1] + [p[2]]
        p[0] = p[1] + [p[2]]

def p_statement_list_single(p):
    'statement_list : statement'
    p[0] = [p[1]]


# Sentencia: puede ser asignación, condicional, ciclo, llamada a procedimiento o bloque compuesto.
def p_statement(p):
    '''statement : if_statement
                 | assignment_statement
                 | while_statement
                 | procedure_call
                 | compound_statement
                 | for_statement
                 | case_statement
                 | with_statement
                 | repeat_statement
                 | empty'''
    p[0] = p[1]

def p_with_statement(p):
    '''with_statement : WITH variable DO statement'''
    p[0] = ('with', p[2], p[4])
    

def p_if_statement(p):
    '''if_statement : IF expression THEN statement ELSE statement
                    | IF expression THEN statement
                    | IF expression IN statement THEN statement ELSE statement
                    | IF expression IN statement THEN statement'''
    if len(p) == 5:
        # IF expression THEN statement
        p[0] = ('if', p[2], p[4])
    elif len(p) == 7 and p[3] == 'THEN':
        # IF expression THEN statement ELSE statement
        p[0] = ('if_else', p[2], p[4], p[6])
    elif len(p) == 7 and p[4] == 'THEN':
        # IF expression IN statement THEN statement
        p[0] = ('if_in', p[2], p[4], p[6])
    elif len(p) == 9:
        # IF expression IN statement THEN statement ELSE statement
        p[0] = ('if_in_else', p[2], p[4], p[6], p[8])

def p_for_statement(p):
    '''for_statement : FOR ID COLON_EQUAL expression TO expression DO block
                     | FOR ID COLON_EQUAL expression DOWNTO expression DO block'''
    if p[5] == 'TO':
        p[0] = ('for', p[2], p[4], p[6], p[8])
    else:
        p[0] = ('for_downto', p[2], p[4], p[6], p[8])
# Asignación: variable, token de asignación, y expresión. EJEMPLO: x := 5 + 3.
def p_assignment_statement(p):
    '''assignment_statement : variable COLON_EQUAL expression
                            | variable COLON_EQUAL BOOLEAN_LITERAL SEMICOLON
                            | variable PLUS COLON_EQUAL expression
                            | variable MINUS COLON_EQUAL expression
                            | variable TIMES COLON_EQUAL expression
                            | variable DIVIDE COLON_EQUAL expression
                            | ID COLON_EQUAL expression'''
    if len(p) == 4 and p[2] == ':=':
        # variable := expression
        p[0] = ('assign', p[1], p[3])
    elif len(p) == 5 and p[2] == ':=':
        # variable := BOOLEAN_LITERAL ;
        p[0] = ('assign_bool', p[1], p[3])
    elif len(p) == 5 and p[2] in ('+', '-', '*', '/'):
        # variable op := expression
        op_map = {'+': 'add_assign', '-': 'sub_assign', '*': 'mul_assign', '/': 'div_assign'}
        p[0] = (op_map[p[2]], p[1], p[4])
    elif len(p) == 4 and isinstance(p[1], str):
        # ID := expression
        p[0] = ('assign', p[1], p[3])

# Una variable es un identificador, con o sin índice (para arreglos).
def p_variable_simple(p):
    '''variable : ID
                | variable LBRACKET expression RBRACKET
                | variable DOT ID'''
    global param_stack
    all_params = [name for scope in param_stack for name in scope]
    if len(p) == 2:
        if p[1] not in symbol_table and p[1] not in all_params:
            lineno = p.lineno(1)
            print(f"Error semántico en la linea {lineno}: Variable '{p[1]}' no declarada.")
            global hay_error
            hay_error = True
        else:
            p[0] = ('var', p[1])
    elif len(p) == 5:
        p[0] = ('array_access', p[1], p[3])
    elif len(p) == 4:
        p[0] = ('record_access', p[1], p[3])
    
def p_variable_index(p):
    'variable : ID LBRACKET index_list RBRACKET'
    p[0] = ('array_access', p[1], p[3])

def p_index_list(p):
    '''index_list : expression
                  | index_list COMMA expression'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]
        
# Sentencia while: WHILE expresión DO sentencia.
def p_while_statement(p):
    'while_statement : WHILE expression DO statement'
    p[0] = ('while', p[2], p[4])
    
# Llamada a procedimiento: ID ( lista de expresiones ).
def p_procedure_call(p):
    '''procedure_call : ID LPAREN expression_list RPAREN
                      | ID'''
    if len(p) == 2:
        p[0] = ('procedure_call', p[1], [])
    else:
        p[0] = ('procedure_call', p[1], p[3])
    
# Lista de expresiones: cero o más expresiones separadas por comas.
def p_expression_list_multi(p):
    'expression_list : expression expression_list_tail'
    if p[2] is None:
        p[0] = [p[1]]
    else:
        # p[2] es una lista de expresiones adicionales
        p[0] = [p[1]] + p[2]
    
def p_expression_list_tail(p):
    '''expression_list_tail : COMMA expression expression_list_tail
                            | empty'''
    if len(p) == 2:
        # empty
        p[0] = None
    else:
        # COMMA expression expression_list_tail
        if p[3] is None:
            p[0] = [p[2]]
        else:
            p[0] = [p[2]] + p[3]
        
def p_expression_list_empty(p):
    'expression_list : empty'
    p[0] = []

# Expresión: una simple expresión, opcionalmente seguida de un operador relacional y otra simple expresión.
def p_expression(p):
    '''expression : simple_expression relop simple_expression
                  | simple_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('relop', p[2], p[1], p[3])
        
# simple_expression: secuencia de términos sumados o restados.
def p_simple_expression(p):
    '''simple_expression : term simple_expression_tail'''
    if p[2] is None:
        p[0] = p[1]
    else:
        # p[2] es una tupla (addop, term)
        p[0] = ('simple_expression', p[1], p[2])
    
def p_simple_expression_tail(p):
    '''simple_expression_tail : addop term
                              | empty'''
    if len(p) == 2:
        # empty
        p[0] = None
    else:
        # addop term
        p[0] = (p[1], p[2])
        
def p_addop(p):
    '''addop : PLUS
             | MINUS'''
    p[0] = p[1]
    
# term: secuencia de factores multiplicados o divididos.
def p_term(p):
    'term : factor term_tail'
    if p[2] is None:
        p[0] = p[1]
    else:
        # p[2] es una tupla (mulop, factor)
        p[0] = ('term', p[1], p[2])
    
def p_term_tail(p):
    '''term_tail : mulop factor
                 | empty'''
    if len(p) == 2:
        # empty
        p[0] = None
    else:
        # mulop factor
        p[0] = (p[1], p[2])
        
def p_mulop(p):
    '''mulop : TIMES
             | DIVIDE
             | DIV'''
    p[0] = p[1]

def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression MOD expression'''
    p[0] = ('binop', p[2], p[1], p[3])
    
# factor: puede ser una expresión entre paréntesis, una variable, un número o una cadena.
def p_factor_expr(p):
    'factor : LPAREN expression RPAREN'
    p[0] = p[2]
    
def p_factor_variable(p):
    'factor : variable'
    p[0] = p[1]
    
def p_factor_number(p):
    'factor : NUMBER'
    p[0] = ('number', p[1])

def p_factor_string(p):
    'factor : STRING_LITERAL'
    p[0] = ('string', p[1])


    
# relop: operadores relacionales.
def p_relop(p):
    '''relop : LESS
             | LESS_EQUAL
             | GREAT
             | GREAT_EQUAL
             | EQUAL
             | DIFFERENT'''
    p[0] = p[1]
    
# Regla para la producción vacía.
def p_empty(p):
    'empty :'
    p[0] = None

def p_statement_readln(p):
    'statement : READLN'
    p[0] = ('readln',)

def p_statement_readln_parent(p):
    '''statement : READLN LPAREN variable RPAREN
                 | READLN LBRACKET variable RBRACKET
                 | READLN LPAREN variable_list RPAREN'''
    p[0] = ('readln_var', p[3])

def p_variable_list(p):
    '''variable_list : variable
                     | variable_list COMMA variable'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_statement_write(p):
    'statement : WRITE LPAREN expression_list RPAREN'
    p[0] = ('write', p[3])

def p_statement_writeln(p):
    '''statement : WRITELN LPAREN write_arguments RPAREN
                 | WRITELN LPAREN expression RPAREN'''
                # | WRITELN LPAREN write_arguments RPAREN SEMICOLON'''
    p[0] = ('writeln', p[3])

def p_write_arguments(p):
    '''write_arguments : write_argument
                       | write_arguments COMMA write_argument'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_write_argument(p):
    '''write_argument : expression
                      | expression COLON expression
                      | expression COLON expression COLON expression'''
    if len(p) == 2:
        p[0] = ('write_arg', p[1])
    elif len(p) == 4:
        p[0] = ('write_arg_format', p[1], p[3])
    else:
        p[0] = ('write_arg_format_width', p[1], p[3], p[5])

#Definición para USES 
def p_statement_uses(p):
    'statement : USES ID SEMICOLON'
    p[0] = ('uses', p[2])  # Guardar el nombre del módulo en la tabla de símbolos

#Definicion para CONST
def p_constant_declaration(p):
    'constant_declaration : CONST constant_list'
    p[0] = ('constant_declaration', p[2])

def p_repeat_statement(p):
    'repeat_statement : REPEAT statement_list UNTIL expression SEMICOLON'
    p[0] = ('repeat', p[2], p[4])

def p_constant_list(p):
    '''constant_list : constant
                     | constant_list constant'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]
    

def p_constant(p):
    '''constant : ID EQUAL CHARACTER_LITERAL SEMICOLON
                | ID EQUAL NUMBER SEMICOLON
                | ID EQUAL STRING_LITERAL SEMICOLON
                | ID EQUAL BOOLEAN_LITERAL SEMICOLON'''
    global symbol_table
    const_name = p[1]
    const_value = p[3]

    if const_name in symbol_table:
        lineno = p.lineno(1)
        print(f"Error semántico en la linea {lineno}: La constante '{const_name}' ya fue declarada.")
        global hay_error
        hay_error = True
    else:
        symbol_table[const_name] = ('const', const_value)  # Store constant in symbol table
        p[0] = ('const', const_name, const_value)

# Expresión lógica
def p_expression_logical(p):
    '''expression : expression AND expression
                  | expression OR expression
                  | NOT expression'''
    if len(p) == 4:
        # AND / OR
        p[0] = ('logical_op', p[2], p[1], p[3])
    else:
        # NOT
        p[0] = ('not_op', p[2])



# Manejo de errores sintácticos
def p_error(p):
    global hay_error
    hay_error = True
    if VERBOSE:
        if p is not None:
            # Calcular la posición relativa dentro de la línea
            line_start = p.lexer.lexdata.rfind('\n', 0, p.lexpos) + 1
            column = p.lexpos - line_start + 1
            print("ERROR SINTÁCTICO EN LA LÍNEA " + str(p.lexer.lineno) + 
                  " NO SE ESPERABA EL Token '" + str(p.value) + 
                  "' EN LA POSICIÓN " + str(column) + ".")
        else:
            print("ERROR SINTÁCTICO AL FINAL DE LA ENTRADA: Entrada incompleta o token inesperado al final.")
    else:
        raise Exception('syntax', 'error')


# Construcción del parser y lexer
parser = yacc.yacc()
lexer = lex.lex(module=lex_rules)

# Ejecución principal
if __name__ == '__main__':

    if (len(sys.argv) > 1):
        fin = sys.argv[1]
    else:
        fin = 'Evaluar_Pascal.txt'

    with open(fin, 'r', encoding='utf-8') as f:
        data = f.read()

    parser.parse(data, tracking=True, lexer=lexer)

    if not hay_error:
        print("Amiguito, tengo el placer de informar que tu parser reconoció correctamente todo.")
        print("\nTabla de símbolos:")
        for name, t in symbol_table.items():
            print(f"  {name} : {t}")
    else:
        print("Lo siento, tu parser detectó errores en la entrada.")
        print("\nTabla de símbolos:")
        for name, t in symbol_table.items():
            print(f"  {name} : {t}")

