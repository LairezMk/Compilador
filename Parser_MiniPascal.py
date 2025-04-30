import ply.yacc as yacc
import ply.lex as lex
import Analizador_Lexico_mini_pascal_arreglado as lex_rules  # Importar todo el módulo léxico
from Analizador_Lexico_mini_pascal_arreglado import tokens
import sys

VERBOSE = True # Cambia a False para menos información en la salida
hay_error = False  # Variable global para saber si hubo errores

# Precedencia para resolver conflictos shift/reduce en if-then-else
precedence = (
    ('nonassoc', 'ELSE'),  # ELSE tiene menor precedencia
)


# Reglas de la gramática
def p_program(p):
    'program : PROGRAM ID SEMICOLON declaration_sections block DOT'
    pass



def p_declaration_sections(p):
    '''declaration_sections : declaration_sections declaration_section
                            | empty'''
    pass

def p_declaration_section(p):
    '''declaration_section : uses_opt
                           | constant_declaration
                           | type_declaration
                           | var_declaration
                           | procedure_declarations
                           | function_declaration
                           | constructor_declaration
                           | method_declaration'''
    pass

def p_method_declaration(p):
    '''method_declaration : CONSTRUCTOR ID DOT ID LPAREN parameter_list RPAREN SEMICOLON block 
                          | PROCEDURE ID DOT ID LPAREN parameter_list RPAREN SEMICOLON block 
                          | FUNCTION ID DOT ID LPAREN parameter_list RPAREN COLON type_specifier SEMICOLON block
                          | DESTRUCTOR ID DOT ID SEMICOLON block'''
    pass

def p_constructor_declaration(p):
    '''constructor_declaration : CONSTRUCTOR ID LPAREN field_list RPAREN SEMICOLON block
                               | CONSTRUCTOR ID LPAREN RPAREN SEMICOLON block'''
    pass

def p_uses_opt(p):
    '''uses_opt : USES id_list SEMICOLON'''
   #             | empty'''
    pass

def p_id_list(p):
    '''id_list : ID
               | id_list COMMA ID'''
    pass     

def p_var_declaration(p):
    'var_declaration : VAR declaration_list'
    pass

def p_declaration_list(p):
    '''declaration_list : declaration
                        | declaration_list declaration'''
    pass

def p_declaration(p):
    'declaration : id_list COLON type_specifier SEMICOLON'
    pass

# El bloque se compone de declaraciones (variables y/o procedimientos) seguidas de una sentencia compuesta.
def p_block(p):
    'block : declaration_sections compound_statement'
    pass



def p_type_declaration(p):
    'type_declaration : TYPE type_list'
    pass

def p_type_list(p):
    '''type_list : type_definition
                 | type_list type_definition'''
    pass

#def p_type_specifier_enum(p):
#    '''type_specifier : LPAREN id_list RPAREN'''
#    p[0] = ('enum', p[2])

def p_type_definition(p):
    'type_definition : ID EQUAL type_specifier SEMICOLON'
    pass

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
    pass

def p_type_expression(p):
    '''type_expression : type_expression COMMA subrange
                       | subrange'''
    pass

def p_subrange(p):
    '''subrange : NUMBER DOTDOT NUMBER
                | NUMBER DOTDOT ID'''

    pass

#RECORD -----------------------------------------------
def p_field_list(p):
    '''field_list : field_list field
                  | field
                  | empty'''
    pass

def p_field(p):
    '''field : id_list COLON type_specifier
             | id_list COLON type_specifier SEMICOLON
             | VAR id_list COLON type_specifier
             | VAR id_list COLON type_specifier SEMICOLON
             | id_list LPAREN STRING_LITERAL RPAREN
             | if_statement
             | case_statement
             | assignment_statement'''
    pass

def p_case_part(p):
    '''case_part : CASE ID COLON type_specifier OF case_list SEMICOLON
                 | empty'''
    pass

def p_case_list(p):
    '''case_list : case_list SEMICOLON case_element
                 | case_element'''
    pass

def p_case_element(p):
    '''case_element : NUMBER COLON LPAREN field_list RPAREN
                    | NUMBER COLON field_list'''
    pass
#---------------------------------------------------

# OBJECT -----------------------------------------------

def p_method_list(p):
    '''method_list : method_list method
                   | method
                   | empty'''
    pass

def p_method(p):
    '''method : CONSTRUCTOR ID LPAREN field_list RPAREN SEMICOLON
              | PROCEDURE ID LPAREN field_list RPAREN SEMICOLON  
              | FUNCTION ID LPAREN field_list RPAREN COLON type_specifier SEMICOLON
              | DESTRUCTOR ID SEMICOLON'''
    pass

# ----------------------------------------------------

def p_case_statement(p):
    '''case_statement : CASE expression OF case_list END SEMICOLON
                      | CASE expression OF case_list ELSE statement END SEMICOLON  
                      | CASE expression OF case_list END'''
    pass

def p_type_specifier_longint(p):
    'type_specifier : LONGINT'
    pass

# Reglas para las declaraciones de procedimientos.
def p_procedure_declarations(p):
    '''procedure_declarations : procedure_declaration
                              | procedure_declarations procedure_declaration'''
    pass

# Definición de un procedimiento.
# En esta gramática simplificada, un procedimiento tiene: 
# PROCEDURE id (lista de parámetros) ; block ;
def p_procedure_declaration(p):
    '''procedure_declaration : PROCEDURE ID LPAREN field_list RPAREN SEMICOLON block SEMICOLON
                             | PROCEDURE ID LPAREN RPAREN SEMICOLON block SEMICOLON
                             | PROCEDURE ID SEMICOLON block SEMICOLON
                             | PROCEDURE ID LPAREN field_list RPAREN SEMICOLON FORWARD SEMICOLON
                             | PROCEDURE ID LPAREN RPAREN SEMICOLON FORWARD SEMICOLON
                             | PROCEDURE ID SEMICOLON FORWARD SEMICOLON'''
    pass

# def p_function_declaration(p):
#     'function_declaration : FUNCTION ID LPAREN parameter_list RPAREN COLON type_specifier SEMICOLON block SEMICOLON'
#     p[0] = ('function', p[2], p[4], p[7], p[9])

def p_function_declaration(p):
    '''function_declaration : FUNCTION ID LPAREN parameter_list RPAREN COLON type_specifier SEMICOLON block SEMICOLON
                            | FUNCTION ID LPAREN parameter_list RPAREN COLON type_specifier SEMICOLON FORWARD SEMICOLON'''
    pass



def p_function_call(p):
    'function_call : ID LPAREN expression_list RPAREN'
    pass


def p_factor_function_call(p):
    'factor : function_call'
    pass
    
# Lista de parámetros: en este ejemplo se permite únicamente una declaración de parámetros.
def p_parameter_list(p):
    '''parameter_list : parameter
                      | parameter_list SEMICOLON parameter
                      | empty'''
    pass

def p_parameter(p):
    '''parameter : id_list COLON type_specifier
                 | VAR id_list COLON type_specifier'''
    pass

# El bloque compuesto se encierra entre BEGIN y END.
def p_compound_statement(p):
    '''compound_statement : BEGIN statement_list END'''
    pass

# Una lista de sentencias: una o varias separadas por punto y coma.
def p_statement_list_multi(p):
    'statement_list : statement statement_list_tail'
    pass

def p_statement_list_tail(p):
    '''statement_list_tail : SEMICOLON statement statement_list_tail 
                           | empty'''
    pass

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
                 | empty'''
    pass

def p_with_statement(p):
    '''with_statement : WITH variable DO statement'''
    pass

def p_if_statement(p):
    '''if_statement : IF expression THEN statement ELSE statement
                    | IF expression THEN statement
                    | IF expression IN statement THEN statement ELSE statement
                    | IF expression IN statement THEN statement'''
    pass


def p_for_statement(p):
    '''for_statement : FOR ID COLON_EQUAL expression TO expression DO statement
                     | FOR ID COLON_EQUAL expression DOWNTO expression DO statement'''
    pass

# Asignación: variable, token de asignación, y expresión. EJEMPLO: x := 5 + 3.
def p_assignment_statement(p):
    '''assignment_statement : variable COLON_EQUAL expression
                            | variable COLON_EQUAL BOOLEAN_LITERAL
                            | ID COLON_EQUAL expression'''
    pass

# Una variable es un identificador, con o sin índice (para arreglos).
def p_variable_simple(p):
    '''variable : ID
                | variable LBRACKET expression RBRACKET
                | variable DOT ID'''  
    pass
    
def p_variable_index(p):
    'variable : ID LBRACKET index_list RBRACKET'
    pass

def p_index_list(p):
    '''index_list : expression
                  | index_list COMMA expression'''
    pass
        
# Sentencia while: WHILE expresión DO sentencia.
def p_while_statement(p):
    'while_statement : WHILE expression DO statement'
    pass
    
# Llamada a procedimiento: ID ( lista de expresiones ).
def p_procedure_call(p):
    '''procedure_call : ID LPAREN expression_list RPAREN
                      | ID'''
    pass
    
# Lista de expresiones: cero o más expresiones separadas por comas.
def p_expression_list_multi(p):
    'expression_list : expression expression_list_tail'
    pass
    
def p_expression_list_tail(p):
    '''expression_list_tail : COMMA expression expression_list_tail
                            | empty'''
    pass
        
def p_expression_list_empty(p):
    'expression_list : empty'
    pass

# Expresión: una simple expresión, opcionalmente seguida de un operador relacional y otra simple expresión.
def p_expression(p):
    '''expression : simple_expression relop simple_expression
                  | simple_expression'''
    pass
        
# simple_expression: secuencia de términos sumados o restados.
def p_simple_expression(p):
    '''simple_expression : term simple_expression_tail'''
    pass
    
def p_simple_expression_tail(p):
    '''simple_expression_tail : addop term
                              | empty'''
    pass
        
def p_addop(p):
    '''addop : PLUS
             | MINUS'''
    pass
    
# term: secuencia de factores multiplicados o divididos.
def p_term(p):
    'term : factor term_tail'
    pass
    
def p_term_tail(p):
    '''term_tail : mulop factor
                 | empty'''
    pass
        
def p_mulop(p):
    '''mulop : TIMES
             | DIVIDE
             | DIV'''
             
    pass

def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression MOD expression'''
    pass
    
# factor: puede ser una expresión entre paréntesis, una variable, un número o una cadena.
def p_factor_expr(p):
    'factor : LPAREN expression RPAREN'
    pass
    
def p_factor_variable(p):
    'factor : variable'
    pass
    
def p_factor_number(p):
    'factor : NUMBER'
    pass

def p_factor_string(p):
    'factor : STRING_LITERAL'
    pass


    
# relop: operadores relacionales.
def p_relop(p):
    '''relop : LESS
             | LESS_EQUAL
             | GREAT
             | GREAT_EQUAL
             | EQUAL
             | DIFFERENT'''
    pass
    
# Regla para la producción vacía.
def p_empty(p):
    'empty :'
    pass

def p_statement_readln(p):
    'statement : READLN'
    pass

def p_statement_readln_parent(p):
    '''statement : READLN LPAREN variable RPAREN
                 | READLN LBRACKET variable RBRACKET'''
    pass

def p_statement_write(p):
    'statement : WRITE LPAREN expression_list RPAREN'
    pass

def p_statement_writeln(p):
    '''statement : WRITELN LPAREN write_arguments RPAREN'''
    pass

def p_write_arguments(p):
    '''write_arguments : write_argument
                       | write_arguments COMMA write_argument'''
    pass

def p_write_argument(p):
    '''write_argument : expression
                      | expression COLON expression
                      | expression COLON expression COLON expression'''
    pass

#Definición para USES 
def p_statement_uses(p):
    'statement : USES ID SEMICOLON'
    pass

#Definicion para CONST
def p_constant_declaration(p):
    'constant_declaration : CONST constant_list'
    pass

def p_constant_list(p):
    '''constant_list : constant
                     | constant_list constant'''
    pass

def p_constant(p):
    '''constant : ID EQUAL CHARACTER_LITERAL SEMICOLON
                | ID EQUAL NUMBER SEMICOLON
                | ID EQUAL STRING_LITERAL SEMICOLON
                | ID EQUAL BOOLEAN_LITERAL SEMICOLON'''
    pass

# Expresión lógica
def p_expression_logical(p):
    '''expression : expression AND expression
                  | expression OR expression
                  | NOT expression'''
    pass



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

    with open( fin, 'r') as f:
        data = f.read()

    parser.parse(data, tracking=True, lexer=lexer)

    if not hay_error:
        print("Amiguito, tengo el placer de informar que tú parser reconoció correctamente todo")
    else:
        print("Lo siento, tu parser detectó errores en la entrada")

