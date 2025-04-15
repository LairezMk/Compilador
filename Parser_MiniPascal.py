import ply.yacc as yacc
import ply.lex as lex
import Analizador_Lexico_mini_pascal_arreglado as lex_rules  # Importar todo el módulo léxico
from Analizador_Lexico_mini_pascal_arreglado import tokens
import sys

VERBOSE = True # Cambia a False para menos información en la salida
hay_error = False  # Variable global para saber si hubo errores

# Reglas de la gramática
def p_program(p):
    'program : PROGRAM ID SEMICOLON uses_opt block DOT'
    p[0] = ('program', p[2], p[4])

def p_uses_opt(p):
    '''uses_opt : USES ID SEMICOLON
                | empty'''
    if len(p) == 2:
        p[0] = None
    else:
        p[0] = ('uses', p[2])

# El bloque se compone de declaraciones (variables y/o procedimientos) seguidas de una sentencia compuesta.
def p_block(p):
    'block : declarations compound_statement'
    p[0] = ('block', p[1], p[2])

# La sección de declaraciones se puede dividir en declaraciones de variables y declaraciones de procedimientos.
def p_declarations_var_proc(p):
    'declarations : VAR declaration_list procedure_declarations'
    p[0] = ('declarations', p[2], p[3])

def p_declarations_var_only(p):
    'declarations : VAR declaration_list'
    p[0] = ('declarations', p[2], [])
    
def p_declarations_proc_only(p):
    'declarations : procedure_declarations'
    p[0] = ('declarations', [], p[1])
    
def p_declarations_empty(p):
    'declarations : empty'
    p[0] = ('declarations', [], [])

# Una lista de declaraciones de variables.
def p_declaration_list(p):
    '''declaration_list : declaration
                        | declaration_list declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

# Una declaración de variable es: lista de identificadores, dos puntos, tipo, y punto y coma.
def p_declaration(p):
    'declaration : id_list COLON type_specifier SEMICOLON'
    p[0] = ('decl', p[1], p[3])

# id_list: uno o más identificadores separados por comas.
def p_id_list_single(p):
    'id_list : ID'
    p[0] = [p[1]]
    
def p_id_list_multi(p):
    'id_list : id_list COMMA ID'
    p[0] = p[1] + [p[3]]

# El tipo puede ser simplemente INTEGER o la declaración de un arreglo.
def p_type_specifier_int(p):
    'type_specifier : INTEGER'
    p[0] = 'integer'

# Definición de un arreglo: ARRAY [n1..n2] OF INTEGER.
def p_type_specifier_array(p):
    'type_specifier : ARRAY LBRACKET NUMBER DOTDOT NUMBER RBRACKET OF INTEGER'
    # Devuelve una tupla con el tipo de arreglo y sus límites.
    p[0] = ('array', p[3], p[5])

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
    'procedure_declaration : PROCEDURE ID LPAREN parameter_list RPAREN SEMICOLON block SEMICOLON'
    p[0] = ('procedure', p[2], p[4], p[7])
    
# Lista de parámetros: en este ejemplo se permite únicamente una declaración de parámetros.
def p_parameter_list(p):
    'parameter_list : id_list COLON type_specifier'
    p[0] = ('params', p[1], p[3])

# El bloque compuesto se encierra entre BEGIN y END.
def p_compound_statement(p):
    'compound_statement : BEGIN statement_list END'
    p[0] = ('compound', p[2])
    
# Una lista de sentencias: una o varias separadas por punto y coma.
def p_statement_list_multi(p):
    'statement_list : statement statement_list_tail'
    p[0] = [p[1]] + p[2]

def p_statement_list_tail(p):
    '''statement_list_tail : SEMICOLON statement statement_list_tail
                           | empty'''
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = [p[2]] + p[3]

# Sentencia: puede ser asignación, condicional, ciclo, llamada a procedimiento o bloque compuesto.
def p_statement_assignment(p):
    'statement : assignment_statement'
    p[0] = p[1]

def p_statement_if(p):
    'statement : if_statement'
    p[0] = p[1]

def p_statement_while(p):
    'statement : while_statement'
    p[0] = p[1]

def p_statement_proc_call(p):
    'statement : procedure_call'
    p[0] = p[1]

def p_statement_compound(p):
    'statement : compound_statement'
    p[0] = p[1]

def p_statement_empty(p):
    'statement : empty'
    p[0] = None

# Asignación: variable, token de asignación, y expresión. EJEMPLO: x := 5 + 3.
def p_assignment_statement(p):
    'assignment_statement : variable COLON_EQUAL expression'
    p[0] = ('assign', p[1], p[3])

# Una variable es un identificador, con o sin índice (para arreglos).
def p_variable_simple(p):
    'variable : ID'
    p[0] = p[1]
    
def p_variable_index(p):
    'variable : ID LBRACKET expression RBRACKET'
    p[0] = ('arrayref', p[1], p[3])
    
# Sentencia if: IF expresión THEN sentencia ELSE sentencia.
def p_if_statement(p):
    'if_statement : IF expression THEN statement ELSE statement'
    p[0] = ('if', p[2], p[4], p[6])
    
# Sentencia while: WHILE expresión DO sentencia.
def p_while_statement(p):
    'while_statement : WHILE expression DO statement'
    p[0] = ('while', p[2], p[4])
    
# Llamada a procedimiento: ID ( lista de expresiones ).
def p_procedure_call(p):
    'procedure_call : ID LPAREN expression_list RPAREN'
    p[0] = ('proc_call', p[1], p[3])
    
# Lista de expresiones: cero o más expresiones separadas por comas.
def p_expression_list_multi(p):
    'expression_list : expression expression_list_tail'
    p[0] = [p[1]] + p[2]
    
def p_expression_list_tail(p):
    '''expression_list_tail : COMMA expression
                            | empty'''
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = [p[2]]
        
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
        p[0] = ('binop', p[2], p[1], p[3])
        
# simple_expression: secuencia de términos sumados o restados.
def p_simple_expression(p):
    '''simple_expression : term simple_expression_tail'''
    if p[2] is None:
        p[0] = p[1]
    else:
        p[0] = ('binop_seq', p[1], p[2])
    
def p_simple_expression_tail(p):
    '''simple_expression_tail : addop term
                              | empty'''
    if len(p) == 2:
        p[0] = None
    else:
        p[0] = ('binop', p[1], p[2])
        
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
        p[0] = ('binop_seq', p[1], p[2])
    
def p_term_tail(p):
    '''term_tail : mulop factor
                 | empty'''
    if len(p) == 2:
        p[0] = None
    else:
        p[0] = ('binop', p[1], p[2])
        
def p_mulop(p):
    '''mulop : TIMES
             | DIVIDE'''
    p[0] = p[1]
    
# factor: puede ser una expresión entre paréntesis, una variable, un número o una cadena.
def p_factor_expr(p):
    'factor : LPAREN expression RPAREN'
    p[0] = p[2]
    
def p_factor_variable(p):
    'factor : variable'
    p[0] = p[1]
    
def p_factor_number(p):
    'factor : NUMBER'
    p[0] = ('num', p[1])

def p_factor_string(p):
    'factor : STRING'
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
    'statement : READLINE'
    p[0] = ('readln',)

def p_statement_writeln(p):
    'statement : WRITELN LPAREN expression_list RPAREN'
    p[0] = ('writeln', p[3])

#Definición para USES 
def p_statement_uses(p):
    'statement : USES ID SEMICOLON'
    p[0] = ('uses', p[2])




# Manejo de errores sintácticos
def p_error(p):
    global hay_error
    hay_error = True
    if VERBOSE:
        if p is not None:
            print("ERROR SINTÁCTICO EN LA LÍNEA " + str(p.lexer.lineno) + " NO SE ESPERABA EL Token '" + str(p.value) + "'")
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

    with open(fin, 'r') as f:
        data = f.read()

    parser.parse(data, tracking=True, lexer=lexer)

    if not hay_error:
        print("Amiguito, tengo el placer de informar que tú parser reconoció correctamente todo")
    else:
        print("Lo siento, tu parser detectó errores en la entrada")

