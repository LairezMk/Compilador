import ply.yacc as yacc
import ply.lex as lex
import Analizador_Lexico_mini_pascal_arreglado as lex_rules  # Importar todo el módulo léxico
from Analizador_Lexico_mini_pascal_arreglado import tokens
import sys

VERBOSE = True # Cambia a False para menos información en la salida
hay_error = False  # Variable global para saber si hubo errores

# Reglas de la gramática

def p_program(p):
    'program : PROGRAM ID SEMICOLON block DOT'
    pass

def p_block(p):
    'block : declarations BEGIN statement_list END'
    pass

def p_declarations(p):
    '''declarations : VAR declaration_list
                    | empty'''
    pass

def p_declaration_list(p):
    '''declaration_list : declaration_list declaration
                        | declaration'''
    pass

def p_declaration(p):
    'declaration : id_list COLON type SEMICOLON'
    pass

def p_id_list(p):
    '''id_list : ID
               | ID COMMA id_list'''
    pass

def p_type(p):
    '''type : INTEGER
            | REAL'''
    pass

def p_statement_list(p):
    '''statement_list : statement SEMICOLON statement_list
                      | statement'''
    pass

def p_statement(p):
    '''statement : assignment
                 | if_statement
                 | while_statement
                 | compound_statement'''
    pass

def p_assignment(p):
    'assignment : ID ASSIGN expression'
    pass

def p_if_statement(p):
    '''if_statement : IF expression THEN statement else_optional'''
    pass

def p_else_optional(p):
    '''else_optional : ELSE statement
                     | empty'''
    pass

def p_while_statement(p):
    'while_statement : WHILE expression DO statement'
    pass

def p_compound_statement(p):
    'compound_statement : BEGIN statement_list END'
    pass

def p_expression(p):
    '''expression : expression PLUS term
                  | expression MINUS term
                  | term'''
    pass

def p_term(p):
    '''term : term TIMES factor
            | term DIVIDE factor
            | factor'''
    pass

def p_factor(p):
    '''factor : LPAREN expression RPAREN
              | NUMBER
              | ID'''
    pass

def p_empty(p):
    'empty :'
    pass


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