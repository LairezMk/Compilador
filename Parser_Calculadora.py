import ply.yacc as yacc
import ply.lex as lex
import Analizador_Lex_Calculadora as lex_rules  # Importar todo el módulo léxico
from Analizador_Lex_Calculadora import tokens
import sys

VERBOSE = True # Cambia a False para menos información en la salida
hay_error = False  # Variable global para saber si hubo errores

# Reglas de la gramática

def p_expression_plus_minus(p):
    '''E : E PLUS T
         | E MINUS T'''
    pass

def p_expression_term(p):
    'E : T'
    pass

def p_term_times_divide(p):
    '''T : T TIMES F
         | T DIVIDE F'''
    pass

def p_term_factor(p):
    'T : F'
    pass

def p_factor_group(p):
    'F : LPAREN E RPAREN'
    pass

def p_factor_number(p):
    'F : NUMBER'
    pass

def p_factor_id(p):
    'F : ID'
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
        fin = 'Evaluar_Calculadora.txt'

    with open(fin, 'r') as f:
        data = f.read()

    parser.parse(data, tracking=True, lexer=lexer)

    if not hay_error:
        print("Amiguito, tengo el placer de informar que tú parser reconoció correctamente todo")
    else:
        print("Lo siento, tu parser detectó errores en la entrada")
        
		