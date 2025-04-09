import ply.yacc as yacc
from Analizador_Lex_Calculadora import tokens
import sys

VERBOSE = True

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

def p_error(p):
	if VERBOSE:
		if p is not None:
			print ("ERROR SINTACTICO EN LA LINEA " + str(p.lexer.lineno) + " NO SE ESPERABA EL Token  " + str(p.value))
		else:
			print ("ERROR SINTACTICO EN LA LINEA: " + str(p.lexer.lineno))
	else:
		raise Exception('syntax', 'error')
		

parser = yacc.yacc()

if __name__ == '__main__':

	if (len(sys.argv) > 1):
		fin = sys.argv[1]
	else:
		fin = 'Evaluar_Calculadora.txt'

	f = open(fin, 'r')
	data = f.read()
	#print (data)
	parser.parse(data, tracking=True)
	print("Amiguito, tengo el placer de informar que Tu parser reconocio correctamente todo")
	#input()
