import ply.lex as lex

# Lista de tokens
tokens = (
    'NUMBER',
    'ID',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'LPAREN',
    'RPAREN',
    'EQUAL'
)

# Reglas regulares para los tokens simples
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_EQUAL   = r'='
t_ignore  = ' \t'  # Ignorar espacios y tabulaciones

# Token para números (enteros o flotantes)
def t_NUMBER(t):
    r'\d+(\.\d+)?'
    t.value = float(t.value) if '.' in t.value else int(t.value)
    return t

# Token para identificadores (nombres de variables)
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    return t

# Manejo de nuevas líneas para llevar el conteo correcto de líneas
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Manejo de errores
def t_error(t):
    print(f'Caracter ilegal: {t.value[0]}')
    t.lexer.skip(1)

def test(data):
    lexer = lex.lex()
    lexer.input(data)

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(f'Token: {tok.type}, Valor: {tok.value}')

def test_from_file(filename):
    try:
        with open(filename, 'r') as file:
            data = file.read()
            print(f"\nContenido del archivo '{filename}':\n{data}")
            print("\nTokens encontrados:")
            test(data)
    except FileNotFoundError:
        print(f"Archivo '{filename}' no encontrado.")

# Ejemplo de uso
if __name__ == "__main__":
    test_from_file("Evaluar_Calculadora.txt")