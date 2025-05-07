import ply.lex as lex

'''Tomas Marulanda Aristizabal
Samuel Herrera Marin
Jonathan Gaviria Ocampo'''

tokens = (
        #Palabras Reservadas
    'AND', 
    'ARRAY', 
    'BEGIN', 
    'CASE', 
    'CONST', 
    'DIV', 
    'DO', 
    'DOWNTO', 
    'ELSE', 
    'END', 
    'FILE', 
    'FOR', 
    'FUNCTION', 
    'GOTO', 
    'IF', 
    'IN', 
    'LABEL', 
    'MOD', 
    'NIL', 
    'NOT', 
    'OF', 
    'OR', 
    'PACKED', 
    'PROCEDURE', 
    'PROGRAM', 
    'RECORD', 
    'REPEAT', 
    'SET', 
    'THEN', 
    'TO', 
    'TYPE', 
    'UNTIL', 
    'VAR', 
    'WRITE',
    'WRITELN',
    'WHILE', 
    'WITH',
    'NUMBER',  # Para los números
    'TRUE',
    'FALSE',
    'STRING_LITERAL', #Para las cadenas de texto
    'CHARACTER_LITERAL', #Para los caracteres
    'BYTE', #Para los bytes
    'CHAR', #Para los caracteres
    'USES', #Para los usos
    'INTEGER', #Para los enteros
    'SUBRANGE', #Para los rangos
    'OBJECT', #Para los objetos
    'CONSTRUCTOR', #Para los constructores
    'DESTRUCTOR', #Para los destructores
    'FORWARD', #Para los avances

    #Symbols
    'PLUS',    # Suma "+"
    'MINUS',   # Resta "-"
    'TIMES',   # Multiplicación "*"
    'DIVIDE',  # División "/"
    'LPAREN',  # Paréntesis izquierdo "("   
    'RPAREN',  # Paréntesis derecho ")"
    'COLON', # Dos puntos igual ":"
    'EQUAL', # Igual "="
    'DOT', # Punto "."
    'SEMICOLON', # Punto y coma ";"
    'COMMA', # Coma ","
    'DIFFERENT', #Diferente de ""
    'LBRACKET', # Llave izquierda "["
    'RBRACKET', # Llave derecha "]"
    'LBLOCK', # Corchete izquierdo "{"
    'RBLOCK', # Corchete derecho "}" 
    'BOOLEAN_LITERAL', #Booleanos
    'LESS',
    'LESS_EQUAL',
    'GREAT',
    'GREAT_EQUAL',
    'COLON_EQUAL', 
    'ID' , # Identificadores
    'STRING', # Para cadenas de texto
    'READLINE', # Para leer líneas
    'DOTDOT', # Para ".."
    'READLN', # Para leer líneas
    'LONGINT',
)

reserved = {
    'and': 'AND',
    'array': 'ARRAY',
    'begin': 'BEGIN',
    'case': 'CASE',
    'const': 'CONST',
    'div': 'DIV',
    'do': 'DO',
    'downto': 'DOWNTO',
    'else': 'ELSE',
    'end': 'END',
    'file': 'FILE',
    'for': 'FOR',
    'function': 'FUNCTION',
    'goto': 'GOTO',
    'if': 'IF',
    'in': 'IN',
    'label': 'LABEL',
    'mod': 'MOD',
    'nil': 'NIL',
    'not': 'NOT',
    'of': 'OF',
    'or': 'OR',
    'packed': 'PACKED',
    'procedure': 'PROCEDURE',
    'program': 'PROGRAM',
    'record': 'RECORD',
    'repeat': 'REPEAT',
    'set': 'SET',
    'then': 'THEN',
    'to': 'TO',
    'type': 'TYPE',
    'until': 'UNTIL',
    'var': 'VAR',
    'write': 'WRITE',
    'writeln': 'WRITELN',
    'while': 'WHILE',
    'with': 'WITH',
    'true': 'BOOLEAN_LITERAL',
    'false': 'BOOLEAN_LITERAL',
    'TRUE': 'BOOLEAN_LITERAL',
    'FALSE': 'BOOLEAN_LITERAL',
    'byte': 'BYTE',
    'char': 'CHAR',
    'integer': 'INTEGER',
    'uses': 'USES',
    'readln': 'READLN',
    'readline': 'READLINE',
    'string': 'STRING',
    'input': 'INPUT',
    'output': 'OUTPUT',
    'longint': 'LONGINT',
    'subrange': 'SUBRANGE',
    'object': 'OBJECT',
    'constructor': 'CONSTRUCTOR',
    'destructor': 'DESTRUCTOR',
    'forward': 'FORWARD',
}

# Regular expression rules for simple tokens
t_PLUS   = r'\+'
t_MINUS  = r'-'
t_TIMES  = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_ignore = ' \t'
t_LESS = r'\<'
t_EQUAL = r'\='  
t_LESS_EQUAL = r'\<\='
t_GREAT= r'\>'
t_GREAT_EQUAL= r'\>\='
t_COMMA= r'\,'
t_SEMICOLON= r'\;'
t_DOT= r'\.'
t_COLON_EQUAL= r'\:\='
t_DIFFERENT= r'\<\>'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBLOCK = r'\{'
t_RBLOCK = r'\}'
t_COLON = r'\:'
t_DOTDOT = r'\.\.'


# Definir las palabras reservadas

def t_AND(t):
    r'\band\b'
    return t

def t_ARRAY(t):
    r'\barray\b'
    return t

def t_BEGIN(t):
    r'\bbegin\b'
    return t

def t_BYTE(t):
    r'\bbyte\b'
    return t

def t_REPEAT(t):
    r'\brepeat\b'
    return t

def t_CASE(t):
    r'\bcase\b'
    return t

def t_CHAR(t):
    r'\bchar\b'
    return t

def t_CONST(t):
    r'\bconst\b'
    return t

def t_CONSTRUCTOR(t):
    r'\bconstructor\b'
    return t

def t_DESTRUCTOR(t):
    r'\bdestructor\b'
    return t

def t_DIV(t):
    r'\bdiv\b'
    return t

def t_DOWNTO(t):
    r'\bdownto\b'
    return t

def t_DO(t):
    r'\bdo\b'
    return t

def t_END(t):
    r'\bend\b'
    return t

def t_ELSE(t):
    r'\belse\b'
    return t

def t_FILE(t):
    r'\bfile\b'
    return t

def t_FOR(t):
    r'\bfor\b'
    return t

def t_FUNCTION(t):
    r'\bfunction\b'
    return t

def t_GOTO(t):
    r'\bgoto\b'
    return t

def t_IF(t):
    r'\bif\b'
    return t

def t_IN(t):
    r'\bin\b'
    return t

def t_INTERFACE(t):
    r'\binterface\b'
    return t

def t_LABEL(t):
    r'\blabel\b'
    return t

def t_NIL(t):
    r'\bnil\b'
    return t

def t_OBJECT(t):
    r'\bobject\b'
    return t

def t_OR(t):
    r'\bor\b'
    return t

def t_PRIVATE(t):
    r'\bprivate\b'
    return t

def t_PROGRAM(t):
    r'\bprogram\b'
    return t


def t_READLN(t):
    r'\breadln\b'
    return t

def t_SHL(t):
    r'\bshl\b'
    return t

def t_STRING(t):
    r'\bstring\b'
    return t

def t_OF(t):
    r'\bof\b'
    return t

def t_TO(t):
    r'\bto\b'
    return t

def t_THEN(t):
    r'\bthen\b'
    return t

def t_TYPE(t):
    r'\btype\b'
    return t

def t_UNIT(t):
    r'\bunit\b'
    return t

def t_UNTIL(t):
    r'\buntil\b'
    return t

def t_USES(t):
    r'\buses\b'
    return t

def t_VAR(t):
    r'\bvar\b'
    return t

def t_VIRTUAL(t):
    r'\bvirtual\b'
    return t

def t_WRITELN(t):
    r'\bwriteln\b'
    return t

def t_WRITE(t):
    r'\bwrite\b'
    return t

def t_WHILE(t):
    r'\bwhile\b'
    return t

def t_INTEGER(t):
    r'\binteger\b'
    return t

def t_READ(t):
    r'\bread\b'
    return t

def t_PROCEDURE(t):
    r'\bprocedure\b'
    return t

def t_FORWARD(t):
    r'\bforward\b'
    return t

def t_XOR(t):
    r'\bxor\b'
    return t

def t_READLINE(t):
    r'\breadline\b'
    return t

def t_NUMBER(t):
    r'\d+(\.\d+)?(?!([a-zA-Z]))'    
    t.value = float(t.value) if '.' in t.value else int(t.value)
    return t

def t_STRING_LITERAL(t):
    r'\"([^\\\n]|(\\.))*?\"|\'([^\\\n]|(\\.))*?\''
    t.value = t.value[1:-1]
    return t

def t_CHARACTER_LITERAL(t):
    r'\#\d+|\'([^\\\n]|(\\.))*?\'|\"([^\\\n]|(\\.))*?\"'
    if t.value.startswith('#'):
        t.value = int(t.value[1:])  # Convierte #<número> a un entero
    else:
        t.value = t.value[1:-1]  # Elimina las comillas para caracteres entre comillas
    return t

def t_BOOLEAN_LITERAL(t):
    r'\btrue\b|\bfalse\b'
    return t

def t_OUTPUT(t):
    r'output'
    return t

def t_INPUT(t):
    r'input'
    return t

def t_ID(t):
    r'\b[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value.lower(), 'ID')  # Reconoce palabras reservadas
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_comment(t):
    r'\{[^}]*\}'
    pass

def t_comment_multiline(t):
    r'\(\*[^*]*\*\)'
    pass

def t_comment_singleline(t): # Para comentarios con //
    r'//.*'
    pass

def t_ILLEGAL(t):
    r'\d+[a-zA-Z_]+'  # Detecta secuencias ilegales como '2n'
    print(f'Caracter Ilegal: {t.value}')
    t.lexer.skip(len(t.value))  # Salta toda la secuencia ilegal

def t_error(t):
    print(f'Caracter Ilegal: {t.value[0]}')
    t.lexer.skip(1)

# Function to test the lexer
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
    test_from_file("Evaluar_Pascal.txt")
