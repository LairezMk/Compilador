import ply.lex as lex

'''Tomas Marulanda Aristizabal
Samuel Herrera Marin
Jonathan Gaviria Ocampo'''

# List of token names
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
    'ID'  # Identificadores

)

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

def t_CASE(t):
    r'\bcase\b'
    return t

def t_CHAR(t):
    r'\bchar\b'
    return t

def t_CONST(t):
    r'\bconst\b'
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

def t_REPEAT(t):
    r'\brepeat\b'
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

def t_XOR(t):
    r'\bxor\b'
    return t

def t_NUMBER(t):
    r'\b\d+\b'
    t.value = int(t.value)
    return t

def t_STRING_LITERAL(t):
    r'\"([^\\\n]|(\\.))*?\"|\'([^\\\n]|(\\.))*?\''
    t.value = t.value[1:-1]
    return t

def t_CHARACTER_LITERAL(t):
    r'\'([^\\\n]|(\\.))*?\'|\"([^\\\n]|(\\.))*?\"'
    t.value = t.value[1:-1]
    return t

def t_BOOLEAN_LITERAL(t):
    r'true|false'
    return t

def t_OUTPUT(t):
    r'output'
    return t

def t_INPUT(t):
    r'input'
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    return t

# Definir un contador de líneas
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Definir comentarios
def t_comment(t):
    r'\{[^}]*\}'
    pass

def t_comment_multiline(t):
    r'\(\*[^*]*\*\)'
    pass

def t_error(t):
    print("Ilegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Function to test the lexer
def test(data):
    lexer = lex.lex()  # Initialize the lexer
    lexer.input(data)
    
    while True:
        tok = lexer.token()
        if not tok:
            break 
        print(f'Token: {tok.type}, Value: {tok.value}')

data = '''program MergeSortPascal;
uses crt;

const
  MAX = 100;

type
  TArray = array[1..MAX] of integer;

procedure Merge(var A: TArray; izquierda, medio, derecha: integer);
var
  i, j, k: integer;
  n1, n2: integer;
  L, R: TArray;
begin
  n1 := medio - izquierda + 1;
  n2 := derecha - medio;

  { Copiar datos a arreglos temporales L[] y R[] }
  for i := 1 to n1 do
    L[i] := A[izquierda + i - 1];
  for j := 1 to n2 do
    R[j] := A[medio + j];

  i := 1; j := 1; k := izquierda;

  { Mezclar los arreglos L[] y R[] en A[] }
  while (i <= n1) and (j <= n2) do
  begin
    if L[i] <= R[j] then
    begin
      A[k] := L[i];
      i := i + 1;
    end
    else
    begin
      A[k] := R[j];
      j := j + 1;
    end;
    k := k + 1;
  end;

  { Copiar los elementos restantes de L[], si hay alguno }
  while i <= n1 do
  begin
    A[k] := L[i];
    i := i + 1;
    k := k + 1;
  end;

  { Copiar los elementos restantes de R[], si hay alguno }
  while j <= n2 do
  begin
    A[k] := R[j];
    j := j + 1;
    k := k + 1;
  end;
end;

procedure MergeSort(var A: TArray; izquierda, derecha: integer);
var
  medio: integer;
begin
  if izquierda < derecha then
  begin
    medio := (izquierda + derecha) div 2;

    { Ordenar la primera y segunda mitad }
    MergeSort(A, izquierda, medio);
    MergeSort(A, medio + 1, derecha);

    { Mezclar ambas mitades ordenadas }
    Merge(A, izquierda, medio, derecha);
  end;
end;

var
  A: TArray;
  i, n: integer;
begin
  clrscr;
  writeln('Ordenamiento por Mezclas (Merge Sort) en Pascal');
  write('Ingrese la cantidad de elementos: ');
  readln(n);

  writeln('Ingrese los elementos del arreglo:');
  for i := 1 to n do
    readln(A[i]);

  MergeSort(A, 1, n);

  writeln('Arreglo ordenado:');
  for i := 1 to n do
    write(A[i], ' ');

  readln;
end.'''
test(data)
