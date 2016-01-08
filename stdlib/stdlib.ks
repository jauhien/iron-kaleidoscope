# Built-in functions
extern printd(x);
extern putchard(x);

# Logical unary not.
def unary!(v)
  if v then
    0
  else
    1;

# Unary negate.
def unary-(v)
  0-v;

# Define > with the same precedence as <.
def binary> 10 (LHS RHS)
  RHS < LHS;

# Binary logical or, which does not short circuit.
def binary| 5 (LHS RHS)
  if LHS then
    1
  else if RHS then
    1
  else
    0;

# Binary logical and, which does not short circuit.
def binary& 6 (LHS RHS)
  if !LHS then
    0
  else
    !!RHS;

# Define ~ with slightly lower precedence than relationals (symbol = is busy by assignmnet).
def binary ~ 9 (LHS RHS)
  !(LHS < RHS | LHS > RHS);

# Define ':' for sequencing: as a low-precedence operator that ignores operands
# and just returns the RHS.
def binary : 1 (x y) y;

# Startup message

def newline() putchard(10);

def putspace() putchard(32);

def puthash() putchard(35);

def putstar() putchard(42);

def starline() (for i = 0, i < 80 in putstar()) : newline();

def start() starline() :
    puthash() : (for i = 0, i < 23 in putspace()) :
    putspace() : putchard(73) : putchard(114) :
    putchard(111) : putchard(110) : putspace() :
    putchard(75) : putchard(97) : putchard(108) : putchard(101) : putchard(105) :
    putchard(100) : putchard(111) : putchard(115) : putchard(99) :
    putchard(111) : putchard(112) : putchard(101) : putspace() :
    putchard(115) : putchard(116) : putchard(100) : putchard(108) :
    putchard(105) : putchard(98) : putspace() :
    putchard(108) : putchard(111) : putchard(97) : putchard(100) : putchard(101) : putchard(100) :
    (for i = 0, i < 23 in putspace()) :
    puthash() : newline() :
    starline()

start();
