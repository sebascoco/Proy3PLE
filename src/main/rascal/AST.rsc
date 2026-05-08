module AST

// ─── Top-level ────────────────────────────────────────────────────────────────
data Program = program(Module m);

data Module = \module(str name, list[Import] imports, list[BodyDecl] body);

data Import = \import(str moduleName);

// ─── Body declarations ────────────────────────────────────────────────────────
data BodyDecl
  = space(str name, list[str] subSpaces)
  | operatorDef(str name, list[VeriType] signature, list[Attribute] attributes)
  | varBlock(list[VarDef] defs)
  | ruleDef(OpApplication lhs, OpApplication rhs)
  | expressionDecl(Expression expr, list[Attribute] attributes)
  ;

// ─── Types (Project 3) ───────────────────────────────────────────────────────
data VeriType
  = typeInt()
  | typeBool()
  | typeChar()
  | typeString()
  | typeUser(str name)       // user-defined type (space name)
  ;

// ─── Variables ───────────────────────────────────────────────────────────────
data VarDef = varDef(str varName, VeriType varType);

// ─── Attributes ──────────────────────────────────────────────────────────────
data Attribute
  = attr(str key, AttrVal val)
  | attrKey(str key)
  ;

data AttrVal
  = attrId(str name)
  | attrLit(Literal lit)
  ;

// ─── Expressions ─────────────────────────────────────────────────────────────
data Expression
  = forall(str var, str space, Expression body)
  | exists(str var, str space, Expression body)
  | binOp(Expression lhs, BinOp op, Expression rhs)
  | negExpr(Expression e)
  | unaryMinus(Expression e)
  | opApp(OpApplication app)
  | idExpr(str name)
  | litExpr(Literal lit)
  ;

data BinOp
  = andOp() | orOp() | equivOp() | impliesOp()
  | eqOp() | ltOp() | gtOp() | leqOp() | geqOp() | neqOp()
  | addOp() | subOp() | mulOp() | divOp() | powOp() | modOp()
  ;

// ─── Operator application ─────────────────────────────────────────────────────
data OpApplication = applyOp(str operator, list[Expression] args);

// ─── Literals ────────────────────────────────────────────────────────────────
data Literal
  = intLit(int intValue)
  | boolLit(bool boolValue)
  | charLit(str charValue)
  | strLit(str stringValue)
  | nullLit()
  ;
