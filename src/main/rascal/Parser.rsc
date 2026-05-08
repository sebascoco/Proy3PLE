module Parser

import Syntax;
import AST;
import ParseTree;
import IO;
import String;

public Tree parseVeriLang(str source) {
  return parse(#start[Program], source, allowAmbiguity=false);
}

public Tree parseVeriLangFile(loc file) {
  return parseVeriLang(readFile(file));
}

public Program loadProgram(loc file) = implodeProgram(parseVeriLangFile(file));

public Program parseProgram(str source) = implodeProgram(parseVeriLang(source));

public Program implodeProgram(Tree t) {
  str moduleName = collectModuleName(t);
  list[Import] imports = collectImports(t);
  list[BodyDecl] body = collectBodyDecls(t);

  if (moduleName != "") {
    return program(\module(moduleName, imports, body));
  }
  throw "Expected VeriLang program, got: <t>";
}

Program implodeProgram2((Program) `<Module m>`) = program(implodeModule(m));

str collectModuleName(Tree t) {
  switch(t) {
    case appl(prod(sort("Module"), _, _), _): {
      if ((Module) `defmodule <Identifier name> <Imports? _> <Body _> end` := t) {
        return "<name>";
      }
    }
    case appl(_, list[Tree] args): {
      for (Tree arg <- args) {
        str found = collectModuleName(arg);
        if (found != "") {
          return found;
        }
      }
    }
    default: ;
  }
  return "";
}

list[Import] collectImports(Tree t) {
  switch(t) {
    case appl(prod(sort("Import"), _, _), _): {
      return [implodeImport(t)];
    }
    case appl(_, list[Tree] args): {
      list[Import] imports = [];
      for (Tree arg <- args) {
        imports += collectImports(arg);
      }
      return imports;
    }
    default: return [];
  }
}

list[BodyDecl] collectBodyDecls(Tree t) {
  switch(t) {
    case appl(prod(sort("BodyDecl"), _, _), _): {
      return [implodeBodyDecl(t)];
    }
    case appl(_, list[Tree] args): {
      list[BodyDecl] decls = [];
      for (Tree arg <- args) {
        decls += collectBodyDecls(arg);
      }
      return decls;
    }
    default: return [];
  }
}

Module implodeModule((Module) `defmodule <Identifier name> <Imports? imports> <Body body> end`) {
  list[Import] importedModules = [];
  if (imports has top) {
    importedModules = implodeImports(imports.top);
  }
  return \module("<name>", importedModules, implodeBody(body));
}

list[Import] implodeImports(value imports) {
  if ((Imports) `<Import+ importTrees>` := imports) {
    return [implodeImport(i) | Import i <- importTrees];
  }
  return [];
}

Import implodeImport(value importTree) {
  if ((Import) `using <Identifier name>` := importTree) {
    return \import("<name>");
  }
  throw "Expected Import, got: <importTree>";
}

list[BodyDecl] implodeBody((Body) `<BodyDecl* decls>`) =
  [implodeBodyDecl(d) | BodyDecl d <- decls];

BodyDecl implodeBodyDecl(value declTree) {
  if ((BodyDecl) `<Space s>` := declTree) {
    return implodeSpace(s);
  }
  if ((BodyDecl) `<OperatorDef opDef>` := declTree) {
    return implodeOperator(opDef);
  }
  if ((BodyDecl) `<VarBlock vars>` := declTree) {
    return implodeVarBlock(vars);
  }
  if ((BodyDecl) `<RuleDef rule>` := declTree) {
    return implodeRule(rule);
  }
  if ((BodyDecl) `<ExpressionDef expDef>` := declTree) {
    return implodeExpressionDef(expDef);
  }
  throw "Expected BodyDecl, got: <declTree>";
}

BodyDecl implodeSpace((Space) `defspace <Identifier name> <SubSpace sub> end`) =
  space("<name>", [implodeSubSpace(sub)]);

BodyDecl implodeSpace((Space) `defspace <Identifier name> end`) =
  space("<name>", []);

str implodeSubSpace((SubSpace) `\< <Identifier name>`) = "<name>";

BodyDecl implodeOperator((OperatorDef) `defoperator <Identifier name> : <CurryingNotation curry> <Attributes attrs> end`) =
  operatorDef("<name>", implodeCurrying(curry), implodeAttributes(attrs));

BodyDecl implodeOperator((OperatorDef) `defoperator <Identifier name> : <CurryingNotation curry> end`) =
  operatorDef("<name>", implodeCurrying(curry), []);

list[VeriType] implodeCurrying((CurryingNotation) `<{TypeRef "-\>"}+ types>`) =
  [implodeTypeRef(t) | TypeRef t <- types];

VeriType implodeTypeRef((TypeRef) `<Type tp>`) = implodeType(tp);

VeriType implodeType((Type) `<Identifier id>`) {
  switch("<id>") {
    case "Int": return typeInt();
    case "Bool": return typeBool();
    case "Char": return typeChar();
    case "String": return typeString();
    default: return typeUser("<id>");
  }
}

BodyDecl implodeVarBlock((VarBlock) `defvar <VarDef+ defs> end`) =
  varBlock([implodeVarDef(d) | VarDef d <- defs]);

VarDef implodeVarDef((VarDef) `<Identifier name> : <Type tp>`) =
  varDef("<name>", implodeType(tp));

BodyDecl implodeRule((RuleDef) `defrule <OpApplication lhs> -\> <OpApplication rhs> end`) =
  ruleDef(implodeOpApp(lhs), implodeOpApp(rhs));

BodyDecl implodeExpressionDef((ExpressionDef) `defexpression <Expression expr> <Attributes attrs> end`) =
  expressionDecl(implodeExpression(expr), implodeAttributes(attrs));

BodyDecl implodeExpressionDef((ExpressionDef) `defexpression <Expression expr> end`) =
  expressionDecl(implodeExpression(expr), []);

Expression implodeExpression((Expression) `<QuantExpr q>`) = implodeQuant(q);
Expression implodeExpression((Expression) `<OrExpr e>`) = implodeOrExpr(e);

Expression implodeQuant((QuantExpr) `forall <Identifier v> in <Identifier sp> . <Expression body>`) =
  forall("<v>", "<sp>", implodeExpression(body));

Expression implodeQuant((QuantExpr) `exists <Identifier v> in <Identifier sp> . <Expression body>`) =
  exists("<v>", "<sp>", implodeExpression(body));

Expression implodeOrExpr((OrExpr) `<OrExpr l> or <AndExpr r>`) =
  binOp(implodeOrExpr(l), orOp(), implodeAndExpr(r));

Expression implodeOrExpr((OrExpr) `<OrExpr l> =\> <AndExpr r>`) =
  binOp(implodeOrExpr(l), impliesOp(), implodeAndExpr(r));

Expression implodeOrExpr((OrExpr) `<AndExpr e>`) = implodeAndExpr(e);

Expression implodeAndExpr((AndExpr) `<AndExpr l> and <NegExpr r>`) =
  binOp(implodeAndExpr(l), andOp(), implodeNegExpr(r));

Expression implodeAndExpr((AndExpr) `<NegExpr e>`) = implodeNegExpr(e);

Expression implodeNegExpr((NegExpr) `neg <NegExpr e>`) = negExpr(implodeNegExpr(e));
Expression implodeNegExpr((NegExpr) `<CmpExpr e>`) = implodeCmpExpr(e);

Expression implodeCmpExpr((CmpExpr) `<CmpExpr l> = <AddExpr r>`) =
  binOp(implodeCmpExpr(l), eqOp(), implodeAddExpr(r));

Expression implodeCmpExpr((CmpExpr) `<CmpExpr l> != <AddExpr r>`) =
  binOp(implodeCmpExpr(l), neqOp(), implodeAddExpr(r));

Expression implodeCmpExpr((CmpExpr) `<CmpExpr l> \< <AddExpr r>`) =
  binOp(implodeCmpExpr(l), ltOp(), implodeAddExpr(r));

Expression implodeCmpExpr((CmpExpr) `<CmpExpr l> \> <AddExpr r>`) =
  binOp(implodeCmpExpr(l), gtOp(), implodeAddExpr(r));

Expression implodeCmpExpr((CmpExpr) `<CmpExpr l> \<= <AddExpr r>`) =
  binOp(implodeCmpExpr(l), leqOp(), implodeAddExpr(r));

Expression implodeCmpExpr((CmpExpr) `<CmpExpr l> \>= <AddExpr r>`) =
  binOp(implodeCmpExpr(l), geqOp(), implodeAddExpr(r));

Expression implodeCmpExpr((CmpExpr) `<AddExpr e>`) = implodeAddExpr(e);

Expression implodeAddExpr((AddExpr) `<AddExpr l> + <MulExpr r>`) =
  binOp(implodeAddExpr(l), addOp(), implodeMulExpr(r));

Expression implodeAddExpr((AddExpr) `<AddExpr l> - <MulExpr r>`) =
  binOp(implodeAddExpr(l), subOp(), implodeMulExpr(r));

Expression implodeAddExpr((AddExpr) `<MulExpr e>`) = implodeMulExpr(e);

Expression implodeMulExpr((MulExpr) `<MulExpr l> ** <UnaryExpr r>`) =
  binOp(implodeMulExpr(l), powOp(), implodeUnary(r));

Expression implodeMulExpr((MulExpr) `<MulExpr l> * <UnaryExpr r>`) =
  binOp(implodeMulExpr(l), mulOp(), implodeUnary(r));

Expression implodeMulExpr((MulExpr) `<MulExpr l> / <UnaryExpr r>`) =
  binOp(implodeMulExpr(l), divOp(), implodeUnary(r));

Expression implodeMulExpr((MulExpr) `<MulExpr l> % <UnaryExpr r>`) =
  binOp(implodeMulExpr(l), modOp(), implodeUnary(r));

Expression implodeMulExpr((MulExpr) `<UnaryExpr e>`) = implodeUnary(e);

Expression implodeUnary((UnaryExpr) `- <UnaryExpr e>`) = unaryMinus(implodeUnary(e));
Expression implodeUnary((UnaryExpr) `<Primary p>`) = implodePrimary(p);

Expression implodePrimary((Primary) `( <Expression e> )`) = implodeExpression(e);
Expression implodePrimary((Primary) `<OpApplication a>`) = opApp(implodeOpApp(a));
Expression implodePrimary((Primary) `<Literal lit>`) = litExpr(implodeLiteral(lit));
Expression implodePrimary((Primary) `<Identifier id>`) = idExpr("<id>");

OpApplication implodeOpApp((OpApplication) `( <Identifier name> <Arg* args> )`) =
  applyOp("<name>", [implodeArg(a) | Arg a <- args]);

Expression implodeArg((Arg) `<Primary p>`) = implodePrimary(p);

list[Attribute] implodeAttributes((Attributes) `[ <AttrItem+ items> ]`) =
  [implodeAttrItem(i) | AttrItem i <- items];

Attribute implodeAttrItem((AttrItem) `<Identifier key>`) = attrKey("<key>");
Attribute implodeAttrItem((AttrItem) `<Identifier key> : <AttrVal v>`) = attr("<key>", implodeAttrVal(v));

AttrVal implodeAttrVal((AttrVal) `<Identifier id>`) = attrId("<id>");
AttrVal implodeAttrVal((AttrVal) `<Literal lit>`) = attrLit(implodeLiteral(lit));

Literal implodeLiteral((Literal) `<IntLit n>`) = intLit(toInt("<n>"));
Literal implodeLiteral((Literal) `True`) = boolLit(true);
Literal implodeLiteral((Literal) `False`) = boolLit(false);
Literal implodeLiteral((Literal) `None`) = nullLit();

Literal implodeLiteral((Literal) `<StrLit s>`) {
  str raw = "<s>";
  return strLit(substring(raw, 1, size(raw) - 1));
}

Literal implodeLiteral((Literal) `<CharLit c>`) {
  str raw = "<c>";
  return charLit(substring(raw, 1, size(raw) - 1));
}

Literal implodeLiteral((Literal) `<NullVal _>`) = nullLit();
