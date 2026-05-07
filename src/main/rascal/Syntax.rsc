module Syntax

extend lang::std::Layout;

keyword Keywords
  = "defmodule" | "using" | "defspace" | "defoperator"
  | "defvar" | "defrule" | "defexpression" | "end"
  | "forall" | "exists" | "in" | "and" | "or" | "neg"
  | "True" | "False" | "None"
  | "Int" | "Bool" | "Char" | "String"
  ;

lexical Letter = [a-zA-Z];
lexical Digit = [0-9];
lexical IdChar = [a-zA-Z0-9\-];
lexical Identifier = Letter IdChar* !>> [a-zA-Z0-9\-] \ Keywords;
lexical IntLit = Digit+;
lexical StrLit = "\"" ![\"]* "\"";
lexical CharLit = "\'" ![\'] "\'";
lexical NullVal = "\u00f8" | "\u2205";

start syntax Program = Module;

syntax Module
  = "defmodule" Identifier Imports? Body "end"
  ;

syntax Imports = Import+;
syntax Import = "using" Identifier;
syntax Body = BodyDecl*;

syntax BodyDecl
  = Space
  | OperatorDef
  | VarBlock
  | RuleDef
  | ExpressionDef
  ;

syntax Space = "defspace" Identifier SubSpace? "end";
syntax SubSpace = "\<" Identifier;

syntax OperatorDef
  = "defoperator" Identifier ":" CurryingNotation Attributes? "end"
  ;

syntax CurryingNotation = {TypeRef "-\>"}+;
syntax TypeRef = Type;

syntax Type
  = "Int"
  | "Bool"
  | "Char"
  | "String"
  | Identifier
  ;

syntax VarBlock = "defvar" VarDef+ "end";
syntax VarDef = Identifier ":" Type;

syntax RuleDef = "defrule" OpApplication "-\>" OpApplication "end";
syntax ExpressionDef = "defexpression" Expression Attributes? "end";

syntax Expression
  = QuantExpr
  | OrExpr
  ;

syntax QuantExpr
  = "forall" Identifier "in" Identifier "." Expression
  | "exists" Identifier "in" Identifier "." Expression
  ;

syntax OrExpr
  = left OrExpr "or" AndExpr
  | left OrExpr "=\>" AndExpr
  | AndExpr
  ;

syntax AndExpr
  = left AndExpr "and" NegExpr
  | NegExpr
  ;

syntax NegExpr
  = "neg" NegExpr
  | CmpExpr
  ;

syntax CmpExpr
  = left CmpExpr "=" AddExpr
  | left CmpExpr "!=" AddExpr
  | left CmpExpr "\<" AddExpr
  | left CmpExpr "\>" AddExpr
  | left CmpExpr "\<=" AddExpr
  | left CmpExpr "\>=" AddExpr
  | AddExpr
  ;

syntax AddExpr
  = left AddExpr "+" MulExpr
  | left AddExpr "-" MulExpr
  | MulExpr
  ;

syntax MulExpr
  = left MulExpr "**" UnaryExpr
  | left MulExpr "*" UnaryExpr
  | left MulExpr "/" UnaryExpr
  | left MulExpr "%" UnaryExpr
  | UnaryExpr
  ;

syntax UnaryExpr
  = "-" UnaryExpr
  | Primary
  ;

syntax Primary
  = "(" Expression ")"
  | OpApplication
  | Literal
  | Identifier
  ;

syntax OpApplication = "(" Identifier Arg* ")";
syntax Arg = Primary;

syntax Attributes = "[" AttrItem+ "]";
syntax AttrItem = Identifier | Identifier ":" AttrVal;
syntax AttrVal = Identifier | Literal;

syntax Literal
  = IntLit
  | "True"
  | "False"
  | "None"
  | StrLit
  | CharLit
  | NullVal
  ;
