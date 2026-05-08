module Main

// Main entry point for VeriLang (Project 3).
// Parses .vl files, runs the interpreter, and runs the type checker.

import Syntax;
import AST;
import Parser;
import Interpreter;
import TypeChecker;
import IO;
import ParseTree;

// ─── Parse & display parse tree ───────────────────────────────────────────────
void parseAndShow(loc file) {
  println("=== Parsing: <file> ===");
  try {
    Tree t = parseVeriLangFile(file);
    println("✓ Parse succeeded!");
    println("<t>");
  } catch e: {
    println("✗ Parse error: <e>");
  }
}

void parseAndShowString(str src) {
  println("=== Parsing string ===");
  try {
    Tree t = parseVeriLang(src);
    println("✓ Parse succeeded!");
    println("<t>");
  } catch e: {
    println("✗ Parse error: <e>");
  }
}

// ─── Parse, type-check, and run a .vl file ────────────────────────────────────
void run(loc file) {
  // Step 1 – parse
  println("=== Step 1: Parsing <file> ===");
  Program prog = program(\module("", [], []));
  try {
    prog = loadProgram(file);
    println("✓ Parse OK");
  } catch e: {
    println("✗ Parse failed: <e>");
    return;
  }

  // Step 2 – type check
  println("\n=== Step 2: Type checking ===");
  list[TypeError] errors = checkProgram(prog);
  printErrors(errors);

  // Step 3 – interpret / run (even if there are warnings)
  int errorCount = size([e | e <- errors, typeError(_) := e]);
  if (errorCount > 0) {
    println("\n✗ Refusing to run program with type errors.");
  } else {
    println("\n=== Step 3: Running program ===");
    runProgram(prog);
  }
}

// Same as run but from a source string
void runStr(str source) {
  println("=== Step 1: Parsing ===");
  Program prog = program(\module("", [], []));
  try {
    prog = parseProgram(source);
    println("✓ Parse OK");
  } catch e: {
    println("✗ Parse failed: <e>");
    return;
  }

  println("\n=== Step 2: Type checking ===");
  list[TypeError] errors = checkProgram(prog);
  printErrors(errors);

  int errorCount = size([e | e <- errors, typeError(_) := e]);
  if (errorCount > 0) {
    println("\n✗ Refusing to run program with type errors.");
  } else {
    println("\n=== Step 3: Running program ===");
    runProgram(prog);
  }
}

// ─── Quick tests ──────────────────────────────────────────────────────────────
void testMinimal() {
  runStr("defmodule test end");
}

void testSet() {
  run(|project://VeriLang3/examples/Set.vl|);
}

void testTypes() {
  run(|project://VeriLang3/examples/TypeTest.vl|);
}

void testErrors() {
  run(|project://VeriLang3/examples/TypeErrors.vl|);
}
