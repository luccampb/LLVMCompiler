#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::sys;

FILE *pFile;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns one of these for known things.
enum TOKEN_TYPE {

  IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
  ASSIGN = int('='), // '='

  // delimiters
  LBRA = int('{'),  // left brace
  RBRA = int('}'),  // right brace
  LPAR = int('('),  // left parenthesis
  RPAR = int(')'),  // right parenthesis
  SC = int(';'),    // semicolon
  COMMA = int(','), // comma

  // types
  INT_TOK = -2,   // "int"
  VOID_TOK = -3,  // "void"
  FLOAT_TOK = -4, // "float"
  BOOL_TOK = -5,  // "bool"

  // keywords
  EXTERN = -6,  // "extern"
  IF = -7,      // "if"
  ELSE = -8,    // "else"
  WHILE = -9,   // "while"
  RETURN = -10, // "return"
  // TRUE   = -12,     // "true"
  // FALSE   = -13,     // "false"

  // literals
  INT_LIT = -14,   // [0-9]+
  FLOAT_LIT = -15, // [0-9]+.[0-9]+
  BOOL_LIT = -16,  // "true" or "false" key words

  // logical operators
  AND = -17, // "&&"
  OR = -18,  // "||"

  // operators
  PLUS = int('+'),    // addition or unary plus
  MINUS = int('-'),   // substraction or unary negative
  ASTERIX = int('*'), // multiplication
  DIV = int('/'),     // division
  MOD = int('%'),     // modular
  NOT = int('!'),     // unary negation

  // comparison operators
  EQ = -19,      // equal
  NE = -20,      // not equal
  LE = -21,      // less than or equal to
  LT = int('<'), // less than
  GE = -23,      // greater than or equal to
  GT = int('>'), // greater than

  // special tokens
  EOF_TOK = 0, // signal end of file

  // invalid
  INVALID = -100 // signal invalid token
};

// TOKEN struct is used to keep track of information about a token
struct TOKEN {
  int type = -100;
  std::string lexeme;
  int lineNo;
  int columnNo;
};

static std::string IdentifierStr; // Filled in if IDENT
static int IntVal;                // Filled in if INT_LIT
static bool BoolVal;              // Filled in if BOOL_LIT
static float FloatVal;            // Filled in if FLOAT_LIT
static std::string StringVal;     // Filled in if String Literal
static int lineNo, columnNo;

static TOKEN returnTok(std::string lexVal, int tok_type) {
  TOKEN return_tok;
  return_tok.lexeme = lexVal;
  return_tok.type = tok_type;
  return_tok.lineNo = lineNo;
  return_tok.columnNo = columnNo - lexVal.length() - 1;
  return return_tok;
}

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
static TOKEN gettok() {

  static int LastChar = ' ';
  static int NextChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar)) {
    if (LastChar == '\n' || LastChar == '\r') {
      lineNo++;
      columnNo = 1;
    }
    LastChar = getc(pFile);
    columnNo++;
  }

  if (isalpha(LastChar) ||
      (LastChar == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
    IdentifierStr = LastChar;
    columnNo++;

    while (isalnum((LastChar = getc(pFile))) || (LastChar == '_')) {
      IdentifierStr += LastChar;
      columnNo++;
    }

    if (IdentifierStr == "int")
      return returnTok("int", INT_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "float")
      return returnTok("float", FLOAT_TOK);
    if (IdentifierStr == "void")
      return returnTok("void", VOID_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "extern")
      return returnTok("extern", EXTERN);
    if (IdentifierStr == "if")
      return returnTok("if", IF);
    if (IdentifierStr == "else")
      return returnTok("else", ELSE);
    if (IdentifierStr == "while")
      return returnTok("while", WHILE);
    if (IdentifierStr == "return")
      return returnTok("return", RETURN);
    if (IdentifierStr == "true") {
      BoolVal = true;
      return returnTok("true", BOOL_LIT);
    }
    if (IdentifierStr == "false") {
      BoolVal = false;
      return returnTok("false", BOOL_LIT);
    }

    return returnTok(IdentifierStr.c_str(), IDENT);
  }

  if (LastChar == '=') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // EQ: ==
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("==", EQ);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("=", ASSIGN);
    }
  }

  if (LastChar == '{') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("{", LBRA);
  }
  if (LastChar == '}') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("}", RBRA);
  }
  if (LastChar == '(') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("(", LPAR);
  }
  if (LastChar == ')') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(")", RPAR);
  }
  if (LastChar == ';') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(";", SC);
  }
  if (LastChar == ',') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(",", COMMA);
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9]+.
    std::string NumStr;

    if (LastChar == '.') { // Floatingpoint Number: .[0-9]+
      do {
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      FloatVal = strtof(NumStr.c_str(), nullptr);
      return returnTok(NumStr, FLOAT_LIT);
    } else {
      do { // Start of Number: [0-9]+
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      if (LastChar == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
        do {
          NumStr += LastChar;
          LastChar = getc(pFile);
          columnNo++;
        } while (isdigit(LastChar));

        FloatVal = strtof(NumStr.c_str(), nullptr);
        return returnTok(NumStr, FLOAT_LIT);
      } else { // Integer : [0-9]+
        IntVal = strtod(NumStr.c_str(), nullptr);
        return returnTok(NumStr, INT_LIT);
      }
    }
  }

  if (LastChar == '&') {
    NextChar = getc(pFile);
    if (NextChar == '&') { // AND: &&
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("&&", AND);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("&", int('&'));
    }
  }

  if (LastChar == '|') {
    NextChar = getc(pFile);
    if (NextChar == '|') { // OR: ||
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("||", OR);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("|", int('|'));
    }
  }

  if (LastChar == '!') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // NE: !=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("!=", NE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("!", NOT);
      ;
    }
  }

  if (LastChar == '<') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // LE: <=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("<=", LE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("<", LT);
    }
  }

  if (LastChar == '>') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // GE: >=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok(">=", GE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok(">", GT);
    }
  }

  if (LastChar == '/') { // could be division or could be the start of a comment
    LastChar = getc(pFile);
    columnNo++;
    if (LastChar == '/') { // definitely a comment
      do {
        LastChar = getc(pFile);
        columnNo++;
      } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

      if (LastChar != EOF)
        return gettok();
    } else
      return returnTok("/", DIV);
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF) {
    columnNo++;
    return returnTok("0", EOF_TOK);
  }

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  std::string s(1, ThisChar);
  LastChar = getc(pFile);
  columnNo++;
  return returnTok(s, int(ThisChar));
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

static TOKEN getNextToken() {

  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok = temp;
}

static void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// ASTnode - Base class for all AST nodes.
class ASTnode {
public:
  virtual ~ASTnode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string() const {return "";};
};

//IDENT in rval ::= production
class IdentASTNode : public ASTnode {
  TOKEN Ident;

public:
  IdentASTNode(TOKEN ident) : Ident(ident) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nIdent";
    str += "\n  ";
    str += Ident.lexeme;
    return str;
  }
};

class ParamASTNode : public ASTnode {
  TOKEN VarType;
  std::unique_ptr<IdentASTNode> Ident;

public:
  ParamASTNode(TOKEN vartype, std::unique_ptr<IdentASTNode> ident) : VarType(vartype), Ident(std::move(ident)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nParam";
    str += "\n  ";
    str += VarType.lexeme;
    str += "\n  ";
    str += Ident->to_string();
    return str;
  }
};

class ParamsASTNode : public ASTnode {
public:
  virtual ~ParamsASTNode() {}
};

class ListParamsASTNode : public ParamsASTNode {
public:
  std::vector<std::unique_ptr<ParamASTNode>> ParamList;
  ListParamsASTNode(std::vector<std::unique_ptr<ParamASTNode>> paramlist) : ParamList(std::move(paramlist)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nParams";
    for(auto&& param : ParamList) {
      str += "\n  ";
      str += param->to_string();
    }
    return str;
  }
};

class VoidParamsASTNode : public ParamsASTNode {
public:
  VoidParamsASTNode() {}
  Value *codegen() override {}
  std::string to_string() const override {
    return "Void Param";
  }
};

// ExternASTNode - Class for extern declarations
class ExternASTNode : public ASTnode {
  TOKEN VarType;
  std::unique_ptr<IdentASTNode> Ident;
  std::unique_ptr<ParamsASTNode> Params;

public:
  ExternASTNode(TOKEN vartype, std::unique_ptr<IdentASTNode> ident, std::unique_ptr<ParamsASTNode> params) : VarType(vartype), Ident(std::move(ident)), Params(std::move(params)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nExtern";
    str += "\n  ";
    str += VarType.lexeme;
    str += "\n  ";
    str += Ident->to_string();
    if(Params != nullptr) {
      str += "\n  ";
      str += Params->to_string();
    }    
    return str;
  }
};

class LocalDeclASTNode : public ASTnode {
  TOKEN VarType;
  std::unique_ptr<IdentASTNode> Ident;

public:
  LocalDeclASTNode(TOKEN vartype, std::unique_ptr<IdentASTNode> ident) : VarType(vartype), Ident(std::move(ident)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nBlock";
    str += "\n  ";
    str += VarType.lexeme;
    str += "\n  ";
    str += Ident->to_string();
    return str;
  }
};

class StmtASTNode : public ASTnode {
public:
  virtual ~StmtASTNode() {}
};

class BlockASTNode : public StmtASTNode {
public:
  std::vector<std::unique_ptr<LocalDeclASTNode>> LocalDecls;
  std::vector<std::unique_ptr<StmtASTNode>> Statements;  
  BlockASTNode(std::vector<std::unique_ptr<LocalDeclASTNode>> localdecls, std::vector<std::unique_ptr<StmtASTNode>> statements) : LocalDecls(std::move(localdecls)), Statements(std::move(statements)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nBlock";
    for(auto&& localD : LocalDecls) {
      str += "\n  ";
      str += localD->to_string();
    }
    for(auto&& s : Statements) {
      str += "\n  ";
      str += s->to_string();
    }
    return str;
  }
};

class DeclASTNode : public ASTnode {
public:
  DeclASTNode() {}
  Value *codegen() override {}
  std::string to_string() const override {
    return "Decl";
  }
};

// FunDeclASTNode - Class for function declarations
class FunDeclASTNode : public DeclASTNode {
  TOKEN VarType;
  std::unique_ptr<IdentASTNode> Ident;
  std::unique_ptr<ParamsASTNode> Params;
  std::unique_ptr<BlockASTNode> Block;

public:
  FunDeclASTNode(TOKEN vartype, std::unique_ptr<IdentASTNode> ident, std::unique_ptr<ParamsASTNode> params, std::unique_ptr<BlockASTNode> block) : VarType(vartype), Ident(std::move(ident)), Params(std::move(params)), Block(std::move(block)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nFun_Decl";
    str += "\n  ";
    str += VarType.lexeme;
    str += "\n  ";
    str += Ident->to_string();    
    if(Params != nullptr){
      str += "\n  ";
      str += Params->to_string();
    }
    if(Block != nullptr) {
      str += "\n  ";
      str += Block->to_string();
    }    
    return str;
  }
};

class VarDeclASTNode : public DeclASTNode {
  TOKEN VarType;
  std::unique_ptr<IdentASTNode> Ident;

public:
  VarDeclASTNode(TOKEN vartype, std::unique_ptr<IdentASTNode> ident) : VarType(vartype), Ident(std::move(ident)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nVar_Decl";
    str += "\n  ";
    str += VarType.lexeme;
    str += "\n  ";
    str += Ident->to_string();
    return str;
  }
};

class ElseStmtASTNode : public StmtASTNode {
  std::unique_ptr<BlockASTNode> Block;

public:
  ElseStmtASTNode(std::unique_ptr<BlockASTNode> block) : Block(std::move(block)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nElse Statement";
    if(Block != nullptr) {
      str += "\n  ";
      str += Block->to_string();
    }    
    return str;
  }
};

class RValASTNode : public ASTnode {
public:
  RValASTNode() {}
};

//IDENT in rval ::= production
class IdentRvalASTNode : public RValASTNode {
  std::unique_ptr<IdentASTNode> Ident;

public:
  IdentRvalASTNode(std::unique_ptr<IdentASTNode> ident) : Ident(std::move(ident)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nRval";
    str += "\n  ";
    str += Ident->to_string();
    return str;
  }
};

//Expression Superclass
class ExprASTNode : public RValASTNode {
public:
  virtual ~ExprASTNode() {}
};

class ExprStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;
public:
  ExprStmtASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nExpr Statement";
    if(Expr != nullptr) {
      str += "\n  ";
      str += Expr->to_string();
    }    
    return str;
  }
};

class IfStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;
  std::unique_ptr<BlockASTNode> Block;
  std::unique_ptr<ElseStmtASTNode> ElseStmt;

public:
  IfStmtASTNode(std::unique_ptr<ExprASTNode> expr, std::unique_ptr<BlockASTNode> block, std::unique_ptr<ElseStmtASTNode> elsestmt) : Expr(std::move(expr)), Block(std::move(block)), ElseStmt(std::move(elsestmt)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nIf Statement";
    str += "\n  ";
    str += Expr->to_string();
    if(Block!=nullptr) {
      str += "\n  ";
      str += Block->to_string();
    }    
    if(ElseStmt != nullptr) {
      str += "\n  ";
      str += ElseStmt->to_string();
    }    
    return str;
  }
};

class WhileStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;
  std::unique_ptr<StmtASTNode> Statement;

public:
  WhileStmtASTNode(std::unique_ptr<ExprASTNode> expr, std::unique_ptr<StmtASTNode> statement) : Expr(std::move(expr)), Statement(std::move(statement)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nWhile Statement";
    str += "\n  ";
    str += Expr->to_string();
    str += "\n  ";
    str += Statement->to_string();
    return str;
  }
};

class ReturnStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;

public:
  ReturnStmtASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nReturn Statement";
    if(Expr!=nullptr) {
      str += "\n  ";
      str += Expr->to_string();
    }    
    return str;
  }
};

/// UnaryOpRValASTNode
class UnaryOpRValASTNode : public RValASTNode {
  TOKEN Op;
  std::unique_ptr<RValASTNode> RVal;
public:
  UnaryOpRValASTNode(TOKEN op, std::unique_ptr<RValASTNode> rval) : Op(op), RVal(std::move(rval)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nUnary Operator Rval";
    str += "\n  ";
    str += Op.lexeme;
    str += "\n  ";
    str += RVal->to_string();
    return str;
  }
};

//expr ::= IDENT "=" expr in production
class AssignmentExprASTNode : public ExprASTNode {
  std::unique_ptr<IdentASTNode> Ident;
  std::unique_ptr<ExprASTNode> SubExpr;

public:
  AssignmentExprASTNode(std::unique_ptr<IdentASTNode> ident, std::unique_ptr<ExprASTNode> subexpr) : Ident(std::move(ident)), SubExpr(std::move(subexpr)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nExpr";
    str += "\n  ";
    str += Ident->to_string();
    str += "\n  ";
    str += SubExpr->to_string();
    return str;
  }
};

class ArgListASTNode : public ASTnode {
public:
  std::vector<std::unique_ptr<ExprASTNode>> Exprs;
  ArgListASTNode(std::vector<std::unique_ptr<ExprASTNode>> exprs) : Exprs(std::move(exprs)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nRval";
    for(auto&& expr : Exprs) {
      str += "\n  ";
      str += expr->to_string();
    }
    return str;
  }
};

//Function call in rval ::= production
class FunctionCallASTNode : public RValASTNode {
  std::unique_ptr<IdentASTNode> FuncName;
  std::unique_ptr<ArgListASTNode> Args;
public:
  FunctionCallASTNode(std::unique_ptr<IdentASTNode> funcname, std::unique_ptr<ArgListASTNode> args) : FuncName(std::move(funcname)), Args(std::move(args)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nRval";
    str += "\n  ";
    str += FuncName->to_string();
    str += "\n  ";
    str += Args->to_string();
    return str;
  }
};

/// IntASTNode - Class for integer literals like 1, 2, 10,
class IntASTNode : public RValASTNode {
  int Val;
  TOKEN Tok;

public:
  IntASTNode(int val, TOKEN tok) : Val(val), Tok(tok) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nInt";
    str += "\n  ";
    str += std::to_string(Val);
    return str;
  }
};

/// BoolASTNode - Class for boolean literals true and false
class BoolASTNode : public RValASTNode {
  bool Val;
  TOKEN Tok;

public:
  BoolASTNode(bool val, TOKEN tok) : Val(val), Tok(tok) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nBool";
    str += "\n  ";
    str += std::to_string(Val);
    return str;
  }
};

/// FloatASTNode - Class for float literals like 1.4, 3.5, 60
class FloatASTNode : public RValASTNode {
  float Val;
  TOKEN Tok;

public:
  FloatASTNode(float val, TOKEN tok) : Val(val), Tok(tok) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nFloat";
    str += "\n  ";
    str += std::to_string(Val);
    return str;
  }
};

class TimesASTNode : public ASTnode {
  
public:
  std::vector<std::unique_ptr<RValASTNode>> RVals;
  std::vector<TOKEN> Ops;
  TimesASTNode(std::vector<std::unique_ptr<RValASTNode>>&& rvals, std::vector<TOKEN> ops) : RVals(std::move(rvals)), Ops(ops) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nOperator_Times";
    for (int index = 0; index < RVals.size(); ++index) {
      str += "\n  ";
      str += RVals.at(index)->to_string();
      if(index != RVals.size()-1) {
        str += "\n  ";
        str += Ops.at(index).lexeme;
      }      
    }
    return str;
  }
};

class AddASTNode : public ASTnode {
  
public:
  std::vector<std::unique_ptr<TimesASTNode>> Times;
  std::vector<TOKEN> Ops;
  AddASTNode(std::vector<std::unique_ptr<TimesASTNode>> times, std::vector<TOKEN> ops) : Times(std::move(times)), Ops(ops) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nOperator_Add";
    for (int index = 0; index < Times.size(); ++index) {
      str += "\n  ";
      str += Times.at(index)->to_string();
      if(index != Times.size()-1) {
        str += "\n  ";
        str += Ops.at(index).lexeme;
      }      
    }
    return str;
  }
};

class CompASTNode : public ASTnode {
  
public:
  std::vector<std::unique_ptr<AddASTNode>> Adds;
  std::vector<TOKEN> Ops;
  CompASTNode(std::vector<std::unique_ptr<AddASTNode>> adds, std::vector<TOKEN> ops) : Adds(std::move(adds)), Ops(ops) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nOperator_Comp";
    for (int index = 0; index < Adds.size(); ++index) {
      str += "\n  ";
      str += Adds.at(index)->to_string();
      if(index != Adds.size()-1) {
        str += "\n  ";
        str += Ops.at(index).lexeme;
      }      
    }
    return str;
  }
};

class EquivASTNode : public ASTnode {
  
public:
  std::vector<std::unique_ptr<CompASTNode>> Comps;
  std::vector<TOKEN> Ops;
  EquivASTNode(std::vector<std::unique_ptr<CompASTNode>> comps, std::vector<TOKEN> ops) : Comps(std::move(comps)), Ops(ops) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nOperator_Equiv";
    for (int index = 0; index < Comps.size(); ++index) {
      str += "\n  ";
      str += Comps.at(index)->to_string();
      if(index != Comps.size()-1) {
        str += "\n  ";
        str += Ops.at(index).lexeme;
      }      
    }
    return str;
  }
};

class AndASTNode : public ASTnode {
  
public:
  std::vector<std::unique_ptr<EquivASTNode>> Equivs;
  AndASTNode(std::vector<std::unique_ptr<EquivASTNode>> equivs) : Equivs(std::move(equivs)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nOperator_And";
    for (int index = 0; index < Equivs.size(); ++index) {
      str += "\n  ";
      str += Equivs.at(index)->to_string();
      if(index != Equivs.size()-1) {
        str += "\n  &&";
      }      
    }
    return str;
  }
};

class OrASTNode : public ASTnode {
  
public:
  std::vector<std::unique_ptr<AndASTNode>> Ands;  
  OrASTNode(std::vector<std::unique_ptr<AndASTNode>> ands) : Ands(std::move(ands)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nOperator_Or";
    for (int index = 0; index < Ands.size(); ++index) {
      str += "\n  ";
      str += Ands.at(index)->to_string();
      if(index != Ands.size()-1) {
        str += "\n  ||";
      }      
    }
    return str;
  }
};

//expr ::= operator_or in production
class OrExprASTNode : public ExprASTNode {
  std::unique_ptr<OrASTNode> OrExpression;

public:
  OrExprASTNode(std::unique_ptr<OrASTNode> orexpression) : OrExpression(std::move(orexpression)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "\nExpr";
    str += "\n  ";
    str += OrExpression->to_string();
    return str;
  }
};

/// ProgramASTNode - Class for program
class ProgramASTNode : public ASTnode {

public:
  std::vector<std::unique_ptr<ExternASTNode>> ExternList;
  std::vector<std::unique_ptr<DeclASTNode>> DeclList;
  ProgramASTNode(std::vector<std::unique_ptr<ExternASTNode>> externlist, std::vector<std::unique_ptr<DeclASTNode>> decllist) : ExternList(std::move(externlist)), DeclList(std::move(decllist)) {}
  Value *codegen() override {}
  std::string to_string() const override {
    std::string str = "Program";
    for (auto&& ext : ExternList) {
      str += "\n  ";
      str += ext->to_string();
    }
    for (auto&& decl : DeclList) {
      str += "\n  ";
      str += decl->to_string();
    }
    return str;
  }
};

/* add other AST nodes as nessasary */

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

/* Add function calls for each production */
static void HandleError(const char *str) {
  std::string msg = "Error on line: " + std::to_string(CurTok.lineNo) + " with token: " + CurTok.lexeme + ": " + str; 
  fprintf(stderr, "%s\n", msg.c_str());
}

std::unique_ptr<ParamsASTNode> ParseParams() {
  //Params consist of vector of paramast
  //Parse void case first
  if (CurTok.type == VOID_TOK) {
    return std::make_unique<VoidParamsASTNode>();
  }
  std::vector<std::unique_ptr<ParamASTNode>> paramList;
  bool isEpsilon = true;
  //Parse param_list
  while (CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    isEpsilon = false;
    TOKEN varType = CurTok;
    getNextToken(); //Consume type
    if (CurTok.type != IDENT) {
      HandleError("Expected token IDENT in param production");
      return nullptr;
    }
    auto ident = std::make_unique<IdentASTNode>(CurTok);
    paramList.push_back(std::make_unique<ParamASTNode>(varType, std::move(ident))); //Add param to paramList vector
    getNextToken(); //Consume ident
    //Next part would either be another param denoted by a comma, or from follow set it would be a bracket.
    if (CurTok.type != COMMA && CurTok.type != RPAR) {
      HandleError("Expected either ',' or ')' in param production");
      return nullptr;
    }
    getNextToken(); //Consume RPAR or COMMA
  }
  //Parse epsilon
  if (isEpsilon) {
    return nullptr;
  }
  return std::make_unique<ListParamsASTNode>(std::move(paramList));  
};

std::unique_ptr<LocalDeclASTNode> ParseLocalDecl() {
  TOKEN varType = CurTok;
  getNextToken(); //Consume var_type
  if(CurTok.type!=IDENT) {
    HandleError("Expected token IDENT in local_decl production");
    return nullptr;
  }
  auto ident = std::make_unique<IdentASTNode>(CurTok);
  getNextToken();
  if(CurTok.type!=SC) {
    HandleError("Expected token ';' in local_decl production");
    return nullptr;
  }
  return std::make_unique<LocalDeclASTNode>(varType, std::move(ident));
}

std::vector<std::unique_ptr<LocalDeclASTNode>> ParseLocalDecls() {
  //local decl or epsilon
  //for epsilon case check follow set so we dont error
  //follow local_decls is "!" "(" "-" ";" "if" "return" "while" "{" "}" BOOL_LIT FLOAT_LIT IDENT INT_LIT
  std::vector<std::unique_ptr<LocalDeclASTNode>> localDecls;
  while (CurTok.type == BOOL_TOK || CurTok.type == FLOAT_TOK || CurTok.type == INT_TOK) {
    localDecls.push_back(std::move(ParseLocalDecl()));
  }
  std::vector<TOKEN_TYPE> followSet = {NOT, LPAR, MINUS, SC, IF, RETURN, WHILE, LBRA, RBRA, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  auto search = std::find(followSet.begin(), followSet.end(), CurTok.type);
  if(search==followSet.end()) {
    HandleError("Expected either: '!', '(', '-', ';', 'if', 'return', 'while', '{', '}', BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT after local_decls production");
    return std::vector<std::unique_ptr<LocalDeclASTNode>>();
  }
  return std::move(localDecls);
}

std::unique_ptr<ExprASTNode> ParseExpr();

std::unique_ptr<ArgListASTNode> ParseArgs() {
  std::vector<std::unique_ptr<ExprASTNode>> exprs;
  //Args production contains epsilon so need to check follow set as well as first (which is ")")
  if(CurTok.type==RPAR) {
    return nullptr;
  }
  bool isExpr = true;
  while(isExpr) {
    exprs.push_back(std::move(ParseExpr()));
    if(CurTok.type!=COMMA){
      isExpr = false;
    }
  }
  return std::make_unique<ArgListASTNode>(std::move(exprs));
}

std::unique_ptr<RValASTNode> ParseRval() {
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in rval production");
    return nullptr;
  }
  if(CurTok.type==NOT || CurTok.type==MINUS) {
    TOKEN op = CurTok;
    getNextToken();
    return std::make_unique<UnaryOpRValASTNode>(op, std::move(ParseRval()));
  } else if(CurTok.type==LPAR) {
    getNextToken(); //Consume (
    std::unique_ptr<ExprASTNode> expr = std::move(ParseExpr());
    if(CurTok.type!=RPAR) {
      HandleError("Expected ')' after 'expr' in rval production");
      return nullptr;
    }
    getNextToken(); //Consume )
    return std::move(expr);
  } else if(CurTok.type==IDENT) {
    auto ident = std::make_unique<IdentASTNode>(CurTok);
    getNextToken(); //Consume IDENT
    // "(" isnt in the follow set of rval so we can use this to check which IDENT it is
    if(CurTok.type==LPAR) {
      getNextToken(); //Consume (
      std::unique_ptr<ArgListASTNode> args = std::move(ParseArgs());
      if (CurTok.type!=RPAR) {
        HandleError("Expected ')' after 'args' in rval production");
        return nullptr;
      }
      getNextToken(); //Consume )
      return std::make_unique<FunctionCallASTNode>(std::move(ident), std::move(args));
    }
    return std::make_unique<IdentRvalASTNode>(std::move(ident));
  } else if(CurTok.type==INT_LIT) {
    TOKEN tok = CurTok;
    int val = std::stoi(CurTok.lexeme);
    getNextToken(); //Consume int literal
    return std::make_unique<IntASTNode>(val, tok);
  } else if(CurTok.type==FLOAT_LIT) {
    TOKEN tok = CurTok;
    float val = std::stof(CurTok.lexeme);
    getNextToken(); //Consume float literal
    return std::make_unique<FloatASTNode>(val, tok);
  } else {
    TOKEN tok = CurTok;
    bool val;
    if(CurTok.lexeme=="true") {
      val = true;
    } else {
      val = false;
    }
    getNextToken(); //Consume bool literal
    return std::make_unique<BoolASTNode>(val, tok);
  }
}

std::unique_ptr<TimesASTNode> ParseOperatorTimes() {
  std::vector<std::unique_ptr<RValASTNode>> rvals;
  std::vector<TOKEN> ops;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_times production");
    return nullptr;
  }
  rvals.push_back(std::move(ParseRval()));
  //Check follow set or * / %
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND, EQ, NE, LT, LE, GT, GE, PLUS, MINUS};
  while(CurTok.type==ASTERIX || CurTok.type==DIV || CurTok.type==MOD) {
    ops.push_back(CurTok);
    getNextToken(); //Consume * or / or %
    rvals.push_back(std::move(ParseRval()));
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and', '==', '!=', '<', '<=', '>', '>=', '+', '-' in operator_times production");
    return nullptr;
  }
  return std::make_unique<TimesASTNode>(std::move(rvals), ops);
}

std::unique_ptr<AddASTNode> ParseOperatorAdd() {
  std::vector<std::unique_ptr<TimesASTNode>> times;
  std::vector<TOKEN> ops;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_add production");
    return nullptr;
  }
  times.push_back(std::move(ParseOperatorTimes()));
  //Check follow set or + or -
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND, EQ, NE, LT, LE, GT, GE};
  while(CurTok.type==MINUS || CurTok.type==PLUS) {
    ops.push_back(CurTok);
    getNextToken(); //Consume + or -
    times.push_back(std::move(ParseOperatorTimes()));
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and', '==', '!=', '<', '<=', '>', '>=', in operator_add production");
    return nullptr;
  }
  return std::make_unique<AddASTNode>(std::move(times), ops);
}

std::unique_ptr<CompASTNode> ParseOperatorComp() {
  std::vector<std::unique_ptr<AddASTNode>> adds;
  std::vector<TOKEN> ops;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_comp production");
    return nullptr;
  }
  adds.push_back(std::move(ParseOperatorAdd()));
  //Check follow set or < or <= or > or >=
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND, EQ, NE};
  while(CurTok.type==LT || CurTok.type==LE || CurTok.type==GT || CurTok.type==GE) {
    ops.push_back(CurTok);
    getNextToken(); //Consume < or <= or > or >=
    adds.push_back(std::move(ParseOperatorAdd()));
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and', '==', '!=' in operator_comp production");
    return nullptr;
  }
  return std::make_unique<CompASTNode>(std::move(adds), ops);
}

std::unique_ptr<EquivASTNode> ParseOperatorEquiv() {
  std::vector<std::unique_ptr<CompASTNode>> comps;
  std::vector<TOKEN> ops;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_equiv production");
    return nullptr;
  }
  comps.push_back(std::move(ParseOperatorComp()));
  //Check follow set or == or !=
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND};
  while(CurTok.type==EQ || CurTok.type==NE) {
    ops.push_back(CurTok);
    getNextToken(); //Consume == or !=
    comps.push_back(std::move(ParseOperatorComp()));
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and' after operator_equiv production");
    return nullptr;
  }
  return std::make_unique<EquivASTNode>(std::move(comps), ops);
}

std::unique_ptr<AndASTNode> ParseOperatorAnd() {
  std::vector<std::unique_ptr<EquivASTNode>> equivs;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_and production");
    return nullptr;
  }
  equivs.push_back(std::move(ParseOperatorEquiv()));
  //Check follow set or &&
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR};
  while(CurTok.type==AND) {
    getNextToken(); //Consume &&
    equivs.push_back(std::move(ParseOperatorEquiv()));
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or' in operator_and production");
    return nullptr;
  }
  return std::make_unique<AndASTNode>(std::move(equivs));
}

std::unique_ptr<OrASTNode> ParseOperatorOr() {
  std::vector<std::unique_ptr<AndASTNode>> ands;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_or production");
    return nullptr;
  }
  ands.push_back(std::move(ParseOperatorAnd()));
  //Check follow set or ||
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC};
  while(CurTok.type==OR) {
    getNextToken(); //Consume ||
    ands.push_back(std::move(ParseOperatorAnd()));
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';' in operator_or production");
    return nullptr;
  }
  return std::make_unique<OrASTNode>(std::move(ands));
}

std::unique_ptr<ExprASTNode> ParseExpr() {
  //Check that the next token will match an expr
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in expr production");
    return nullptr;
  }
  //must be an operator_or
  if(CurTok.type!=IDENT) {
    return std::make_unique<OrExprASTNode>(std::move(ParseOperatorOr()));
  } else {
    //Need to do more investigation to see if it is operator_or or not
    //IDENT in first part of expr is followed by an "=". This can never happen if the
    //IDENT is from rval. To do this we need to lookahead one token
    TOKEN ident = CurTok;
    TOKEN secondTok = getNextToken();
    if(CurTok.type==ASSIGN) {
      //expr ::= IDENT "=" expr
      putBackToken(secondTok);
      CurTok = ident;
      auto identAST = std::make_unique<IdentASTNode>(ident);
      getNextToken(); //Consume IDENT
      getNextToken(); //Consume "="
      return std::make_unique<AssignmentExprASTNode>(std::move(identAST), std::move(ParseExpr()));
    } else {
      //rval ::= IDENT | IDENT "(" args ")" so would be an operator_or
      putBackToken(secondTok);
      CurTok = ident;
      return std::make_unique<OrExprASTNode>(std::move(ParseOperatorOr()));
    }
  }
}

std::unique_ptr<BlockASTNode> ParseBlock();

std::unique_ptr<ElseStmtASTNode> ParseElseStmt() {
  if(CurTok.type!=ELSE) {
    std::vector<TOKEN_TYPE> followSet =	{NOT, LPAR, MINUS, SC, IF, RETURN, WHILE, LBRA, RBRA, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
    auto search = std::find(followSet.begin(), followSet.end(), CurTok.type);
    if(search==followSet.end()) {
      HandleError("Expected either: '!', '(', '-', ';', 'if', 'return', 'while', '{', '}', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in else production");
      return nullptr;
    }
    return nullptr;
  }
  getNextToken(); //Consume "else"
  return std::make_unique<ElseStmtASTNode>(std::move(ParseBlock()));
}

std::unique_ptr<IfStmtASTNode> ParseIfStmt() {
  //"(" expr ")" block else_stmt
  if(CurTok.type!=LPAR) {
    HandleError("Expected '(' in if production");
    return nullptr;
  }
  getNextToken(); //Consume (
  std::unique_ptr<ExprASTNode> expr = std::move(ParseExpr());
  if(CurTok.type!=RPAR) {
    HandleError("Expected '(' in if production");
    return nullptr;
  }
  getNextToken(); //Consume )
  std::unique_ptr<BlockASTNode> block = std::move(ParseBlock());
  std::unique_ptr<ElseStmtASTNode> elseStmt = std::move(ParseElseStmt());
  return std::make_unique<IfStmtASTNode>(std::move(expr), std::move(block), std::move(elseStmt));
}

std::unique_ptr<StmtASTNode> ParseStmt();

std::unique_ptr<WhileStmtASTNode> ParseWhileStmt() {
  //"(" expr ")" stmt 
  if(CurTok.type!=LPAR) {
    HandleError("Expected '(' in while production");
    return nullptr;
  }
  getNextToken(); //Consume (
  std::unique_ptr<ExprASTNode> expr = std::move(ParseExpr());
  if(CurTok.type!=RPAR) {
    HandleError("Expected ')' in if production");
    return nullptr;
  }
  getNextToken(); //Consume )
  std::unique_ptr<StmtASTNode> stmt = std::move(ParseStmt());
  return std::make_unique<WhileStmtASTNode>(std::move(expr), std::move(stmt));
}

std::unique_ptr<ReturnStmtASTNode> ParseReturnStmt() {
  //";" | expr ";"  
  if(CurTok.type==SC) {
    getNextToken(); //Consume ;
    return std::make_unique<ReturnStmtASTNode>(nullptr);
  }
  std::unique_ptr<ExprASTNode> expr = std::move(ParseExpr());
  if(CurTok.type!=SC) {
    HandleError("Expected ';' in return production");
    return nullptr;
  }
  getNextToken(); //Consume ;
  return std::make_unique<ReturnStmtASTNode>(std::move(expr));
}

std::unique_ptr<ExprStmtASTNode> ParseExprStmt() {
  //";" | expr ";"  
  if(CurTok.type==SC) {
    getNextToken(); //Consume ;
    return std::make_unique<ExprStmtASTNode>(nullptr);
  }
  std::unique_ptr<ExprASTNode> expr = std::move(ParseExpr());
  if(CurTok.type!=SC) {
    HandleError("Expected ';' in expr_stmt production");
    return nullptr;
  }
  getNextToken(); //Consume ;
  return std::make_unique<ExprStmtASTNode>(std::move(expr));
}

std::unique_ptr<StmtASTNode> ParseStmt() {
  //expr, block, if, while, return
  std::unique_ptr<StmtASTNode> stmt;
  if(CurTok.type==IF) {
    getNextToken(); //Consume IF
    //IF
    stmt = std::move(ParseIfStmt());
  } else if(CurTok.type==WHILE) {
    getNextToken(); //Consume WHILE
    //WHILE
    stmt = std::move(ParseWhileStmt());
  } else if(CurTok.type==RETURN) {
    getNextToken(); //Consume RETURN
    stmt = std::move(ParseReturnStmt());
    //RETURN
  } else if(CurTok.type==LBRA) {
    stmt = std::move(ParseBlock());
  } else {
    //EXPR_STMT
    stmt = std::move(ParseExprStmt());
  }
  return std::move(stmt);
}

std::vector<std::unique_ptr<StmtASTNode>> ParseStmtList() {
  std::vector<std::unique_ptr<StmtASTNode>> stmtList;
  bool isStmt = true;
  while(isStmt) {
    std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, SC, IF, RETURN, WHILE, LBRA, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
    auto search = std::find(firstSet.begin(), firstSet.end(), CurTok.type);
    if(search==firstSet.end()) {
      isStmt = false;
    } else {
      stmtList.push_back(std::move(ParseStmt()));
    }
  }
  if(stmtList.empty()) {
    HandleError("Expected '!', '(', '-', ';', BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT in stmt production");
    return std::vector<std::unique_ptr<StmtASTNode>>();
  }
  return std::move(stmtList);
}

std::unique_ptr<BlockASTNode> ParseBlock() {
  //"{" local_decls stmt_list "}" 
  if (CurTok.type!=LBRA) {
    HandleError("Expected '{' in block production");
    return nullptr;
  }
  getNextToken(); //Consume "{"
  //local_decls
  std::vector<std::unique_ptr<LocalDeclASTNode>> localDecls = std::move(ParseLocalDecls());
  //stmt_list
  std::vector<std::unique_ptr<StmtASTNode>> statements = std::move(ParseStmtList());
  if (CurTok.type!=RBRA) {
    HandleError("Expected '}' in block production");
    return nullptr;
  }
  getNextToken(); //Consume "}"
  return std::make_unique<BlockASTNode>(std::move(localDecls), std::move(statements));
}

std::vector<std::unique_ptr<ExternASTNode>> ParseExternList() {
  std::vector<std::unique_ptr<ExternASTNode>> externList;
  TOKEN type;
  std::unique_ptr<IdentASTNode> ident;
  //"extern" type_spec IDENT "(" params ")" ";"
  while(CurTok.type == EXTERN) {
    getNextToken(); //Consume "extern"
    if(CurTok.type==BOOL_TOK || CurTok.type==INT_TOK || CurTok.type==VOID_TOK || CurTok.type==FLOAT_TOK) {
      type = CurTok;
    } else {
      HandleError("Expected BOOL_TOK, INT_TOK, FLOAT_TOK, VOID_TOK in extern production");
      return std::vector<std::unique_ptr<ExternASTNode>>();
    }
    getNextToken(); //Consumes type_spec
    if (CurTok.type==IDENT) {
      ident = std::make_unique<IdentASTNode>(CurTok);
    } else {
      HandleError("Expected IDENT in extern production");
      return std::vector<std::unique_ptr<ExternASTNode>>();
    }
    getNextToken(); //Consumes IDENT
    if (CurTok.type!=LPAR) {
      HandleError("Expected token '(' in extern production");
      return std::vector<std::unique_ptr<ExternASTNode>>();
    }
    getNextToken(); //Consume "("
    std::unique_ptr<ParamsASTNode> params = std::move(ParseParams()); //Parses params
    if (CurTok.type!=SC) {
      HandleError("Expected ';' in extern production");
      return std::vector<std::unique_ptr<ExternASTNode>>();
    }
    getNextToken(); //Consumes ";"
    externList.push_back(std::make_unique<ExternASTNode>(type, std::move(ident), std::move(params)));
  }
  return std::move(externList);
};

std::unique_ptr<VarDeclASTNode> ParseVarDecl() {
  if (CurTok.type!=INT_TOK && CurTok.type!=FLOAT_TOK && CurTok.type!=BOOL_TOK) {
    HandleError("Expected BOOL_TOK, INT_TOK, FLOAT_TOK in var_decl production");
    return nullptr;
  }
  TOKEN varType = CurTok;
  TOKEN ident = getNextToken(); //Consume var_type
  if (CurTok.type!=IDENT) {
    HandleError("Expected IDENT in var_decl production");
    return nullptr;
  }
  auto identAST = std::make_unique<IdentASTNode>(ident);
  getNextToken(); //Consume IDENT
  if(CurTok.type!=SC) {
    HandleError("Expected ';' in var_decl production");
    return nullptr;
  }
  getNextToken(); //Consume ";"
  return std::make_unique<VarDeclASTNode>(varType, std::move(identAST));
}

std::unique_ptr<FunDeclASTNode> ParseFunDecl() {
  TOKEN varType;
  std::unique_ptr<IdentASTNode> ident;
  if (CurTok.type!=INT_TOK && CurTok.type!=FLOAT_TOK && CurTok.type!=BOOL_TOK && CurTok.type!=VOID_TOK) {
    HandleError("Expected BOOL_TOK, INT_TOK, FLOAT_TOK in fun_decl production");
    return nullptr;
  } else {
    varType = CurTok;
  }
  getNextToken(); //Consumes type_spec
  if (CurTok.type==IDENT) {
    ident = std::make_unique<IdentASTNode>(CurTok);
  } else {
    HandleError("Expected IDENT in fun_decl production");
    return nullptr;
  }
  getNextToken(); //Consumes IDENT
  if (CurTok.type!=LPAR) {
    HandleError("Expected token '(' in fun_decl production");
    return nullptr;
  }
  getNextToken(); //Consume "("
  std::unique_ptr<ParamsASTNode> params = std::move(ParseParams()); //Parses params
  std::unique_ptr<BlockASTNode> block = std::move(ParseBlock()); //Parses block
  return std::make_unique<FunDeclASTNode>(varType, std::move(ident), std::move(params), std::move(block));
}

std::vector<std::unique_ptr<DeclASTNode>> ParseDeclList() {
  std::vector<std::unique_ptr<DeclASTNode>> declList;
  bool isDecl = true;
  while(isDecl) {
    // Doing this requires a lookahead since the difference between them (unless theres a void) is 3 tokens ahead
    TOKEN current = CurTok;
    TOKEN secondTok = getNextToken();  
    TOKEN thirdTok = getNextToken();
    putBackToken(thirdTok);
    putBackToken(secondTok);
    CurTok = current;
    if (thirdTok.type == SC) {
      //Is var_decl
      declList.push_back(std::move(ParseVarDecl()));
    } else if(thirdTok.type == LPAR || current.type == VOID_TOK) {
      //Is fun_decl
      declList.push_back(std::move(ParseFunDecl()));
    } else {
      isDecl = false;
    }
  }
  return std::move(declList);
};

// program ::= extern_list decl_list
static void parser() {
  // add body
  std::vector<std::unique_ptr<ExternASTNode>> externList;
  if (CurTok.type == EXTERN) {
    externList = std::move(ParseExternList());
  } else {
    externList = std::vector<std::unique_ptr<ExternASTNode>>();
  }  
  std::vector<std::unique_ptr<DeclASTNode>> declList = std::move(ParseDeclList());
  std::unique_ptr<ProgramASTNode> prog = std::make_unique<ProgramASTNode>(std::move(externList), std::move(declList));
  llvm::outs() << prog->to_string() << "\n";
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const ASTnode &ast) {
  os << ast.to_string();
  return os;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  if (argc == 2) {
    pFile = fopen(argv[1], "r");
    if (pFile == NULL)
      perror("Error opening file");
  } else {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // initialize line number and column numbers to zero
  lineNo = 1;
  columnNo = 1;

  // get the first token
  getNextToken();
  // while (CurTok.type != EOF_TOK) {
  //   fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
  //           CurTok.type);
  //   getNextToken();
  // }
  fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  parser();
  fprintf(stderr, "Parsing Finished\n");

  //********************* Start printing final IR **************************
  // Print out all of the generated code into a file called output.ll
  auto Filename = "output.ll";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }
  // TheModule->print(errs(), nullptr); // print IR to terminal
  TheModule->print(dest, nullptr);
  //********************* End printing final IR ****************************

  fclose(pFile); // close the file that contains the code that was parsed
  return 0;
}
