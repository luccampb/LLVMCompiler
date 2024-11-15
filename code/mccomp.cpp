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

//TODO: 
// - Lazy operators
// - Test Parsing and Codegen
// - Error handling
// - Comments
// - Report
// - Add grammar rules above AST nodes and parser funcs
// - remove unnecessary nodes from printing

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
// Codegen Stores
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

//Tables for local variables, global variables and functions
static std::map<std::string, AllocaInst*> NamedValues; 
static std::map<std::string, Function *> Functions;
static std::map<std::string, GlobalVariable *> Globals;

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

enum StmtType {
  EXPR = 1,
  BLOCK = 2,
  IFSTMT = 3,
  ELSESTMT = 4,
  WHILESTMT = 5,
  RETSTMT = 6
};

/// ASTnode - Base class for all AST nodes.
class ASTnode {
public:
  virtual ~ASTnode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string(int indent) const {return "";};
};

class ParamASTNode : public ASTnode {
public:
  std::string Ident;
  TOKEN VarType;
  ParamASTNode(TOKEN vartype, std::string ident) : VarType(vartype), Ident(ident) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Param\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += VarType.lexeme;
    str += "\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += Ident;
    return str;
  }
};

class ParamsASTNode : public ASTnode {
public:
  bool IsVoid;
  std::vector<std::unique_ptr<ParamASTNode>> ParamList;
  ParamsASTNode(std::vector<std::unique_ptr<ParamASTNode>> paramlist, bool isVoid) : ParamList(std::move(paramlist)), IsVoid(isVoid) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Params";
    if (ParamList.empty() && !IsVoid) {
      str += "\n";
    } else if (ParamList.empty() && IsVoid) {
      str += "\nVoid Param";
    }
    for(auto&& param : ParamList) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      int newIndent = indent + 1;
      str += param->to_string(newIndent);
    }
    return str;
  }
};

// ExternASTNode - Class for extern declarations
class ExternASTNode : public ASTnode {
  TOKEN VarType;
  std::string FuncName;
  std::unique_ptr<ParamsASTNode> Params;

public:
  ExternASTNode(TOKEN vartype, std::string funcName, std::unique_ptr<ParamsASTNode> params) : VarType(vartype), FuncName(funcName), Params(std::move(params)) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Extern\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += VarType.lexeme;
    str += "\n  ";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += FuncName;
    str += "\n";
    if(Params != nullptr) {
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Params->to_string(newIndent);
    }
    return str;
  }
};

class LocalDeclASTNode : public ASTnode {
  TOKEN VarType;

public:
  std::string Ident;
  LocalDeclASTNode(TOKEN vartype, std::string ident) : VarType(vartype), Ident(ident) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Local Decl\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += VarType.lexeme;
    str += "\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += Ident;
    return str;
  }
};

class StmtASTNode : public ASTnode {
public:
  virtual StmtType stmtType();
  virtual ~StmtASTNode() {}
};

class BlockASTNode : public StmtASTNode {
public:
  std::vector<std::unique_ptr<LocalDeclASTNode>> LocalDecls;
  std::vector<std::unique_ptr<StmtASTNode>> Statements;  
  BlockASTNode(std::vector<std::unique_ptr<LocalDeclASTNode>> localdecls, std::vector<std::unique_ptr<StmtASTNode>> statements) : LocalDecls(std::move(localdecls)), Statements(std::move(statements)) {}
  StmtType stmtType() override {
    return StmtType::BLOCK;
  }
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Block";
    for(auto&& localD : LocalDecls) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      int newIndent = indent + 1;
      str += localD->to_string(newIndent);   
    }
    for(auto&& s : Statements) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      int newIndent = indent + 1;
      str += s->to_string(newIndent);  
    }
    if (LocalDecls.empty() && Statements.empty()) {
      str += "\n";
    }
    return str;
  }
};

class DeclASTNode : public ASTnode {
public:
  DeclASTNode() {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    return "Decl";
  }
};

// FunDeclASTNode - Class for function declarations
class FunDeclASTNode : public DeclASTNode {
  TOKEN VarType;
  std::string FuncName;
  std::unique_ptr<ParamsASTNode> Params;
  std::unique_ptr<BlockASTNode> Block;

public:
  FunDeclASTNode(TOKEN vartype, std::string funcName, std::unique_ptr<ParamsASTNode> params, std::unique_ptr<BlockASTNode> block) : VarType(vartype), FuncName(funcName), Params(std::move(params)), Block(std::move(block)) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Fun_Decl\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += VarType.lexeme;
    str += "\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += FuncName;    
    if(Params != nullptr){
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Params->to_string(newIndent);
    }
    if(Block != nullptr) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Block->to_string(newIndent);
    }    
    return str;
  }
};

class VarDeclASTNode : public DeclASTNode {
  TOKEN VarType;
  std::string Ident;

public:
  VarDeclASTNode(TOKEN vartype, std::string ident) : VarType(vartype), Ident(ident) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Var_Decl\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += VarType.lexeme;
    str += "\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += Ident;
    return str;
  }
};

class ElseStmtASTNode : public StmtASTNode {

public:
  std::unique_ptr<BlockASTNode> Block;
  ElseStmtASTNode(std::unique_ptr<BlockASTNode> block) : Block(std::move(block)) {}
  StmtType stmtType() override {
    return StmtType::ELSESTMT;
  }
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Else Statement\n";
    if(Block != nullptr) {
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      int newIndent = indent + 1;
      str += Block->to_string(newIndent);
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
  std::string Ident;

public:
  IdentRvalASTNode(std::string ident) : Ident(ident) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Rval\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += Ident;
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
  StmtType stmtType() override {
    return StmtType::EXPR;
  }
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Expr Statement";
    if(Expr != nullptr) {
      str+="\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      int newIndent = indent + 1;
      str += Expr->to_string(newIndent);
    }    
    return str;
  }
};

class IfStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;

public:
  std::unique_ptr<BlockASTNode> Block;
  std::unique_ptr<ElseStmtASTNode> ElseStmt;
  IfStmtASTNode(std::unique_ptr<ExprASTNode> expr, std::unique_ptr<BlockASTNode> block, std::unique_ptr<ElseStmtASTNode> elsestmt) : Expr(std::move(expr)), Block(std::move(block)), ElseStmt(std::move(elsestmt)) {}
  StmtType stmtType() override {
    return StmtType::IFSTMT;
  }
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "If Statement\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += Expr->to_string(newIndent);
    if(Block!=nullptr) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Block->to_string(newIndent);
    }    
    if(ElseStmt != nullptr) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += ElseStmt->to_string(newIndent);
    }    
    return str;
  }
};

class WhileStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;
  std::unique_ptr<StmtASTNode> Statement;

public:
  WhileStmtASTNode(std::unique_ptr<ExprASTNode> expr, std::unique_ptr<StmtASTNode> statement) : Expr(std::move(expr)), Statement(std::move(statement)) {}
  StmtType stmtType() override {
    return StmtType::WHILESTMT;
  }
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "While Statement\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += Expr->to_string(newIndent);
    str += "\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += Statement->to_string(newIndent);
    return str;
  }
};

class ReturnStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;

public: 
  ReturnStmtASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  StmtType stmtType() override {
    return StmtType::RETSTMT;
  }
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Return Statement\n";
    if(Expr!=nullptr) {
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      int newIndent = indent + 1;
      str += Expr->to_string(newIndent);
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
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Unary Operator Rval\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += Op.lexeme;
    str += "\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += RVal->to_string(newIndent);
    return str;
  }
};

//expr ::= IDENT "=" expr in production
class AssignmentExprASTNode : public ExprASTNode {
  std::string Ident;
  std::unique_ptr<ExprASTNode> SubExpr;

public:
  AssignmentExprASTNode(std::string ident, std::unique_ptr<ExprASTNode> subexpr) : Ident(ident), SubExpr(std::move(subexpr)) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Expr\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += Ident;
    str += "\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += SubExpr->to_string(newIndent);
    return str;
  }
};

class ArgListASTNode : public ASTnode {
public:
  std::vector<std::unique_ptr<ExprASTNode>> Exprs;
  ArgListASTNode(std::vector<std::unique_ptr<ExprASTNode>> exprs) : Exprs(std::move(exprs)) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Rval";
    if (Exprs.empty()) {
      str += "\n";
    }
    for(auto&& expr : Exprs) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      int newIndent = indent + 1;
      str += expr->to_string(newIndent);
    }
    return str;
  }
};

//Function call in rval ::= production
class FunctionCallASTNode : public RValASTNode {
  std::string FuncName;
  std::unique_ptr<ArgListASTNode> Args;
public:
  FunctionCallASTNode(std::string funcname, std::unique_ptr<ArgListASTNode> args) : FuncName(std::move(funcname)), Args(std::move(args)) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Rval\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += FuncName;
    str += "\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += Args->to_string(newIndent);
    return str;
  }
};

/// IntASTNode - Class for integer literals like 1, 2, 10,
class IntASTNode : public RValASTNode {
  int Val;
  TOKEN Tok;

public:
  IntASTNode(int val, TOKEN tok) : Val(val), Tok(tok) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Rval\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += "Int\n";
    for(int i = 0; i<indent+1; i++) {
      str += "  ";
    }
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
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Rval\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += "Int\n";
    for(int i = 0; i<indent+1; i++) {
      str += "  ";
    }
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
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Rval\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += "Int\n";
    for(int i = 0; i<indent+1; i++) {
      str += "  ";
    }
    str += std::to_string(Val);
    return str;
  }
};

class TimesASTNode : public ASTnode {

public:
  std::unique_ptr<RValASTNode> Left;
  std::unique_ptr<TimesASTNode> Right;
  TOKEN Op;
  TimesASTNode(std::unique_ptr<RValASTNode> left, std::unique_ptr<TimesASTNode> right, TOKEN op) : Left(std::move(left)), Right(std::move(right)), Op(op) {}
  Value* codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    bool hasRight = false;
    int newIndent = indent;
    if(Right!=nullptr) {
      str += "Operator_Times\n";
      hasRight = true;
      newIndent++;
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
    }    
    str += Left->to_string(newIndent);
    if(hasRight) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Op.lexeme;
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Right->to_string(newIndent);
    }
    return str;
  }
};

class AddASTNode : public ASTnode {

public:
  std::unique_ptr<TimesASTNode> Left;
  std::unique_ptr<AddASTNode> Right;
  TOKEN Op;
  AddASTNode(std::unique_ptr<TimesASTNode> left, std::unique_ptr<AddASTNode> right, TOKEN op) : Left(std::move(left)), Right(std::move(right)), Op(op) {}
  Value* codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    bool hasRight = false;
    int newIndent = indent;
    if(Right!=nullptr) {
      str += "Operator_Add\n";
      hasRight = true;
      newIndent++;
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
    }
    str += Left->to_string(newIndent);
    if(hasRight) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Op.lexeme;
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Right->to_string(newIndent);
    }
    return str;
  }
};

class CompASTNode : public ASTnode {
  std::unique_ptr<AddASTNode> Left;
  std::unique_ptr<CompASTNode> Right;
  TOKEN Op;

public:
  CompASTNode(std::unique_ptr<AddASTNode> left, std::unique_ptr<CompASTNode> right, TOKEN op) : Left(std::move(left)), Right(std::move(right)), Op(op) {}
  Value* codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    bool hasRight = false;
    int newIndent = indent;
    if(Right!=nullptr) {
      str += "Operator_Comp\n";
      hasRight = true;
      newIndent++;
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
    }
    str += Left->to_string(newIndent);
    if(hasRight) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Op.lexeme;
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Right->to_string(newIndent);
    }
    return str;
  }
};

class EquivASTNode : public ASTnode {
  std::unique_ptr<CompASTNode> Left;
  std::unique_ptr<EquivASTNode> Right;
  TOKEN Op;

public:
  EquivASTNode(std::unique_ptr<CompASTNode> left, std::unique_ptr<EquivASTNode> right, TOKEN op) : Left(std::move(left)), Right(std::move(right)), Op(op) {}
  Value* codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    bool hasRight = false;
    int newIndent = indent;
    if(Right!=nullptr) {
      str += "Operator_Equiv\n";
      hasRight = true;
      newIndent++;
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
    }
    str += Left->to_string(newIndent);
    if(hasRight) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Op.lexeme;
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Right->to_string(newIndent);
    }
    return str;
  }
};

class AndASTNode : public ASTnode {
  std::unique_ptr<EquivASTNode> Left;
  std::unique_ptr<AndASTNode> Right;

public:
  AndASTNode(std::unique_ptr<EquivASTNode> left, std::unique_ptr<AndASTNode> right) : Left(std::move(left)), Right(std::move(right)) {}
  Value* codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    bool hasRight = false;
    int newIndent = indent;
    if(Right!=nullptr) {
      str += "Operator_And\n";
      hasRight = true;
      newIndent++;
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
    }
    str += Left->to_string(newIndent);
    if(hasRight) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += "&&";
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Right->to_string(newIndent);
    }
    return str;
  }
};

class OrASTNode : public ASTnode {
  std::unique_ptr<AndASTNode> Left;
  std::unique_ptr<OrASTNode> Right;

public:
  OrASTNode(std::unique_ptr<AndASTNode> left, std::unique_ptr<OrASTNode> right) : Left(std::move(left)), Right(std::move(right)) {}
  Value* codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    bool hasRight = false;
    int newIndent = indent;
    if(Right!=nullptr) {
      str += "Operator_Or\n";
      hasRight = true;
      newIndent++;
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
    }
    str += Left->to_string(newIndent);
    if(hasRight) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += "||";
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += Right->to_string(newIndent);
    }
    return str;
  }
};

//expr ::= operator_or in production
class OrExprASTNode : public ExprASTNode {
  std::unique_ptr<OrASTNode> OrExpression;

public:
  OrExprASTNode(std::unique_ptr<OrASTNode> orexpression) : OrExpression(std::move(orexpression)) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Expr\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    int newIndent = indent + 1;
    str += OrExpression->to_string(newIndent);
    return str;
  }
};

/// ProgramASTNode - Class for program
class ProgramASTNode : public ASTnode {

public:
  std::vector<std::unique_ptr<ExternASTNode>> ExternList;
  std::vector<std::unique_ptr<DeclASTNode>> DeclList;
  ProgramASTNode(std::vector<std::unique_ptr<ExternASTNode>> externlist, std::vector<std::unique_ptr<DeclASTNode>> decllist) : ExternList(std::move(externlist)), DeclList(std::move(decllist)) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Program";
    int newIndent = indent + 1;
    for (auto&& ext : ExternList) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
          str += "  ";
      }
      str += ext->to_string(newIndent);
    }
    for (auto&& decl : DeclList) {
      str += "\n";
      for(int i = 0; i<indent; i++) {
        str += "  ";
      }
      str += decl->to_string(newIndent);
    }
    return str;
  }
};

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

/* Add function calls for each production */
static void HandleError(const char *str) {
  std::string msg = "Error on line: " + std::to_string(CurTok.lineNo) + " with token: " + CurTok.lexeme + ": " + str; 
  fprintf(stderr, "%s\n", msg.c_str());
  exit(0);
}


std::unique_ptr<ParamsASTNode> ParseParams() {
  //Params consist of vector of paramast
  //Parse void case first
  std::vector<std::unique_ptr<ParamASTNode>> paramList;
  if (CurTok.type == VOID_TOK) {
    getNextToken(); //Consume void
    getNextToken(); //Consume RPAR
    paramList = std::vector<std::unique_ptr<ParamASTNode>>();
    return std::make_unique<ParamsASTNode>(std::move(paramList), true);
  }  
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
    std::string ident = CurTok.lexeme;
    paramList.push_back(std::make_unique<ParamASTNode>(varType, ident)); //Add param to paramList vector
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
    getNextToken(); //Consume RPAR
    return nullptr;
  }
  return std::make_unique<ParamsASTNode>(std::move(paramList), false);  
};

std::unique_ptr<LocalDeclASTNode> ParseLocalDecl() {
  TOKEN varType = CurTok;
  getNextToken(); //Consume var_type
  if(CurTok.type!=IDENT) {
    HandleError("Expected token IDENT in local_decl production");
    return nullptr;
  }
  std::string ident = CurTok.lexeme;
  getNextToken();
  if(CurTok.type!=SC) {
    HandleError("Expected token ';' in local_decl production");
    return nullptr;
  }
  getNextToken(); //Consume Semicolon
  return std::make_unique<LocalDeclASTNode>(varType, ident);
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
    std::string ident = CurTok.lexeme;
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
      return std::make_unique<FunctionCallASTNode>(ident, std::move(args));
    }
    return std::make_unique<IdentRvalASTNode>(ident);
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
  std::unique_ptr<RValASTNode> left;
  std::unique_ptr<TimesASTNode> right = nullptr;
  TOKEN op;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_times production");
    return nullptr;
  }
  left = std::move(ParseRval());
  //Check follow set or * / %
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND, EQ, NE, LT, LE, GT, GE, PLUS, MINUS};
  if(CurTok.type==ASTERIX || CurTok.type==DIV || CurTok.type==MOD) {
    op = CurTok;
    getNextToken(); //Consume * or / or %
    right = std::move(ParseOperatorTimes());
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and', '==', '!=', '<', '<=', '>', '>=', '+', '-' in operator_times production");
    return nullptr;
  }
  return std::make_unique<TimesASTNode>(std::move(left), std::move(right), op);
}

std::unique_ptr<AddASTNode> ParseOperatorAdd() {
  std::unique_ptr<TimesASTNode> left;
  std::unique_ptr<AddASTNode> right = nullptr;
  TOKEN op;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_add production");
    return nullptr;
  }
  left = std::move(ParseOperatorTimes());
  //Check follow set or + or -
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND, EQ, NE, LT, LE, GT, GE};
  if(CurTok.type==MINUS || CurTok.type==PLUS) {
    op = CurTok;
    getNextToken(); //Consume + or -
    right = std::move(ParseOperatorAdd());
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and', '==', '!=', '<', '<=', '>', '>=', in operator_add production");
    return nullptr;
  }
  return std::make_unique<AddASTNode>(std::move(left), std::move(right), op);
}

std::unique_ptr<CompASTNode> ParseOperatorComp() {
  std::unique_ptr<AddASTNode> left;
  std::unique_ptr<CompASTNode> right = nullptr;
  TOKEN op;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_comp production");
    return nullptr;
  }
  left = std::move(ParseOperatorAdd());
  //Check follow set or < or <= or > or >=
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND, EQ, NE};
  if(CurTok.type==LT || CurTok.type==LE || CurTok.type==GT || CurTok.type==GE) {
    op = CurTok;
    getNextToken(); //Consume < or <= or > or >=
    right = std::move(ParseOperatorComp());
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and', '==', '!=' in operator_comp production");
    return nullptr;
  }
  return std::make_unique<CompASTNode>(std::move(left), std::move(right), op);
}

std::unique_ptr<EquivASTNode> ParseOperatorEquiv() {
  std::unique_ptr<CompASTNode> left;
  std::unique_ptr<EquivASTNode> right = nullptr;
  TOKEN op;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_equiv production");
    return nullptr;
  }
  left = std::move(ParseOperatorComp());
  //Check follow set or == or !=
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND};
  if(CurTok.type==EQ || CurTok.type==NE) {
    op = CurTok;
    getNextToken(); //Consume == or !=
    right = std::move(ParseOperatorEquiv());
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and' after operator_equiv production");
    return nullptr;
  }
  return std::make_unique<EquivASTNode>(std::move(left), std::move(right), op);
}

std::unique_ptr<AndASTNode> ParseOperatorAnd() {
  std::unique_ptr<EquivASTNode> left;
  std::unique_ptr<AndASTNode> right;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_and production");
    return nullptr;
  }
  left = std::move(ParseOperatorEquiv());
  //Check follow set or &&
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR};
  if(CurTok.type==AND) {
    getNextToken(); //Consume &&
    right = std::move(ParseOperatorAnd());
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or' in operator_and production");
    return nullptr;
  }
  return std::make_unique<AndASTNode>(std::move(left), std::move(right));
}

std::unique_ptr<OrASTNode> ParseOperatorOr() {
  std::unique_ptr<AndASTNode> left;
  std::unique_ptr<OrASTNode> right = nullptr;
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_or production");
    return nullptr;
  }
  left = std::move(ParseOperatorAnd());
  //Check follow set or ||
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC};
  if(CurTok.type==OR) {
    getNextToken(); //Consume ||
    right = std::move(ParseOperatorOr());
  }
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';' in operator_or production");
    return nullptr;
  }
  return std::make_unique<OrASTNode>(std::move(left), std::move(right));
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
      getNextToken(); //Consume IDENT
      getNextToken(); //Consume "="
      return std::make_unique<AssignmentExprASTNode>(ident.lexeme, std::move(ParseExpr()));
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
  std::string ident;
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
      ident = CurTok.lexeme;
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
    externList.push_back(std::make_unique<ExternASTNode>(type, ident, std::move(params)));
  }
  return std::move(externList);
};

std::unique_ptr<VarDeclASTNode> ParseVarDecl() {
  if (CurTok.type!=INT_TOK && CurTok.type!=FLOAT_TOK && CurTok.type!=BOOL_TOK) {
    HandleError("Expected BOOL_TOK, INT_TOK, FLOAT_TOK in var_decl production");
    return nullptr;
  }
  TOKEN varType = CurTok;
  std::string ident = getNextToken().lexeme; //Consume var_type
  if (CurTok.type!=IDENT) {
    HandleError("Expected IDENT in var_decl production");
    return nullptr;
  }
  getNextToken(); //Consume IDENT
  if(CurTok.type!=SC) {
    HandleError("Expected ';' in var_decl production");
    return nullptr;
  }
  getNextToken(); //Consume ";"
  return std::make_unique<VarDeclASTNode>(varType, ident);
}

std::unique_ptr<FunDeclASTNode> ParseFunDecl() {
  TOKEN varType;
  std::string ident;
  if (CurTok.type!=INT_TOK && CurTok.type!=FLOAT_TOK && CurTok.type!=BOOL_TOK && CurTok.type!=VOID_TOK) {
    HandleError("Expected BOOL_TOK, INT_TOK, FLOAT_TOK in fun_decl production");
    return nullptr;
  } else {
    varType = CurTok;
  }
  getNextToken(); //Consumes type_spec
  if (CurTok.type==IDENT) {
    ident = CurTok.lexeme;
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
  return std::make_unique<FunDeclASTNode>(varType, ident, std::move(params), std::move(block));
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
static std::unique_ptr<ProgramASTNode> parser() {
  std::vector<std::unique_ptr<ExternASTNode>> externList;
  if (CurTok.type == EXTERN) {
    externList = std::move(ParseExternList());
  } else {
    externList = std::vector<std::unique_ptr<ExternASTNode>>();
  }  
  std::vector<std::unique_ptr<DeclASTNode>> declList = std::move(ParseDeclList());
  return std::make_unique<ProgramASTNode>(std::move(externList), std::move(declList)); 
}

//===----------------------------------------------------------------------===//
// Codegen Functions
//===----------------------------------------------------------------------===//

Value *HandleErrorValue(std::string str) {
  std::string msg = "Semantic Error: " + str; 
  fprintf(stderr, "%s\n", msg.c_str());
  exit(0);
}

static AllocaInst* CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName, Type *Type_) {
  IRBuilder<> tmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
  return tmpB.CreateAlloca(Type_, 0, VarName.c_str());
}

static Type *GetTypeOfToken(TOKEN tok) {
  switch (tok.type) {
    case BOOL_TOK:
      return Type::getInt1Ty(TheContext);
    case INT_TOK:
      return Type::getInt32Ty(TheContext);    
    case FLOAT_TOK:
      return Type::getFloatTy(TheContext);
    case VOID_TOK:
      return Type::getVoidTy(TheContext);
  }
  return nullptr;
}

//A type can be widened regardless of associated value, but a narrowing conversion should only happen in certain boolean cases
static Value* AttemptCast(Type *goalType, Value *v) {
  //Goal is bool, we make a comparison between 0 and v:
  if (goalType->isIntegerTy(1)) {
    if (v->getType()->isIntegerTy(32)) {
      return Builder.CreateICmpNE(v, ConstantInt::get(TheContext, APInt(32, 0, true)), "tobool");
    } else if (v->getType()->isFloatTy()) {
      return Builder.CreateFCmpONE(v, ConstantFP::get(TheContext, APFloat(0.0f)), "tobool");
    } else if (v->getType()->isIntegerTy(1)) {
      return v;
    } else {
      return nullptr;
    }  
  }  
  //Goal is int:
  else if (goalType->isIntegerTy(32)) {
    if (v->getType()->isIntegerTy(1)) {
      return Builder.CreateZExt(v, Builder.getInt32Ty());
    } else if (v->getType()->isFloatTy()) {
      return Builder.CreateFPToSI(v, Builder.getInt32Ty());
    } else if (v->getType()->isIntegerTy(32)) {
      return v;
    } else {
      return nullptr;   
    }
  }  
  //Goal is float:
  else if (goalType->isFloatTy()) {
    if (v->getType()->isIntegerTy(32)) {
      return Builder.CreateSIToFP(v, Builder.getFloatTy());
    } else if (v->getType()->isIntegerTy(1)) {
      return Builder.CreateSIToFP(v, Builder.getFloatTy());
    } else if (v->getType()->isFloatTy()) {
      return v;
    } else {
      return nullptr;   
    }
  }  
  else {
    //VOID
    return nullptr;
  }  
}

//TYPES ARE VOID, BOOL -> INT -> FLOAT
static Type* HighestType(Type *t1, Type *t2) {
  if(t1->isFloatTy() || t2->isFloatTy()) {
    return Builder.getFloatTy();
  }
  if(t1->isIntegerTy(32) || t2->isIntegerTy(32)) {
    return Builder.getInt32Ty();
  }
  return Builder.getInt1Ty();
}

static bool CheckAllPathsReturn(BlockASTNode* block) {
  //Only care about the block's statements
  for(auto &&stmt : block->Statements) {
    if(stmt->stmtType()==RETSTMT) {
      return true;
    }
    //Must be either if/while/block/expr
    if(stmt->stmtType()==BLOCK) {
      //Can static cast because we know it must be a BlockASTNode
      BlockASTNode* subBlock = static_cast<BlockASTNode*>(stmt.get());
      return CheckAllPathsReturn(subBlock);
    }
    if(stmt->stmtType()==IFSTMT) {
      //Can static cast because we know it must be an IfStmtASTNode
      IfStmtASTNode* ifBlock = static_cast<IfStmtASTNode*>(stmt.get());
      if(!ifBlock->ElseStmt) {
        continue; //If it has no else stmt it doesnt matter whether the if has a return statement or not
      } else {
        if(CheckAllPathsReturn(ifBlock->Block.get()) && CheckAllPathsReturn(ifBlock->ElseStmt->Block.get())){
          return true;
        }
      }
    }
  }
  return false;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

Value *ParamASTNode::codegen() {
  //Handled in FunDeclASTNode and ExternASTNode instead of here due to how functions are created in llvm IR
  return nullptr;
}

Value *ParamsASTNode::codegen() {
  //Handled in FunDeclASTNode and ExternASTNode instead of here due to how functions are created in llvm IR
  return nullptr;
}

Value *ExternASTNode::codegen() {
  if(Functions[FuncName]) {
    return HandleErrorValue("Function declared multiple times.");
  }
  std::vector<Type*> argTypes;
  if(Params->IsVoid) {
    argTypes.push_back(Type::getVoidTy(TheContext));
  }
  for(auto&& param : Params->ParamList) {
    argTypes.push_back(GetTypeOfToken(param->VarType));
  }
  Type *funcType = GetTypeOfToken(VarType);
  FunctionType *FT = FunctionType::get(funcType, argTypes, false);
  Function *F = Function::Create(FT, Function::ExternalLinkage, FuncName, TheModule.get());
  unsigned idx = 0;
  for(auto &arg : F->args()) {
    arg.setName(Params->ParamList[idx++]->Ident);
  }    
  if(verifyFunction(*F)) {
    return nullptr;
  }
  Functions[FuncName] = F;
  return F;
}

Value *LocalDeclASTNode::codegen() {
  BasicBlock *currBlock = Builder.GetInsertBlock();
  Function *function = currBlock->getParent();
  if(NamedValues[Ident]) {
    return HandleErrorValue("Local variable declared multiple times.");
  }
  Type *type = GetTypeOfToken(VarType);
  AllocaInst *alloca = CreateEntryBlockAlloca(function, Ident, type);
  NamedValues[Ident] = alloca;
  return alloca;
}

Value *BlockASTNode::codegen() {
  std::map<std::string, AllocaInst*> OldNamedVals;
  OldNamedVals.insert(NamedValues.begin(), NamedValues.end());
  //Handle local decls vector
  for(auto &&decl : LocalDecls){
    decl->codegen();
  }
  //Handle Statements vector
  for(auto &&stmt : Statements){
    stmt->codegen();
  }
  //Now want to remove any new variables from NamedVals;
  for(auto &&kvp : NamedValues) {
    if(OldNamedVals.count(kvp.first)==1) {
      OldNamedVals[kvp.first] = NamedValues[kvp.first];
    }
  }
  NamedValues.clear();
  NamedValues.insert(OldNamedVals.begin(), OldNamedVals.end());
  return Constant::getNullValue(Builder.getInt1Ty());
}

Value *FunDeclASTNode::codegen() {
  if(Functions.count(FuncName)!=0) {
    return HandleErrorValue("Function declared multiple times. ");
  } 
  std::vector<Type*> argTypes;
  bool isVoid=false;
  if(Params) {
    isVoid = Params->IsVoid;
    for(auto&& param : Params->ParamList) {
      argTypes.push_back(GetTypeOfToken(param->VarType));
    }
  }
  Type *funcType = GetTypeOfToken(VarType);
  FunctionType *FT = FunctionType::get(funcType, argTypes, false);
  Function *F = Function::Create(FT, Function::ExternalLinkage, FuncName, TheModule.get());
  if(!F) {
    return HandleErrorValue("Unable to create function.");
  }
  unsigned idx = 0;
  for(auto &arg : F->args()) {
    if(!isVoid) {
      arg.setName(Params->ParamList[idx++]->Ident);
    }
  }
  BasicBlock *BB = BasicBlock::Create(TheContext, "func", F);
  Builder.SetInsertPoint(BB);
  NamedValues.clear();
  if(!isVoid){
    for(auto &arg : F->args()){
      AllocaInst *alloca = CreateEntryBlockAlloca(F, arg.getName().data(), arg.getType());
      Builder.CreateStore(&arg, alloca);
      NamedValues[arg.getName().data()] = alloca;
    }
  }  
  Block->codegen();
  if(!CheckAllPathsReturn(Block.get())) {
    return HandleErrorValue("Not all function code paths return a value.");
  }
  
  verifyFunction(*F);
  Functions[FuncName] = F;
  return F;
}

Value *VarDeclASTNode::codegen() {
  //Only happen outside functions because of local_decl
  Type *type = GetTypeOfToken(VarType);
  if (Globals.count(Ident)==1) {
    return HandleErrorValue("Global value declared multiple times.");
  }
  GlobalVariable *g = new GlobalVariable(*TheModule, type, false, GlobalValue::CommonLinkage, Constant::getNullValue(type), Ident);
  if(!g) {
    return HandleErrorValue("Unable to create global variable declaration.");
  }
  Globals[Ident] = g;
  return g;
}

Value *ElseStmtASTNode::codegen() {
  return Block->codegen();
}

Value *IdentRvalASTNode::codegen() {
  //Prioritises named values over globals first
  Value *temp;
  if(Globals[Ident]) {
    temp = Builder.CreateLoad(Globals[Ident]->getValueType(), Globals[Ident], Ident);
  }
  if(NamedValues[Ident]) {
    temp = Builder.CreateLoad(NamedValues[Ident]->getAllocatedType(), NamedValues[Ident], Ident);
  }
  if (!temp)
    HandleErrorValue("Reference to undefined variable name.");
  return temp;
}

Value *ExprStmtASTNode::codegen() {
  if(Expr) {
    return Expr->codegen();
  } else {
    return nullptr;
  }
}

Value *IfStmtASTNode::codegen() {
  Value *CondV = Expr->codegen();
  CondV = AttemptCast(Type::getInt1Ty(TheContext), CondV);
  if (!CondV)
    return HandleErrorValue("If conditional not a conditional.");
  Function *function = Builder.GetInsertBlock()->getParent();
  BasicBlock *true_ = BasicBlock::Create(TheContext, "iftrue", function);
  BasicBlock *else_ = BasicBlock::Create(TheContext, "else");
  BasicBlock *end_ = BasicBlock::Create(TheContext, "end");
  Builder.CreateCondBr(CondV, true_, else_);
  Builder.SetInsertPoint(true_);
  Value *TrueV = Block->codegen();
  if (!TrueV)
    return HandleErrorValue("Error generating if body.");
  Builder.CreateBr(end_);
  function->insert(function->end(), else_);
  Builder.SetInsertPoint(else_);
  if(ElseStmt) {
    Value *ElseV = ElseStmt->codegen();
    if (!ElseV)
      return HandleErrorValue("Error generating else body code.");;
  }
  Builder.CreateBr(end_);
  function->insert(function->end(), end_);
  Builder.SetInsertPoint(end_);
  return nullptr;
}

Value *WhileStmtASTNode::codegen() {
  Function *function = Builder.GetInsertBlock()->getParent();
  BasicBlock *condition = BasicBlock::Create(TheContext, "cond", function);
  BasicBlock *true_ = BasicBlock::Create(TheContext, "iftrue", function);
  BasicBlock *end_ = BasicBlock::Create(TheContext, "end");
  Builder.CreateBr(condition);
  Builder.SetInsertPoint(condition);
  Value *CondV = Expr->codegen();
  CondV = AttemptCast(Type::getInt1Ty(TheContext), CondV);
  if (!CondV)
    return HandleErrorValue("While conditional not a conditional.");
  Builder.CreateCondBr(CondV, true_, end_);
  Builder.SetInsertPoint(true_);
  Value *TrueV = Statement->codegen();
  if (!TrueV)
    return HandleErrorValue("Error generating while body code.");
  Builder.CreateBr(condition);
  function->insert(function->end(), end_);
  Builder.SetInsertPoint(end_);
  return nullptr;
}

Value *ReturnStmtASTNode::codegen() {
  Function *function = Builder.GetInsertBlock()->getParent();
  Type *returnType = function->getReturnType();
  if(Expr) {
    Value *retVal = Expr->codegen();
    Type *type = HighestType(retVal->getType(), returnType);
    retVal = AttemptCast(type, retVal);
    if(!retVal || type != returnType) {
      return HandleErrorValue("Function returns value of invalid type.");
    }
    return Builder.CreateRet(retVal);
  } else {
    return Builder.CreateRetVoid();
  }
}

Value *UnaryOpRValASTNode::codegen() {
  Value *RvalValue = RVal->codegen();
  if(!RvalValue) {
    return HandleErrorValue("Declared '!' or '-' with no associated value.");
  }
  if(Op.lexeme=="!") {
    //Must be a boolean
    RvalValue = AttemptCast(Builder.getInt1Ty(), RvalValue);
    if(!RvalValue) {
      return HandleErrorValue("Unable to cast negated value to boolean.");
    }
    return Builder.CreateNot(RvalValue, "not");
  } else {
    if(RvalValue->getType()->isFloatTy()) {
      return Builder.CreateFNeg(RvalValue, "neg");
    }
    return Builder.CreateNeg(RvalValue, "neg");
  }
}

Value *AssignmentExprASTNode::codegen() {
  Value *subExpr = SubExpr->codegen();
  if(NamedValues[Ident]) {
    Type *storedType = NamedValues[Ident]->getAllocatedType();
    subExpr = AttemptCast(NamedValues[Ident]->getAllocatedType(), subExpr);
    if(!subExpr || storedType != HighestType(storedType, subExpr->getType())) {
      return HandleErrorValue("Attempt to assign local variable with expression of unmatching type.");
    }
    return Builder.CreateStore(subExpr, NamedValues[Ident]);
  } else if(Globals[Ident]) {
    Type *storedType = Globals[Ident]->getValueType();
    subExpr = AttemptCast(Globals[Ident]->getValueType(), subExpr);
    if(!subExpr || storedType != HighestType(storedType, subExpr->getType())) {
      return HandleErrorValue("Attempt to assign global variable with expression of unmatching type.");
    }
    return Builder.CreateStore(subExpr, Globals[Ident]);
  } else {
    return HandleErrorValue("Reference to undeclared variable.");
  }
}

Value *ArgListASTNode::codegen() {
  //Handled in FunctionCallASTNode
  return nullptr;
}

Value *FunctionCallASTNode::codegen() {
  Function* calleeFunc = TheModule->getFunction(FuncName);
  std::vector<std::unique_ptr<ExprASTNode>> args = std::move(Args->Exprs);
  if (!calleeFunc) {
    return HandleErrorValue("Reference to undeclared function.");
  }
  if (calleeFunc->arg_size() != args.size()) {
    return HandleErrorValue("Incorrect number of arguments passed to function.");
  }  
  std::vector<Value*> codegenArgsV;
  Value *argV;
  Type *highest;
  unsigned int idx = 0;
  for (auto &arg : calleeFunc->args()) {
    argV = args[idx++]->codegen();
    highest = HighestType(arg.getType(), argV->getType());
    //Ensures the passed argument value is a lower or equal type to the intended argument type
    argV = AttemptCast(arg.getType(), argV);
    //Error if the casting happens to go wrong
    if (!argV || arg.getType()!=highest) {
      return HandleErrorValue("Incorrect value type for function argument.");
    }
    codegenArgsV.push_back(argV);
  }
  return Builder.CreateCall(calleeFunc, codegenArgsV, "calltmp");
}

Value *IntASTNode::codegen() {
  return ConstantInt::get(TheContext, APInt(32, Val, true));
}

Value *BoolASTNode::codegen() {
  if(Val) {
    return ConstantInt::get(TheContext, APInt(1, 1, true));
  }
  return ConstantInt::get(TheContext, APInt(1, 0, true));
}

Value *FloatASTNode::codegen() {
  return ConstantFP::get(TheContext, APFloat(Val));
}

Value *TimesASTNode::codegen() {
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  std::vector<Value*> valueVec = {LHS};
  std::vector<TOKEN> operators = {Op};
  std::unique_ptr<TimesASTNode> rightTmp = std::move(Right);
  int count = 1;
  while (rightTmp.get()!=nullptr) {
    valueVec.push_back(rightTmp->Left->codegen());
    if(rightTmp->Right) {
      operators.push_back(rightTmp->Op); 
    }
    rightTmp = std::move(rightTmp->Right);  
  }
  //Loop through valuevec and operators and createbinops
  Value *currVal = valueVec[0];
  for(int i = 0; i < valueVec.size()-1; i++) {
    TOKEN currOp = operators[i];
    Value *nextVal = valueVec[i+1];
    Type *targetType = HighestType(currVal->getType(), nextVal->getType());
    currVal = AttemptCast(targetType, currVal);
    nextVal = AttemptCast(targetType, nextVal);
    if(currOp.lexeme=="%" && targetType->isFloatTy()) {
      return HandleErrorValue("Modulus operator carried out with floating point numbers");
    }
    if(targetType->isIntegerTy()) {
      if(currOp.lexeme=="*") {
        currVal = Builder.CreateBinOp(Instruction::Mul, currVal, nextVal, "multmp");
      } else if(Op.lexeme=="/") {
        currVal = Builder.CreateBinOp(Instruction::SDiv, currVal, nextVal, "divtmp");
      } else {
        currVal = Builder.CreateBinOp(Instruction::SRem, currVal, nextVal, "modtmp");
      } 
    } else {
      if(currOp.lexeme=="*") {
        currVal = Builder.CreateBinOp(Instruction::FMul, currVal, nextVal, "multmp");
      } else {
        currVal = Builder.CreateBinOp(Instruction::FDiv, currVal, nextVal, "divtmp");
      }
    }
  }
  return currVal;
}

Value *AddASTNode::codegen() {
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  std::vector<Value*> valueVec = {LHS};
  std::vector<TOKEN> operators = {Op};
  std::unique_ptr<AddASTNode> rightTmp = std::move(Right);
  while (rightTmp.get()) {
    valueVec.push_back(rightTmp->Left->codegen());
    if(rightTmp->Right) {
      operators.push_back(rightTmp->Op); 
    }
    rightTmp = std::move(rightTmp->Right);    
  }
  //Loop through valuevec and operators and createbinops
  Value *currVal = valueVec[0];
  for(int i = 0; i < valueVec.size()-1; i++) {
    TOKEN currOp = operators[i];
    Value *nextVal = valueVec[i+1];
    Type *targetType = HighestType(currVal->getType(), nextVal->getType());
    currVal = AttemptCast(targetType, currVal);
    nextVal = AttemptCast(targetType, nextVal);
    if(targetType->isIntegerTy()) {
      if(currOp.lexeme=="+") {
        currVal = Builder.CreateBinOp(Instruction::Add, currVal, nextVal, "addtmp");
      } else {
        currVal = Builder.CreateBinOp(Instruction::Sub, currVal, nextVal, "subtmp");
      }
    } else {
      if(currOp.lexeme=="+") {
        currVal = Builder.CreateBinOp(Instruction::FAdd, currVal, nextVal, "addtmp");
      } else {
        currVal = Builder.CreateBinOp(Instruction::FSub, currVal, nextVal, "subtmp");
      }
    }
  }
  return currVal;
}

Value *CompASTNode::codegen() {
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  Value *RHS = Right->codegen();
  Type *targetType = HighestType(LHS->getType(), RHS->getType());
  LHS = AttemptCast(targetType, LHS);
  RHS = AttemptCast(targetType, RHS);
  // Value *temp;
  if(targetType->isIntegerTy()) {
    if(Op.lexeme==">") {
      return Builder.CreateICmpSGT(LHS, RHS, "gt");
    } else if(Op.lexeme==">=") {
      return Builder.CreateICmpSGE(LHS, RHS, "ge");
    } else if(Op.lexeme=="<") {
      return Builder.CreateICmpSLT(LHS, RHS, "lt");
    } else {
      return Builder.CreateICmpSLE(LHS, RHS, "le");
    }
  } else {
    if(Op.lexeme==">") {
      return Builder.CreateFCmpUGT(LHS, RHS, "gt");
    } else if(Op.lexeme==">=") {
      return Builder.CreateFCmpUGE(LHS, RHS, "ge");
    } else if(Op.lexeme=="<") {
      return Builder.CreateFCmpULT(LHS, RHS, "lt");
    } else {
      return Builder.CreateFCmpULE(LHS, RHS, "le");
    }
  }
  // return Builder.CreateUIToFP(temp, Type::getFloatTy(TheContext), "booltmp");
}

Value *EquivASTNode::codegen() {
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  Value *RHS = Right->codegen();
  Type *targetType = HighestType(LHS->getType(), RHS->getType());
  LHS = AttemptCast(targetType, LHS);
  RHS = AttemptCast(targetType, RHS);
  // Value *temp;
  if(targetType->isIntegerTy()) {
    if(Op.lexeme=="==") {
      return Builder.CreateICmpEQ(LHS, RHS, "eq");
    } else {
      return Builder.CreateICmpNE(LHS, RHS, "ne");
    }
  } else {
    if(Op.lexeme=="==") {
      return Builder.CreateFCmpUEQ(LHS, RHS, "eq");
    } else {
      return Builder.CreateFCmpUNE(LHS, RHS, "ne");
    }
  }
  // return Builder.CreateUIToFP(temp, Type::getFloatTy(TheContext), "booltmp");
}

Value *AndASTNode::codegen() {
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  Value *RHS = Right->codegen();
  //Ands should only be done on booleans. 
  //Program converts non bools to bools by checking if the value = 0. If not then the bool = 1
  LHS = AttemptCast(Builder.getInt1Ty(), LHS);
  RHS = AttemptCast(Builder.getInt1Ty(), RHS);
  return Builder.CreateBinOp(Instruction::And, LHS, RHS, "and");
}

Value *OrASTNode::codegen() {  
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  Value *RHS = Right->codegen();
  //Ors should only be done on booleans. 
  //Program converts non bools to bools by checking if the value = 0. If not then the bool = 1
  LHS = AttemptCast(Builder.getInt1Ty(), LHS);
  RHS = AttemptCast(Builder.getInt1Ty(), RHS);
  return Builder.CreateBinOp(Instruction::Or, LHS, RHS, "or");
}

Value *OrExprASTNode::codegen() {
  return OrExpression->codegen();
}

Value *ProgramASTNode::codegen() {
  for(auto &&extern_ : ExternList) {
    extern_->codegen();
  }
  for(auto &&decl : DeclList) {
    decl->codegen();
  }
  return nullptr;
}
//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const ASTnode &ast) {
  os << ast.to_string(0);
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
  std::unique_ptr<ProgramASTNode> prog = parser();
  llvm::outs() << prog->to_string(1) << "\n";
  fprintf(stderr, "Parsing Finished\n");
  // prog->codegen();

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
