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
// Codegen Stores
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

//Tables for local variables, global variables and functions
static std::map<std::string, AllocaInst*> NamedValues; 
static std::vector<std::string> localD;
static std::map<std::string, Function *> Functions;
static std::map<std::string, GlobalVariable *> Globals;

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

// StmtType - Enum representing statement types, used in each StmtASTNode
// Used to distinguish different statements within the CheckAllPathsReturn() function
enum StmtType {
  EXPR = 1,
  BLOCK = 2,
  IFSTMT = 3,
  ELSESTMT = 4,
  WHILESTMT = 5,
  RETSTMT = 6
}; 

// ASTnode - Base class for all AST nodes.
class ASTnode {
public:
  virtual ~ASTnode() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string(int indent) const {return "";};
};

// ParamASTNode - Represents a parameter to a function/extern function
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

// ParamsASTNode - Represents a group of parameters to a function/extern function
// If the parameter is void, then IsVoid = true and ParamList is empty.
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

// ExternASTNode - Represents extern function declarations
// Params can = nullptr to represent function with no arguments
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
    str += "\n";
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

// LocalDeclASTNode - Represents a local variable declaration within a function/block
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

// StmtASTNode - Abstract parent class to all statement types
// Children:
//      - BlockASTNode
//      - ExprStmtASTNode
//      - IfStmtASTNode
//      - WhileStmtASTNode
//      - ReturnStmtASTNode
class StmtASTNode : public ASTnode {
public:
  virtual StmtType stmtType(); // Function which gets the type of a statement
  virtual ~StmtASTNode() {}
};

// BlockASTNode - Represents a code block
// Both LocalDecls and Statements can be empty lists as the productions are nullable
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

// DeclASTNode - Parent class for FunDecl and VarDecl
class DeclASTNode : public ASTnode {
public:
  DeclASTNode() {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    return "Decl";
  }
};

// FunDeclASTNode - Class for function declarations
// Params can be a nullptr as params is nullable in the grammar
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

// VarDeclASTNode - Represents a global variable declaration
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

// ElseStmtASTNode - Represents the else part of an if-else statement
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

// RValASTNode - Abstract parent class of every type of rval defined in the grammar
// Children:
//     - IdentRvalASTNode
//     - ExprASTNode
//     - UnaryOpRValASTNode
//     - FunctionCallASTNode
//     - IntASTNode
//     - BoolASTNode
//     - FloatASTNode
class RValASTNode : public ASTnode {
public:
  virtual ~RValASTNode() {}
};

// IdentRvalASTNode - Represents a stored variable used in an expression
class IdentRvalASTNode : public RValASTNode {
  std::string Ident;

public:
  IdentRvalASTNode(std::string ident) : Ident(ident) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    str += Ident;
    return str;
  }
};

// ExprASTNode - Abstract parent class for expressions
// Children:
//    - AssignmentExprASTNode
//    - OrASTNode
class ExprASTNode : public RValASTNode {
public:
  virtual ~ExprASTNode() {}
};

// ExprStmtASTNode - Represents an expr_stmt
// Expr can be a nullptr according to the grammar
class ExprStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;
public:
  ExprStmtASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  StmtType stmtType() override {
    return EXPR;
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

// IfStmtASTNode - Represents an if statement
// Else pointer can = nullptr if there is no else
class IfStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;

public:
  std::unique_ptr<BlockASTNode> Block;
  std::unique_ptr<ElseStmtASTNode> ElseStmt;
  IfStmtASTNode(std::unique_ptr<ExprASTNode> expr, std::unique_ptr<BlockASTNode> block, std::unique_ptr<ElseStmtASTNode> elsestmt) : Expr(std::move(expr)), Block(std::move(block)), ElseStmt(std::move(elsestmt)) {}
  StmtType stmtType() override {
    return IFSTMT;
  }
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "If Statement\n";
    for(int i = 0; i<indent+1; i++) {
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

// WhileStmtASTNode - Represents a while statement
class WhileStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;
  std::unique_ptr<StmtASTNode> Statement;

public:
  WhileStmtASTNode(std::unique_ptr<ExprASTNode> expr, std::unique_ptr<StmtASTNode> statement) : Expr(std::move(expr)), Statement(std::move(statement)) {}
  StmtType stmtType() override {
    return WHILESTMT;
  }
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "While Statement\n";
    for(int i = 0; i<indent+1; i++) {
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

// ReturnStmtASTNode - Represents a return statement
class ReturnStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;

public: 
  ReturnStmtASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  StmtType stmtType() override {
    return RETSTMT;
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

// UnaryOpRValASTNode - Represents a negated or not rval
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

// AssignmentExprASTNode - Represents an assignment of an expression to a variable
class AssignmentExprASTNode : public ExprASTNode {
  std::string Ident;
  std::unique_ptr<ExprASTNode> SubExpr;

public:
  AssignmentExprASTNode(std::string ident, std::unique_ptr<ExprASTNode> subexpr) : Ident(ident), SubExpr(std::move(subexpr)) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str = "Assignment Expr\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += Ident;
    str += "\n";
    for(int i = 0; i<indent; i++) {
      str += "  ";
    }
    str += SubExpr->to_string(indent);
    return str;
  }
};

// ArgListASTNode - Represents a list of expressions that are arguments for a function call
// Exprs vector can be empty according to the grammar
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
      str += expr->to_string(indent);
    }
    return str;
  }
};

// FunctionCallASTNode - Represents a call to a defined function in an expression
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

// IntASTNode - Represents an integer literal
class IntASTNode : public RValASTNode {
  int Val;
public:
  IntASTNode(int val) : Val(val) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    str += std::to_string(Val);
    return str;
  }
};

// BoolASTNode - Represents a boolean literal
class BoolASTNode : public RValASTNode {
  bool Val;

public:
  BoolASTNode(bool val) : Val(val) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    str += std::to_string(Val);
    return str;
  }
};

// FloatASTNode - Represents a float literal
class FloatASTNode : public RValASTNode {
  float Val;

public:
  FloatASTNode(float val) : Val(val) {}
  Value *codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    str += std::to_string(Val);
    return str;
  }
};

// TimesASTNode - Represents two expressions multiplied/divided/modulused together
// Right and Op can be null according to spec
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
    if(Right!=nullptr) {
      hasRight = true;
    }    
    str += Left->to_string(indent);
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
      str += Right->to_string(indent);
    }
    return str;
  }
};

// AddASTNode - Represents two expressions added/subtracted together
// Right and Op can be null according to spec
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
    if(Right!=nullptr) {
      hasRight = true;
    }
    str += Left->to_string(indent);
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
      str += Right->to_string(indent);
    }
    return str;
  }
};

// CompASTNode - Represents two expressions compared with "<"/"<="/">"/">"
// Right and Op can be null according to spec
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
    if(Right!=nullptr) {
      hasRight = true;
    }
    str += Left->to_string(indent);
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
      str += Right->to_string(indent);
    }
    return str;
  }
};

// EquivASTNode - Represents two expressions compared with "=="/"!="
// Right and Op can be null according to spec
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
    if(Right!=nullptr) {
      hasRight = true;
    }
    str += Left->to_string(indent);
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
      str += Right->to_string(indent);
    }
    return str;
  }
};

// AndASTNode - Represents two expressions &&'ed together
// Right and Op can be null according to spec
class AndASTNode : public ASTnode {
  std::unique_ptr<EquivASTNode> Left;
  std::unique_ptr<AndASTNode> Right;

public:
  AndASTNode(std::unique_ptr<EquivASTNode> left, std::unique_ptr<AndASTNode> right) : Left(std::move(left)), Right(std::move(right)) {}
  Value* codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    bool hasRight = false;
    if(Right!=nullptr) {
      hasRight = true;
    }
    str += Left->to_string(indent);
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
      str += Right->to_string(indent);
    }
    return str;
  }
};

// OrASTNode - Represents two expressions ||'ed together
// Right and Op can be null according to spec
class OrASTNode : public ExprASTNode {
  std::unique_ptr<AndASTNode> Left;
  std::unique_ptr<OrASTNode> Right;

public:
  OrASTNode(std::unique_ptr<AndASTNode> left, std::unique_ptr<OrASTNode> right) : Left(std::move(left)), Right(std::move(right)) {}
  Value* codegen() override;
  std::string to_string(int indent) const override {
    std::string str;
    bool hasRight = false;
    if(Right!=nullptr) {
      hasRight = true;
    }
    str += Left->to_string(indent);
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
      str += Right->to_string(indent);
    }
    return str;
  }
};

// ProgramASTNode - Represents the first production in the grammar
// ExternList can be empty, DeclList cannot
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

// Prints a parsing error and stops parsing
static void HandleError(const char *str) {
  std::string msg = "Error on line: " + std::to_string(CurTok.lineNo) + " with token: " + CurTok.lexeme + ": " + str; 
  fprintf(stderr, "%s\n", msg.c_str());
  exit(0);
}

// params ::= param_list  
//          |  "void" | epsilon
// param_list ::= param "," param_list
// 	            |   param
// param ::= var_type IDENT
// Combines params, param_list and param into one production
std::unique_ptr<ParamsASTNode> ParseParams() {
  // Define our param list
  std::vector<std::unique_ptr<ParamASTNode>> paramList;
  // Deal with void case first
  if (CurTok.type == VOID_TOK) {
    getNextToken(); // Consume void
    getNextToken(); // Consume RPAR
    // Create Params with IsVoid = true with an empty list
    return std::make_unique<ParamsASTNode>(std::move(paramList), true);
  }  
  // Tracks if param list has any params in it
  bool isEpsilon = true;
  // Parse param_list. If the next token is one of the below then we know it's a param
  while (CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    // Param list won't be empty so set epsilon tracker to false
    isEpsilon = false;
    TOKEN varType = CurTok;
    getNextToken(); // Consume type
    if (CurTok.type != IDENT) {
      HandleError("Expected token IDENT in param production");
      return nullptr;
    }
    std::string ident = CurTok.lexeme;
    // Add param to paramList vector
    paramList.push_back(std::make_unique<ParamASTNode>(varType, ident));
    getNextToken(); // Consume ident
    // Next part would either be another param denoted by a comma, or from follow set it would be a bracket.
    if (CurTok.type != COMMA && CurTok.type != RPAR) {
      HandleError("Expected either ',' or ')' in param production");
      return nullptr;
    }
    getNextToken(); // Consume RPAR or COMMA
  }
  //Parse epsilon case
  if (isEpsilon) {
    getNextToken(); // Consume RPAR
    // If it is the epsilon case then we just return a nullptr to the function declaration
    return nullptr;
  }
  return std::make_unique<ParamsASTNode>(std::move(paramList), false);  
};

// local_decl ::= var_type IDENT ";"
std::unique_ptr<LocalDeclASTNode> ParseLocalDecl() {
  TOKEN varType = CurTok;
  getNextToken(); // Consume var_type
  if(CurTok.type!=IDENT) {
    HandleError("Expected token IDENT in local_decl production");
    return nullptr;
  }
  std::string ident = CurTok.lexeme;
  getNextToken(); // Consume ident
  if(CurTok.type!=SC) {
    HandleError("Expected token ';' in local_decl production");
    return nullptr;
  }
  getNextToken(); // Consume semicolon
  return std::make_unique<LocalDeclASTNode>(varType, ident);
}

// local_decls ::= local_decl local_decls
// 	             | epsilon
std::vector<std::unique_ptr<LocalDeclASTNode>> ParseLocalDecls() {
  // Create localDecls list
  std::vector<std::unique_ptr<LocalDeclASTNode>> localDecls;
  // If CurTok is one of the below then we have another localDecl to add to the list
  while (CurTok.type == BOOL_TOK || CurTok.type == FLOAT_TOK || CurTok.type == INT_TOK) {
    localDecls.push_back(std::move(ParseLocalDecl()));
  }
  // Follow set of local_decl
  std::vector<TOKEN_TYPE> followSet = {NOT, LPAR, MINUS, SC, IF, RETURN, WHILE, LBRA, RBRA, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  // Check follow set as we have an epsilon in the production
  auto search = std::find(followSet.begin(), followSet.end(), CurTok.type);
  if(search==followSet.end()) {
    HandleError("Expected either: '!', '(', '-', ';', 'if', 'return', 'while', '{', '}', BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT after local_decls production");
    return std::vector<std::unique_ptr<LocalDeclASTNode>>();
  }
  return std::move(localDecls);
}

// Declaring ParseExpr() here due to cycle in grammar
// expr ::= IDENT "=" expr
//        | operator_or
std::unique_ptr<ExprASTNode> ParseExpr();

// args ::= arg_list 
//        |  epsilon
// arg_list ::= expr "," arg_list
// 	          |   expr
//Combines both productions into one by checking the epsilon case then going into the arg_list production
std::unique_ptr<ArgListASTNode> ParseArgs() {
  // Declare list of exprs
  std::vector<std::unique_ptr<ExprASTNode>> exprs;
  // Left parenthesis has already been consumed in ParseRval()
  // If the current token is ")" then we have a function call with no arguments so return nullptr
  if(CurTok.type==RPAR) {
    return nullptr;
  }
  // If the current token is a comma then we have another expr to parse
  do {
    exprs.push_back(std::move(ParseExpr()));
  } while(CurTok.type==COMMA);
  return std::make_unique<ArgListASTNode>(std::move(exprs));
}

// rval ::= "-" rval | "!" rval
//        | "(" expr ")"
//        | IDENT | IDENT "(" args ")"
//        | INT_LIT | FLOAT_LIT | BOOL_LIT
std::unique_ptr<RValASTNode> ParseRval() {
  // Check the first set to handle errors in one go
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in rval production");
    return nullptr;
  }
  // Deal with the unary rvals
  if(CurTok.type==NOT || CurTok.type==MINUS) {
    TOKEN op = CurTok;
    getNextToken(); // Consume unary operator
    return std::make_unique<UnaryOpRValASTNode>(op, std::move(ParseRval()));
  // Deal with bracketed expressions
  } else if(CurTok.type==LPAR) {
    getNextToken(); //Consume "("
    std::unique_ptr<ExprASTNode> expr = std::move(ParseExpr());
    if(CurTok.type!=RPAR) {
      HandleError("Expected ')' after 'expr' in rval production");
      return nullptr;
    }
    getNextToken(); //Consume ")"
    return std::move(expr);
  // Deal with either a variable use or function declaration
  } else if(CurTok.type==IDENT) {
    std::string ident = CurTok.lexeme;
    getNextToken(); //Consume IDENT
    // "(" isnt in the follow set of rval so we can use this to check which IDENT it is
    if(CurTok.type==LPAR) {
      getNextToken(); //Consume "("
      std::unique_ptr<ArgListASTNode> args = std::move(ParseArgs());
      if (CurTok.type!=RPAR) {
        HandleError("Expected ')' after 'args' in rval production");
        return nullptr;
      }
      getNextToken(); //Consume ")"
      return std::make_unique<FunctionCallASTNode>(ident, std::move(args));
    }
    return std::make_unique<IdentRvalASTNode>(ident);
  // Deal with an integer rval
  } else if(CurTok.type==INT_LIT) {
    TOKEN tok = CurTok;
    int val = std::stoi(CurTok.lexeme);
    getNextToken(); //Consume int literal
    return std::make_unique<IntASTNode>(val);
  // Deal with a float rval
  } else if(CurTok.type==FLOAT_LIT) {
    TOKEN tok = CurTok;
    float val = std::stof(CurTok.lexeme);
    getNextToken(); //Consume float literal
    return std::make_unique<FloatASTNode>(val);
  // Deal with a boolean rval
  } else {
    TOKEN tok = CurTok;
    bool val;
    if(CurTok.lexeme=="true") {
      val = true;
    } else {
      val = false;
    }
    getNextToken(); //Consume bool literal
    return std::make_unique<BoolASTNode>(val);
  }
}

// operator_times ::= rval "*" operator_times
//                  | rval "/" operator_times
//                  | rval "%" operator_times
//                  | rval
std::unique_ptr<TimesASTNode> ParseOperatorTimes() {
  // Declare our left and right sides of the tree
  std::unique_ptr<RValASTNode> left;
  std::unique_ptr<TimesASTNode> right = nullptr;
  TOKEN op;
  // Check the first set to ensure no error
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_times production");
    return nullptr;
  }
  left = std::move(ParseRval());
  // If next token is a * or / or % then we have a right branch of the tree
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND, EQ, NE, LT, LE, GT, GE, PLUS, MINUS};
  if(CurTok.type==ASTERIX || CurTok.type==DIV || CurTok.type==MOD) {
    op = CurTok;
    getNextToken(); //Consume * or / or %
    right = std::move(ParseOperatorTimes());
  }
  // Check follow set for an error
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and', '==', '!=', '<', '<=', '>', '>=', '+', '-' in operator_times production");
    return nullptr;
  }
  return std::make_unique<TimesASTNode>(std::move(left), std::move(right), op);
}

// operator_add ::= operator_times "+" operator_add
//                | operator_times "-" operator_add
//                | operator_times
std::unique_ptr<AddASTNode> ParseOperatorAdd() {
  // Declare our left and right sides of the tree
  std::unique_ptr<TimesASTNode> left;
  std::unique_ptr<AddASTNode> right = nullptr;
  TOKEN op;
  // Check the first set to ensure no error
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_add production");
    return nullptr;
  }
  left = std::move(ParseOperatorTimes());
  // If next token is a + or - then we have a right branch of the tree
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND, EQ, NE, LT, LE, GT, GE};
  if(CurTok.type==MINUS || CurTok.type==PLUS) {
    op = CurTok;
    getNextToken(); //Consume + or -
    right = std::move(ParseOperatorAdd());
  }
  // Check follow set for an error
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and', '==', '!=', '<', '<=', '>', '>=', in operator_add production");
    return nullptr;
  }
  return std::make_unique<AddASTNode>(std::move(left), std::move(right), op);
}

// operator_comp ::= operator_add "<" operator_comp
//                  | operator_add "<=" operator_comp
//                  | operator_add ">" operator_comp
//                  | operator_add ">=" operator_comp
//                  | operator_add
std::unique_ptr<CompASTNode> ParseOperatorComp() {
  // Declare our left and right sides of the tree
  std::unique_ptr<AddASTNode> left;
  std::unique_ptr<CompASTNode> right = nullptr;
  TOKEN op;
  // Check the first set to ensure no error
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_comp production");
    return nullptr;
  }
  left = std::move(ParseOperatorAdd());
  // If next token is a < or <= or > or >= then we have a right branch of the tree
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND, EQ, NE};
  if(CurTok.type==LT || CurTok.type==LE || CurTok.type==GT || CurTok.type==GE) {
    op = CurTok;
    getNextToken(); //Consume < or <= or > or >=
    right = std::move(ParseOperatorComp());
  }
  // Check follow set for an error
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and', '==', '!=' in operator_comp production");
    return nullptr;
  }
  return std::make_unique<CompASTNode>(std::move(left), std::move(right), op);
}

// operator_equiv ::= operator_comp "==" operator_equiv
//                  | operator_comp "!=" operator_equiv
//                  | operator_comp
std::unique_ptr<EquivASTNode> ParseOperatorEquiv() {
  // Declare our left and right sides of the tree
  std::unique_ptr<CompASTNode> left;
  std::unique_ptr<EquivASTNode> right = nullptr;
  TOKEN op;
  // Check the first set to ensure no error
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_equiv production");
    return nullptr;
  }
  left = std::move(ParseOperatorComp());
  // If next token is a == or != then we have a right branch of the tree
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR, AND};
  if(CurTok.type==EQ || CurTok.type==NE) {
    op = CurTok;
    getNextToken(); //Consume == or !=
    right = std::move(ParseOperatorEquiv());
  }
  // Check follow set for an error
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or', 'and' after operator_equiv production");
    return nullptr;
  }
  return std::make_unique<EquivASTNode>(std::move(left), std::move(right), op);
}

// operator_and ::= operator_equiv "&&" operator_and
//               | operator_equiv
std::unique_ptr<AndASTNode> ParseOperatorAnd() {
  // Declare our left and right sides of the tree
  std::unique_ptr<EquivASTNode> left;
  std::unique_ptr<AndASTNode> right;
  // Check the first set to ensure no error
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_and production");
    return nullptr;
  }
  left = std::move(ParseOperatorEquiv());
  // If next token is a && then we have a right branch of the tree
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC, OR};
  if(CurTok.type==AND) {
    getNextToken(); //Consume &&
    right = std::move(ParseOperatorAnd());
  }
  // Check follow set for an error
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';', 'or' in operator_and production");
    return nullptr;
  }
  return std::make_unique<AndASTNode>(std::move(left), std::move(right));
}

// operator_or ::= operator_and "||" operator_or
//               | operator_and
std::unique_ptr<OrASTNode> ParseOperatorOr() {
  // Declare our left and right sides of the tree
  std::unique_ptr<AndASTNode> left;
  std::unique_ptr<OrASTNode> right = nullptr;
  // Check the first set to ensure no error
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in operator_or production");
    return nullptr;
  }
  left = std::move(ParseOperatorAnd());
  // If next token is a || then we have a right branch of the tree
  std::vector<TOKEN_TYPE> followSet = {RPAR, COMMA, SC};
  if(CurTok.type==OR) {
    getNextToken(); //Consume ||
    right = std::move(ParseOperatorOr());
  }
  // Check follow set for an error
  if (std::find(followSet.begin(), followSet.end(), CurTok.type) == followSet.end()) {
    HandleError("Expected either: ')', ',', ';' in operator_or production");
    return nullptr;
  }
  return std::make_unique<OrASTNode>(std::move(left), std::move(right));
}

// expr ::= IDENT "=" expr
//        | operator_or
std::unique_ptr<ExprASTNode> ParseExpr() {
  // Check next token is in first set
  std::vector<TOKEN_TYPE> firstSet = {NOT, LPAR, MINUS, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  if (std::find(firstSet.begin(), firstSet.end(), CurTok.type) == firstSet.end()) {
    HandleError("Expected either: '!', '(', '-', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in expr production");
    return nullptr;
  }
  // If the token is not an ident the expr must be an operator_or
  if(CurTok.type!=IDENT) {
    return std::move(ParseOperatorOr());
  // Otherwise we need to do more investigation to see if it is operator_or or not
  } else {    
    TOKEN ident = CurTok;
    TOKEN secondTok = getNextToken();
    CurTok = ident;
    // Put the lookahead token back in the queue
    putBackToken(secondTok);
    // IDENT in first part of expr is followed by an "=". This can never happen if the
    // IDENT is from rval. To check this we need to lookahead one token
    if(secondTok.type==ASSIGN) {
      // expr ::= IDENT "=" expr
      getNextToken(); //Consume IDENT
      getNextToken(); //Consume "="
      return std::make_unique<AssignmentExprASTNode>(ident.lexeme, std::move(ParseExpr()));
    } else {
      // expr ::= operator_or
      return std::move(ParseOperatorOr());
    }
  }
}

// block ::= "{" local_decls stmt_list "}"
// Declaring ParseBlock() here due to cycle in grammar
std::unique_ptr<BlockASTNode> ParseBlock();

// else_stmt ::= "else" block
//             | ε
std::unique_ptr<ElseStmtASTNode> ParseElseStmt() {
  // Check there is an else. If not then check the follow set to ensure no errors
  // If there is not an else then the else pointer in an if statement will be null
  if(CurTok.type!=ELSE) {
    std::vector<TOKEN_TYPE> followSet =	{NOT, LPAR, MINUS, SC, IF, RETURN, WHILE, LBRA, RBRA, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
    auto search = std::find(followSet.begin(), followSet.end(), CurTok.type);
    if(search==followSet.end()) {
      HandleError("Expected either: '!', '(', '-', ';', 'if', 'return', 'while', '{', '}', BOOL_LIT, INT_LIT, FLOAT_LIT, IDENT in else production");
    }
    return nullptr;
  }
  getNextToken(); //Consume "else"
  return std::make_unique<ElseStmtASTNode>(std::move(ParseBlock()));
}

// if_stmt ::= "if" "(" expr ")" block else_stmt
std::unique_ptr<IfStmtASTNode> ParseIfStmt() {
  // If token already consumed in ParseStmt() so check the left parenthesis
  if(CurTok.type!=LPAR) {
    HandleError("Expected '(' in if production");
    return nullptr;
  }
  getNextToken(); //Consume (
  // Get the expr
  std::unique_ptr<ExprASTNode> expr = std::move(ParseExpr());
  // Check the expr is followed by ")"
  if(CurTok.type!=RPAR) {
    HandleError("Expected '(' in if production");
    return nullptr;
  }
  getNextToken(); //Consume )
  std::unique_ptr<BlockASTNode> block = std::move(ParseBlock());
  std::unique_ptr<ElseStmtASTNode> elseStmt = std::move(ParseElseStmt());
  return std::make_unique<IfStmtASTNode>(std::move(expr), std::move(block), std::move(elseStmt));
}

// stmt ::= expr_stmt
//        | block
//        | if_stmt
//        | while_stmt
//        | return_stmt
// Declaring ParseStmt() here due to cycle in grammar
std::unique_ptr<StmtASTNode> ParseStmt();

// while_stmt ::= "while" "(" expr ")" stmt
std::unique_ptr<WhileStmtASTNode> ParseWhileStmt() {
  // While token consumed in ParseExpr() so check left parenthesis
  if(CurTok.type!=LPAR) {
    HandleError("Expected '(' in while production");
    return nullptr;
  }
  getNextToken(); //Consume (
  std::unique_ptr<ExprASTNode> expr = std::move(ParseExpr());
  // Check expression is followed by ")"
  if(CurTok.type!=RPAR) {
    HandleError("Expected ')' in if production");
    return nullptr;
  }
  getNextToken(); //Consume )
  std::unique_ptr<StmtASTNode> stmt = std::move(ParseStmt());
  return std::make_unique<WhileStmtASTNode>(std::move(expr), std::move(stmt));
}

// return_stmt ::= "return" ";"
//               | "return" expr ";"
std::unique_ptr<ReturnStmtASTNode> ParseReturnStmt() {
  // Return token consumed in ParseStmt() so check expr or semicolon
  // ";" | expr ";"
  // Check case with no expr first
  if(CurTok.type==SC) {
    getNextToken(); //Consume ;
    return std::make_unique<ReturnStmtASTNode>(nullptr);
  }
  std::unique_ptr<ExprASTNode> expr = std::move(ParseExpr());
  // Ensure return expr is followed by a semicolon
  if(CurTok.type!=SC) {
    HandleError("Expected ';' in return production");
    return nullptr;
  }
  getNextToken(); //Consume ;
  return std::make_unique<ReturnStmtASTNode>(std::move(expr));
}

// expr_stmt ::= expr ";"
//             | ";"
std::unique_ptr<ExprStmtASTNode> ParseExprStmt() {
  // Check case with no expr first
  if(CurTok.type==SC) {
    getNextToken(); //Consume ;
    return std::make_unique<ExprStmtASTNode>(nullptr);
  }
  std::unique_ptr<ExprASTNode> expr = std::move(ParseExpr());
  // Ensure expr is followed by semicolon
  if(CurTok.type!=SC) {
    HandleError("Expected ';' in expr_stmt production");
    return nullptr;
  }
  getNextToken(); //Consume ;
  return std::make_unique<ExprStmtASTNode>(std::move(expr));
}

// stmt ::= expr_stmt
//        | block
//        | if_stmt
//        | while_stmt
//        | return_stmt
std::unique_ptr<StmtASTNode> ParseStmt() {
  // Declare statement
  std::unique_ptr<StmtASTNode> stmt;
  // Handle if statement
  if(CurTok.type==IF) {
    getNextToken(); //Consume IF
    stmt = std::move(ParseIfStmt());
  // Handle while statement
  } else if(CurTok.type==WHILE) {
    getNextToken(); //Consume WHILE
    stmt = std::move(ParseWhileStmt());
  // Handle return statement
  } else if(CurTok.type==RETURN) {
    getNextToken(); //Consume RETURN
    stmt = std::move(ParseReturnStmt());
  // Handle block
  } else if(CurTok.type==LBRA) {
    stmt = std::move(ParseBlock());
  // Handle expr_stmt
  } else {
    stmt = std::move(ParseExprStmt());
  }
  return std::move(stmt);
}

// stmt_list ::= stmt stmt_list
//             | ε
std::vector<std::unique_ptr<StmtASTNode>> ParseStmtList() {
  // Declare statement list
  std::vector<std::unique_ptr<StmtASTNode>> stmtList;
  // Keep parsing statements until the current token is not in the first set of stmt_list
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
  return std::move(stmtList);
}

// block ::= "{" local_decls stmt_list "}"
std::unique_ptr<BlockASTNode> ParseBlock() {
  if (CurTok.type!=LBRA) {
    HandleError("Expected '{' in block production");
    return nullptr;
  }
  getNextToken(); //Consume "{"
  // Declare lists
  std::vector<std::unique_ptr<LocalDeclASTNode>> localDecls = std::move(ParseLocalDecls());
  std::vector<std::unique_ptr<StmtASTNode>> statements = std::move(ParseStmtList());
  // Check block ends in "}"
  if (CurTok.type!=RBRA) {
    HandleError("Expected '}' in block production");
    return nullptr;
  }
  getNextToken(); //Consume "}"
  return std::make_unique<BlockASTNode>(std::move(localDecls), std::move(statements));
}

// extern_list ::= extern extern_list
//               | extern
// extern ::= "extern" type_spec IDENT "(" params ")" ";"
// Handles both productions
std::vector<std::unique_ptr<ExternASTNode>> ParseExternList() {
  std::vector<std::unique_ptr<ExternASTNode>> externList;
  TOKEN type;
  std::string ident;
  // Keep parsing extern until current token is not an extern
  while(CurTok.type == EXTERN) {
    getNextToken(); //Consume "extern"
    // Handle type_spec
    if(CurTok.type==BOOL_TOK || CurTok.type==INT_TOK || CurTok.type==VOID_TOK || CurTok.type==FLOAT_TOK) {
      type = CurTok;
    } else {
      HandleError("Expected BOOL_TOK, INT_TOK, FLOAT_TOK, VOID_TOK in extern production");
      return std::vector<std::unique_ptr<ExternASTNode>>();
    }
    getNextToken(); //Consumes type_spec
    // Handle ident
    if (CurTok.type==IDENT) {
      ident = CurTok.lexeme;
    } else {
      HandleError("Expected IDENT in extern production");
      return std::vector<std::unique_ptr<ExternASTNode>>();
    }
    getNextToken(); //Consumes IDENT
    // Check there is a left parenthesis
    if (CurTok.type!=LPAR) {
      HandleError("Expected token '(' in extern production");
      return std::vector<std::unique_ptr<ExternASTNode>>();
    }
    getNextToken(); //Consume "("
    // Parse and store the params of an extern function
    std::unique_ptr<ParamsASTNode> params = std::move(ParseParams());
    // Check extern ends in a semicolon
    if (CurTok.type!=SC) {
      HandleError("Expected ';' in extern production");
      return std::vector<std::unique_ptr<ExternASTNode>>();
    }
    getNextToken(); //Consumes ";"
    // Add extern to the externList
    externList.push_back(std::make_unique<ExternASTNode>(type, ident, std::move(params)));
  }
  // If an extern is never encountered an empty vector is returned
  // Otherwise the list containing all externs is returned
  return std::move(externList);
};

// var_decl ::= var_type IDENT ";"
std::unique_ptr<VarDeclASTNode> ParseVarDecl() {
  // Check first set of var_decl
  if (CurTok.type!=INT_TOK && CurTok.type!=FLOAT_TOK && CurTok.type!=BOOL_TOK) {
    HandleError("Expected BOOL_TOK, INT_TOK, FLOAT_TOK in var_decl production");
    return nullptr;
  }
  TOKEN varType = CurTok;
  getNextToken(); //Consume var_type
  // Check next token is an ident
  if (CurTok.type!=IDENT) {
    HandleError("Expected IDENT in var_decl production");
    return nullptr;
  }
  std::string ident = CurTok.lexeme; 
  getNextToken(); //Consume IDENT
  // CHeck var_decl ends in semicolon
  if(CurTok.type!=SC) {
    HandleError("Expected ';' in var_decl production");
    return nullptr;
  }
  getNextToken(); //Consume ";"
  return std::make_unique<VarDeclASTNode>(varType, ident);
}

// fun_decl ::= type_spec IDENT "(" params ")" block
std::unique_ptr<FunDeclASTNode> ParseFunDecl() {
  // Check fun_decl first set
  if (CurTok.type!=INT_TOK && CurTok.type!=FLOAT_TOK && CurTok.type!=BOOL_TOK && CurTok.type!=VOID_TOK) {
    HandleError("Expected BOOL_TOK, INT_TOK, FLOAT_TOK in fun_decl production");
    return nullptr;
  }
  TOKEN varType = CurTok;
  getNextToken(); //Consumes type_spec
  if (CurTok.type!=IDENT) {
    HandleError("Expected IDENT in fun_decl production");
    return nullptr;
  }
  std::string ident = CurTok.lexeme;
  getNextToken(); //Consumes IDENT
  // Check for left parenthesis
  if (CurTok.type!=LPAR) {
    HandleError("Expected token '(' in fun_decl production");
    return nullptr;
  }
  getNextToken(); //Consume "("
  // Parse the function's parameters and body
  std::unique_ptr<ParamsASTNode> params = std::move(ParseParams());
  std::unique_ptr<BlockASTNode> block = std::move(ParseBlock());
  return std::make_unique<FunDeclASTNode>(varType, ident, std::move(params), std::move(block));
}

// decl_list ::= decl decl_list
//             | decl
// decl ::= var_decl 
//        | fun_decl
// Combines the two productions together
std::vector<std::unique_ptr<DeclASTNode>> ParseDeclList() {
  std::vector<std::unique_ptr<DeclASTNode>> declList;
  bool isDecl = true;
  // Parse decls until there are none left
  while(isDecl) {
    // Determining which the type of decl requires a lookahead since 
    // the difference between them (unless it's a void function decl) is 3 tokens ahead
    TOKEN current = CurTok;
    TOKEN secondTok = getNextToken();  
    TOKEN thirdTok = getNextToken();
    // Get the lookaheads then put them back in the queue
    putBackToken(thirdTok);
    putBackToken(secondTok);
    CurTok = current;
    // Must be a var_decl
    if (thirdTok.type == SC) {
      declList.push_back(std::move(ParseVarDecl()));
    // Must be a fun_decl
    } else if(thirdTok.type == LPAR || current.type == VOID_TOK) {
      declList.push_back(std::move(ParseFunDecl()));
    // No more decls to parse
    } else {
      isDecl = false;
    }
  }
  return std::move(declList);
};

// program ::= extern_list decl_list
//           | decl_list
static std::unique_ptr<ProgramASTNode> parser() {
  std::vector<std::unique_ptr<ExternASTNode>> externList = std::move(ParseExternList());
  std::vector<std::unique_ptr<DeclASTNode>> declList = std::move(ParseDeclList());
  return std::make_unique<ProgramASTNode>(std::move(externList), std::move(declList)); 
}

//===----------------------------------------------------------------------===//
// Codegen Functions
//===----------------------------------------------------------------------===//

// Functions which assist in the codegen

// Handles semantic errors
Value *HandleErrorValue(std::string str) {
  std::string msg = "Semantic Error: " + str; 
  fprintf(stderr, "%s\n", msg.c_str());
  exit(0);
}

// Creates allocas
static AllocaInst* CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName, Type *Type_) {
  IRBuilder<> tmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
  return tmpB.CreateAlloca(Type_, 0, VarName.c_str());
}

// Gets the llvm type from a TOKEN
// Returns a null pointer if the TOKEN is not a type
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

// Attempts to cast a value to a given goalType
// A type can be widened regardless of associated value, but a narrowing conversion should only happen in certain boolean cases
static Value* AttemptCast(Type *goalType, Value *v) {
  // Narrowing conversion to boolean is allowed in some cases so we handle whether it's valid there
  // Goal is bool so we make a comparison between the value and 0
  if (goalType->isIntegerTy(1)) {
    if (v->getType()->isIntegerTy(32)) {
      return Builder.CreateICmpNE(v, ConstantInt::get(TheContext, APInt(32, 0, true)), "tobool");
    } else if (v->getType()->isFloatTy()) {
      return Builder.CreateFCmpONE(v, ConstantFP::get(TheContext, APFloat(0.0f)), "tobool");
    } else if (v->getType()->isIntegerTy(1)) {
      // No conversion needed
      return v;
    } else {
      return nullptr;
    }  
  }  
  // Goal is an integer
  else if (goalType->isIntegerTy(32)) {
    if (v->getType()->isIntegerTy(1)) {
      return Builder.CreateZExt(v, Type::getInt32Ty(TheContext));
    // Do not allow narrowing conversion from float to an integer
    } else if (v->getType()->isFloatTy()) {
      return HandleErrorValue("Narrowing conversion not allowed: converting float to int.");
    } else if (v->getType()->isIntegerTy(32)) {
      // No conversion needed
      return v;
    } else {
      return nullptr;   
    }
  }  
  // Goal is a float
  // Floats are the highest type so any other type can be widened to a float
  else if (goalType->isFloatTy()) {
    if (v->getType()->isIntegerTy(32)) {
      return Builder.CreateSIToFP(v, Type::getFloatTy(TheContext));
    } else if (v->getType()->isIntegerTy(1)) {
      return Builder.CreateSIToFP(v, Type::getFloatTy(TheContext));
    } else if (v->getType()->isFloatTy()) {
      // No conversion needed
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

// Returns the "highest" of two types where hierarchy is defined as float -> int -> bool
static Type* HighestType(Type *t1, Type *t2) {
  if(t1->isFloatTy() || t2->isFloatTy()) {
    return Type::getFloatTy(TheContext);
  }
  if(t1->isIntegerTy(32) || t2->isIntegerTy(32)) {
    return Type::getInt32Ty(TheContext);
  }
  return Type::getInt1Ty(TheContext);
}

// Checks if all code paths in a block return a value
static bool CheckAllPathsReturn(BlockASTNode* block) {
  //Only care about the block's statements
  for(auto &&stmt : block->Statements) {
    // If a return statement is defined first then we don't have to check any statements defined after it as they won't be executed
    if(stmt->stmtType()==RETSTMT) {
      return true;
    }
    // Must be either if/while/block/expr
    // Handle block case
    if(stmt->stmtType()==BLOCK) {
      // Can static cast because we know it must be a BlockASTNode
      BlockASTNode* subBlock = static_cast<BlockASTNode*>(stmt.get());
      // Recursively check if all paths in the subBlock have a return statement;
      return CheckAllPathsReturn(subBlock);
    }
    // Handle if case
    if(stmt->stmtType()==IFSTMT) {
      // Can static cast because we know it must be an IfStmtASTNode
      IfStmtASTNode* ifBlock = static_cast<IfStmtASTNode*>(stmt.get());
      // Check if the if statement has an else
      if(!ifBlock->ElseStmt) {
        //If the if statement has no else stmt it doesn't matter whether it has a return statement or not
        continue; 
      // If it has an else stmt then check that both the if body and else body return values
      } else {
        if(CheckAllPathsReturn(ifBlock->Block.get()) && CheckAllPathsReturn(ifBlock->ElseStmt->Block.get())){
          return true;
        }
      }
    }
    // We don't need to handle exprs since they have no sub blocks
    // We don't need to handle whiles since we cannot guarantee that a return defined within will be reached before runtime
  }
  // Return false if no sub blocks return something
  return false;
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

Value *ParamASTNode::codegen() {
  // Handled in FunDeclASTNode and ExternASTNode due to how functions are created in llvm IR
  return nullptr;
}

Value *ParamsASTNode::codegen() {
  // Handled in FunDeclASTNode and ExternASTNode due to how functions are created in llvm IR
  return nullptr;
}

Value *ExternASTNode::codegen() {
  // Check if the function is declared already
  if(Functions[FuncName]) {
    return HandleErrorValue("Function declared multiple times. ");
  } 
  std::vector<Type*> argTypes;
  bool isVoid=false;
  // Check if the parameter is void
  // Get the types of each parameter
  if(Params) {
    isVoid = Params->IsVoid;
    for(auto&& param : Params->ParamList) {
      argTypes.push_back(GetTypeOfToken(param->VarType));
    }
  }
  // Get the type of the function
  Type *funcType = GetTypeOfToken(VarType);
  // Create the function
  FunctionType *FT = FunctionType::get(funcType, argTypes, false);
  Function *F = Function::Create(FT, Function::ExternalLinkage, FuncName, TheModule.get());
  unsigned idx = 0;
  // Set the names of the arguments
  for(auto &arg : F->args()) {
    if(!isVoid) {
      arg.setName(Params->ParamList[idx++]->Ident);
    }
  }
  // Verify function returns true if there is an error verifying
  if(verifyFunction(*F)) {
    return nullptr;
  }
  // Add the function to the function map
  Functions[FuncName] = F;
  return F;
}

Value *LocalDeclASTNode::codegen() {
  // Get the current block being inserted into
  BasicBlock *currBlock = Builder.GetInsertBlock();
  // LocalDecl only happens in a function so get the function we are declaring in
  Function *function = currBlock->getParent();
  // Get the type of the variable
  Type *type = GetTypeOfToken(VarType);
  if(NamedValues[Ident]) {
    // Variable has been previously defined in this block
    if(std::find(localD.begin(), localD.end(), Ident)!=localD.end()) {
      return HandleErrorValue("Local variable declared multiple times.");
    }
    // Add the variable name to the store to indicate this is a local variable
    localD.push_back(Ident);
  }  
  // Create alloca and store in the local variables map
  AllocaInst *alloca = CreateEntryBlockAlloca(function, Ident, type);
  NamedValues[Ident] = alloca;
  return alloca;
}

Value *BlockASTNode::codegen() {
  // Blocks (aside from function decls) retain local variables from the outer block
  // Variables declared in the block should not persist after the block
  std::map<std::string, AllocaInst*> OldNamedVals;
  std::vector<std::string> OldLocalD;
  OldNamedVals.insert(NamedValues.begin(), NamedValues.end());
  OldLocalD = localD;
  // Handle local decls vector
  for(auto &&decl : LocalDecls){
    decl->codegen();
  }
  // Handle Statements vector
  for(auto &&stmt : Statements){
    stmt->codegen();
  }
  // Now remove any new variables from NamedVals;
  // Loop updates the value of any variable in NamedValues that was declared before the block
  // Second expression checks if a variable was redeclared in the block
  for(auto &&kvp : NamedValues) {
    if(OldNamedVals.count(kvp.first)!=0 && std::find(localD.begin(), localD.end(), kvp.first)==localD.end()) {
      OldNamedVals[kvp.first] = NamedValues[kvp.first];
    }
  }
  // Return stores back to how they were before the block (aside from updated variables)
  localD.clear();
  localD = OldLocalD;
  NamedValues.clear();
  NamedValues.insert(OldNamedVals.begin(), OldNamedVals.end());
  return nullptr;
}

Value *FunDeclASTNode::codegen() {
  // Check if the function is declared already
  if(Functions[FuncName]) {
    return HandleErrorValue("Function declared multiple times. ");
  } 
  std::vector<Type*> argTypes;
  bool isVoid=false;
  // Check if the parameter is void
  // Get the types of each parameter
  if(Params) {
    isVoid = Params->IsVoid;
    for(auto&& param : Params->ParamList) {
      argTypes.push_back(GetTypeOfToken(param->VarType));
    }
  }
  // Get the type of the function
  Type *funcType = GetTypeOfToken(VarType);
  // Create the function
  FunctionType *FT = FunctionType::get(funcType, argTypes, false);
  Function *F = Function::Create(FT, Function::ExternalLinkage, FuncName, TheModule.get());
  unsigned idx = 0;
  // Set the names of the arguments
  for(auto &arg : F->args()) {
    if(!isVoid) {
      arg.setName(Params->ParamList[idx++]->Ident);
    }
  }
  BasicBlock *BB = BasicBlock::Create(TheContext, "func", F);
  // Insert into the function
  Builder.SetInsertPoint(BB);
  // Function definition will have new local variables
  NamedValues.clear();
  // Allocate the arguments and store in NamedValues
  if(!isVoid){
    for(auto &arg : F->args()){
      AllocaInst *alloca = CreateEntryBlockAlloca(F, arg.getName().data(), arg.getType());
      Builder.CreateStore(&arg, alloca);
      NamedValues[arg.getName().data()] = alloca;
    }
  }  
  // Generate IR for the function body
  Block->codegen();
  // Check all code paths return something
  if(!CheckAllPathsReturn(Block.get())) {
    return HandleErrorValue("Not all function code paths return a value.");
  }  
  // Verify function returns true if there is an error verifying
  if(verifyFunction(*F)) {
    return nullptr;
  }
  Functions[FuncName] = F;
  return F;
}

Value *VarDeclASTNode::codegen() {
  // Only happen outside functions because of local_decl
  // Get the llvm type of the stored type
  Type *type = GetTypeOfToken(VarType);
  if (Globals[Ident]) {
    return HandleErrorValue("Global value declared multiple times.");
  }
  // Create and store a global variable
  GlobalVariable *g = new GlobalVariable(*TheModule, type, false, GlobalValue::CommonLinkage, Constant::getNullValue(type), Ident);
  Globals[Ident] = g;
  return g;
}

Value *ElseStmtASTNode::codegen() {
  // Generate the else body's IR
  return Block->codegen();
}

Value *IdentRvalASTNode::codegen() {
  // Prioritises named values over globals
  Value *temp = nullptr;
  // Load the correct alloca
  if(Globals[Ident]) {
    temp = Builder.CreateLoad(Globals[Ident]->getValueType(), Globals[Ident], Ident);
  }
  if(NamedValues[Ident]) {
    temp = Builder.CreateLoad(NamedValues[Ident]->getAllocatedType(), NamedValues[Ident], Ident);
  }
  if (!temp) {
    return HandleErrorValue("Reference to undefined variable name.");
  }
  return temp;
}

Value *ExprStmtASTNode::codegen() {
  // ExprStmt can have null expr so need to check
  if(Expr) {
    return Expr->codegen();
  } else {
    return nullptr;
  }
}

Value *IfStmtASTNode::codegen() {
  // Generate the conditional's IR
  Value *CondV = Expr->codegen();
  // Convert the conditional expression into a bool if it isn't already
  CondV = AttemptCast(Type::getInt1Ty(TheContext), CondV);
  if (!CondV)
    return HandleErrorValue("If conditional not a conditional.");
  // Define the code blocks
  Function *function = Builder.GetInsertBlock()->getParent();
  BasicBlock *true_ = BasicBlock::Create(TheContext, "iftrue", function);
  BasicBlock *else_ = BasicBlock::Create(TheContext, "else");
  BasicBlock *end_ = BasicBlock::Create(TheContext, "end");
  // Create a conditional branch based on whether CondV is true or false
  Builder.CreateCondBr(CondV, true_, else_);
  Builder.SetInsertPoint(true_);
  // Generate the IR for the if body in the true block
  Block->codegen();
  // Create unconditional branch to the end block
  Builder.CreateBr(end_);
  function->insert(function->end(), else_);
  Builder.SetInsertPoint(else_);
  // Generate the IR for the else body in the else block, if the else body exists
  if(ElseStmt) {
    Value *ElseV = ElseStmt->codegen();
  }
  // Create unconditional branch to the end block
  Builder.CreateBr(end_);
  function->insert(function->end(), end_);
  // Now following code is written to end block
  Builder.SetInsertPoint(end_);
  return nullptr;
}

Value *WhileStmtASTNode::codegen() {
  // Define the code blocks
  Function *function = Builder.GetInsertBlock()->getParent();
  BasicBlock *condition = BasicBlock::Create(TheContext, "cond", function);
  BasicBlock *true_ = BasicBlock::Create(TheContext, "iftrue", function);
  BasicBlock *end_ = BasicBlock::Create(TheContext, "end");
  // Create unconditional branch to the condition statement
  Builder.CreateBr(condition);
  Builder.SetInsertPoint(condition);
  // Generate the conditional's IR within the condition block
  Value *CondV = Expr->codegen();
  // Convert to bool if it isn't already
  CondV = AttemptCast(Type::getInt1Ty(TheContext), CondV);
  if (!CondV) {
    return HandleErrorValue("While conditional not a conditional.");
  }
  // Create a conditional branch based on whether CondV is true or false
  Builder.CreateCondBr(CondV, true_, end_);
  Builder.SetInsertPoint(true_);
  // Generate the IR for the while body in the true block
  Statement->codegen();
  // Create unconditional branch back to the condition block
  Builder.CreateBr(condition);
  function->insert(function->end(), end_);
  // Now following code is written to end block
  Builder.SetInsertPoint(end_);
  return nullptr;
}

Value *ReturnStmtASTNode::codegen() {
  // Get the function the return statement is defined in
  Function *function = Builder.GetInsertBlock()->getParent();
  Type *funcType = function->getReturnType();
  // If Expr is not a nullptr then we have a return with an expression
  if(Expr) {
    Value *retVal = Expr->codegen();
    // Get the highest type between the type of the return value and the function type
    Type *type = HighestType(retVal->getType(), funcType);
    // Cast the return value to the highest type
    retVal = AttemptCast(type, retVal);
    // If the highest type is not the same as the function type, then we would be narrowing a value when returning so error
    if(!retVal || type != funcType) {
      // If !retVal then the return statement has an expression. This creates an error if the function is defined as void.
      if(funcType->isVoidTy()) {
        return HandleErrorValue("Void function returns value.");
      }
      return HandleErrorValue("Function return value has higher type than defined return type.");
    }
    return Builder.CreateRet(retVal);
  // If Expr is a nullptr then we have a void return
  } else {
    // Make sure the function is defined as void if there is a void return
    if(!funcType->isVoidTy()) {
      return HandleErrorValue("Non-void function is returning void.");
    }
    return Builder.CreateRetVoid();
  }
}

Value *UnaryOpRValASTNode::codegen() {
  // Generate the rval's IR
  Value *RvalValue = RVal->codegen();
  if(Op.lexeme=="!") {
    // "!" can only be used with booleans
    RvalValue = AttemptCast(Type::getInt1Ty(TheContext), RvalValue);
    if(!RvalValue) {
      return HandleErrorValue("Unable to cast negated value to boolean.");
    }
    return Builder.CreateNot(RvalValue, "not");
  } else {
    // Must be a negated value
    if(RvalValue->getType()->isFloatTy()) {
      return Builder.CreateFNeg(RvalValue, "neg");
    }
    return Builder.CreateNeg(RvalValue, "neg");
  }
}

Value *AssignmentExprASTNode::codegen() {
  // Generate the expression to the right of the "="'s IR
  Value *subExpr = SubExpr->codegen();
  // Check NamedValues first as we prioritise local variables over globals with the same name
  if(NamedValues[Ident]) {
    Type *storedType = NamedValues[Ident]->getAllocatedType();
    // Cast the subexpression to the type of the variable
    subExpr = AttemptCast(NamedValues[Ident]->getAllocatedType(), subExpr);
    if(!subExpr || storedType != HighestType(storedType, subExpr->getType())) {
      return HandleErrorValue("Attempt to assign local variable with expression of unmatching type.");
    }
    // Store the value of the subexpression in the variable
    return Builder.CreateStore(subExpr, NamedValues[Ident]);
  } else if(Globals[Ident]) {
    Type *storedType = Globals[Ident]->getValueType();
    // Cast the subexpression to the type of the global variable
    subExpr = AttemptCast(Globals[Ident]->getValueType(), subExpr);
    if(!subExpr || storedType != HighestType(storedType, subExpr->getType())) {
      return HandleErrorValue("Attempt to assign global variable with expression of unmatching type.");
    }
    // Store the value of the subexpression in the global variable
    return Builder.CreateStore(subExpr, Globals[Ident]);
  } else {
    return HandleErrorValue("Reference to undeclared variable.");
  }
}

Value *ArgListASTNode::codegen() {
  // Handled in FunctionCallASTNode
  return nullptr;
}

Value *FunctionCallASTNode::codegen() {
  // Get the function being called
  Function* calleeFunc = TheModule->getFunction(FuncName);
  // Get the arguments of the function call
  std::vector<std::unique_ptr<ExprASTNode>> args;
  if(Args) {
    args = std::move(Args->Exprs);
  }
  if (!calleeFunc) {
    return HandleErrorValue("Reference to undeclared function.");
  }
  if (calleeFunc->arg_size() != args.size()) {
    return HandleErrorValue("Incorrect number of arguments passed to function.");
  }  
  // Store the generated values of the arguments passed
  std::vector<Value*> codegenArgsV;
  Value *argV;
  Type *highest;
  unsigned int idx = 0;
  // Loop through the arguments of the called function
  for (auto &arg : calleeFunc->args()) {
    argV = args[idx++]->codegen();
    highest = HighestType(arg.getType(), argV->getType());
    // Ensures the passed argument value is a lower or equal type to the intended argument type
    argV = AttemptCast(arg.getType(), argV);
    // Error if it cannot cast or the passed value has a higher type than the function defined
    if (!argV || arg.getType()!=highest) {
      return HandleErrorValue("Incorrect value type for function argument.");
    }
    // Add the argument value to the store
    codegenArgsV.push_back(argV);
  }
  return Builder.CreateCall(calleeFunc, codegenArgsV, "call");
}

Value *IntASTNode::codegen() {
  // Return the value as a constant int32
  return ConstantInt::get(TheContext, APInt(32, Val, true));
}

Value *BoolASTNode::codegen() {
  // Return the value as a constant int1
  if(Val) {
    return ConstantInt::get(TheContext, APInt(1, 1, true));
  }
  return ConstantInt::get(TheContext, APInt(1, 0, true));
}

Value *FloatASTNode::codegen() {
  // Return the value as a constant float
  return ConstantFP::get(TheContext, APFloat(Val));
}

Value *TimesASTNode::codegen() {
  // Return the IR for the left side of the times if there is no right side
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  // Stores to calculate the values of the TimesNode children
  std::vector<Value*> valueVec = {LHS};
  std::vector<TOKEN> operators = {Op};
  std::unique_ptr<TimesASTNode> rightTmp = std::move(Right);
  int count = 1;
  // While the next right subchild has its own right subchild
  while (rightTmp.get()!=nullptr) {
    // Generate IR for rightTmp's left subchild
    valueVec.push_back(rightTmp->Left->codegen());
    // Store the of a rightTmp if it has a right subchild
    if(rightTmp->Right) {
      operators.push_back(rightTmp->Op); 
    }
    // rightTmp now points to its right subchild
    rightTmp = std::move(rightTmp->Right);  
  }
  // Loop through valuevec and operators to createbinops
  Value *currVal = valueVec[0];
  for(int i = 0; i < valueVec.size()-1; i++) {
    TOKEN currOp = operators[i];
    Value *nextVal = valueVec[i+1];
    Type *targetType = HighestType(currVal->getType(), nextVal->getType());
    // Equalise the types of the two values about to be used
    currVal = AttemptCast(targetType, currVal);
    nextVal = AttemptCast(targetType, nextVal);
    // Modulus operator only works with integers, not floats
    if(currOp.lexeme=="%" && targetType->isFloatTy()) {
      return HandleErrorValue("Modulus operator carried out with floating point numbers");
    }
    // Integer binops
    if(targetType->isIntegerTy()) {
      if(currOp.lexeme=="*") {
        currVal = Builder.CreateBinOp(Instruction::Mul, currVal, nextVal, "multmp");
      } else if(Op.lexeme=="/") {
        currVal = Builder.CreateBinOp(Instruction::SDiv, currVal, nextVal, "divtmp");
      } else {
        currVal = Builder.CreateBinOp(Instruction::SRem, currVal, nextVal, "modtmp");
      } 
    // Float binops
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
  // Return the IR for the left side of the times if there is no right side
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  // Stores to calculate the values of the AddNode children
  std::vector<Value*> valueVec = {LHS};
  std::vector<TOKEN> operators = {Op};
  std::unique_ptr<AddASTNode> rightTmp = std::move(Right);
  // While the next right subchild has its own right subchild
  while (rightTmp.get()) {
    // Generate IR for rightTmp's left subchild
    valueVec.push_back(rightTmp->Left->codegen());
    // Store the of a rightTmp if it has a right subchild
    if(rightTmp->Right) {
      operators.push_back(rightTmp->Op); 
    }
    // rightTmp now points to its right subchild
    rightTmp = std::move(rightTmp->Right);    
  }
  //Loop through valuevec and operators and createbinops
  Value *currVal = valueVec[0];
  for(int i = 0; i < valueVec.size()-1; i++) {
    TOKEN currOp = operators[i];
    Value *nextVal = valueVec[i+1];
    Type *targetType = HighestType(currVal->getType(), nextVal->getType());
    // Equalise the types of the two values about to be used
    currVal = AttemptCast(targetType, currVal);
    nextVal = AttemptCast(targetType, nextVal);
    // Integer binops
    if(targetType->isIntegerTy()) {
      if(currOp.lexeme=="+") {
        currVal = Builder.CreateBinOp(Instruction::Add, currVal, nextVal, "addtmp");
      } else {
        currVal = Builder.CreateBinOp(Instruction::Sub, currVal, nextVal, "subtmp");
      }
    // Float binops
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
  // Return the IR for the left side of the times if there is no right side
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  Value *RHS = Right->codegen();
  // Equalise the types of the two values to be compared
  Type *targetType = HighestType(LHS->getType(), RHS->getType());
  LHS = AttemptCast(targetType, LHS);
  RHS = AttemptCast(targetType, RHS);
  // Integer binops
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
  // Float binops
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
}

Value *EquivASTNode::codegen() {
  // Return the IR for the left side of the times if there is no right side
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  Value *RHS = Right->codegen();
  // Equalise the types of the two values to be compared
  Type *targetType = HighestType(LHS->getType(), RHS->getType());
  LHS = AttemptCast(targetType, LHS);
  RHS = AttemptCast(targetType, RHS);
  // Integer binops
  if(targetType->isIntegerTy()) {
    if(Op.lexeme=="==") {
      return Builder.CreateICmpEQ(LHS, RHS, "eq");
    } else {
      return Builder.CreateICmpNE(LHS, RHS, "ne");
    }
  // Float binops
  } else {
    if(Op.lexeme=="==") {
      return Builder.CreateFCmpUEQ(LHS, RHS, "eq");
    } else {
      return Builder.CreateFCmpUNE(LHS, RHS, "ne");
    }
  }
}

Value *AndASTNode::codegen() {
  // Return the IR for the left side of the times if there is no right side
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  std::unique_ptr<AndASTNode> rightTmp = std::move(Right);
  Function* function = Builder.GetInsertBlock()->getParent();
  // Block for after the and has been dealt with
  BasicBlock *end_ = BasicBlock::Create(Builder.getContext(), "end_", function);
  // Create value and basic block stores for phi node
  std::vector<Value*> valueVec = {LHS};
  std::vector<BasicBlock*> bbVec = {Builder.GetInsertBlock()};
  // Loop through the right subchildren of the AndNode
  while (rightTmp.get()) {
    // Create blocks for evaluating RHS
    BasicBlock *rhs_ = BasicBlock::Create(Builder.getContext(), "rhs_", function);
    // If LHS is true it branches to the end block and doesn't execute the unnecessary conditional
    Builder.CreateCondBr(LHS, rhs_, end_);
    Builder.SetInsertPoint(rhs_);
    // Generate IR for the next conditional
    LHS = rightTmp->Left->codegen();
    // Add the value and basic block to the stores for the phi node
    valueVec.push_back(LHS);
    bbVec.push_back(rhs_);
    rightTmp = std::move(rightTmp->Right);
  }
  Builder.CreateBr(end_);
  Builder.SetInsertPoint(end_);
  // Add the values and blocks to a phi node
  PHINode *Result = Builder.CreatePHI(Type::getInt1Ty(TheContext), valueVec.size(), "andtmp");
  for(int i = 0; i < valueVec.size(); i++) {
    Result->addIncoming(valueVec[i], bbVec[i]);
  }
  return Result;
}

Value *OrASTNode::codegen() {  
  // Return the IR for the left side of the times if there is no right side
  Value *LHS = Left->codegen();
  if(!Right) {
    return LHS;
  }
  std::unique_ptr<OrASTNode> rightTmp = std::move(Right);
  Function* function = Builder.GetInsertBlock()->getParent();
  // Block for after the and has been dealt with
  BasicBlock *end_ = BasicBlock::Create(Builder.getContext(), "end_", function);
  // Create value and basic block stores for phi node
  std::vector<Value*> valueVec = {LHS};
  std::vector<BasicBlock*> bbVec = {Builder.GetInsertBlock()};
  while (rightTmp.get()) {
    // Create blocks for evaluating RHS
    BasicBlock *rhs_ = BasicBlock::Create(Builder.getContext(), "rhs_", function);
    // If LHS is true it branches to the end block and doesn't execute the unnecessary conditional
    Builder.CreateCondBr(LHS, end_, rhs_);
    Builder.SetInsertPoint(rhs_);
    // Generate IR for the next conditional
    LHS = rightTmp->Left->codegen();
    // Add the value and basic block to the stores for the phi node
    valueVec.push_back(LHS);
    bbVec.push_back(rhs_);
    rightTmp = std::move(rightTmp->Right);
  }
  Builder.CreateBr(end_);
  Builder.SetInsertPoint(end_);
  // Add the values and blocks to a phi node
  PHINode *Result = Builder.CreatePHI(Type::getInt1Ty(TheContext), valueVec.size(), "ortmp");
  for(int i = 0; i < valueVec.size(); i++) {
    Result->addIncoming(valueVec[i], bbVec[i]);
  }  
  return Result;
}

Value *ProgramASTNode::codegen() {
  // Generate the IR for the externs and decls
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
  // llvm::outs() << prog->to_string(1) << "\n";
  fprintf(stderr, "Parsing Finished\n");
  prog->codegen();

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
