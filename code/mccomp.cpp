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

class ParamASTNode : public ASTnode {
  TOKEN VarType;
  TOKEN Ident;

public:
  ParamASTNode(TOKEN vartype, TOKEN ident) : VarType(vartype), Ident(ident) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class ParamsASTNode : public ASTnode {
public:
  virtual ~ParamsASTNode() {}
};

class ListParamsASTNode : public ParamsASTNode {
  std::vector<std::unique_ptr<ParamASTNode>> ParamList;
  
public:
  ListParamsASTNode(std::vector<std::unique_ptr<ParamASTNode>> paramlist) : ParamList(std::move(paramlist)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class VoidParamsASTNode : public ParamsASTNode {
public:
  VoidParamsASTNode() {}
};

// ExternASTNode - Class for extern declarations
class ExternASTNode : public ASTnode {
  TOKEN VarType;
  TOKEN Ident;
  std::unique_ptr<ParamsASTNode> Params;

public:
  ExternASTNode(TOKEN vartype, TOKEN ident, std::unique_ptr<ParamsASTNode> params) : VarType(vartype), Ident(ident), Params(std::move(params)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class LocalDeclASTNode : public ASTnode {
  TOKEN VarType;
  TOKEN Ident;

public:
  LocalDeclASTNode(TOKEN vartype, TOKEN ident) : VarType(vartype), Ident(ident) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class StmtASTNode : public ASTnode {
public:
  virtual ~StmtASTNode() {}
};

class BlockASTNode : public StmtASTNode {
  std::vector<std::unique_ptr<LocalDeclASTNode>> LocalDecls;
  std::vector<std::unique_ptr<StmtASTNode>> Statements;  

public:
  BlockASTNode(std::vector<std::unique_ptr<LocalDeclASTNode>> localdecls, std::vector<std::unique_ptr<StmtASTNode>> statements) : LocalDecls(std::move(localdecls)), Statements(std::move(statements)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class DeclASTNode : public ASTnode {
public:
  virtual ~DeclASTNode();
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

// FunDeclASTNode - Class for function declarations
class FunDeclASTNode : public DeclASTNode {
  TOKEN VarType;
  TOKEN Ident;
  std::unique_ptr<ParamsASTNode> Params;
  std::unique_ptr<BlockASTNode> Block;

public:
  FunDeclASTNode(TOKEN vartype, TOKEN ident, std::unique_ptr<ParamsASTNode> params, std::unique_ptr<BlockASTNode> block) : VarType(vartype), Ident(ident), Params(std::move(params)), Block(std::move(block)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class VarDeclASTNode : public DeclASTNode {
  TOKEN VarType;
  TOKEN Ident;

public:
  VarDeclASTNode(TOKEN vartype, TOKEN ident) : VarType(vartype), Ident(ident) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class ElseStmtASTNode : public StmtASTNode {
  std::unique_ptr<BlockASTNode> Block;

public:
  ElseStmtASTNode(std::unique_ptr<BlockASTNode> block) : Block(std::move(block)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class RValASTNode : public ASTnode {
public:
  RValASTNode() {}
};

//Expression Superclass
class ExprASTNode : public RValASTNode {
public:
  virtual ~ExprASTNode() {}
};

class IfStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;
  std::unique_ptr<BlockASTNode> Block;
  std::unique_ptr<ElseStmtASTNode> ElseStmt;

public:
  IfStmtASTNode(std::unique_ptr<ExprASTNode> expr, std::unique_ptr<BlockASTNode> block, std::unique_ptr<ElseStmtASTNode> elsestmt) : Expr(std::move(expr)), Block(std::move(block)), ElseStmt(std::move(elsestmt)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class WhileStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;
  std::unique_ptr<StmtASTNode> Statement;

public:
  WhileStmtASTNode(std::unique_ptr<ExprASTNode> expr, std::unique_ptr<StmtASTNode> statement) : Expr(std::move(expr)), Statement(std::move(statement)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class ReturnStmtASTNode : public StmtASTNode {
  std::unique_ptr<ExprASTNode> Expr;

public:
  ReturnStmtASTNode(std::unique_ptr<ExprASTNode> expr) : Expr(std::move(expr)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

/// UnaryOpRValASTNode
class UnaryOpRValASTNode : public RValASTNode {
  TOKEN Op;
  std::unique_ptr<RValASTNode> RVal;
public:
  UnaryOpRValASTNode(TOKEN op, std::unique_ptr<RValASTNode> rval) : Op(op), RVal(std::move(rval)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

//expr ::= IDENT "=" expr in production
class AssignmentExprASTNode : public ExprASTNode {
  std::vector<TOKEN> Idents;
  std::vector<std::unique_ptr<ExprASTNode>> SubExprs;

public:
  AssignmentExprASTNode(std::vector<TOKEN> idents, std::vector<std::unique_ptr<ExprASTNode>> subexprs) : Idents(idents), SubExprs(std::move(subexprs)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class ArgListASTNode : public ASTnode {
  std::vector<std::unique_ptr<ExprASTNode>> Exprs;
public:
  ArgListASTNode(std::vector<std::unique_ptr<ExprASTNode>> exprs) : Exprs(std::move(exprs)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

//IDENT in rval ::= production
class IdentASTNode : public RValASTNode {
  TOKEN Ident;

public:
  IdentASTNode(TOKEN ident) : Ident(ident) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

//Function call in rval ::= production
class FunctionCallASTNode : public RValASTNode {
  TOKEN FuncName;
  std::vector<std::unique_ptr<ArgListASTNode>> Args;
public:
  FunctionCallASTNode(TOKEN funcname, std::vector<std::unique_ptr<ArgListASTNode>> args) : FuncName(funcname), Args(std::move(args)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

/// IntASTNode - Class for integer literals like 1, 2, 10,
class IntASTNode : public RValASTNode {
  int Val;
  TOKEN Tok;
  std::string Name;
public:
  IntASTNode(TOKEN tok, int val) : Val(val), Tok(tok) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

/// BoolASTNode - Class for boolean literals true and false
class BoolASTNode : public RValASTNode {
  bool Val;
  TOKEN Tok;
  std::string Name;

public:
  BoolASTNode(TOKEN tok, bool val) : Val(val), Tok(tok) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

/// FloatASTNode - Class for float literals like 1.4, 3.5, 60
class FloatASTNode : public RValASTNode {
  float Val;
  TOKEN Tok;
  std::string Name;

public:
  FloatASTNode(TOKEN tok, float val) : Val(val), Tok(tok) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class TimesASTNode : public ASTnode {
  std::vector<std::unique_ptr<RValASTNode>> RVals;
  std::vector<TOKEN> Ops;
public:
  TimesASTNode(std::vector<std::unique_ptr<RValASTNode>> rvals, std::vector<TOKEN> ops) : RVals(std::move(rvals)), Ops(ops) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class AddASTNode : public ASTnode {
  std::vector<std::unique_ptr<TimesASTNode>> Times;
  std::vector<TOKEN> Ops;
public:
  AddASTNode(std::vector<std::unique_ptr<TimesASTNode>> times, std::vector<TOKEN> ops) : Times(std::move(times)), Ops(ops) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class CompASTNode : public ASTnode {
  std::vector<std::unique_ptr<AddASTNode>> Adds;
  std::vector<TOKEN> Ops;
public:
  CompASTNode(std::vector<std::unique_ptr<AddASTNode>> adds, std::vector<TOKEN> ops) : Adds(std::move(adds)), Ops(ops) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class EquivASTNode : public ASTnode {
  std::vector<std::unique_ptr<CompASTNode>> Comps;
  std::vector<TOKEN> Ops;
public:
  EquivASTNode(std::vector<std::unique_ptr<CompASTNode>> comps, std::vector<TOKEN> ops) : Comps(std::move(comps)), Ops(ops) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class AndASTNode : public ASTnode {
  std::vector<std::unique_ptr<EquivASTNode>> Equivs;
public:
  AndASTNode(std::vector<std::unique_ptr<EquivASTNode>> equivs) : Equivs(std::move(equivs)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

class OrASTNode : public ASTnode {
  std::vector<std::unique_ptr<AndASTNode>> Ands;
public:
  OrASTNode(std::vector<std::unique_ptr<AndASTNode>> ands) : Ands(std::move(ands)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

//expr ::= operator_or in production
class OrExprASTNode : public ExprASTNode {
  std::unique_ptr<OrASTNode> OrExpression;

public:
  OrExprASTNode(std::unique_ptr<OrASTNode> orexpression) : OrExpression(std::move(orexpression)) {}
  // virtual Value *codegen() override;
  // virtual std::string to_string() const override {
  // return a sting representation of this AST node
  //};
};

/// ProgramASTNode - Class for program
class ProgramASTNode : public ASTnode {
  std::vector<std::unique_ptr<ExternASTNode>> ExternList;
  std::vector<std::unique_ptr<DeclASTNode>> DeclList;

public:
  ProgramASTNode(std::vector<std::unique_ptr<ExternASTNode>> externlist, std::vector<std::unique_ptr<DeclASTNode>> decllist) : ExternList(std::move(externlist)), DeclList(std::move(decllist)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    //return a sting representation of this AST node
  };
};

/* add other AST nodes as nessasary */

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

/* Add function calls for each production */

static std::unique_ptr<ParamsASTNode> ParseParams() {
  //Params consist of vector of paramast
  if (CurTok.type!=LPAR) {
    //Error
  }
  getNextToken(); //Consume "("
  //Parse void case first
  if (CurTok.type == VOID_TOK) {
    return std::make_unique<VoidParamsASTNode>();
  }
  std::vector<std::unique_ptr<ParamASTNode>> paramList;
  //Parse param_list
  while (CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    TOKEN varType = CurTok;
    getNextToken(); //Consume type
    if (CurTok.type != IDENT) {
      //Error
    }
    TOKEN ident = CurTok;
    paramList.push_back(std::make_unique<ParamASTNode>(varType, ident)); //Add param to paramList vector
    getNextToken(); //Consume ident
    //Next part would either be another param denoted by a comma, or from follow set it would be a bracket.
    if (CurTok.type != COMMA && CurTok.type != RPAR) {
      //Error
    }
    getNextToken(); //Consume RPAR or COMMA
  }
  //Parse epsilon
  if (paramList.empty()) {
    return nullptr;
  }
  return std::make_unique<ListParamsASTNode>(paramList);  
};

static std::unique_ptr<LocalDeclASTNode> ParseLocalDecl() {
  TOKEN varType = CurTok;
  TOKEN ident = getNextToken(); //Consume var_type
  if(ident.type!=IDENT) {
    //ERROR
  }
  getNextToken();
  if(CurTok.type!=SC) {
    //ERROR
  }
  return std::make_unique<LocalDeclASTNode>(varType, ident);
}

static std::vector<std::unique_ptr<LocalDeclASTNode>> ParseLocalDecls() {
  //local decl or epsilon
  //for epsilon case check follow set so we dont error
  //follow local_decls is "!" "(" "-" ";" "if" "return" "while" "{" "}" BOOL_LIT FLOAT_LIT IDENT INT_LIT
  std::vector<TOKEN_TYPE> followSet = {NOT, LPAR, MINUS, IF, RETURN, WHILE, LBRA, RBRA, BOOL_LIT, FLOAT_LIT, IDENT, INT_LIT};
  auto search = std::find(followSet.begin(), followSet.end(), CurTok.type);
  if(search==followSet.end()) {
    //Error
  }
  // std::vector<TOKEN_TYPE> varTypes = {BOOL_LIT, FLOAT_LIT, INT_LIT};
  // search = std::find(varTypes.begin(), varTypes.end(), CurTok.type);
  // if(search==followSet.end()) {
  //   return nullptr;
  // }
  std::vector<std::unique_ptr<LocalDeclASTNode>> localDecls;
  while (CurTok.type == BOOL_LIT || CurTok.type == FLOAT_LIT || CurTok.type == INT_LIT) {
    localDecls.push_back(std::move(ParseLocalDecl()));
  }
  return std::move(localDecls);
}

static std::vector<std::unique_ptr<StmtASTNode>> ParseStmtList() {
  
}

static std::unique_ptr<BlockASTNode> ParseBlock() {
  //"{" local_decls stmt_list "}" 
  if (CurTok.type!=LBRA) {
    //ERROR
  }
  getNextToken(); //Consume "{"
  //local_decls
  std::vector<std::unique_ptr<LocalDeclASTNode>> localDecls = std::move(ParseLocalDecls());
  //stmt_list
  std::vector<std::unique_ptr<StmtASTNode>> statements = std::move(ParseStmtList());
  if (CurTok.type!=RBRA) {
    //ERROR
  }
  getNextToken(); //Consume "}"
  return std::make_unique<BlockASTNode>(localDecls, statements);
}

static std::vector<std::unique_ptr<ExternASTNode>> ParseExternList() {
  std::vector<std::unique_ptr<ExternASTNode>> externList;
  TOKEN type;
  TOKEN ident;
  //"extern" type_spec IDENT "(" params ")" ";"
  while(CurTok.type == EXTERN) {
    getNextToken(); //Consume "extern"
    if(CurTok.type==BOOL_TOK || CurTok.type==INT_TOK || CurTok.type==VOID_TOK || CurTok.type==FLOAT_TOK) {
      type = CurTok;
    } else {
      //Error
    }
    getNextToken(); //Consumes type_spec
    if (CurTok.type==IDENT) {
      ident = CurTok;
    } else {
      //Error
    }
    getNextToken(); //Consumes IDENT
    std::unique_ptr<ParamsASTNode> params = std::move(ParseParams()); //Parses params
    if (CurTok.type!=SC) {
      //Error
    }
    getNextToken(); //Consumes ";"
    std::unique_ptr<ExternASTNode> ext = std::make_unique<ExternASTNode>(type, ident, std::move(params));
    externList.push_back(ext);
  }
  return std::move(externList);
};

static std::unique_ptr<VarDeclASTNode> ParseVarDecl() {
  if (CurTok.type!=INT_TOK && CurTok.type!=FLOAT_TOK && CurTok.type!=BOOL_TOK) {
    //ERROR
  }
  TOKEN varType = CurTok;
  TOKEN ident = getNextToken(); //Consume var_type
  if (CurTok.type!=IDENT) {
    //ERROR
  }
  getNextToken(); //Consume IDENT
  if(CurTok.type!=SC) {
    //ERROR
  }
  getNextToken(); //Consume ";"
  return std::make_unique<VarDeclASTNode>(varType, ident);
}

static std::unique_ptr<FunDeclASTNode> ParseFunDecl() {
  TOKEN varType;
  TOKEN ident;
  if (CurTok.type!=INT_TOK && CurTok.type!=FLOAT_TOK && CurTok.type!=BOOL_TOK && CurTok.type!=VOID_TOK) {
    //ERROR
  } else {
    varType = CurTok;
  }
  getNextToken(); //Consumes type_spec
  if (CurTok.type==IDENT) {
    ident = CurTok;
  } else {
    //Error
  }
  getNextToken(); //Consumes IDENT
  std::unique_ptr<ParamsASTNode> params = std::move(ParseParams()); //Parses params
  std::unique_ptr<BlockASTNode> block = std::move(ParseBlock()); //Parses block
  return std::make_unique<FunDeclASTNode>(varType, ident, std::move(params), std::move(block));
}

static std::vector<std::unique_ptr<DeclASTNode>> ParseDeclList() {
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
  if(declList.empty()) {
    //Error
  }
  return std::move(declList);
};

// program ::= extern_list decl_list
static void parser() {
  // add body
  switch (CurTok.type) {
    case EXTERN:
    {
      std::vector<std::unique_ptr<ExternASTNode>> externList = std::move(ParseExternList());
      std::vector<std::unique_ptr<DeclASTNode>> declList = std::move(ParseDeclList());
      if(getNextToken().type != EOF_TOK) {
        //Error
      }
      std::make_unique<ProgramASTNode>(std::move(externList), std::move(declList));
      break;
    }
    case BOOL_TOK:
    {
      std::vector<std::unique_ptr<DeclASTNode>> declList = std::move(ParseDeclList());
      if(getNextToken().type != EOF_TOK) {
        //Error
      }
      std::make_unique<ProgramASTNode>(nullptr, std::move(declList));
      break;
    }
    case INT_TOK:
    {
      std::vector<std::unique_ptr<DeclASTNode>> declList = std::move(ParseDeclList());
      if(getNextToken().type != EOF_TOK) {
        //Error
      }
      std::make_unique<ProgramASTNode>(nullptr, std::move(declList));
      break;
    }
    case VOID_TOK:
    {
      std::vector<std::unique_ptr<DeclASTNode>> declList = std::move(ParseDeclList());
      if(getNextToken().type != EOF_TOK) {
        //Error
      }
      std::make_unique<ProgramASTNode>(nullptr, std::move(declList));
      break;
    }
    case FLOAT_TOK:
    {
      std::vector<std::unique_ptr<DeclASTNode>> declList = std::move(ParseDeclList());
      if(getNextToken().type != EOF_TOK) {
        //Error
      }
      std::make_unique<ProgramASTNode>(nullptr, std::move(declList));
      break;
    }
    default:
      //Error
      break;
    }
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
  while (CurTok.type != EOF_TOK) {
    fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
            CurTok.type);
    getNextToken();
  }
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
