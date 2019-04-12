#include <sstream>
#include <iostream>

#include "sbl_ast.hpp"
#include "peglib.h"

namespace sbl {

using namespace std;

const char* sbl_syntax =
	R"(
		# SBL grammar

		SOURCE      <- DECL_LIST STAT_LIST
		DECL_LIST   <- 'variables' VAR_DECL*
		STAT_LIST   <- 'program' STATEMENT*
		STATEMENT   <- (STAT_ASSIGN / STAT_IF / STAT_PRINT) ';'
		STAT_ASSIGN <- LABEL ID '=' EXPR 'goto' LABEL
		STAT_IF     <- LABEL 'if' COND 'then' LABEL 'else' LABEL
		STAT_PRINT  <- LABEL 'print' EXPR 'goto' LABEL
		COND        <- EXPR COND_OP EXPR
		COND_OP     <- '<=' / '>=' / '<' / '>' / '=' 

		EXPR         <- EX_INT / EX_BINARY / EX_UNARY / EX_ARR_ACCESS  / EX_ARR_MAKE / EX_ARR_LEN / EX_VAR
		EX_INT       <- < [0-9]+ >
		EX_VAR       <- ID
		EX_BINARY    <- '(' EXPR BINARY_OP EXPR ')'
		BINARY_OP    <- '+' / '*' / '-' / '/' / '%'
		EX_UNARY     <- UNARY_OP EXPR
		UNARY_OP     <- '-'
		EX_ARR_ACCESS<- '[' EXPR '[' EXPR ']' ']'
		EX_ARR_MAKE  <- '[' EXPR (',' EXPR )* ']'
		EX_ARR_LEN   <- '|' EXPR '|'
		VAR_DECL     <- ID ':' TYPE ';'

		TYPE      <- TP_INT / TP_ARRAY
		TP_INT    <- 'int'
		TP_ARRAY  <- '[' TYPE ']'

		ID          <- < [a-zA-Z] [a-zA-Z0-9_]* >
		LABEL       <- < [0-9]+ >

		%whitespace <- [ \t\r\n]*  
	)";

template<class T>
std::function<T (const peg::SemanticValues&)> wrap_scalar_error(std::function<T (const peg::SemanticValues&)> f) {
	return [f](const peg::SemanticValues& sv) {
		try {
			return f(sv);
		} catch (CompileError& err) {
			err.line = sv.line_info().first;
			err.col = sv.line_info().second;
			throw err;
		}
	};
}

template<class T>
std::function<T* (const peg::SemanticValues&)> wrap_error(std::function<T* (const peg::SemanticValues&)> f) {
	return [f](const peg::SemanticValues& sv) {
		try {
			return f(sv);
		} catch (CompileError& err) {
			err.line = sv.line_info().first;
			err.col = sv.line_info().second;
			throw err;
		}
	};
}

peg::parser parser(const string& file) {
	peg::parser parser(sbl_syntax);
	if (!parser) {
		throw CompileError("Error in SBL grammar");
	}
	parser["label"] = wrap_scalar_error<int>([](const peg::SemanticValues& sv) {
		return stoi(sv.token());
	});
	parser["ID"] = [](const peg::SemanticValues& sv) {
		return sv.token();
	};
	parser["TP_INT"] = [](const peg::SemanticValues& sv) {
		return static_cast<Type*>(new IntType());
	};
	parser["TP_ARRAY"] = wrap_error<Type>([](const peg::SemanticValues& sv) {
		return static_cast<Type*>(new ArrayType(sv[0].get<Type*>()));
	});
	parser["TYPE"] = [](const peg::SemanticValues& sv) {
		return sv[0].get<Type*>();
	};
	parser["EX_INT"] = wrap_error<Expr>([](const peg::SemanticValues& sv) {
		return static_cast<Expr*>(new IntConst(stoi(sv.token())));
	});
	parser["EX_BINARY"] = wrap_error<Expr>([](const peg::SemanticValues& sv) {
		return static_cast<Expr*>(new BinOp(sv[0].get<Expr*>(), sv[2].get<Expr*>(), sv[1].get<BinOp::Kind>()));
	});
	parser["BINARY_OP"] = [](const peg::SemanticValues& sv) {
		switch (sv.choice()) {
		case 0:  return BinOp::PLUS;
		case 1:  return BinOp::MULT;
		case 2:  return BinOp::SUB;
		case 3:  return BinOp::DIV;
		case 4:  return BinOp::RES;
		default: throw CompileError("impossible choice in BINARY_OP");
		}
	};
	parser["EX_UNARY"] = wrap_error<Expr>([](const peg::SemanticValues& sv) {
		return static_cast<Expr*>(new UnOp(sv[1].get<Expr*>(), sv[0].get<UnOp::Kind>()));
	});
	parser["UNARY_OP"] = [](const peg::SemanticValues& sv) {
		return UnOp::MINUS;
	};
	parser["EX_ARR_ACCESS"] = wrap_error<Expr>([](const peg::SemanticValues& sv) {
		return static_cast<Expr*>(new ArrayAccess(sv[0].get<Expr*>(), sv[1].get<Expr*>()));
	});
	parser["EX_ARR_MAKE"] = wrap_error<Expr>([](const peg::SemanticValues& sv) {
		return static_cast<Expr*>(new ArrayMake(sv.transform<Expr*>()));
	});
	parser["EX_ARR_LEN"] = wrap_error<Expr>([](const peg::SemanticValues& sv) {
		return static_cast<Expr*>(new ArrayLen(sv[0].get<Expr*>()));
	});
	parser["VAR_DECL"] = wrap_error<VarDecl>([](const peg::SemanticValues& sv) {
		return new VarDecl(sv[0].get<string>(), sv[1].get<Type*>());
	});
	parser["EXPR"] = [](const peg::SemanticValues& sv) {
		return sv[0].get<Expr*>();
	};
	parser["COND"] = wrap_error<Cond>([](const peg::SemanticValues& sv) {
		return new Cond(sv[0].get<Expr*>(), sv[2].get<Expr*>(), sv[1].get<Cond::Kind>()
		);
	});
	parser["COND_OP"] = [](const peg::SemanticValues& sv) {
		switch (sv.choice()) {
		case 0:  return Cond::LESSEQ;
		case 1:  return Cond::GREATEQ;
		case 2:  return Cond::LESS;
		case 3:  return Cond::GREAT;
		case 4:  return Cond::EQ;
		default: throw CompileError("impossible choice in COND_OP");
		}
	};
	parser["STAT_IF"] = wrap_error<Statement>([](const peg::SemanticValues& sv) {
		return static_cast<Statement*>(new If(
			sv[0].get<int>(),
			sv[1].get<Cond*>(),
			sv[2].get<int>(),
			sv[3].get<int>()
		));
	});
	parser["STAT_ASSIGN"] = wrap_error<Statement>([](const peg::SemanticValues& sv) {
		return static_cast<Statement*>(new Assign(
			sv[0].get<int>(),
			sv[1].get<string>(),
			sv[2].get<Expr*>(),
			sv[3].get<int>()
		));
	});
	parser["STAT_PRINT"] = wrap_error<Statement>([](const peg::SemanticValues& sv) {
		return static_cast<Statement*>(new Print(
			sv[0].get<int>(),
			sv[2].get<Expr*>(),
			sv[3].get<int>()
		));
	});
	parser["STATEMENT"] = [](const peg::SemanticValues& sv) {
		return sv[0].get<Statement*>();
	};
	parser["DECL_LIST"] = [](const peg::SemanticValues& sv) {
		return sv.transform<VarDecl*>();
	};
	parser["STAT_LIST"] = [](const peg::SemanticValues& sv) {
		return sv.transform<Statement*>();
	};
	parser["SOURCE"] = wrap_error<Prog>([](const peg::SemanticValues& sv) {
		return new Prog(sv[0].get<vector<VarDecl*>>(), sv[2].get<vector<Statement*>>());
	});
	parser.log = [](size_t line, size_t col, const std::string& msg) {
		throw CompileError(msg, pair<int, int>(line, col));
	};
	return parser;
}

Prog* parse(const string& file, const string& src) {
	Prog* ret = nullptr;
	try {
		if (!parser(file).parse<Prog*>(src.c_str(), ret)) {
			throw CompileError("parsing of " + file + " failed");
		}
	} catch (CompileError& err) {
		err.file = file;
		throw err;
	}
	return ret;
}

}
