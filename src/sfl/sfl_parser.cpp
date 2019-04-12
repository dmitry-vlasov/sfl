#include <sstream>
#include <iostream>

#include "sfl_ast.hpp"
#include "peglib.h"

namespace sfl {

using namespace std;

const char* sfl_syntax =
	R"(
		# SFL grammar

		SOURCE      <- STAT_SEQ
		STATEMENT   <- STAT_ASSIGN / STAT_IF / '{' STAT_SEQ '}' / STAT_WHILE / STAT_PRINT /STAT_EXPR
		STAT_SEQ    <- COMMENT* STATEMENT COMMENT* (';' COMMENT* STATEMENT COMMENT*)* ';' ? COMMENT* 
		STAT_ASSIGN <- ID '=' EXPR / VAR_DECL '=' EXPR
		STAT_IF     <- 'if' COND 'then' STATEMENT 'else' STATEMENT
		STAT_WHILE  <- 'while' COND 'do' STATEMENT
		STAT_PRINT  <- 'print' EXPR
		STAT_EXPR   <- EXPR
		COND        <- EXPR COND_OP EXPR
		COND_OP     <- '<=' / '>=' / '<' / '>' / '=' 

		EXPR         <- EX_INT / EX_BINARY / EX_UNARY / EX_ARR_ACCESS  / EX_ARR_MAKE / EX_ARR_LEN / EX_FUN_CALL / EX_LAMBDA / EX_VAR
		EX_INT       <- < [0-9]+ >
		EX_VAR       <- ID
		EX_BINARY    <- '(' EXPR BINARY_OP EXPR ')'
		BINARY_OP    <- '+' / '*' / '-' / '/' / '%'
		EX_UNARY     <- UNARY_OP EXPR
		UNARY_OP     <- '-'
		EX_ARR_ACCESS<- '[' EXPR '[' EXPR ']' ']'
		EX_ARR_MAKE  <- '[' EXPR (',' EXPR )* ']'
		EX_ARR_LEN   <- '|' EXPR '|'
		EX_FUN_CALL  <- '(' EXPR '(' CALL_ARGS ? ')' ')'
		CALL_ARGS    <- EXPR (',' EXPR)*
		EX_LAMBDA    <- '/\\' LAMBDA_ARGS ? '->' STATEMENT
		LAMBDA_ARGS  <- VAR_DECL (',' VAR_DECL)*
		VAR_DECL     <- ID ':' TYPE

		TYPE      <- TP_INT / TP_ARRAY / TP_FUNC
		TP_INT    <- 'int'
		TP_ARRAY  <- '[' TYPE ']'
		TP_FUNC   <- '(' TP_ARGS ? ')' '->' TYPE
		TP_ARGS   <- TYPE (',' TYPE)*

		ID          <- < [a-zA-Z] [a-zA-Z0-9_]* >

		~COMMENT    <- COMMENT_ML / COMMENT_SL
		~COMMENT_ML <- '/*' < (!'*/' .)* > '*/'
		~COMMENT_SL <- '//' < (![\n$] .)+ >

		%whitespace <- [ \t\r\n]*  
	)";

struct Context {
	struct Decl {
		void add(Type* t) {
			if (type_) initial_ = false;
			type_.reset(t);
		}
		Type* type() { return type_.get(); }
		bool initial() const { return initial_; }
	private:
		unique_ptr<Type> type_;
		bool initial_ = true;
	};
	~Context() {
		for (auto e : unusedExprs) delete e;
		for (auto s : unusedStats) delete s;
	}
	bool empty() const {
		return types.empty();
	}
	void push() {
		types.push_back(map<string, Decl>());
	}
	void pop() {
		types.pop_back();
	}
	void addDecl(const string& name, Type* type) {
		if (Type* tp = findType(name)) {
			if (!type->equal(tp)) {
				throw CompileError("variable " + name + " is declared with different type: " + type->dump() + ", original type was: " + tp->dump());
			} else {
				types.back()[name].add(type);
			}
		} else {
			types.back()[name].add(type);
		}
	}
	void newDecl(const string& name, Type* type) {
		if (Type* tp = findType(name)) {
			throw CompileError("variable " + name + " is already declared");
		} else {
			types.back()[name].add(type);
		}
	}
	bool initialDecl(const string& name) {
		if (Decl* decl = findDecl(name)) {
			return decl->initial();
		} else {
			throw CompileError("variable " + name + " is undefined");
		}
	}
	Type* getType(const string& name) {
		if (Type* type = findType(name)) {
			return type;
		} else {
			throw CompileError("variable " + name + " is not typed");
		}
	}
	Type* findType(const string& name) {
		if (Decl* decl = findDecl(name)) {
			return decl->type();
		} else {
			return nullptr;
		}
	}
	Decl* findDecl(const string& name) {
		for (auto& m : types) {
			auto x = m.find(name);
			if (x != m.end()) {
				return &x->second;
			}
		}
		return nullptr;
	}
	template<class T> void reg(T* t);
	template<class T> T* use(const peg::any& t);
	template<class T> vector<T*> useVect(const peg::any& t);

private:
	vector<map<string, Decl>> types;
	set<Expr*> unusedExprs;
	set<Statement*> unusedStats;
};

template<> void Context::reg<Expr>(Expr* e) { unusedExprs.insert(e); }
template<> void Context::reg<Statement>(Statement* s) { unusedStats.insert(s); }
template<> Expr* Context::use<Expr>(const peg::any& a) {
	Expr* e = a.get<Expr*>();
	unusedExprs.erase(e);
	return e;
}
template<> Statement* Context::use<Statement>(const peg::any& a) {
	Statement* s = a.get<Statement*>();
	unusedStats.erase(s);
	return s;
}
template<> vector<Expr*> Context::useVect<Expr>(const peg::any& a) {
	vector<Expr*> ev = a.get<vector<Expr*>>();
	for (auto e : ev) unusedExprs.erase(e);
	return ev;
}
template<> vector<Statement*> Context::useVect<Statement>(const peg::any& a) {
	vector<Statement*> sv = a.get<vector<Statement*>>();
	for (auto s : sv) unusedStats.erase(s);
	return sv;
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

template<class T>
std::function<T* (const peg::SemanticValues&, peg::any& ctx)> wrap_error(std::function<T* (const peg::SemanticValues&, peg::any& ctx)> f) {
	return [f](const peg::SemanticValues& sv, peg::any& ctx) {
		try {
			return f(sv, ctx);
		} catch (CompileError& err) {
			err.line = sv.line_info().first;
			err.col = sv.line_info().second;
			throw err;
		}
	};
}

peg::parser parser(const string& file) {
	peg::parser parser(sfl_syntax);
	if (!parser) {
		throw CompileError("Error in SPL grammar");
	}
	parser["ID"] = [](const peg::SemanticValues& sv) {
		return sv.token();
	};
	parser["TP_INT"] = [](const peg::SemanticValues& sv) {
		return static_cast<Type*>(new IntType());
	};
	parser["TP_ARRAY"] = wrap_error<Type>([](const peg::SemanticValues& sv) {
		return static_cast<Type*>(new ArrayType(sv[0].get<Type*>()));
	});
	parser["TP_ARGS"] = [](const peg::SemanticValues& sv) {
		return sv.transform<Type*>();
	};
	parser["TP_FUNC"] = wrap_error<Type>([](const peg::SemanticValues& sv) {
		if (sv.size() == 1) {
			return static_cast<Type*>(new FuncType(sv[0].get<Type*>()));
		} else {
			return static_cast<Type*>(new FuncType(sv[1].get<Type*>(), sv[0].get<vector<Type*>>()));
		}
	});
	parser["TYPE"] = [](const peg::SemanticValues& sv) {
		return sv[0].get<Type*>();
	};
	parser["EX_INT"] = wrap_error<Expr>([](const peg::SemanticValues& sv) {
		return static_cast<Expr*>(new IntConst(stoi(sv.token())));
	});
	parser["EX_BINARY"] = wrap_error<Expr>([](const peg::SemanticValues& sv, peg::any& ctx) {
		return static_cast<Expr*>(new BinOp(
			ctx.get<Context*>()->use<Expr>(sv[0]),
			ctx.get<Context*>()->use<Expr>(sv[2]),
			sv[1].get<BinOp::Kind>()
		));
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
	parser["EX_UNARY"] = wrap_error<Expr>([](const peg::SemanticValues& sv, peg::any& ctx) {
		return static_cast<Expr*>(new UnOp(
			ctx.get<Context*>()->use<Expr>(sv[1]),
			sv[0].get<UnOp::Kind>()
		));
	});
	parser["UNARY_OP"] = [](const peg::SemanticValues& sv) {
		return UnOp::MINUS;
	};
	parser["EX_ARR_ACCESS"] = wrap_error<Expr>([](const peg::SemanticValues& sv, peg::any& ctx) {
		return static_cast<Expr*>(new ArrayAccess(
			ctx.get<Context*>()->use<Expr>(sv[0]),
			ctx.get<Context*>()->use<Expr>(sv[1])
		));
	});
	parser["EX_ARR_MAKE"] = wrap_error<Expr>([](const peg::SemanticValues& sv, peg::any& ctx) {
		return static_cast<Expr*>(new ArrayMake(
			ctx.get<Context*>()->useVect<Expr>(sv.transform<Expr*>())
		));
	});
	parser["EX_ARR_LEN"] = wrap_error<Expr>([](const peg::SemanticValues& sv, peg::any& ctx) {
		return static_cast<Expr*>(new ArrayLen(
			ctx.get<Context*>()->use<Expr>(sv[0])
		));
	});
	parser["EX_FUN_CALL"] = wrap_error<Expr>([](const peg::SemanticValues& sv, peg::any& ctx) {
		if (sv.size() == 1) {
			return static_cast<Expr*>(new FunCall(
				ctx.get<Context*>()->use<Expr>(sv[0])
			));
		} else {
			return static_cast<Expr*>(new FunCall(
				ctx.get<Context*>()->use<Expr>(sv[0]),
				ctx.get<Context*>()->useVect<Expr>(sv[1])
			));
		}
	});
	parser["CALL_ARGS"] = [](const peg::SemanticValues& sv) {
		return sv.transform<Expr*>();
	};
	parser["LAMBDA_ARGS"].enter = [](const char* s, size_t n, peg::any& ctx) {
		ctx.get<Context*>()->push();
	};
	parser["EX_LAMBDA"] = wrap_error<Expr>([](const peg::SemanticValues& sv, peg::any& ctx) {
		ctx.get<Context*>()->pop();
		if (sv.size() == 1) {
			return static_cast<Expr*>(new Lambda(
				ctx.get<Context*>()->use<Statement>(sv[0])
			));
		} else {
			return static_cast<Expr*>(new Lambda(
				ctx.get<Context*>()->use<Statement>(sv[1]),
				sv[0].get<vector<VarDecl*>>()
			));
		}
	});
	parser["VAR_DECL"] = wrap_error<VarDecl>([](const peg::SemanticValues& sv, peg::any& ctx) {
		string name = sv[0].get<string>();
		Type* type = sv[1].get<Type*>();
		ctx.get<Context*>()->addDecl(name, type);
		return new VarDecl(name, type);
	});
	parser["LAMBDA_ARGS"] = [](const peg::SemanticValues& sv) {
		return sv.transform<VarDecl*>();
	};
	parser["EX_VAR"] = wrap_error<Expr>([](const peg::SemanticValues& sv, peg::any& ctx) {
		string name = sv[0].get<string>();
		Type* type = ctx.get<Context*>()->getType(name);
		return static_cast<Expr*>(new VarAccess(name, type));
	});
	parser["EXPR"] = [](const peg::SemanticValues& sv, peg::any& ctx) {
		Expr* e = sv[0].get<Expr*>();
		ctx.get<Context*>()->reg<Expr>(e);
		return e;
	};
	parser["COND"] = wrap_error<Cond>([](const peg::SemanticValues& sv, peg::any& ctx) {
		return new Cond(
			ctx.get<Context*>()->use<Expr>(sv[0]),
			ctx.get<Context*>()->use<Expr>(sv[2]),
			sv[1].get<Cond::Kind>()
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
	parser["STAT_WHILE"] = wrap_error<Statement>([](const peg::SemanticValues& sv, peg::any& ctx) {
		return static_cast<Statement*>(new While(
			sv[0].get<Cond*>(),
			ctx.get<Context*>()->use<Statement>(sv[1])
		));
	});
	parser["STAT_IF"] = wrap_error<Statement>([](const peg::SemanticValues& sv, peg::any& ctx) {
		return static_cast<Statement*>(new If(
			sv[0].get<Cond*>(),
			ctx.get<Context*>()->use<Statement>(sv[1]),
			ctx.get<Context*>()->use<Statement>(sv[2])
		));
	});
	parser["STAT_ASSIGN"] = wrap_error<Statement>([](const peg::SemanticValues& sv, peg::any& ctx) {
		switch (sv.choice()) {
		case 0:  {
			string name = sv[0].get<string>();
			Type* type = ctx.get<Context*>()->getType(name);
			return static_cast<Statement*>(new Assign(
				new VarDecl(name, type->clone()),
				ctx.get<Context*>()->use<Expr>(sv[1]),
				false
			));
		}
		case 1:  {
			VarDecl* decl = sv[0].get<VarDecl*>();
			return static_cast<Statement*>(new Assign(
				sv[0].get<VarDecl*>(),
				ctx.get<Context*>()->use<Expr>(sv[1]),
				ctx.get<Context*>()->initialDecl(decl->name)
			));
		}
		default: throw CompileError("impossible choice in STAT_ASSIGN");
		}
	});
	parser["STAT_SEQ"].enter = [](const char* s, size_t n, peg::any& ctx) {
		ctx.get<Context*>()->push();
	};
	parser["STAT_SEQ"] = wrap_error<Statement>([](const peg::SemanticValues& sv, peg::any& ctx) {
		ctx.get<Context*>()->pop();
		return static_cast<Statement*>(new Seq(
			ctx.get<Context*>()->useVect<Statement>(sv.transform<Statement*>())
		));
	});
	parser["STAT_EXPR"] = wrap_error<Statement>([](const peg::SemanticValues& sv, peg::any& ctx) {
		return static_cast<Statement*>(new StatExpr(
			ctx.get<Context*>()->use<Expr>(sv[0])
		));
	});
	parser["STAT_PRINT"] = wrap_error<Statement>([](const peg::SemanticValues& sv, peg::any& ctx) {
		return static_cast<Statement*>(new Print(
			ctx.get<Context*>()->use<Expr>(sv[0])
		));
	});
	parser["STATEMENT"] = [](const peg::SemanticValues& sv, peg::any& ctx) {
		Statement* s = sv[0].get<Statement*>();
		ctx.get<Context*>()->reg<Statement>(s);
		return s;
	};
	parser["SOURCE"].enter = [](const char* s, size_t n, peg::any& ctx) {
		ctx.get<Context*>()->push();
		ctx.get<Context*>()->newDecl("init", new ArrayType(new IntType));
	};
	parser["SOURCE"] = wrap_error<Prog>([](const peg::SemanticValues& sv, peg::any& ctx) {
		Prog* prog = new Prog(
			 new Lambda(
				ctx.get<Context*>()->use<Statement>(sv[0]),
				{new VarDecl("init", ctx.get<Context*>()->getType("init"))}
			)
		);
		ctx.get<Context*>()->pop();
		return prog;
	});
	parser.log = [](size_t line, size_t col, const std::string& msg) {
		throw CompileError(msg, pair<int, int>(line, col));
	};
	return parser;
}

Prog* parse(const string& file, const string& src) {
	Prog* ret = nullptr;
	unique_ptr<Context> context(make_unique<Context>());
	peg::any ctx(context.get());
	try {
		if (!parser(file).parse<Prog*>(src.c_str(), ctx, ret)) {
			throw CompileError("parsing of " + file + " failed");
		}
	} catch (CompileError& err) {
		err.file = file;
		throw err;
	}
	if (!context->empty()) {
		throw CompileError("type stack is not empty, " + file + " failed");
	}
	return ret;
}

}
