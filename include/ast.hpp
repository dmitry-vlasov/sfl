#pragma once

#include <string>
#include <variant>
#include <vector>
#include <memory>
#include <stack>
#include <numeric>
#include <set>
#include <iostream>

#include "sema.hpp"

namespace sfl {

using namespace std;

struct Type {
	virtual ~Type() { }
	virtual string dump() const = 0;
	virtual bool equal(const Type*) const = 0;
	virtual Type* clone() const = 0;
};

struct Int : public Type {
	string dump() const override { return "int"; }
	bool equal(const Type* t) const override { return dynamic_cast<const Int*>(t); }
	Type* clone() const override { return new Int(); }
};

struct Array : public Type {
	Array(Type* t) : type(t) { }
	const unique_ptr<Type> type;
	string dump() const override { return "[" + type->dump() + "]"; }
	bool equal(const Type* t) const override {
		if (const Array* a = dynamic_cast<const Array*>(t)) {
			return type->equal(a->type.get());
		} else {
			return false;
		}
	}
	Type* clone() const override { return new Array(type->clone()); }
};

struct Func : public Type {
	Func(Type* v, const vector<Type*>& a = vector<Type*>()) : val(v) {
		for (auto t : a) args.emplace_back(t);
	}
	vector<unique_ptr<Type>> args;
	const unique_ptr<Type> val;
	int arity() const { return args.size(); }
	string dump() const override {
		return "(" + dumpArray(args) + ") -> " + val->dump();
	}
	bool equal(const Type* t) const override {
		if (const Func* f = dynamic_cast<const Func*>(t)) {
			if (args.size() != f->args.size() || !val->equal(f->val.get())) {
				return false;
			}
			for (int i = 0; i < args.size(); ++ i) {
				if (!args[i]->equal(f->args[i].get())) {
					return false;
				}
			}
			return true;
		} else {
			return false;
		}
	}
	Type* clone() const override {
		return new Func(
			val->clone(),
			accumulate(
				args.begin(),
				args.end(),
				vector<Type*>(),
				[](vector<Type*>& acc, auto& tp) { acc.emplace_back(tp->clone()); return acc;}
			)
		);
	}
};

struct Expr {
	virtual ~Expr() { }
	virtual const Type* type() const = 0;
	virtual string dump() const = 0;
	virtual set<string> freeVars() const = 0;
	virtual Value* eval(State&) const = 0;
};

struct IntConst : public Expr {
	IntConst(int v) : val(v) { }
	int val;
	const Type* type() const override { static Int tp; return &tp; }
	string dump() const override { return to_string(val); }
	set<string> freeVars() const override { return set<string>(); }
	Value* eval(State&) const override { return new IntValue(val); }
};

struct BinOp : public Expr {
	enum Kind { PLUS, MULT, SUB, DIV, RES };
	BinOp(Expr* l, Expr* r, Kind o) : lhs(l), rhs(r), op(o) {
		switch (op) {
		case PLUS:
			if (!lhs->type()->equal(rhs->type())) {
				throw CompileError("binary operation " + dumpOp(op) + " operand types must be equal");
			}
			if (dynamic_cast<const Func*>(lhs->type())) {
				throw CompileError("no binary operations on functions are allowed");
			}
			break;
		default:
			if (!dynamic_cast<const Int*>(lhs->type()) || !dynamic_cast<const Int*>(rhs->type())) {
				throw CompileError("binary operation " + dumpOp(op) + " operand types must be integer");
			}
		}
	}
	const unique_ptr<Expr> lhs;
	const unique_ptr<Expr> rhs;
	const Kind op;
	const Type* type() const override {
		return lhs->type();
	}
	static string dumpOp(Kind op) {
		switch (op) {
		case PLUS: return "+";
		case MULT: return "*";
		case SUB:  return "-";
		case DIV:  return "/";
		case RES:  return "%";
		}
		return "";
	}
	string dump() const override {
		return "(" + lhs->dump() + " " + dumpOp(op) + " " + rhs->dump() + ")";
	}
	set<string> freeVars() const override {
		return unite(lhs->freeVars(), rhs->freeVars());
	}
	Value* eval(State& state) const override {
		unique_ptr<Value> lhsVal(lhs->eval(state));
		unique_ptr<Value> rhsVal(rhs->eval(state));
		if (dynamic_cast<const Int*>(type())) {
			int lhsInt = dynamic_cast<IntValue*>(lhsVal.get())->val;
			int rhsInt = dynamic_cast<IntValue*>(rhsVal.get())->val;
			int ret = 0;
			switch (op) {
			case PLUS: return new IntValue(lhsInt + rhsInt);
			case MULT: return new IntValue(lhsInt * rhsInt);
			case SUB:  return new IntValue(lhsInt - rhsInt);
			case DIV:  return new IntValue(lhsInt / rhsInt);
			case RES:  return new IntValue(lhsInt % rhsInt);
			default: throw RuntimeError("illegal binary operator");
			}
		} else if (dynamic_cast<const Array*>(type())) {
			if (op == PLUS) {
				ArrayValue* lhsArr = dynamic_cast<ArrayValue*>(lhsVal.get());
				ArrayValue* rhsArr = dynamic_cast<ArrayValue*>(rhsVal.get());
				ArrayValue* ret = dynamic_cast<ArrayValue*>(lhsArr->clone());
				for (const auto& x : rhsArr->vect) {
					ret->vect.emplace_back(x->clone());
				}
				return ret;
			} else {
				throw RuntimeError("illegal arguments in binary operator");
			}
		} else {
			throw RuntimeError("illegal arguments in binary operator");
		}
	}
};

struct UnOp : public Expr {
	enum Kind { MINUS };
	UnOp(Expr* e, Kind o) : expr(e), op(o) {
		if (!expr->type()->equal(type())) {
			throw CompileError("Unary operator - argument must be integer");
		}
	}
	const unique_ptr<Expr> expr;
	const Kind op;
	const Type* type() const override { static Int tp; return &tp; }
	string dump() const override {
		return "-" + expr->dump();
	}
	set<string> freeVars() const override {
		return expr->freeVars();
	}
	Value* eval(State& state) const override {
		unique_ptr<Value> val(expr->eval(state));
		if (dynamic_cast<const Int*>(type())) {
			int valInt = dynamic_cast<IntValue*>(val.get())->val;
			int ret = 0;
			switch (op) {
			case MINUS: return new IntValue(-valInt);
			default: throw RuntimeError("illegal binary operator");
			}
		} else {
			throw RuntimeError("illegal arguments in binary operator");
		}
	}
};

struct VarAccess : public Expr {
	VarAccess(const string& n, Type* t) : name(n), tp(t->clone()) { }
	const string name;
	const unique_ptr<Type> tp;
	const Type* type() const override { return tp.get(); }
	string dump() const override { return name; }
	set<string> freeVars() const override {
		return set<string>({name});
	}
	Value* eval(State& state) const override {
		return state.get(name)->clone();
	}
};

struct ArrayAccess : public Expr {
	ArrayAccess(Expr* a, Expr* i) : arr(a), ind(i) {
		if (!dynamic_cast<const Array*>(arr->type())) {
			throw CompileError("Array expression is needed");
		}
		if (!dynamic_cast<const Int*>(ind->type())) {
			throw CompileError("Array index expression must have int type");
		}
	}
	const unique_ptr<Expr> arr;
	const unique_ptr<Expr> ind;
	const Type* type() const override {
		return dynamic_cast<const Array*>(arr->type())->type.get();
	}
	string dump() const override {
		return "[" + arr->dump() + "[" + ind->dump() + "]]";
	}
	set<string> freeVars() const override {
		return unite(arr->freeVars(), ind->freeVars());
	}
	Value* eval(State& state) const override {
		unique_ptr<ArrayValue> arrVal(dynamic_cast<ArrayValue*>(arr->eval(state)));
		unique_ptr<IntValue>   indVal(dynamic_cast<IntValue*>(ind->eval(state)));
		if (0 <= indVal->val && indVal->val < arrVal->vect.size()) {
			return arrVal->vect[indVal->val]->clone();
		} else {
			throw RuntimeError("Array index is out of bounds");
		}
	}
};

struct ArrayMake : public Expr {
	ArrayMake(vector<Expr*> es) {
		if (!es.size()) {
			throw CompileError("Cannot create empty array because it's type is unknown");
		}
		const Type* elementType = es[0]->type();
		tp.reset(new Array(elementType->clone()));
		for (Expr* e : es) {
			if (!e->type()->equal(elementType)) {
				throw CompileError("All elements of an array must be of the same type");
			}
			arr.emplace_back(e);
		}
	}
	vector<unique_ptr<Expr>> arr;
	unique_ptr<Type> tp;
	const Type* type() const override { return tp.get(); }
	string dump() const override {
		return "[" + dumpArray(arr) + "]";
	}
	set<string> freeVars() const override {
		set<string> ret;
		for (const auto& e : arr) {
			ret = unite(ret, e->freeVars());
		}
		return ret;
	}
	Value* eval(State& state) const override {
		vector<Value*> values;
		for (const auto& e : arr) {
			values.push_back(e->eval(state));
		}
		return new ArrayValue(values);
	}
};

struct ArrayLen : public Expr {
	ArrayLen(Expr* a) : arr(a) {
		if (!dynamic_cast<const Array*>(a->type())) {
			throw CompileError("Array length argument must be an array");
		}
	}
	unique_ptr<Expr> arr;
	const Type* type() const override {
		static Int tp; return &tp;
	}
	string dump() const override {
		return "|" + arr->dump() + "|";
	}
	set<string> freeVars() const override {
		return arr->freeVars();
	}
	Value* eval(State& state) const override {
		unique_ptr<ArrayValue> arrVal(dynamic_cast<ArrayValue*>(arr->eval(state)));
		return new IntValue(arrVal->vect.size());
	}
};

struct FunCall : public Expr {
	FunCall(Expr* f, const vector<Expr*>& ars = vector<Expr*>()) : fun(f) {
		const Func* fun_type = dynamic_cast<const Func*>(f->type());
		if (!fun_type) {
			throw CompileError("Function expression is needed");
		}
		if (fun_type->arity() != ars.size()) {
			throw CompileError("Function arity mismatch");
		}
		for (int i = 0; i < ars.size(); ++ i) {
			if (!fun_type->args[i]->equal(ars[i]->type())) {
				throw CompileError("Function argument type mismatch: " + fun_type->args[i]->dump() + " != " + ars[i]->type()->dump());
			}
			args.emplace_back(ars[i]);
		}
	}
	const unique_ptr<Expr> fun;
	vector<unique_ptr<Expr>> args;
	const Type* type() const override {
		return dynamic_cast<const Func*>(fun->type())->val.get();
	}
	string dump() const override {
		return "(" + fun->dump() + "(" + dumpArray(args) + "))";
	}
	set<string> freeVars() const override {
		set<string> ret(fun->freeVars());
		for (const auto& e : args) {
			ret = unite(ret, e->freeVars());
		}
		return ret;
	}
	Value* eval(State& state) const override {
		unique_ptr<FuncValue> funVal(dynamic_cast<FuncValue*>(fun->eval(state)));
		vector<unique_ptr<Value>> argVals;
		for (const auto& arg : args) {
			argVals.emplace_back(arg->eval(state));
		}
		return funVal->call(argVals);
	}
};

struct VarDecl {
	VarDecl(const string& n, Type* t) : name(n), tp(t->clone()) { }
	const string name;
	const unique_ptr<Type> tp;
	const Type* type() const { return tp.get(); }
	string dump() const {
		return name + " : " + tp->dump();
	}
};

struct ArgDecls {
	ArgDecls(const vector<VarDecl*> ds) {
		for (auto d : ds) decls.emplace_back(d);
	}
	vector<unique_ptr<VarDecl>> decls;
	string dump() const {
		return dumpArray(decls);
	}
	set<string> boundVars() const {
		set<string> ret;
		for (const auto& d : decls) {
			ret.insert(d->name);
		}
		return ret;
	}
};

class Statement;

struct Lambda : public Expr {
	Lambda(Statement* b, const vector<VarDecl*>& a = vector<VarDecl*>());
	ArgDecls args;
	const unique_ptr<Statement> body;
	unique_ptr<Type> tp;
	set<string> closureVars;

	int arity() const { return args.decls.size(); }
	const Type* type() const override { return tp.get(); }
	string dump() const override;
	set<string> freeVars() const override;
	Value* eval(State& state) const override;
	Value* call(State& state) const;
};

struct Cond {
	enum Kind { LESS, LESSEQ, GREAT, GREATEQ, EQ };
	Cond(Expr* l, Expr* r, Kind o) : lhs(l), rhs(r), op(o) {
		switch (o) {
		case EQ:
			if (!lhs->type()->equal(rhs->type())) {
				throw CompileError("when comparing with = both operands must have same type");
			}
			break;
		default:
			if (!dynamic_cast<const Int*>(lhs->type()) || !dynamic_cast<const Int*>(rhs->type())) {
				throw CompileError("when comparing with " + dumpOp(op) + " operands must have int type");
			}
		}
	}
	const unique_ptr<Expr> lhs;
	const unique_ptr<Expr> rhs;
	const Kind op;
	static string dumpOp(Kind op) {
		switch (op) {
		case LESS:    return "<";
		case LESSEQ:  return "<=";
		case GREAT:   return ">";
		case GREATEQ: return ">=";
		case EQ:      return "=";
		}
		return "";
	}
	string dump() const {
		return lhs->dump() + " " + dumpOp(op) + " " + rhs->dump();
	}
	set<string> freeVars() const {
		return unite(lhs->freeVars(), rhs->freeVars());
	}
	bool eval(State& state) const {
		unique_ptr<Value> lhsVal(lhs->eval(state));
		unique_ptr<Value> rhsVal(rhs->eval(state));
		if (dynamic_cast<const Int*>(lhs->type())) {
			int lhsInt = dynamic_cast<IntValue*>(lhsVal.get())->val;
			int rhsInt = dynamic_cast<IntValue*>(rhsVal.get())->val;
			switch (op) {
			case LESS:    return lhsInt < rhsInt;
			case LESSEQ:  return lhsInt <= rhsInt;
			case GREAT:   return lhsInt > rhsInt;
			case GREATEQ: return lhsInt >= rhsInt;
			case EQ:      return lhsInt = rhsInt;
			}
			return false;
		} else {
			return lhsVal->equal(rhsVal.get());
		}
	}
};

struct Statement {
	virtual ~Statement() { }
	virtual const Type* type() const = 0;
	virtual string dump() const = 0;
	virtual set<string> freeVars() const = 0;
	virtual set<string> declVars() const = 0;
	virtual Value* eval(State& state) const = 0;
};

struct While : public Statement {
	While(Cond* c, Statement* b) : cond(c), body(b) { }
	const unique_ptr<Cond> cond;
	const unique_ptr<Statement> body;
	const Type* type() const override { return body->type(); }
	string dump() const override {
		return "while " + cond->dump() + " do\n" + indent(body->dump());
	}
	set<string> freeVars() const override {
		return unite(cond->freeVars(), body->freeVars());
	}
	set<string> declVars() const override {
		return body->declVars();
	}
	Value* eval(State& state) const override {
		Value* ret = nullptr;
		while (cond->eval(state)) {
			ret = body->eval(state);
		}
		return ret;
	}
};

struct If : public Statement {
	If(Cond* c, Statement* p, Statement* n) : cond(c), pos(p), neg(n) {
		if (!pos->type()->equal(neg->type())) {
			throw CompileError("Types differ in positive and negative branches: " + pos->type()->dump() + " != " + neg->type()->dump());
		}
	}
	const unique_ptr<Cond> cond;
	const unique_ptr<Statement> pos;
	const unique_ptr<Statement> neg;
	const Type* type() const override { return pos->type(); }
	string dump() const override {
		return "if " + cond->dump() + " then\n" +
			indent(pos->dump()) + "\nelse\n" + indent(neg->dump());
	}
	set<string> freeVars() const override {
		return unite(unite(cond->freeVars(), pos->freeVars()), neg->freeVars());
	}
	set<string> declVars() const override {
		return unite(pos->declVars(), neg->declVars());
	}
	Value* eval(State& state) const override {
		if (cond->eval(state)) {
			return pos->eval(state);
		} else {
			return neg->eval(state);
		}
	}
};

struct Assign : public Statement {
	Assign(const string& n, Expr* e, Type* t) : name(n), expr(e), tp(t) {
		if (!t->equal(e->type())) {
			throw CompileError("Type mismatch in assignment: " + t->dump() + " != " + e->type()->dump());
		}
	}
	const string name;
	const unique_ptr<Expr> expr;
	const unique_ptr<Type> tp;
	const Type* type() const override { return tp.get(); }
	string dump() const override {
		if (tp) {
			return name + " : " + tp->dump() + " = " + expr->dump();
		} else {
			return name + " = " + expr->dump();
		}
	}
	set<string> freeVars() const override {
		return expr->freeVars();
	}
	set<string> declVars() const override {
		return set<string>({name});
	}
	Value* eval(State& state) const override {
		Value* val = expr->eval(state);
		state.set(name, val->clone());
		return val;
	}
};

struct Seq : public Statement{
	Seq(const vector<Statement*>& v) {
		for (auto s : v) seq.emplace_back(s);
	}
	vector<unique_ptr<Statement>> seq;
	const Type* type() const override {
		return seq.back()->type();
	}
	string dump() const override {
		string ret = "{";
		if (seq.size()) {
			ret += "\n" + indent(seq[0]->dump());
			for (int i = 1; i < seq.size(); ++ i) {
				ret += ";\n" + indent(seq[i]->dump());
			}
			ret += "\n";
		}
		return ret + "}";
	}
	set<string> freeVars() const override {
		set<string> ret;
		for (const auto& s : seq) {
			ret = unite(ret, s->freeVars());
		}
		return ret;
	}
	set<string> declVars() const override {
		set<string> ret;
		for (const auto& s : seq) {
			ret = unite(ret, s->declVars());
		}
		return ret;
	}
	Value* eval(State& state) const override {
		Value* val = nullptr;
		for (const auto& s : seq) {
			val = s->eval(state);
		}
		return val;
	}
};

struct StatExpr : public Statement {
	StatExpr(Expr* e) : expr(e) { }
	const unique_ptr<Expr> expr;
	const Type* type() const override { return expr->type(); }
	string dump() const override { return expr->dump(); }
	set<string> freeVars() const override {
		return expr->freeVars();
	}
	set<string> declVars() const override {
		return set<string>();
	}
	Value* eval(State& state) const override {
		return expr->eval(state);
	}
};

struct Print : public Statement {
	Print(Expr* e) : expr(e) { }
	const unique_ptr<Expr> expr;
	const Type* type() const override { return expr->type(); }
	string dump() const override { return "print " + expr->dump(); }
	set<string> freeVars() const override {
		return expr->freeVars();
	}
	set<string> declVars() const override {
		return set<string>();
	}
	Value* eval(State& state) const override {
		Value* val = expr->eval(state);
		cout << val->dump() << endl;
		return val;
	}
};

struct Prog {
	Prog(Lambda* p) : lambda(p) { }
	unique_ptr<Lambda> lambda;
	const Type* type() const { return lambda->type(); }
	string dump() const { return lambda->dump(); }
	set<string> freeVars() const { return lambda->freeVars(); }
	void run(const vector<int>&);
};

inline Lambda::Lambda(Statement* b, const vector<VarDecl*>& a) :
	args(a),
	body(b),
	tp(new Func(
		body->type()->clone(),
		accumulate(
			args.decls.begin(),
			args.decls.end(),
			vector<Type*>(),
			[](vector<Type*>& acc, auto& decl) {
				acc.emplace_back(decl->type()->clone()); return acc;
			}
		)
	)),
	closureVars(setminus(freeVars(), body->declVars())) { }

inline string Lambda::dump() const {
	return "/\\ " + args.dump() + " -> " + body->dump();
}

inline set<string> Lambda::freeVars() const {
	return setminus(body->freeVars(), args.boundVars());
}

inline Value* Lambda::eval(State& state) const {
	return new FuncValue(this, state);
}

inline Value* Lambda::call(State& state) const {
	return body->eval(state);
}


Prog* parse(const string& file, const string& src);

}
