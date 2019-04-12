#pragma once

#include <string>
#include <variant>
#include <vector>
#include <memory>
#include <stack>
#include <numeric>
#include <set>
#include <iostream>

#include "sfl_sema.hpp"

namespace sfl {

using namespace std;

class Expr;
class UndefType;

struct Type {
	virtual ~Type() { }
	virtual string dump() const = 0;
	virtual bool equal(const Type*) const = 0;
	virtual Type* clone() const = 0;
	virtual Expr* defaultExpr() const = 0;
	virtual string toCxx() const = 0;
	virtual bool isDefined() const = 0;
};

struct UndefType : public Type {
	string dump() const override { return "?"; }
	bool equal(const Type* t) const override { return dynamic_cast<const UndefType*>(t); }
	Type* clone() const override { return new UndefType(); }
	Expr* defaultExpr() const override;
	string toCxx() const override { throw CompileError("undefined type cannot be translated"); };
	bool isDefined() const override { return false; }
};

struct IntType : public Type {
	string dump() const override { return "int"; }
	bool equal(const Type* t) const override { return dynamic_cast<const IntType*>(t); }
	Type* clone() const override { return new IntType(); }
	Expr* defaultExpr() const override;
	string toCxx() const override { return "int"; };
	bool isDefined() const override { return true; }
};

struct ArrayType : public Type {
	ArrayType(Type* t) : type(t) { }
	const unique_ptr<Type> type;
	string dump() const override { return "[" + type->dump() + "]"; }
	bool equal(const Type* t) const override {
		if (const ArrayType* a = dynamic_cast<const ArrayType*>(t)) {
			return type->equal(a->type.get());
		} else {
			return false;
		}
	}
	Type* clone() const override { return new ArrayType(type->clone()); }
	Expr* defaultExpr() const override;
	string toCxx() const override {
		return "std::vector<" + type->toCxx() + ">";
	};
	bool isDefined() const override { return type->isDefined(); }
};

struct FuncType : public Type {
	FuncType(Type* v, const vector<Type*>& a = vector<Type*>()) : val(v) {
		for (auto t : a) args.emplace_back(t);
	}
	vector<unique_ptr<Type>> args;
	const unique_ptr<Type> val;
	int arity() const { return args.size(); }
	string dump() const override {
		return "(" + dumpArray(args) + ") -> " + val->dump();
	}
	bool equal(const Type* t) const override {
		if (const FuncType* f = dynamic_cast<const FuncType*>(t)) {
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
		return new FuncType(
			val->clone(),
			accumulate(
				args.begin(),
				args.end(),
				vector<Type*>(),
				[](vector<Type*>& acc, auto& tp) { acc.emplace_back(tp->clone()); return acc;}
			)
		);
	}
	Expr* defaultExpr() const override;
	string toCxx() const override {
		string ret("std::function<");
		ret += val->toCxx() + "(";
		if (args.size()) {
			ret += args[0]->toCxx();
			for (int i = 1; i < args.size(); ++i) {
				ret += ", " + args[i]->toCxx();
			}
		}
		return ret + ")>";
	};
	bool isDefined() const override {
		if (!val->isDefined()) return false;
		for (const auto& arg : args) {
			if (!arg->isDefined()) return false;
		}
		return true;
	}
};

struct AstNode {
	virtual ~AstNode() { }
	virtual const Type* type() const = 0;
	virtual string dump() const = 0;
	virtual set<string> freeVars() const = 0;
	virtual Value* eval(State&) const = 0;
	const AstNode* parent = nullptr;
};

struct Expr : public AstNode {
	virtual string toCxx() const = 0;
};

struct Statement : public AstNode {
	virtual set<string> declVars() const = 0;
	virtual string toCxx(const string& retvar = "") const = 0;
};

struct IntConst : public Expr {
	IntConst(int v) : val(v) { }
	int val;
	const Type* type() const override { static IntType tp; return &tp; }
	string dump() const override { return to_string(val); }
	set<string> freeVars() const override { return set<string>(); }
	Value* eval(State&) const override { return new IntValue(val); }
	string toCxx() const override { return to_string(val); }
};

struct BinOp : public Expr {
	enum Kind { PLUS, MULT, SUB, DIV, RES };
	BinOp(Expr* l, Expr* r, Kind o) : lhs(l), rhs(r), op(o) {
		lhs->parent = this;
		rhs->parent = this;
		switch (op) {
		case PLUS:
			if (!lhs->type()->equal(rhs->type())) {
				throw CompileError("binary operation " + dumpOp(op) + " operand types must be equal");
			}
			if (dynamic_cast<const FuncType*>(lhs->type())) {
				throw CompileError("no binary operations on functions are allowed");
			}
			break;
		default:
			if (!dynamic_cast<const IntType*>(lhs->type()) || !dynamic_cast<const IntType*>(rhs->type())) {
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
		if (dynamic_cast<const IntType*>(type())) {
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
		} else if (dynamic_cast<const ArrayType*>(type())) {
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
	string toCxx() const override {
		if (dynamic_cast<const IntType*>(type())) {
			return "(" + lhs->toCxx() + " " + dumpOp(op) + " " + rhs->toCxx() + ")";
		} else {
			//array case
			return "concat_arrays(" + lhs->toCxx() + ", " + rhs->toCxx()+ ")";
		}
	}
};

struct UnOp : public Expr {
	enum Kind { MINUS };
	UnOp(Expr* e, Kind o) : expr(e), op(o) {
		expr->parent = this;
		if (!expr->type()->equal(type())) {
			throw CompileError("Unary operator - argument must be integer");
		}
	}
	const unique_ptr<Expr> expr;
	const Kind op;
	const Type* type() const override { static IntType tp; return &tp; }
	string dump() const override {
		return "-" + expr->dump();
	}
	set<string> freeVars() const override {
		return expr->freeVars();
	}
	Value* eval(State& state) const override {
		unique_ptr<Value> val(expr->eval(state));
		if (dynamic_cast<const IntType*>(type())) {
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
	string toCxx() const override { return "-" + expr->toCxx(); }
};

struct VarAccess : public Expr {
	VarAccess(const string& n, const Type* t) : name(n), tp(t->clone()) { }
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
	string toCxx() const override { return name; }
};

struct ArrayAccess : public Expr {
	ArrayAccess(Expr* a, Expr* i) : arr(a), ind(i) {
		arr->parent = this;
		ind->parent = this;
		if (!dynamic_cast<const ArrayType*>(arr->type())) {
			throw CompileError("Array expression is needed");
		}
		if (!dynamic_cast<const IntType*>(ind->type())) {
			throw CompileError("Array index expression must have int type");
		}
	}
	const unique_ptr<Expr> arr;
	const unique_ptr<Expr> ind;
	const Type* type() const override {
		return dynamic_cast<const ArrayType*>(arr->type())->type.get();
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
	string toCxx() const override {
		return arr->toCxx() + "[" + ind->toCxx() + "]";
	}
};

struct ArrayMake : public Expr {
	ArrayMake(const vector<Expr*>& es, Type* elType = nullptr) {
		if (!es.size()) {
			throw CompileError("Cannot create empty array because it's type is unknown");
		}
		elementType.reset(elType ? elType : es[0]->type()->clone());
		tp.reset(new ArrayType(elementType->clone()));
		for (Expr* e : es) {
			if (!e->type()->equal(elementType.get())) {
				throw CompileError("All elements of an array must be of the same type");
			}
			arr.emplace_back(e);
			arr.back()->parent = this;
		}
	}
	vector<unique_ptr<Expr>> arr;
	unique_ptr<Type> tp;
	unique_ptr<Type> elementType;
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
	string toCxx() const override {
		string ret;
		ret += type()->toCxx() + "({";
		if (arr.size()) {
			ret += arr[0]->toCxx();
			for (int i = 1; i < arr.size(); ++ i) {
				ret += ", " + arr[i]->toCxx();
			}
		}
		return ret + "})";
	}
};

struct ArrayLen : public Expr {
	ArrayLen(Expr* a) : arr(a) {
		arr->parent = this;
		if (!dynamic_cast<const ArrayType*>(a->type())) {
			throw CompileError("Array length argument must be an array");
		}
	}
	unique_ptr<Expr> arr;
	const Type* type() const override {
		static IntType tp; return &tp;
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
	string toCxx() const override {
		return arr->toCxx() + ".size()";
	}
};

struct FunCall : public Expr {
	FunCall(Expr* f, const vector<Expr*>& ars = vector<Expr*>()) : fun(f) {
		fun->parent = this;
		const FuncType* fun_type = dynamic_cast<const FuncType*>(f->type());
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
			args.back()->parent = this;
		}
	}
	const unique_ptr<Expr> fun;
	vector<unique_ptr<Expr>> args;
	const Type* type() const override {
		return dynamic_cast<const FuncType*>(fun->type())->val.get();
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
		vector<unique_ptr<Value>> argVals;
		for (const auto& arg : args) {
			argVals.emplace_back(arg->eval(state));
		}
		unique_ptr<Value> val(fun->eval(state));
		if (FuncValue* fv = dynamic_cast<FuncValue*>(val.get())) {
			return fv->call(argVals);
		} else if (FuncRef* fr = dynamic_cast<FuncRef*>(val.get())) {
			return fr->func()->call(argVals);
		} else {
			throw RuntimeError("call to non-function: " + val->dump());
		}
	}
	string toCxx() const override {
		string ret;
		ret += fun->toCxx() + "(";
		if (args.size()) {
			ret += args[0]->toCxx();
			for (int i = 1; i < args.size(); ++ i) {
				ret += ", " + args[i]->toCxx();
			}
		}
		return ret + ")";
	}
};

struct VarDecl {
	VarDecl(const string& n, const Type* t) : name(n), tp(t->clone()) { }
	const string name;
	const unique_ptr<Type> tp;
	const Type* type() const { return tp.get(); }
	string dump() const {
		return name + " : " + tp->dump();
	}
	string toCxx() const {
		return tp->toCxx() + " " + name;
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
	string toCxx() const {
		string ret;
		ret += "(";
		if (decls.size()) {
			ret += decls[0]->toCxx();
			for (int i = 1; i < decls.size(); ++ i) {
				ret += ", " + decls[i]->toCxx();
			}
		}
		return ret + ")";
	}
};

struct Lambda : public Expr {
	Lambda(Statement* b, const vector<VarDecl*>& a = vector<VarDecl*>());
	ArgDecls args;
	const unique_ptr<Statement> body;
	unique_ptr<Type> tp;
	set<string> closureVars;
	string recName;

	int arity() const { return args.decls.size(); }
	const Type* type() const override { return tp.get(); }
	string dump() const override;
	set<string> freeVars() const override;
	Value* eval(State& state) const override;
	Value* call(State& state) const;
	string toCxx() const override;
};

struct Cond : public AstNode {
	enum Kind { LESS, LESSEQ, GREAT, GREATEQ, EQ };
	Cond(Expr* l, Expr* r, Kind o) : lhs(l), rhs(r), op(o) {
		lhs->parent = this;
		rhs->parent = this;
		switch (o) {
		case EQ:
			if (!lhs->type()->equal(rhs->type())) {
				throw CompileError("when comparing with = both operands must have same type");
			}
			break;
		default:
			if (!dynamic_cast<const IntType*>(lhs->type()) || !dynamic_cast<const IntType*>(rhs->type())) {
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
		case EQ:      return "==";
		}
		return "";
	}
	const Type* type() const override {
		// TODO: add bool type and implement this method.
		throw CompileError("boolean type is not supported yet");
	}
	string dump() const override {
		return lhs->dump() + " " + dumpOp(op) + " " + rhs->dump();
	}
	set<string> freeVars() const override {
		return unite(lhs->freeVars(), rhs->freeVars());
	}
	Value* eval(State& state) const override {
		// TODO: add bool type and implement this method.
		throw CompileError("boolean type is not supported yet");
	}
	bool evalBool(State& state) const {
		unique_ptr<Value> lhsVal(lhs->eval(state));
		unique_ptr<Value> rhsVal(rhs->eval(state));
		if (dynamic_cast<const IntType*>(lhs->type())) {
			int lhsInt = dynamic_cast<IntValue*>(lhsVal.get())->val;
			int rhsInt = dynamic_cast<IntValue*>(rhsVal.get())->val;
			switch (op) {
			case LESS:    return lhsInt < rhsInt;
			case LESSEQ:  return lhsInt <= rhsInt;
			case GREAT:   return lhsInt > rhsInt;
			case GREATEQ: return lhsInt >= rhsInt;
			case EQ:      return lhsInt == rhsInt;
			}
			return false;
		} else {
			return lhsVal->equal(rhsVal.get());
		}
	}
	string toCxx() const {
		return "(" + lhs->toCxx() + " " + dumpOp(op) + " " + rhs->toCxx() + ")";
	}
};

struct While : public Statement {
	While(Cond* c, Statement* b) : cond(c), body(b), defaultExpr(b->type()->defaultExpr()) {
		cond->parent = this;
		body->parent = this;
	}
	const unique_ptr<Cond> cond;
	const unique_ptr<Statement> body;
	const unique_ptr<Expr> defaultExpr;
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
		unique_ptr<Value> ret(defaultExpr->eval(state));
		while (cond->evalBool(state)) {
			ret.reset(body->eval(state));
		}
		return ret.release();
	}
	string toCxx(const string& retvar = "") const override {
		return "while (" + cond->toCxx() + ")\n" + indent(body->toCxx(retvar)) + "\n;";
	}
};

struct If : public Statement {
	If(Cond* c, Statement* p, Statement* n) : cond(c), pos(p), neg(n) {
		cond->parent = this;
		pos->parent = this;
		neg->parent = this;
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
		if (cond->evalBool(state)) {
			return pos->eval(state);
		} else {
			return neg->eval(state);
		}
	}
	string toCxx(const string& retvar = "") const override {
		return "if (" + cond->dump() + ")\n" + indent(pos->toCxx(retvar)) + "\nelse\n" + indent(neg->toCxx(retvar)) + "\n;";
	}
};

struct Assign : public Statement {
	Assign(VarDecl* d, Expr* e, bool i) : initial(i), decl(d), expr(e), defaultExpr(e->type()->defaultExpr()) {
		expr->parent = this;
		if (!d->type()->equal(e->type())) {
			throw CompileError("Type mismatch in assignment: " + d->type()->dump() + " != " + e->type()->dump());
		}
		if (Lambda* lambda = dynamic_cast<Lambda*>(e)) {
			lambda->recName = decl->name;
		}
	}
	const bool initial;
	unique_ptr<VarDecl> decl;
	const unique_ptr<Expr> expr;
	const unique_ptr<Expr> defaultExpr;
	const Type* type() const override { return decl->type(); }
	string dump() const override {
		return decl->dump() + " = " + expr->dump();
	}
	set<string> freeVars() const override {
		return expr->freeVars();
	}
	set<string> declVars() const override {
		return set<string>({decl->name});
	}
	Value* eval(State& state) const override {
		if (!state.find(decl->name)) {
			state.set(decl->name, defaultExpr->eval(state));
		}
		Value* val = expr->eval(state);
		if (FuncValue* func = dynamic_cast<FuncValue*>(val)) {
			func->closure.set(decl->name, new FuncRef(func));
		}
		state.set(decl->name, val->clone());
		return val;
	}
	string toCxx(const string& retvar = "") const override {
		string var = initial ? decl->toCxx() : decl->name;
		if (retvar != "") {
			return var + " = " + expr->toCxx() + ";\n" + retvar + " = " + decl->name + ";";
		} else {
			return var + " = " + expr->toCxx() + ";";
		}
	}
};

struct Seq : public Statement{
	Seq(const vector<Statement*>& v) {
		for (auto s : v) {
			seq.emplace_back(s);
			seq.back()->parent = this;
		}
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
		unique_ptr<Value> val;
		for (const auto& s : seq) {
			val.reset(s->eval(state));
		}
		return val.release();
	}
	string toCxx(const string& retvar = "") const override {
		string ret = "{\n";
		for (int i = 0; i < seq.size(); ++ i) {
			ret += indent(seq[i]->toCxx(i == seq.size() - 1 ? retvar : "")) + "\n";
		}
		return ret + "}";
	}
};

struct StatExpr : public Statement {
	StatExpr(Expr* e) : expr(e) {
		expr->parent = this;
	}
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
	string toCxx(const string& retvar = "") const override {
		if (retvar != "") {
			return retvar + " = " + expr->toCxx() + ";";
		} else {
			return expr->toCxx() + ";";
		}
	}
};

struct Print : public Statement {
	Print(Expr* e) : expr(e) {
		expr->parent = this;
	}
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
	string toCxx(const string& retvar = "") const override {
		string to_print(expr->toCxx());
		if (const FuncType* tp = dynamic_cast<const FuncType*>(type())) {
			to_print = "\"" + expr->dump() + " : " + tp->dump() + "\"";
		}
		if (retvar == "") {
			return "std::cout << " + to_print + " << std::endl;";
		} else {
			string ret;
			ret += retvar + " = " + to_print + ";\n";
			ret += "std::cout << " + retvar + " << std::endl;";
			return ret;
		}
	}
};

const string cxx_includes =
R"(#include <vector>
#include <functional>
#include <string>
#include <iostream>

)";

const string cxx_runtime =
R"(template<class T>
std::vector<T> concat_arrays(const std::vector<T>& a1, const std::vector<T>& a2) {
	std::vector<T> ret(a1);
	for (const auto& x : a2) ret.push_back(x);
	return ret; 
}

template<class T>
std::ostream& operator << (std::ostream& os, const std::vector<T>& arr) {
	os << "[";
	if (arr.size()) {
		os << arr[0];
		for (int i = 1; i < arr.size(); ++i) {
			os << ", " << arr[i];
		}
	}
	os << "]";
	return os;
}

)";

const string cxx_main = R"(
int main(int argc, const char** argv) {
	std::vector<int> input;
	for (int c = 1; c < argc; ++ c) {
		input.push_back(std::stoi(argv[c]));
	}
	try {
		run(input);
	} catch (std::exception& e) {
		std::cerr << e.what() << std::endl;
		return 1;
	} catch (...) {
		std::cerr << "runtime error" << std::endl;
		return 1;
	}
	return 0;
}
)";

struct Prog {
	Prog(Lambda* p) : lambda(p) { }
	unique_ptr<Lambda> lambda;
	const Type* type() const { return lambda->type(); }
	string dump() const { return lambda->dump(); }
	set<string> freeVars() const { return lambda->freeVars(); }
	void run(const vector<int>&);
	string toCxx() const {
		string body;
		body += "auto prog = " + lambda->toCxx() + ";\n";
		body += "std::cout << prog(init) << std::endl;\n";
		string run;
		run = "void run(const std::vector<int>& init) {\n";
		run += indent(body);
		run += "}\n";
		return cxx_includes + cxx_runtime + run + cxx_main;
	}
};

inline Lambda::Lambda(Statement* b, const vector<VarDecl*>& a) :
	args(a),
	body(b),
	tp(new FuncType(
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
	return setminus(setminus(body->freeVars(), args.boundVars()), body->declVars());
}

inline Value* Lambda::eval(State& state) const {
	return new FuncValue(this, state);
}

inline Value* Lambda::call(State& state) const {
	return body->eval(state);
}

inline string Lambda::toCxx() const {
	string closure_cxx;
	closure_cxx += "[";
	if (closureVars.size()) {
		string var;
		auto i = closureVars.begin();
		var = *i++;
		closure_cxx += (var == recName ? "&" + var : var);
		while (i != closureVars.end()) {
			var = *i++;
			closure_cxx += ", " + (var == recName ? "&" + var : var);
		}
	}
	closure_cxx += "]";
	string body_cxx;
	static int retvar_counter = 0;
	string retvar = "ret_var_" + to_string(retvar_counter++);
	body_cxx += body->type()->toCxx() + " " + retvar + ";\n";
	body_cxx += body->toCxx(retvar) + "\n";
	body_cxx += "return " + retvar + ";\n";
	return closure_cxx + args.toCxx() + "{\n" + indent(body_cxx) + "\n}";
}

inline Expr* UndefType::defaultExpr() const {
	throw CompileError("undefined type cannot have a default value");
}

inline Expr* IntType::defaultExpr() const {
	return new IntConst(0);
}

inline Expr* ArrayType::defaultExpr() const {
	return new ArrayMake({type->defaultExpr()});
}

inline Expr* FuncType::defaultExpr() const {
	return new Lambda(
		new StatExpr(val->defaultExpr()),
		accumulate(
			args.begin(),
			args.end(),
			vector<VarDecl*>(),
			[](vector<VarDecl*>& acc,  const auto& type) {
				acc.push_back(new VarDecl("_", type.get())); return acc;
			}
		)
	);
}

Prog* parse(const string& file, const string& src);

}
