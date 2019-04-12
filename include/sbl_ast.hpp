#pragma once

#include <string>
#include <variant>
#include <vector>
#include <memory>
#include <stack>
#include <numeric>
#include <set>
#include <iostream>

#include "sbl_sema.hpp"

namespace sbl {

using namespace std;


struct Type {
	virtual ~Type() { }
	virtual string dump() const = 0;
	virtual bool equal(const Type*) const = 0;
	virtual Type* clone() const = 0;
	virtual Value* defaultValue() const = 0;
};

struct IntType : public Type {
	string dump() const override { return "int"; }
	bool equal(const Type* t) const override { return dynamic_cast<const IntType*>(t); }
	Type* clone() const override { return new IntType(); }
	Value* defaultValue() const override { return new IntValue(0); }
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
	Value* defaultValue() const override { return new ArrayValue(vector<Value*>(1, type->defaultValue())); }
};

struct AstNode {
	virtual ~AstNode() { }
	virtual string dump() const = 0;
};

struct Expr : public AstNode {
	virtual const Type* type() const = 0;
	virtual Value* eval(State&) const = 0;
};

struct IntConst : public Expr {
	IntConst(int v) : val(v) { }
	int val;
	const Type* type() const override { static IntType tp; return &tp; }
	Value* eval(State&) const override { return new IntValue(val); }
	string dump() const override { return to_string(val); }
};

struct BinOp : public Expr {
	enum Kind { PLUS, MULT, SUB, DIV, RES };
	BinOp(Expr* l, Expr* r, Kind o) : lhs(l), rhs(r), op(o) {
		switch (op) {
		case PLUS:
			if (!lhs->type()->equal(rhs->type())) {
				throw CompileError("binary operation " + dumpOp(op) + " operand types must be equal");
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
	const Type* type() const override { static IntType tp; return &tp; }
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
	string dump() const override {
		return "-" + expr->dump();
	}
};

struct VarAccess : public Expr {
	VarAccess(const string& n, const Type* t) : name(n), tp(t->clone()) { }
	const string name;
	const unique_ptr<Type> tp;
	const Type* type() const override { return tp.get(); }
	Value* eval(State& state) const override {
		return state.get(name)->clone();
	}
	string dump() const override { return name; }
};

struct ArrayAccess : public Expr {
	ArrayAccess(Expr* a, Expr* i) : arr(a), ind(i) {
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
	Value* eval(State& state) const override {
		unique_ptr<ArrayValue> arrVal(dynamic_cast<ArrayValue*>(arr->eval(state)));
		unique_ptr<IntValue>   indVal(dynamic_cast<IntValue*>(ind->eval(state)));
		if (0 <= indVal->val && indVal->val < arrVal->vect.size()) {
			return arrVal->vect[indVal->val]->clone();
		} else {
			throw RuntimeError("Array index is out of bounds");
		}
	}
	string dump() const override {
		return "[" + arr->dump() + "[" + ind->dump() + "]]";
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
		}
	}
	vector<unique_ptr<Expr>> arr;
	unique_ptr<Type> tp;
	unique_ptr<Type> elementType;
	const Type* type() const override { return tp.get(); }
	Value* eval(State& state) const override {
		vector<Value*> values;
		for (const auto& e : arr) {
			values.push_back(e->eval(state));
		}
		return new ArrayValue(values);
	}
	string dump() const override {
		return "[" + dumpArray(arr) + "]";
	}
};

struct ArrayLen : public Expr {
	ArrayLen(Expr* a) : arr(a) {
		if (!dynamic_cast<const ArrayType*>(a->type())) {
			throw CompileError("Array length argument must be an array");
		}
	}
	unique_ptr<Expr> arr;
	const Type* type() const override {
		static IntType tp; return &tp;
	}
	Value* eval(State& state) const override {
		unique_ptr<ArrayValue> arrVal(dynamic_cast<ArrayValue*>(arr->eval(state)));
		return new IntValue(arrVal->vect.size());
	}
	string dump() const override {
		return "|" + arr->dump() + "|";
	}
};

struct VarDecl {
	VarDecl(const string& n, const Type* t) : name(n), tp(t->clone()) { }
	const string name;
	const unique_ptr<Type> tp;
	const Type* type() const { return tp.get(); }
	string dump() const {
		return name + " : " + tp->dump() + " ;";
	}
};

struct VarDecls {
	VarDecls(const vector<VarDecl*> ds) {
		for (auto d : ds) decls.emplace_back(d);
	}
	vector<unique_ptr<VarDecl>> decls;
	string dump() const {
		return "variables\n" + dumpArray(decls, "\n");
	}
};

struct Cond : public AstNode {
	enum Kind { LESS, LESSEQ, GREAT, GREATEQ, EQ };
	Cond(Expr* l, Expr* r, Kind o) : lhs(l), rhs(r), op(o) {
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
	string dump() const override {
		return lhs->dump() + " " + dumpOp(op) + " " + rhs->dump();
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
};

struct Statement : public AstNode {
	Statement(int l) : label(l) { }
	virtual int exec(State&) const = 0;
	const int label;
};

struct If : public Statement {
	If(int l, Cond* c, int p, int n) : Statement(l), cond(c), pos(p), neg(n) { }
	const unique_ptr<Cond> cond;
	const int pos;
	const int neg;
	int exec(State& state) const override {
		return cond->evalBool(state) ? pos : neg;
	}
	string dump() const override {
		return to_string(label) + " if " + cond->dump() + " then " + to_string(pos) + " else " + to_string(neg) + ";";
	}
};

struct Assign : public Statement {
	Assign(int l, const string& v, Expr* e, int g) :
		Statement(l), var(v), expr(e), goto_(g) { }
	const string var;
	const unique_ptr<Expr> expr;
	int goto_;
	int exec(State& state) const override {
		state.set(var, expr->eval(state));
		return goto_;
	}
	string dump() const override {
		return to_string(label) + var + " = " + expr->dump() + " goto " + to_string(goto_) + " ;";
	}
};

struct Print : public Statement {
	Print(int l, Expr* e, int g) : Statement(l), expr(e), goto_(g) { }
	const unique_ptr<Expr> expr;
	const int goto_;
	int exec(State& state) const override {
		unique_ptr<Value> val(expr->eval(state));
		cout << val->dump() << endl;
		return goto_;
	}
	string dump() const override { return to_string(label) + " print " + expr->dump() + " goto " + to_string(goto_) + " ;"; }
};

struct Prog {
	Prog(const vector<VarDecl*>& ds, const vector<Statement*>& ps) {
		for (auto d : ds) {
			decls.emplace_back(d);
		}
		for (auto p : ps) {
			prog.emplace(p->label, p);
		}
	}
	string dump() const {
		string decls_str = "variables\n" + dumpArray<unique_ptr<VarDecl>>(decls, "\n");
		string prog_str = "\nprogram\n";
		for (const auto& p : prog){
			prog_str += "\t" + p.second->dump() + "\n";
		}
		return decls_str + prog_str;
	}
	void run(const vector<int>& input);

	vector<unique_ptr<VarDecl>> decls;
	map<int, unique_ptr<Statement>> prog;
};

Prog* parse(const string& file, const string& src);

}
