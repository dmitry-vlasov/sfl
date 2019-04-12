#include "sfl_ast.hpp"

namespace sfl {

Value* FuncValue::call(const vector<unique_ptr<Value>>& args) const {
	if (lambda->arity() != args.size()) {
		throw RuntimeError("arity of function call violated");
	}
	State state(closure);
	for (int i = 0; i < lambda->arity(); ++i) {
		state.set(lambda->args.decls[i]->name, args[i]->clone());
	}
	return lambda->call(state);
}

FuncValue::FuncValue(const Lambda* l, const State& state) : lambda(l) {
	for (const auto& cv : lambda->closureVars) {
		closure.set(cv, state.get(cv)->clone());
	}
}
string FuncValue::dump() const {
	return lambda->dump();
}

bool FuncValue::equal(const Value* v) const {
	if (const FuncValue* fv = dynamic_cast<const FuncValue*>(v)) {
		return lambda == fv->lambda && closure.equal(fv->closure);
	} else if (const FuncRef* fr = dynamic_cast<const FuncRef*>(v)) {
		return equal(fr->func());
	} else {
		return false;
	}
}

void Prog::run(const vector<int>& input) {
	vector<Value*> args;
	for (int x : input) {
		args.push_back(new IntValue(x));
	}
	vector<unique_ptr<Value>> arr;
	arr.emplace_back(new ArrayValue(args));
	FuncValue func(lambda.get());
	unique_ptr<Value> ret(func.call(arr));
	if (!ret) {
		throw RuntimeError("any program must return non-null value");
	}
	cout << ret->dump() << endl;
}

}


