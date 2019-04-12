#include "sbl_ast.hpp"

namespace sbl {

void Prog::run(const vector<int>& input) {
	State state;
	for (const auto& p : decls) {
		state.set(p->name, p->type()->defaultValue());
	}
	vector<Value*> args;
	for (int x : input) {
		args.push_back(new IntValue(x));
	}
	state.set("init", new ArrayValue(args));
	int label = 0;
	while (true) {
		auto p = prog.find(label);
		if (p == prog.end()) {
			break;
		}
		label = p->second->exec(state);
	}
	if (Value* ret = state.find("ret")) {
		cout << ret->dump() << endl;
	} else {
		throw RuntimeError("any program must return non-null value");
	}
}

}
