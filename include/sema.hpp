#pragma once

#include <string>
#include <vector>
#include <map>
#include <memory>

#include "util.hpp"

namespace sfl {

using namespace std;

struct Value {
	virtual ~Value() { }
	virtual string dump() const = 0;
	virtual Value* clone() const = 0;
	virtual bool equal(const Value*) const = 0;
};

struct State {
	State() { }
	State(const State& s) {
		for (const auto& p : s.state) set(p.first, p.second->clone());
	}
	void set(const string& n, Value* v) {
		if (!find(n)) {
			state.emplace(n, v);
		} else {
			state[n].reset(v);
		}
	}
	Value* get(const string& n) const {
		if (Value* ret = find(n)) {
			return ret;
		} else {
			throw RuntimeError("unknown variable: " + n);
		}
	}
	bool equal(const State& s) const {
		if (state.size() != s.state.size()) return false;
		for (const auto& p : s.state) {
			auto i = state.find(p.first);
			if (i != state.end()) {
				if (!p.second->equal(i->second.get())) {
					return false;
				}
			} else {
				return false;
			}
		}
		return true;
	}
	string dump() const {
		string ret;
		for (const auto& p : state) {
			ret += p.first + " -> " + p.second->dump() + "\n";
		}
		return ret;
	}
private:
	Value* find(const string& n) const {
		auto i = state.find(n);
		if (i != state.end()) {
			return i->second.get();
		} else {
			return nullptr;
		}
	}
	map<string, unique_ptr<Value>> state;
};

struct IntValue : public Value {
	IntValue(int v = 0) : val(v) { }
	int val;
	string dump() const override { return to_string(val); }
	Value* clone() const override { return new IntValue(val); }
	bool equal(const Value* v) const override {
		if (const IntValue* iv = dynamic_cast<const IntValue*>(v)) {
			return val == iv->val;
		} else {
			return false;
		}
	}
};

struct ArrayValue : public Value {
	ArrayValue(vector<Value*> v = vector<Value*>()) {
		for (auto x : v) vect.emplace_back(x);
	}
	vector<unique_ptr<Value>> vect;
	string dump() const override {
		return "[" + dumpArray(vect) + "]";
	}
	Value* clone() const override {
		vector<Value*> arr;
		for (auto& x : vect) arr.push_back(x->clone());
		return new ArrayValue(arr);
	}
	bool equal(const Value* v) const override {
		if (const ArrayValue* av = dynamic_cast<const ArrayValue*>(v)) {
			if (av->vect.size() != vect.size()) return false;
			for (int i = 0; i < vect.size(); ++ i) {
				if (!vect[i]->equal(av->vect[i].get())) {
					return false;
				}
			}
			return true;
		} else {
			return false;
		}
	}
};

struct Lambda;

struct FuncValue : public Value {
	FuncValue(const Lambda* l, const State& state = State());
	const Lambda* lambda;
	State closure;
	string dump() const override;
	Value* clone() const override {
		return new FuncValue(lambda, closure);
	}
	Value* call(const vector<unique_ptr<Value>>& args) const;
	bool equal(const Value* v) const override;
};

struct FuncRef : public Value {
	FuncRef(FuncValue* r) : ref(r) { }
	string dump() const override { return ref->dump(); }
	Value* clone() const override { return new FuncRef(ref); }
	Value* call(const vector<unique_ptr<Value>>& args) const {
		return ref->call(args);
	}
	bool equal(const Value* v) const override {
		if (const FuncValue* fv = dynamic_cast<const FuncValue*>(v)) {
			return func()->equal(v);
		} else if (const FuncRef* fr = dynamic_cast<const FuncRef*>(v)) {
			return func()->equal(fr->func());
		} else {
			return false;
		}
	}
	FuncValue* func() { return ref; }
	const FuncValue* func() const { return ref; }
private:
	FuncValue* ref;
};

}


