#include <cassert>

#include "sfl_ast.hpp"

namespace sfl {

enum TypeKind { INT, ARRAY, FUNC, UNDEF, FAIL };

struct TypeDescr {
	TypeDescr(TypeKind k, int a = 0) : kind(k), arity(a) { }
	bool operator == (const TypeDescr& td) const {
		return kind == td.kind && arity == td.arity;
	}
	bool operator != (const TypeDescr& td) const {
		return !operator == (td);
	}
	operator bool () const {
		return kind != FAIL;
	}
	TypeKind kind;
	int arity;
};

TypeDescr getDescr(const Type* t) {
	if (dynamic_cast<const UndefType*>(t)) {
		return TypeDescr(UNDEF);
	} else if (dynamic_cast<const IntType*>(t)) {
		return TypeDescr(INT);
	} if (dynamic_cast<const ArrayType*>(t)) {
		return TypeDescr(ARRAY, 1);
	} if (const FuncType* ft = dynamic_cast<const FuncType*>(t)) {
		return TypeDescr(UNDEF, ft->args.size());
	} else {
		return TypeDescr(FAIL);
	}
}

TypeDescr merge(TypeDescr td1, TypeDescr td2) {
	if (td1.kind == UNDEF) return td2;
	else if (td2.kind == UNDEF) return td1;
	else {
		if (td1 != td2) return TypeDescr(FAIL);
		else return td1;
	}
}

Type* unify(const vector<const Type*>& types) {
	if (!types.size()) return nullptr;
	TypeDescr td = getDescr(types.front());
	for (auto t : types) {
		td = merge(td, getDescr(t));
		if (!td) return nullptr;
	}
	switch (td.kind) {
	case UNDEF: return new UndefType();
	case INT:   return new IntType();
	case ARRAY: {
		vector<const Type*> elementTypes;
		for (auto t : types) {
			if (const ArrayType* arrType = dynamic_cast<const ArrayType*>(t)) {
				elementTypes.push_back(arrType->type.get());
			}
		}
		if (Type* unifiedElementType = unify(elementTypes)) {
			return new ArrayType(unifiedElementType);
		} else {
			return nullptr;
		}
	}
	case FUNC: {
		vector<const Type*> valueTypes;
		vector<vector<const Type*>> argsTypes(td.arity);
		for (auto t : types) {
			if (const FuncType* funcType = dynamic_cast<const FuncType*>(t)) {
				valueTypes.push_back(funcType->val.get());
				for (int i = 0; i < td.arity; ++ i) {
					argsTypes[i].push_back(funcType->args[i].get());
				}
			}
		}
		if (Type* unifiedValueType = unify(valueTypes)) {
			vector<Type*> unifiedArgTypes;
			for (int i = 0; i < td.arity; ++ i) {
				if (Type* unifiedArg = unify(argsTypes[i])) {
					unifiedArgTypes.push_back(unifiedArg);
				} else {
					for (auto t : unifiedArgTypes) delete t;
					return nullptr;
				}
			}
			return new FuncType(unifiedValueType, unifiedArgTypes);
		} else {
			return nullptr;
		}
	}
	default: throw CompileError("impossible switch case");
	}
}

struct VarInfo {
	vector<VarDecl*>   decls;
	vector<Assign*>    assigns;
	vector<VarAccess*> accesses;
};

struct TypingEnv {
	void pushSubenv(TypingEnv* env) {
		env->parent = this;
		subenvs.emplace_back(env);
	}
	VarInfo& findInfo(const string& name) {
		TypingEnv* env = this;
		while (env) {
			auto i = env->varInfos.find(name);
			if (i != env->varInfos.end()) {
				return (*i).second;
			} else {
				env = env->parent;
			}
		}
		// New variable
		return varInfos[name];
	}

private:
	TypingEnv* parent = nullptr;
	map<string, VarInfo> varInfos;
	vector<unique_ptr<TypingEnv>> subenvs;
};

TypingEnv* gatherTypingInfo(Lambda* lambda);

void gatherTypingInfo(AstNode* node, TypingEnv& env) {
	if (BinOp* binOp = dynamic_cast<BinOp*>(node)) {
		gatherTypingInfo(binOp->lhs.get(), env);
		gatherTypingInfo(binOp->rhs.get(), env);
	} else if (UnOp* unOp = dynamic_cast<UnOp*>(node)) {
		gatherTypingInfo(unOp->expr.get(), env);
	} else if (ArrayMake* arr = dynamic_cast<ArrayMake*>(node)) {
		for (auto& el : arr->arr) {
			gatherTypingInfo(el.get(), env);
		}
	} else if (ArrayLen* arr = dynamic_cast<ArrayLen*>(node)) {
		gatherTypingInfo(arr->arr.get(), env);
	}else if (FunCall* call = dynamic_cast<FunCall*>(node)) {
		gatherTypingInfo(call->fun.get(), env);
		for (auto& arg : call->args) {
			gatherTypingInfo(arg.get(), env);
		}
	} else if (VarAccess* var = dynamic_cast<VarAccess*>(node)) {
		env.findInfo(var->name).accesses.push_back(var);
	} else if (Lambda* lambda = dynamic_cast<Lambda*>(node)) {
		env.pushSubenv(gatherTypingInfo(lambda));
	} else if (Cond* cond = dynamic_cast<Cond*>(node)) {
		gatherTypingInfo(cond->lhs.get(), env);
		gatherTypingInfo(cond->rhs.get(), env);
	} else if (While* while_ = dynamic_cast<While*>(node)) {
		gatherTypingInfo(while_->cond.get(), env);
		gatherTypingInfo(while_->body.get(), env);
	} else if (If* if_ = dynamic_cast<If*>(node)) {
		gatherTypingInfo(if_->cond.get(), env);
		gatherTypingInfo(if_->pos.get(), env);
		gatherTypingInfo(if_->neg.get(), env);
	} else if (Assign* assign = dynamic_cast<Assign*>(node)) {
		gatherTypingInfo(assign->expr.get(), env);
		VarInfo& info = env.findInfo(var->name);
		info.assigns.push_back(assign);
		info.decls.push_back(assign->decl.get());
	} else if (Seq* seq = dynamic_cast<Seq*>(node)) {
		for (auto& s : seq->seq) {
			gatherTypingInfo(s.get(), env);
		}
	} else if (StatExpr* expr = dynamic_cast<StatExpr*>(node)) {
		gatherTypingInfo(expr->expr.get(), env);
	} else if (Print* print = dynamic_cast<Print*>(node)) {
		gatherTypingInfo(print->expr.get(), env);
	}
}

TypingEnv* gatherTypingInfo(Lambda* lambda) {
	TypingEnv* env = new TypingEnv;
	for (auto& decl : lambda->args.decls) {
		env->findInfo(decl->name).decls.push_back(decl.get());
	}
	gatherTypingInfo(lambda->body.get(), *env);
	return env;
}

vector<const Type*> makeConstraint(const VarAccess* var) {
	vector<const Type*> ret;
	if (const ArrayMake* arr = dynamic_cast<const ArrayMake*>(var->parent)) {
		for (auto& el : arr->arr) {
			ret.push_back(el->type());
		}
	} else if (const FunCall* call = dynamic_cast<const FunCall*>(var->parent)) {
		const FuncType* funType = dynamic_cast<const FuncType*>(call->type());
		if (call->fun.get() == var) {
			if (funType) {
				ret.push_back(funType->val.get());
			}
			ret.push_back(call->fun->type());
		} else {
			for (int i = 0; i < call->args.size(); ++ i) {
				if (call->args[i].get() == var) {
					if (funType) {
						ret.push_back(funType->args[i].get());
					}
					ret.push_back(call->args[i]->type());
					break;
				}
			}
		}
	}
	return ret;
}

vector<const Type*> typeConstraints(const VarInfo& info) {
	vector<const Type*> ret;
	for (auto& decl : info.decls) {
		ret.push_back(decl->type());
	}
	for (auto assign : info.assigns) {
		ret.push_back(assign->expr->type());
	}
	for (auto access : info.accesses) {
		for (auto c : makeConstraint(access)) {
			ret.push_back(c);
		}
	}
	return ret;
}

Type* inferType(const VarInfo& info) {
	return unify(typeConstraints(info));
}


}




