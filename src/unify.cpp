#include <cassert>
#include "ast.hpp"

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

}


