#pragma once

#include <string>
#include <variant>
#include <vector>
#include <memory>
#include <stack>
#include <numeric>
#include <set>

namespace sfl {

using namespace std;

struct CompileError : public exception {
	CompileError(const string& m, pair<int, int> lc = pair<int, int>(-1, -1)) :
	msg(m), line(lc.first), col(lc.second) { }
	string msg;
	string file;
	int line;
	int col;
	const char* what() const noexcept override {
		static string message;
		message = "Compilation error: " + msg;
		if (file.size() || line != -1 || col != -1) message += "\n";
		if (file.size()) message += "file: " + file + " ";
		if (line != -1) message += "line: " + to_string(line) + " ";
		if (col != -1) message += "col: " + to_string(col) + " ";
		return message.c_str();
	}
};

struct RuntimeError : public exception {
	RuntimeError(const string& m) : msg(m) {
	}
	string msg;
	const char* what() const noexcept override {
		static string message;
		message = "Runtime error: " + msg;
		return message.c_str();
	}
};

template<class T>
inline set<T> unite(const set<T>& s1, const set<T>& s2) {
	set<T> ret(s1);
	for (auto x : s2) ret.insert(x);
	return ret;
}

template<class T>
inline set<T> setminus(const set<T>& s1, const set<T>& s2) {
	set<T> ret(s1);
	for (auto x : s2) ret.erase(x);
	return ret;
}

inline string indent(const string& str, string d = "\t") {
	if (!str.size()) return "";
	string indented(d);
	for (auto i = str.begin(); i != str.end(); ++ i) {
		if (*i == '\n' && i + 1 != str.end()) indented += "\n" + d;
		else indented += *i;
	}
	return indented;
}

template<class T>
inline string dumpArray(const vector<T>& arr, const string& delim = ", ") {
	string ret;
	if (arr.size()) {
		ret += arr[0]->dump();
		for (int i = 1; i < arr.size(); ++i) {
			ret += delim + arr[i]->dump();
		}
	}
	return ret;
}

}
