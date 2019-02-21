#include <iostream>
#include <fstream>
#include <iterator>
#include "ast.hpp"

using namespace std;
using namespace sfl;

void print_usage() {
	cout << "Usage: sfl [--show-ast] <program> <int arg 1> ... <int arg n>" << endl;
	exit(0);
}

int main(int argc, char **argv) {
	if (argc <= 1) print_usage();
	int c = 1;
	bool show_ast = false;
	if ("--show-ast" == string(argv[c])) { ++c; show_ast = true; }
	if (c == argc) print_usage();
	string file(argv[c++]);
	ifstream in(file);
	if (!in) {
		cout << "cannot read " << file << endl;
	}
	in.unsetf(std::ios::skipws);
	string src;
	copy(
		istream_iterator<char>(in),
		istream_iterator<char>(),
		back_inserter(src));
	in.close();
	try {
		unique_ptr<Prog> prog(parse(file, src));
		if (show_ast) {
			cout << prog->dump() << endl;
			cout << "-----------" << endl;
		}
		vector<int> input;
		for (; c < argc; ++ c) {
			input.push_back(stoi(argv[c]));
		}
		prog->run(input);
	} catch (exception& err) {
		cout << err.what() << endl;
	} catch (...) {
		cout << "error occurred" << endl;
	}
	return 0;
}
