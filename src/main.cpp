#include <iostream>
#include <fstream>
#include <iterator>
#include "ast.hpp"

using namespace std;
using namespace sfl;

int main(int argc, char **argv) {
	if (argc <= 1) {
		cout << "Usage: sfl <program>" << endl;
		return 0;
	}
	string file(argv[1]);
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
		cout << prog->dump() << endl;
		cout << "free vars: ";
		for (auto v : prog->freeVars()) {
			cout << v << ", ";
		}
		cout << endl;

		cout << "closure vars: ";
		for (auto v : prog->lambda->closureVars) {
			cout << v << ", ";
		}
		cout << endl;

		vector<int> input;
		for (int i = 2; i < argc; ++ i) {
			input.push_back(stoi(argv[i]));
		}
		prog->run(input);
		cout << "END" << endl;
	} catch (exception& err) {
		cout << err.what() << endl;
	} catch (...) {
		cout << "error occurred" << endl;
	}
	return 0;
}
