#include "Expressions.h"
#include "Tests.h"
#include <iostream>
#include <iomanip>
#include <unistd.h>

using std::string;
using std::cout;
using std::cin;
using std::cerr;
using std::endl;
using std::getline;

int main(int argc, char* argv[])
{
	char* testName = nullptr;
	if (argc != 1) {
		int opt;
		while ((opt = getopt(argc, argv, "t:")) != -1) {
			switch (opt) {
				case 't':
					testName = optarg;
					break;
				case '?':
					cerr << "Usage:\n\tExpressions\n\tExpressions [-t test_name]" << endl;
					return 1;
			}
		}
	}

	if (testName != nullptr) {
		string true_name(testName);
		cout << "Testing mode activated. Attempting to fetch test " << true_name << endl;

		register_tests();

		auto result = tests.find(true_name);
		if (result == tests.end()) { 
			cerr << "The test with the name '" << true_name << "' could not be resolved to a test" << endl;
			return 2;
		}
		else {
			auto& fn = (*result).second;
			fn();
		}

		return 0;
	}

	/*
	while (true) //This will input a string (assumed to be infix), or the word "exit" and act accordingly. 
	{
		string Command;
		cout << "Please enter an expression in infix notation, or type \"exit\" to exit: ";
		getline(cin, Command);
		if (Command == "exit") //Quit the program for the user.
			break;

		try //In try because the to_postfix and evaluatePostfix functions will throw FormatError or std::logic_error if something goes wrong. 
		{
			ParsedExpression parsed = parse(Command);
			PostfixExpression postfix = to_postfix(parsed);
			print_postfix(postfix);
			double result = evaluatePostfix(postfix);

			cout << "Result: " << std::fixed << std::setprecision(3) << result << '\n';
		}
		catch (FormatError& e)
		{
			cerr << "Error: " << e.what() << endl;
		}
		catch (std::logic_error& e)
		{
			cerr << "Internal Error: " << e.what() << endl;
		}

		cout << endl;
	}
		*/

	return 0;
}