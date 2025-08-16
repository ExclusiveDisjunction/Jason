#include "Expressions.h"

#include <sstream>
#include <cassert>

using std::string;
using std::vector;
using std::stringstream;
using std::queue;
using std::stack;

bool IsBalancedString(const std::string& obj)
{
	/*
		This function will, for each character in the string, push the character to the stack if it is an opening brace, and pop it if it is a closing.
		When popping, if the brace that gets popped is not the opening pair for the current brace, then it returns false.
		If the stack is not empty when the string is exhausted, then it returns false.
	*/

	stack<Brace> st;
	for (char c : obj) {
		try {
			Brace target = Brace::resolve(c);
			if (target.opening) {
				st.push(target);
			}
			else {
				if (st.empty())
					return false;
				Brace top = st.top();
				st.pop();

				if (top.inverse() != target) 
					return false; //The opening brace was not matched
			}
		}
		catch (...) { continue; }
	}

	return st.empty();
}
bool IsNumericString(const std::string& obj)
{
	//These states are used to determine if the string is numerical, minus the whitespace at the front and end. 
	bool lastWasNumber = false, lastWasSpace = false;
	for (char item : obj)
	{
		if (isdigit(item)) //if it is currently a digit, but the last was a space, then we know that it is not a proper numerical format. Otherwise keep looking.
		{
			if (lastWasSpace)
				return false;
			else
				lastWasNumber = true;
		}
		else if (isspace(item)) //If the currnet is a space, and the last was a space, then we keep looking. Otherwise, we set the state.
		{
			if (lastWasSpace)
				continue;
			else if (lastWasNumber)
				lastWasSpace = true;
		}
		else //If its not a number or a space, then we know its not numerical. 
			return false;
	}

	return true;
}

class ParsingError : public std::logic_error {
	public:
		ParsingError(const std::string& what) : std::logic_error(what) { }
};
class UnbalancedStringError : public ParsingError {
	public:
		UnbalancedStringError(const std::string& expression) : ParsingError("the expression '" + expression + "' does not have a balanced number of braces") { }
};

ParsedExpression parse(const std::string& infix) {
	if (!IsBalancedString(infix)) 
		throw UnbalancedStringError(infix);

	ParsedExpression result;
	std::string prev_num;
	bool first = true, last_oper = false, last_num = false;

	for (const char& item : infix) {
		if (isdigit(item)) {
			if (!first && !last_oper && !last_num) {

			}
		}
	}
}

PostfixExpression to_postfix(ParsedExpression& input) {

}

void print_postfix(const PostfixExpression& s) {

}

double evaluatePostfix(PostfixExpression& postfix) {

}

/*
std::string to_postfix(const std::string& infix)
{
	//Check One: Determine if it is balanced
	if (!IsBalancedString(infix))
		throw FormatError(infix, "There is not a balance of braces.");

	//Return is the return string, prevNum is used to store numbers as they apear.
	std::string Return, prevNum;
	//These states are used to validate the string being inputted, and tells the loop to do different actions.
	bool PrevWasOper = false,
		PrevWasNum = false,
		FirstChar = true;
	stack<char> opers; //Used to store the operators. 
	for (const char& item : infix)
	{
		if (isdigit(item)) //Append to the prevNum string.
		{
			if (!FirstChar && !PrevWasOper && !PrevWasNum)
				throw FormatError(infix, "An operator or brace was expected, but a number was given.");

			prevNum += item;
			PrevWasNum = true;
			PrevWasOper = false;
			FirstChar = false;
		}
		else if (isalpha(item)) //Invalid character.
			throw FormatError(infix, "Alphabetical characters are not allowed.");
		else if (item == '(' || item == '{' || item == '[') //opening brace
		{
			if (prevNum.size()) //Flush prevNum
			{
				Return += prevNum + " ";
				prevNum.clear();
			}

			//Set state
			PrevWasOper = true;
			PrevWasNum = false;
			FirstChar = false;

			opers.push(item);
		}
		else if (item == ')' || item == '}' || item == ']') //closing brace
		{
			if (prevNum.size()) //Flush prevNum
			{
				Return += prevNum + " ";
				prevNum.clear();
			}

			//Set state
			PrevWasOper = false;
			PrevWasNum = false;
			FirstChar = false;

			//We need to clear out the operators in the stack until we get to the opening brace.
			char CurrOper = opers.top();
			char MatchOpen = item == ')' ? '(' : item == '}' ? '{' : item == ']' ? '[' : 0;
			while (!opers.empty() && CurrOper != MatchOpen)
			{
				Return += string(1, CurrOper) + ' ';
				opers.pop();
				CurrOper = opers.top();
			}

			if (opers.top() == MatchOpen)
				opers.pop(); //Removes brace
		}
		else if (item == ' ') //Space
		{
			if (prevNum.size()) //Flush prevNum
			{
				Return += prevNum + " ";
				prevNum.clear();
			}

			//Only sperates numbers, but carries the state of previous oper & firstChar
			PrevWasNum = false;
		}
		else if (IsOperator(item))
		{
			if (FirstChar || PrevWasOper)
				throw FormatError(infix, FirstChar ? "The first character cannot be an operator." : "The previous item was an operator, and a number was expected.");

			if (prevNum.size()) //Flush prevNum
			{
				Return += prevNum + " ";
				prevNum.clear();
			}

			if (!opers.empty()) 
			{
				char lastOper = opers.top();
				int p1 = GetOperPrecidence(lastOper), p2 = GetOperPrecidence(item);
				if (p1 == GetOperPrecidence('(')) //Sees if it is a brace, if so just push
					opers.push(item);
				else if (p1 == 0 || p2 == 0) //Error with GetOperPrecidence
					throw FormatError(infix, string("The operators (") + item + ", " + lastOper + ") could not be determined.");
				else //We need to loop through until we reach somehting with greater precidence. 
				{
					while (!opers.empty() && p1 != GetOperPrecidence('(') && (p2 < p1 || (p1 == p2 && item != '^')))
					{
						Return += string(1, lastOper) + ' ';
						opers.pop();
						if (!opers.empty()) //Update the current operator.
						{
							lastOper = opers.top();
							p1 = GetOperPrecidence(lastOper);
						}
					}
					opers.push(item);
				}       
			}
			else
				opers.push(item); //Nothing else to do but add it

			//Update state
			PrevWasOper = true;
			PrevWasNum = false;
			FirstChar = false;
		}
		else
			throw FormatError(infix, string("Invalid character: \'") + item + "\'");
	}

	if (prevNum.size()) //Flush prevNum
		Return += prevNum + " ";

	//Now that the string has been parsed, push all remaining operators.
	while (!opers.empty())
	{
		Return += opers.top();
		if (opers.size() != 1)
		 	Return += ' ';
		opers.pop();
	}

	if (Return[Return.size() - 1] == ' ')
		Return = Return.substr(0, Return.size() - 1); //Removes extra white space

	return Return;
}
void print_postfix(const std::string& s)
{
	std::cout << "Postfix Expression: " << s << std::endl;
}
double evaluatePostfix(const std::string& postfix)
{
	//Since all numbers & operators are seperated by spaces, a stringstream will be used for extraction.
	stringstream stream(postfix);
	if (!stream)
		return 0;

	//Checks simple case, if it is just a numerical literal, output that result.
	if (IsNumericString(postfix))
	{
		double Return;
		stream >> Return;
		return Return;
	}

	//Used to push all results.
	stack<double> Results;
	while (!stream.eof())
	{
		string curr;
		std::streamoff pos = stream.tellg(); //Represents the current position before the retrival. Used later if the string is numerical. 
		if (pos == postfix.size()) //end of stream
			break;

		stream >> curr;

		if (curr.size() == 1 && IsOperator(curr[0])) //operator
		{
			if (Results.size() < 2)
				throw FormatError(postfix, "The expression calls for an operator, but does not contain enough numbers.");

			double b = Results.top();
			Results.pop();
			double a = Results.top();
			Results.pop();

			Results.push(EvaluateOperator(curr[0], a, b));
		}			
		else if (IsNumericString(curr))
		{
			stream.seekg(pos); //Backtrack and use stringstream to extract the number. 
			int num;
			stream >> num;

			Results.push((double)num);
		}
		else
			throw FormatError(curr, "Invalid operator/numerical expression.");
	}

	if (Results.size() != 1) //Invalid operations.
		throw FormatError(postfix, "The expression does not have the correct number of operators to numbers.");

	return Results.top();
}
*/