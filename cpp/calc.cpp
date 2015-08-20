#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_istream_iterator.hpp>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include "skipper.hpp"
#include "parser.hpp"
#include "verilog.hpp"

////////////////////////////////////////////////////////////////////////////
using namespace std;
namespace spirit = boost::spirit;
namespace qi = boost::spirit::qi;

int
main()
{
    vector<string> attr;
    client::verilog::Verilog verilog;
    client::parser::G<spirit::istream_iterator> grammar;
    client::parser::skipper<spirit::istream_iterator> skipper;
    // open file 
    cin.unsetf(std::ios::skipws);
    
    // wrap istream into iterator
    spirit::istream_iterator begin(cin);
    spirit::istream_iterator end;

    // use iterator to parse file data
    //parse_info<> info = parse(str.c_str(), calc, space_p);
    if (qi::phrase_parse(begin, end, grammar, skipper, verilog))
    {
        cout << "OK" << endl;
    }
    else {
        cout << "ERROR" << endl;
    }
    return 0;
}
