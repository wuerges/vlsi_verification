#include <boost/spirit/include/qi_lexeme.hpp>
#include <boost/spirit/include/qi_lit.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/lambda/lambda.hpp>
#include "test_harness.hpp"
#include <string>
#include <vector>
using namespace std;
using namespace boost::lambda;

using boost::spirit::qi::lexeme;
using boost::spirit::qi::rule;
using boost::spirit::qi::space;
using boost::spirit::qi::lit;
using boost::spirit::ascii::digit;
using boost::spirit::ascii::char_;

namespace qi = boost::spirit::qi;

int main() {
    string s;
    vector<string> vs;
     r = lexeme[ -char_("+-") >> +digit ];
    rule<const char*> r2 = +(r[ std::cout << "!" << _1 << "\n"] >> -space);
    // int i;
    // static_assert(decltype(i)::dummy_error, "DUMP MY TYPE" );
    //
    r.name("r");
    r2.name("r2");
    debug(r);
    debug(r2);
    test_phrase_parser_attr("12345125125", r2, s);
    //for (string v : vs) {
    //    cout << "<" << v << ">"<< endl;
    //}
}

