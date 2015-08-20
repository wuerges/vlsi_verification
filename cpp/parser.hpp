#if !defined(VERILOG_PARSER_HPP)
#define VERILOG_PARSER_HPP

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_istream_iterator.hpp>
#include <boost/lambda/lambda.hpp>
#include <string>
#include <vector>
#include "skipper.hpp"
#include "verilog.hpp"

namespace client { namespace parser
{

    namespace qi = boost::spirit::qi;
    namespace spirit = boost::spirit;
    //typedef client::parser::skipper<spirit::istream_iterator> 

    using spirit::ascii::char_;
    using spirit::ascii::string;
    using namespace boost::lambda;
    using namespace std;



    //////////////////////////
    //  The module grammar  //
    //////////////////////////
    template <typename Iterator, typename Skipper = client::parser::skipper<Iterator> >
    struct G : qi::grammar<Iterator, Skipper>
    {
        typedef qi::rule<Iterator> rule_noskip;
        typedef qi::rule<Iterator, Skipper> rule;

        qi::rule<Iterator, Skipper, std::string> identifier, value;
        qi::rule<Iterator, Skipper, std::vector<std::string> > identifier_list;

        rule //value,
            module, module_decl, module_end,
            input_decl, output_decl,
            wire_decl, port_decl, 
            port_name;

        G() : G::base_type(module)
        {
            identifier.name("identifier"); 
            value.name("value"); 
            identifier_list.name("id_list");
            module.name("module"); 
            module_decl.name("module_decl"); 
            module_end.name("module_end");
            input_decl.name("input_decl"); 
            output_decl.name("output_decl");
            wire_decl.name("wire_decl"); 
            port_decl.name("port_decl"); 
            port_name.name("port_name");

            identifier_list = 
                value  >> *("," >> value ) ;

            value = string("1'b0") | identifier ; // [ std::cout << _1 << "\n" ]

            module_decl = 
                string("module")
                >> identifier 
                >> "(" >> identifier_list >> ")" >> ";" ;
            module = module_decl 
                  >> input_decl 
                  >> output_decl 
                  >> +(wire_decl | port_decl)
                  >> module_end; 


            input_decl  = "input"  >> identifier_list  >> ";" ;
            output_decl = "output" >> identifier_list >> ";" ;
            wire_decl   = "wire"   >> identifier_list >> ";" ;

            port_decl   = port_name 
                >> "(" >> identifier_list >> ")" >>  ";" ;
            module_end = "endmodule" ;

            port_name = string("and")
                      | "or" 
                      | "buf" 
                      | "xor" 
                      | "xnor" 
                      | "not" 
                      | "nor" 
                      | "nand" ;

            identifier = qi::lexeme[ char_("a-zA-Z") >> *char_("a-zA-Z0-9_") ];

            debug(module) ;
            debug(input_decl) ;
            debug(output_decl) ;
            debug(module_decl) ;
            debug(wire_decl);
            debug(port_decl);
            debug(module_end);
            debug(identifier_list);
            debug(identifier_list);
            debug(identifier);
            debug(value);
        }




    };
}}

#endif
