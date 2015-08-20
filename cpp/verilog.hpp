#if !defined(VERILOG_HPP)
#define VERILOG_HPP

#include <map>
#include <vector>
#include <string>
using namespace std;

namespace client { namespace verilog
{
    enum Function {
        AND,
        OR,
        XOR,
        XNOR,
        NOT,
        NOR,
        NAND
    };

    struct Port {
        vector<int> inputs;
        int output;
        Function function;
    };

    struct WireMap {
        int seed;
        map<int, string> int_to_wire;

        int GetWire(string w);
        WireMap();
    private:
        map<string, int> wire_to_int;
    };

    struct Verilog {
        // inputs
        // outputs
        // fios
        // portas
        vector<int> inputs;
        vector<int> outputs;
        vector<Port> ports;
        vector<int> wires;
    };


    typedef pair<int, bool> Edge;
    typedef int Node;

    struct Graph {
        map<Node, vector<Edge> > outs;
        map<Node, vector<Edge> > ins;
    };
}}

#endif
