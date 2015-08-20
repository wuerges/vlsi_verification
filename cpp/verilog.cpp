#include "verilog.hpp"
#include <string>
#include <vector>
#include <map>
using namespace std;

namespace client { namespace verilog {

    /*
     * Implements indexing of wires through an int
     */
    int WireMap::GetWire(string w) {
        auto it = wire_to_int.find(w);
        if (it != wire_to_int.end()) {
            return it->second;
        }
        int i = ++seed;
        int_to_wire[i] = w;
        wire_to_int[w] = i;
        return i;
    }
}}

