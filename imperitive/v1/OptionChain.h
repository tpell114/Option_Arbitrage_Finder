#ifndef OPTION_CHAIN_H
#define OPTION_CHAIN_H

#include <string>
#include <map>
#include <optional>
#include <fstream>
#include <sstream>
#include <iostream>
#include <iomanip>

using namespace std;

enum class OptionType {Call, Put};

struct Option {
    
    OptionType type;
    double strike;
    double bid;
    double ask;

    Option(OptionType option_type, double strike, double bid, double ask)
        : type(option_type), strike(strike), bid(bid), ask(ask) {}
        
};

class OptionChain {

public:

    map<double, pair<optional<Option>, optional<Option>>> strike_map; // strike -> {call, put}

    void load_from_file(const string& filename);
    void print_chain() const;

private:
    
    void add_option(OptionType type, double strike, double bid, double ask);

};


#endif
