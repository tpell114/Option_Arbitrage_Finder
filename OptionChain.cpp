#include "OptionChain.h"

#include <fstream>
#include <sstream>
#include <iostream>
#include <iomanip>

using namespace std;

void OptionChain::add_option(OptionType type, double strike, double bid, double ask) {

    Option option(type, strike, bid, ask);

    if (strike_map.find(strike) == strike_map.end()) {
        strike_map[strike] = make_pair(nullopt, nullopt);
    }
    
    if (type == OptionType::Call) {
        strike_map[strike].first = option;
    } else {
        strike_map[strike].second = option;
    }

}

void OptionChain::load_from_file(const string& filename) {

    ifstream file(filename);

    if (!file.is_open()) {
        cerr << "Error: Could not open file " << filename << endl;
        return;
    }

    string line;
    getline(file, line);

    while (getline(file, line)) {

        stringstream ss(line);
        string type_str, strike_str, bid_str, ask_str;

        getline(ss, type_str, ',');
        getline(ss, strike_str, ',');
        getline(ss, bid_str, ',');
        getline(ss, ask_str, ',');

        OptionType type;
        if (type_str == "call") {
            type = OptionType::Call;
        } else {
            type = OptionType::Put;
        }

        double strike = stod(strike_str);
        double bid = stod(bid_str);
        double ask = stod(ask_str);

        this->add_option(type, strike, bid, ask);
    }

    file.close();
}

void OptionChain::print_chain() const {

    cout << "Option Chain:" << endl;
    cout << "--------------------------------------------------" << endl;
    cout << "Call Bid | Call Ask | Strike | Put Bid  | Put Ask" << endl;
    cout << "--------------------------------------------------" << endl;
    
    for (const auto& [strike, options] : strike_map) {
        const auto& [call_option, put_option] = options;
        
        cout << fixed << setprecision(2);
        
        // Print call information
        if (call_option.has_value()) {
            cout << setw(8) << call_option->bid << " | ";
            cout << setw(8) << call_option->ask << " | ";
        } else {
            cout << setw(8) << "N/A" << " | ";
            cout << setw(8) << "N/A" << " | ";
        }
        
        // Print strike price
        cout << setw(6) << strike << " | ";
        
        // Print put information
        if (put_option.has_value()) {
            cout << setw(8) << put_option->bid << " | ";
            cout << setw(8) << put_option->ask;
        } else {
            cout << setw(8) << "N/A" << " | ";
            cout << setw(8) << "N/A";
        }
        
        cout << endl;
    }
    cout << "--------------------------------------------------" << endl;
}
