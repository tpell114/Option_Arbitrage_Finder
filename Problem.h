#ifndef PROBLEM_H
#define PROBLEM_H

#include "OptionChain.h"
#include <vector>

using namespace std;

enum class PositionType {Long, Short};

struct OptionLeg {

    Option* option;
    PositionType position;
    
    OptionLeg(Option* option, PositionType position) 
        : option(option), position(position) {}
    
    double getCost() const {
        return (position == PositionType::Long) ? option->ask : -option->bid;
    }
};

class Problem {

public:

    Problem(OptionChain& market_data) 
        : market_data(market_data) {}
        

    void addLeg(const OptionLeg& leg);
    void removeLeg();
    //bool isSolved() const;
    //vector<OptionLeg> getPossibleMoves() const;
    //bool solve();
    double getTotalCost() const;


private:

    vector<OptionLeg> combination;
    OptionChain& market_data;


    



};













#endif