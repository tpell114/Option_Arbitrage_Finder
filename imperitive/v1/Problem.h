#ifndef PROBLEM_H
#define PROBLEM_H

#include "OptionChain.h"
#include <vector>
#include <set>
#include <algorithm>
#include <iostream>

using namespace std;

enum class PositionType {Long, Short};

struct OptionLeg {

    const Option* option;
    PositionType position;
    
    OptionLeg(const Option* option, PositionType position) 
        : option(option), position(position) {}
    
    double getCost() const {
        return (position == PositionType::Long) ? option->ask : -option->bid;
    }
};

inline bool operator<(const OptionLeg& a, const OptionLeg& b) {
    // Compare by Option pointer address first
    if (a.option != b.option){
        return a.option < b.option;
    }
    return a.position < b.position;
}

class Problem {

private:

    vector<OptionLeg> combination;
    set<vector<OptionLeg>> allSolutions;
    OptionChain& market_data;

    set<double> getCriticalPrices() const;
    double calculatePayoffAt(double price) const;
    vector<OptionLeg> getPossibleMoves() const;

public:

    Problem(OptionChain& market_data) 
        : market_data(market_data) {}
        
    void addLeg(const OptionLeg& leg);
    void removeLeg();
    bool isSolved() const;
    void solve();
    double getTotalCost() const;
    void findAllSolutions();
    void printAllSolutions() const;
    void clearSolutions();
    void clearCombination();

};

#endif
