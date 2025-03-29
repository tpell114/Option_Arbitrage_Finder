#ifndef PROBLEM_H
#define PROBLEM_H

#include "OptionChain.h"
#include <vector>
#include <set>

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

private:

    vector<OptionLeg> combination;
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
    bool solve();
    double getTotalCost() const;
    void printSolution() const;
    void clearCombination();
    




    



};













#endif