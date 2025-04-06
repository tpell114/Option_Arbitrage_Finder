#ifndef BRUTEFORCE_H
#define BRUTEFORCE_H

#include "OptionChain.h"
#include "Problem.h"
#include <vector>
#include <set>

class BruteForce {
private:
    OptionChain& market_data;
    set<vector<OptionLeg>> allSolutions;
    
    // Helper method to get all possible option legs
    vector<OptionLeg> getAllPossibleLegs() const;
    
    // Helper method to check if a combination is an arbitrage opportunity
    bool isArbitrage(const vector<OptionLeg>& combination) const;
    
    // Helper methods for the arbitrage check
    double getTotalCost(const vector<OptionLeg>& combination) const;
    set<double> getCriticalPrices(const vector<OptionLeg>& combination) const;
    double calculatePayoffAt(const vector<OptionLeg>& combination, double price) const;

public:
    BruteForce(OptionChain& market_data) 
        : market_data(market_data) {}
    
    // Find all arbitrage opportunities using brute force approach
    void findAllArbitrageOpportunities();
    
    // Print all found solutions
    void printAllSolutions() const;
    
    // Clear all solutions
    void clearSolutions();
};

#endif