#include "Problem.h"

bool Problem::isSolved() const {

    // 1. There must be at least two legs
    if (combination.size() < 2) {
        return false;
    }
    
    // 2. The total cost is negative (we receive money upfront)
    double cost = getTotalCost();
    if (cost >= 0) {
        return false;
    }
    
    
    
    return true;
}

void Problem::addLeg(const OptionLeg& leg) {
    combination.push_back(leg);
}

void Problem::removeLeg() {

    if (!combination.empty()) {
        combination.pop_back();
    }
}

double Problem::getTotalCost() const {

    double total_cost = 0.0;
    
    for (const auto& leg : combination) {
        total_cost += leg.getCost();
    }
    
    return total_cost;
}

set<double> Problem::getCriticalPrices() const {

    set<double> criticalPrices;
    
    for (const auto& leg : combination) {
        criticalPrices.insert(leg.option->strike);
    }
    
    return criticalPrices;
}







