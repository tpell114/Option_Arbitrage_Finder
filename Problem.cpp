#include "Problem.h"

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









