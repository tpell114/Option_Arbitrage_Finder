#include "BruteForce.h"
#include <iostream>
#include <algorithm>

using namespace std;

vector<OptionLeg> BruteForce::getAllPossibleLegs() const {
    vector<OptionLeg> allLegs;
    
    for (const auto& [strike, options] : market_data.strike_map) {
        // Add call options
        if (options.first.has_value()) {
            const Option* call_ptr = &options.first.value();
            allLegs.push_back(OptionLeg(call_ptr, PositionType::Long));
            allLegs.push_back(OptionLeg(call_ptr, PositionType::Short));
        }
        
        // Add put options
        if (options.second.has_value()) {
            const Option* put_ptr = &options.second.value();
            allLegs.push_back(OptionLeg(put_ptr, PositionType::Long));
            allLegs.push_back(OptionLeg(put_ptr, PositionType::Short));
        }
    }
    
    return allLegs;
}

double BruteForce::getTotalCost(const vector<OptionLeg>& combination) const {

    double total_cost = 0.0;
    
    for (const auto& leg : combination) {
        total_cost += leg.getCost();
    }
    
    return total_cost;
}

set<double> BruteForce::getCriticalPrices(const vector<OptionLeg>& combination) const {

    set<double> criticalPrices;
    
    for (const auto& leg : combination) {
        criticalPrices.insert(leg.option->strike);
    }

    criticalPrices.insert(10000.0);
    criticalPrices.insert(0.0);
    
    return criticalPrices;
}

double BruteForce::calculatePayoffAt(const vector<OptionLeg>& combination, double price) const {

    double payoff = -getTotalCost(combination);
    
    for (const auto& leg : combination) {
        if (leg.option->type == OptionType::Call) {
            double intrinsic = max(0.0, price - leg.option->strike);
            payoff += (leg.position == PositionType::Long) ? intrinsic : -intrinsic;
        } else {
            double intrinsic = max(0.0, leg.option->strike - price);
            payoff += (leg.position == PositionType::Long) ? intrinsic : -intrinsic;
        }
    }
    
    return payoff;
}

bool BruteForce::isArbitrage(const vector<OptionLeg>& combination) const {

    if (combination.size() < 2) {
        return false;
    }
    
    // Check payoff at each critical price
    set<double> criticalPrices = getCriticalPrices(combination);
    for (const auto& price : criticalPrices) {
        double payoff = calculatePayoffAt(combination, price);
        
        if (payoff < 0.01) {
            return false;
        }
    }
    
    return true;
}

void BruteForce::findAllArbitrageOpportunities() {

    clearSolutions();
    vector<OptionLeg> allLegs = getAllPossibleLegs();
    
    for (size_t size = 2; size <= 4; size++) {

        // Initialize indices vector used to generate combinations
        vector<size_t> indices(size);
        for (size_t i = 0; i < size; i++) {
            indices[i] = i;
        }
        
        bool done = false;
        while (!done) {

            // Create possible combination
            vector<OptionLeg> combination;
            for (size_t idx : indices) {
                combination.push_back(allLegs[idx]);
            }
            
            // Check if there is an arbitrage opportunity, and sort before inserting
            if (isArbitrage(combination)) {
                sort(combination.begin(), combination.end());
                allSolutions.insert(combination);
            }
            
            // Generate next combination
            size_t i = size;
            done = true;
            while (i > 0) {
                i--;
                if (indices[i] < allLegs.size() - (size - i)) {
                    indices[i]++;
                    for (size_t j = i + 1; j < size; j++) {
                        indices[j] = indices[j-1] + 1;
                    }
                    done = false;
                    break;
                }
            }
        }
    }
}

void BruteForce::printAllSolutions() const {

    if (allSolutions.empty()) {
        cout << "\nNo arbitrage opportunities found in this option chain." << endl;
        return;
    }

    cout << "\nFound " << allSolutions.size() << " arbitrage opportunities using brute force!" << endl;

    int solutionIndex = 1;

    for (const auto& solution : allSolutions) {
        cout << "\nArbitrage Solution #" << solutionIndex++ << endl;
        cout << "--------------------------------" << endl;

        double totalCost = getTotalCost(solution);
        cout << "Net Credit: $" << -totalCost << endl;
        cout << "Legs:" << endl << endl;

        int legIndex = 1;
        for (const auto& leg : solution) {
            cout << legIndex++ << ". ";
            cout << (leg.position == PositionType::Long ? "Long " : "Short ");
            cout << (leg.option->type == OptionType::Call ? "Call" : "Put");
            cout << " Strike: $" << leg.option->strike;
            cout << " Price: $" << abs(leg.getCost()) << endl;
        }

        set<double> criticalPrices = getCriticalPrices(solution);

        cout << "\nPayoff at critical prices:" << endl;
        for (const auto& price : criticalPrices) {
            double payoff = calculatePayoffAt(solution, price);
            cout << "Price: $" << price << " -> Profit: $" << payoff << endl;
        }

        cout << "--------------------------------" << endl;
    }
}

void BruteForce::clearSolutions() {
    allSolutions.clear();
}
