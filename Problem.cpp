#include "Problem.h"
#include <iostream>

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
    
    // 3. check payoff at each critical price
    set<double> criticalPrices = getCriticalPrices();

    for (const auto& price : criticalPrices) {

        double payoff = calculatePayoffAt(price);

        if (payoff <= 0) {
            return false;
        }

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

void Problem::clearCombination() {
    combination.clear();
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

    criticalPrices.insert(10000.0);
    criticalPrices.insert(0.0);
    
    return criticalPrices;
}

double Problem::calculatePayoffAt(double price) const {

    double payoff = -getTotalCost();
    
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


vector<OptionLeg> Problem::getPossibleMoves() const {

    vector<OptionLeg> possibleMoves;

    if (combination.size() >= 4) {
        return possibleMoves; // Return empty vector
    }
    
    for (const auto& [strike, options] : market_data.strike_map) {
        // calls
        if (options.first.has_value()) {
            
            Option* call_ptr = const_cast<Option*>(&options.first.value());

            bool hasLeg = false;

            for (const auto& leg : combination) {

                if (leg.option == call_ptr) {
                    hasLeg = true;
                }
            }

            if(!hasLeg) {
                possibleMoves.push_back(OptionLeg(call_ptr, PositionType::Long));
                possibleMoves.push_back(OptionLeg(call_ptr, PositionType::Short));
            }
        }
        
        // puts
        if (options.second.has_value()) {
        
            Option* put_ptr = const_cast<Option*>(&options.second.value());

            bool hasLeg = false;

            for (const auto& leg : combination) {

                if (leg.option == put_ptr) {
                    hasLeg = true;
                }
            }

            if(!hasLeg) {
                possibleMoves.push_back(OptionLeg(put_ptr, PositionType::Long));
                possibleMoves.push_back(OptionLeg(put_ptr, PositionType::Short));
            }
        }
    }
    
    return possibleMoves;
}


bool Problem::solve() {
    
    if (isSolved()) return true;
    
    vector<OptionLeg> possibleMoves = getPossibleMoves();
    
    for (const auto& move : possibleMoves) {
        
        addLeg(move);
        
        if (solve()) return true;  // Solution found
        
        removeLeg();
    }

    return false;
}

void Problem::printSolution() const {
    
    cout << "\nArbitrage opportunity found!" << endl;
    cout << "--------------------------------" << endl;
    cout << "Net Credit: $" << -getTotalCost() << endl;
    cout << "Legs:" << endl << endl;
    
    for (size_t i = 0; i < combination.size(); i++) {
        const auto& leg = combination[i];
        cout << i+1 << ". ";
        cout << (leg.position == PositionType::Long ? "Long " : "Short ");
        cout << (leg.option->type == OptionType::Call ? "Call" : "Put");
        cout << " Strike: $" << leg.option->strike;
        cout << " Price: $" << abs(leg.getCost()) << endl;
    }
    
    cout << "\nPayoff at critical prices:" << endl;
    for (const auto& price : getCriticalPrices()) {
        cout << "Price: $" << price << " -> Profit: $" << calculatePayoffAt(price) << endl;
    }
    cout << "--------------------------------" << endl;
}




