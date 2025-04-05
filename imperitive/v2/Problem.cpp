#include "Problem.h"
#include <iostream>
#include <limits>

bool Problem::isSolved() const {

    if (combination.size() < 2) {
        return false;
    }

    set<double> criticalPrices = getCriticalPrices();
    
    for (const auto& price : criticalPrices) {
        double payoff = calculatePayoffAt(price);

        if (payoff < 0.01) {
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
            
            const Option* call_ptr = &options.first.value();

            // Skip adding short position for options with zero bid
            if (call_ptr->bid <= 0.00) {
                possibleMoves.push_back(OptionLeg(call_ptr, PositionType::Long));
            } else {

                bool hasLeg = false;

                for (const auto& leg : combination) {

                    if (leg.option == call_ptr) {
                        hasLeg = true;
                        break;
                    }
                }

                if(!hasLeg) {
                    possibleMoves.push_back(OptionLeg(call_ptr, PositionType::Long));
                    possibleMoves.push_back(OptionLeg(call_ptr, PositionType::Short));
                }
            }
        }
        
        // puts
        if (options.second.has_value()) {
        
            const Option* put_ptr = &options.second.value();

            // Skip adding short position for options with zero bid
            if (put_ptr->bid <= 0.0) {
                possibleMoves.push_back(OptionLeg(put_ptr, PositionType::Long));
            } else {

                bool hasLeg = false;

                for (const auto& leg : combination) {

                    if (leg.option == put_ptr) {
                        hasLeg = true;
                        break;
                    }
                }

                if(!hasLeg) {
                    possibleMoves.push_back(OptionLeg(put_ptr, PositionType::Long));
                    possibleMoves.push_back(OptionLeg(put_ptr, PositionType::Short));
                }
            }
        }
    }
    
    return possibleMoves;
}



void Problem::solve() {
    
    if (isSolved()) {
        vector<OptionLeg> sortedCombo = combination;
        sort(sortedCombo.begin(), sortedCombo.end());
        allSolutions.insert(sortedCombo);
        return;
    }
    
    
    vector<OptionLeg> possibleMoves = getPossibleMoves();
    
    for (const auto& move : possibleMoves) {
        
        addLeg(move);
        
        solve();  // Continue searching for more solutions
        
        removeLeg();
    }
}

void Problem::findAllSolutions() {
    clearSolutions();
    solve();
}

void Problem::clearSolutions() {
    allSolutions.clear();
}

void Problem::printAllSolutions() const {
    if (allSolutions.empty()) {
        cout << "\nNo arbitrage opportunities found in this option chain." << endl;
        return;
    }

    cout << "\nFound " << allSolutions.size() << " arbitrage opportunities!" << endl;

    int solutionIndex = 1;

    for (const auto& solution : allSolutions) {
        cout << "\nArbitrage Solution #" << solutionIndex++ << endl;
        cout << "--------------------------------" << endl;

        double totalCost = 0.0;
        for (const auto& leg : solution) {
            totalCost += leg.getCost();
        }

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

        // Calculate and print payoff
        set<double> criticalPrices;
        for (const auto& leg : solution) {
            criticalPrices.insert(leg.option->strike);
        }
        criticalPrices.insert(0.0);
        criticalPrices.insert(10000.0);

        cout << "\nPayoff at critical prices:" << endl;
        for (const auto& price : criticalPrices) {
            double payoff = -totalCost;

            for (const auto& leg : solution) {
                if (leg.option->type == OptionType::Call) {
                    double intrinsic = max(0.0, price - leg.option->strike);
                    payoff += (leg.position == PositionType::Long) ? intrinsic : -intrinsic;
                } else {
                    double intrinsic = max(0.0, leg.option->strike - price);
                    payoff += (leg.position == PositionType::Long) ? intrinsic : -intrinsic;
                }
            }

            cout << "Price: $" << price << " -> Profit: $" << payoff << endl;
        }

        cout << "--------------------------------" << endl;
    }
}


