#include "Problem.h"

/**
 * Determines if the current problem setup is solved, indicating an arbitrage opportunity.
 * 
 * An arbitrage opportunity is considered solved if:
 * 1. There are at least two legs in the option combination.
 * 2. The payoff at each critical price point is greater than or equal to 0.01.
 * 
 * @return true if the problem is solved (i.e., an arbitrage opportunity is found), false otherwise.
 */
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

/**
 * Add a leg to the combination.
 * 
 * @param leg The leg to add.
 */
void Problem::addLeg(const OptionLeg& leg) {
    combination.push_back(leg);
}

/**
 * Remove the most recently added leg from the combination.
 */
void Problem::removeLeg() {

    if (!combination.empty()) {
        combination.pop_back();
    }
}

/**
 * Clear the combination of legs.
 */
void Problem::clearCombination() {
    combination.clear();
}

/**
 * Calculate the total cost of the combination.
 * 
 * The total cost is the sum of the individual costs of all legs in the combination.
 * 
 * @return The total cost of the combination.
 */
double Problem::getTotalCost() const {

    double total_cost = 0.0;
    
    for (const auto& leg : combination) {
        total_cost += leg.getCost();
    }
    
    return total_cost;
}

/**
 * Retrieve a set of critical prices for the current option combination.
 * 
 * Critical prices include the strike prices of all the options in the 
 * combination, as well as two predefined boundary values: 0.0 and 10000.0.
 * These prices are crucial for evaluating potential payoff scenarios.
 * 
 * @return A set of critical prices relevant to the option combination.
 */
set<double> Problem::getCriticalPrices() const {

    set<double> criticalPrices;
    
    for (const auto& leg : combination) {
        criticalPrices.insert(leg.option->strike);
    }

    criticalPrices.insert(10000.0);
    criticalPrices.insert(0.0);
    
    return criticalPrices;
}

/**
 * Calculate the total payoff of the combination at a specific price.
 * 
 * The payoff is calculated by summing the intrinsic values of all legs in the combination.
 * The intrinsic value of a call is max(0, price - strike) and the intrinsic value of a put is max(0, strike - price).
 * 
 * @param price The price at which to calculate the payoff.
 * 
 * @return The total payoff of the combination at the given price.
 */
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


/**
 * Get all possible legs that can be added to the combination.
 * 
 * The returned vector only includes options that are not already in the combination.
 * If the combination already has 4 legs, an empty vector is returned.
 * 
 * @return A vector of OptionLegs that can be added to the combination.
 */
vector<OptionLeg> Problem::getPossibleMoves() const {

    vector<OptionLeg> possibleMoves;

    if (combination.size() >= 4) {
        return possibleMoves; // Return empty vector
    }
    
    for (const auto& [strike, options] : market_data.strike_map) {
        // calls
        if (options.first.has_value()) {
            
            const Option* call_ptr = &options.first.value();

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
        
        // puts
        if (options.second.has_value()) {
        
            const Option* put_ptr = &options.second.value();

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
    
    return possibleMoves;
}

/**
 * Recursively solves the problem by exploring all possible combinations
 * of option legs. If a valid solution is found, it is added to the set
 * of all solutions.
 *
 * The function checks if the current combination of option legs forms
 * a valid and profitable solution. If so, it sorts the combination and
 * inserts it into the set of all solutions to avoid duplicates.
 *
 * The function then generates all possible moves (option legs that can
 * be added) and iterates over each move. For each move, it adds the leg
 * to the current combination, recursively calls itself to explore further
 * combinations, and then removes the leg to backtrack and explore other
 * possibilities.
 */
void Problem::solve() {
    
    if (isSolved()) {
        vector<OptionLeg> sortedCombo = combination;
        sort(sortedCombo.begin(), sortedCombo.end());
        allSolutions.insert(sortedCombo);
    }
    
    
    vector<OptionLeg> possibleMoves = getPossibleMoves();
    
    for (const auto& move : possibleMoves) {
        
        addLeg(move);
        
        solve();  // Continue searching for more solutions
        
        removeLeg();
    }
}

/**
 * Finds all possible arbitrage opportunities in the option chain and
 * stores them in the set of all solutions.
 *
 * This function clears the set of all solutions and then calls the
 * solve() function to start the recursive search for all
 * combinations of option legs that form valid and profitable
 * arbitrage opportunities.
 */
void Problem::findAllSolutions() {
    clearSolutions();
    solve();
}

/**
 * Clear the set of all solutions.
 */
void Problem::clearSolutions() {
    allSolutions.clear();
}

/**
 * Prints all the arbitrage opportunities found in the option chain.
 *
 * If no arbitrage opportunities are found, it prints a message indicating so.
 * Otherwise, it iterates through each solution in the set of all solutions,
 * displaying details such as the net credit, legs involved, and the payoff
 * at critical prices.
 *
 * The function calculates the total cost for each solution and prints the
 * net credit. It then lists each leg, indicating its position (long or short),
 * type (call or put), strike price, and cost.
 *
 * Finally, it computes and prints the payoff at various critical prices,
 * including the strikes of the options involved and hypothetical extreme prices.
 */
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


