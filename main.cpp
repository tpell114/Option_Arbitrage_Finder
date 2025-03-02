#include "OptionChain.h"
#include "Problem.h"
#include <iostream>

int main() {
    // Load option chain from file
    OptionChain chain;
    chain.load_from_file("data.txt");
    
    // Print the loaded option chain
    std::cout << "Initial option chain:" << std::endl;
    chain.print_chain();
    
    // Create problem instance
    Problem arbitrageFinder(chain);
    
    // Run the search
    std::cout << "\nSearching for arbitrage opportunities..." << std::endl;
    bool found = arbitrageFinder.solve();
    
    // Check results
    if (found) {
        arbitrageFinder.printSolution();
    } else {
        std::cout << "\nNo arbitrage opportunities found in this option chain." << std::endl;
    }
    
    return 0;
}