#include "OptionChain.h"
#include "Problem.h"
#include <iostream>
#include <chrono>

int main() {
    
    OptionChain chain;
    chain.load_from_file("data3.txt");
    chain.print_chain();
    
    Problem arbitrageFinder(chain);
    
    std::cout << "\nSearching for arbitrage opportunities..." << std::endl;

    auto start_time = chrono::high_resolution_clock::now();
    bool found = arbitrageFinder.solve();
    auto end_time = chrono::high_resolution_clock::now();
    
    if (found) {
        arbitrageFinder.printSolution();
    } else {
        std::cout << "\nNo arbitrage opportunities found in this option chain." << std::endl;
    }

    auto duration = chrono::duration_cast<std::chrono::microseconds>(end_time - start_time);
    cout << "\nSearch completed in " << duration.count() << " microseconds." << std::endl;
    
    return 0;
}

