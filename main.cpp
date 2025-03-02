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


    chrono::microseconds total_time(0);
    int loops = 3;

    for (int i = 0; i < loops; i++) {

        auto start_time = chrono::high_resolution_clock::now();
        bool found = arbitrageFinder.solve();
        auto end_time = chrono::high_resolution_clock::now();

        if (found) {
            arbitrageFinder.printSolution();
        } else {
            std::cout << "\nNo arbitrage opportunities found in this option chain." << std::endl;
        }
    
        auto duration = chrono::duration_cast<std::chrono::microseconds>(end_time - start_time);
        total_time += duration;
        cout << "\nSearch completed in " << duration.count() << " microseconds." << std::endl;

        arbitrageFinder.clearCombination();
    }

    cout << "--------------------------------" << endl;
    cout << "\nAverage search time for " << loops << " iterations: " << total_time.count() / loops << " microseconds." << endl << endl;
    
    
    
    return 0;
}

