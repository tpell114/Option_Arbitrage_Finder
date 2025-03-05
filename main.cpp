#include "OptionChain.h"
#include "Problem.h"
#include <iostream>
#include <chrono>

int main() {

    auto programStartTime = chrono::high_resolution_clock::now();
    
    OptionChain chain;
    chain.load_from_file("data1.txt");
    chain.print_chain();
    
    Problem arbitrageFinder(chain);
    
    std::cout << "\nSearching for arbitrage opportunities..." << std::endl;


    chrono::microseconds totalTime(0);
    int loops = 1;

    for (int i = 0; i < loops; i++) {

        auto loopStartTime = chrono::high_resolution_clock::now();
        bool found = arbitrageFinder.solve();
        auto loopEndTime = chrono::high_resolution_clock::now();

        if (found) {
            arbitrageFinder.printSolution();
        } else {
            std::cout << "\nNo arbitrage opportunities found in this option chain." << std::endl;
        }
    
        auto duration = chrono::duration_cast<std::chrono::microseconds>(loopEndTime - loopStartTime);
        totalTime += duration;
        cout << "\nSearch completed in " << duration.count() << " microseconds." << std::endl;

        arbitrageFinder.clearCombination();
    }

    auto programEndTime = chrono::high_resolution_clock::now();

    cout << "--------------------------------" << endl;
    cout << "\nAverage search time for " << loops << " iterations: " << totalTime.count() / loops << " microseconds." << endl << endl;
    cout << "Program completed in " << chrono::duration_cast<std::chrono::microseconds>(programEndTime - programStartTime).count() / loops << " microseconds." << endl << endl;
    
    return 0;
}

