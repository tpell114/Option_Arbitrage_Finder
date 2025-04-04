#include "OptionChain.h"
#include "Problem.h"
#include <iostream>
#include <chrono>

int main() {

    auto programStartTime = chrono::steady_clock::now();
    
    OptionChain chain;
    chain.load_from_file("data1.txt");
    chain.print_chain();
    
    Problem arbitrageFinder(chain);
    
    std::cout << "\nSearching for arbitrage opportunities..." << std::endl;


    chrono::microseconds totalTime(0);
    int loops = 10;

    for (int i = 0; i < loops; i++) {

        auto loopStartTime = chrono::steady_clock::now();
        
        arbitrageFinder.findAllSolutions();
        
        auto loopEndTime = chrono::steady_clock::now();

        if (i == 0) { // Only print solutions on the first loop
            arbitrageFinder.printAllSolutions();
        }
    
        auto duration = chrono::duration_cast<std::chrono::microseconds>(loopEndTime - loopStartTime);
        totalTime += duration;
        cout << "\nSearch completed in " << duration.count() << " microseconds." << std::endl;

        arbitrageFinder.clearCombination();
        arbitrageFinder.clearSolutions();
    }

    auto programEndTime = chrono::steady_clock::now();

    cout << "--------------------------------" << endl;
    cout << "\nAverage search time for " << loops << " iterations: " << totalTime.count() / loops << " microseconds." << endl << endl;
    cout << "Program completed in " << chrono::duration_cast<std::chrono::microseconds>(programEndTime - programStartTime).count() / loops << " microseconds." << endl << endl;
    
    return 0;
}

