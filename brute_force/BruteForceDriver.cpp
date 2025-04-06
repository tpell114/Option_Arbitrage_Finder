#include "OptionChain.h"
#include "BruteForce.h"
#include <iostream>
#include <chrono>
#include <iomanip>

using namespace std;

int main(int argc, char* argv[]) {

    auto programStartTime = chrono::steady_clock::now();

    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " <data_file_path>" << endl;
        return 1;
    }
    
    string filename = argv[1];
    OptionChain chain;
    chain.load_from_file(filename);
    chain.print_chain();
    BruteForce bruteForceSolver(chain);
    
    cout << "\n========== BRUTE FORCE SOLVER ==========\n";
    cout << "Searching for arbitrage opportunities using brute force..." << endl;

    chrono::microseconds totalTime(0);
    int loops = 5;

    for (int i = 0; i < loops; i++) {

        auto loopStartTime = chrono::steady_clock::now();
        bruteForceSolver.findAllArbitrageOpportunities();
        auto loopEndTime = chrono::steady_clock::now();

        if (i == 0) {
            bruteForceSolver.printAllSolutions();
        }
    
        auto duration = chrono::duration_cast<chrono::microseconds>(loopEndTime - loopStartTime);
        totalTime += duration;
        cout << "\nSearch completed in " << duration.count() << " microseconds." << endl;

        bruteForceSolver.clearSolutions();
    }

    auto programEndTime = chrono::steady_clock::now();

    cout << "--------------------------------" << endl;
    cout << "\nAverage search time for " << loops << " iterations: " << totalTime.count() / loops << " microseconds." << endl << endl;
    cout << "Program completed in " << chrono::duration_cast<chrono::microseconds>(programEndTime - programStartTime).count() / loops << " microseconds." << endl << endl;
    
    return 0;
}
