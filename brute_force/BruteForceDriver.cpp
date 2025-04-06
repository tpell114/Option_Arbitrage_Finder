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

    chrono::microseconds bruteForceTime(0);
    int loops = 5;

    for (int i = 0; i < loops; i++) {

        auto loopStartTime = chrono::steady_clock::now();
        bruteForceSolver.findAllArbitrageOpportunities();
        auto loopEndTime = chrono::steady_clock::now();

        if (i == 0) {
            bruteForceSolver.printAllSolutions();
        }
    
        auto duration = chrono::duration_cast<chrono::microseconds>(loopEndTime - loopStartTime);
        bruteForceTime += duration;
        
        cout << "\nBrute force search iteration " << (i+1) << " completed in " 
             << duration.count() << " microseconds." << endl;

        bruteForceSolver.clearSolutions();
    }

    double avgBruteForceTime = static_cast<double>(bruteForceTime.count()) / loops;

    auto programEndTime = chrono::steady_clock::now();
    auto totalProgramTime = chrono::duration_cast<chrono::microseconds>(programEndTime - programStartTime).count();

    cout << "\n========== PERFORMANCE RESULTS ==========\n";
    cout << fixed << setprecision(2);
    cout << "Average brute force solver time: " << avgBruteForceTime << " microseconds" << endl;
    cout << "Total program run time: " << totalProgramTime << " microseconds" << endl;
    
    return 0;
}
