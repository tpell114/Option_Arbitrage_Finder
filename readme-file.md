# Options Arbitrage Finder

## Overview
This project implements a tool to find arbitrage opportunities in options markets. The tool analyzes a set of options (calls and puts) at different strike prices and identifies combinations that guarantee a risk-free profit.

The project includes two solutions:
1. An imperative implementation in C++
2. A functional implementation in Haskell

## Building the Project

### C++ Version
To build the C++ version:
```bash
g++ -std=c++17 main.cpp Problem.cpp OptionChain.cpp -o options-arbitrage
```

### Haskell Version
To build the Haskell version:
```bash
cabal build
```

## Usage

### C++ Version
```bash
./options-arbitrage input_file.csv
```

### Haskell Version
```bash
cabal run options_arbitrage_finder -- input_file.csv
```

## Input Format
The program expects a CSV file with the following format:
```
type,strike,bid,ask
call,100,9.8,10.0
put,100,4.8,5.0
call,110,2.8,3.0
put,110,12.8,13.0
```

- `type`: Either "call" or "put"
- `strike`: The strike price of the option
- `bid`: The bid price (what you would receive when selling)
- `ask`: The ask price (what you would pay when buying)

## Output
The program will output:
1. The loaded option chain
2. Any arbitrage opportunities found
3. For each arbitrage opportunity:
   - The net credit (initial cash flow)
   - The option legs in the combination
   - The profit at all critical prices (strike prices, 0, and 10000)
4. Performance metrics (execution time)

## Core Algorithm
The program uses a depth-first search with backtracking to explore all possible combinations of option positions (long/short calls/puts) up to a maximum of 4 legs. For each combination, it verifies that:
1. The combination includes at least 2 legs
2. The payoff is positive at all critical prices (strike prices, 0, and 10000)

## Contributors
[Your Team Members]
