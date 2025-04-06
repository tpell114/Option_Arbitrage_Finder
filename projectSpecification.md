# Project Specification

## 1. Introduction

The Options Arbitrage Finder is a tool designed to identify risk-free profit opportunities (arbitrage) in options markets. It analyzes combinations of options at different strike prices to find positions that guarantee a positive profit regardless of the underlying asset's price movement.

This document specifies the requirements, design, and functionality of both the imperative (C++) and functional (Haskell) implementations of the tool.

## 2. Problem Domain

### 2.1 Options Trading Background

Options are financial derivatives that give the holder the right (but not obligation) to buy (call option) or sell (put option) an underlying asset at a specified price (strike price) on or before a specific date.

Key components of an option:
- **Type**: Call (right to buy) or Put (right to sell)
- **Strike Price**: The price at which the option holder can exercise their right
- **Premium**: The price paid to acquire the option (debit) or received when selling it (credit).
- **Position**: Long (bought the option) or Short (sold the option)

### 2.2 Arbitrage Opportunities

An arbitrage opportunity exists when a trader can establish a position that guarantees a positive payoff at all possible future prices of the underlying asset.

This tool focuses on finding such opportunities within an options chain (a set of options for the same underlying asset with various strike prices).

## 3. Tool Requirements

### 3.1 Functional Requirements

1. **Input Processing**:
   - Read option chain data from a .txt file
   - Parse option type, strike price, bid price, and ask price

2. **Core Functionality**:
   - Systematically search all possible combinations of option positions (2-4 legs)
   - Calculate the initial debit/credit of each combination
   - Determine the payoff at all critical prices
   - Identify combinations that result in arbitrage opportunities

3. **Output Generation**:
   - Display the loaded option chain
   - List all identified arbitrage opportunities
   - For each opportunity, show:
     - The combination of option positions
     - The net debit/credit
     - The payoff at all critical prices
   - Performance metrics (execution time)

### 3.2 Non-Functional Requirements

1. **Performance**:
   - Complete the search in a reasonable time
   - Design data structures for multi-threading support to handle large option chains

2. **Usability**:
   - Clear command-line interface
   - Well-formatted output

3. **Maintainability**:
   - Well-documented code
   - Modular design
   - Adherence to coding standards

## 4. Input and Output Specifications

### 4.1 Input Format

The tool accepts a .txt file with the following format:
```
type,strike,bid,ask
call,100,9.8,10.0
put,100,4.8,5.0
...
```

Each row represents an option with:
- `type`: Either "call" or "put"
- `strike`: The strike price (positive number)
- `bid`: The bid price (positive number)
- `ask`: The ask price (positive number)

### 4.2 Command-Line Interface

The C++ tool is run as follows:
```
./program <input-file>
```

For the Haskell implementation:
```
cabal run options_arbitrage_finder -- <input-file>
```

### 4.3 Output Format

The tool produces text output to the console with the following sections:

1. **Option Chain Display**:
   ```
   Option Chain:
   --------------------------------------------------
   Call Bid | Call Ask | Strike | Put Bid  | Put Ask
   --------------------------------------------------
   9.80     | 10.00    | 100    | 4.80     | 5.00
   2.80     | 3.00     | 110    | 12.80    | 13.00
   --------------------------------------------------
   ```

2. **Arbitrage Opportunities**:
   ```
   Found 1 arbitrage opportunities.

   Arbitrage Solution #1
   --------------------------------
   Net Credit: $0.60
   Legs:

   1. Long Put Strike: $110 Price: $13.00
   2. Short Call Strike: $110 Price: $2.80
   3. Short Put Strike: $100 Price: $4.80
   4. Long Call Strike: $100 Price: $10.00

   Payoff at critical prices:
   Price: $0.00 -> Profit: $0.60
   Price: $100.00 -> Profit: $0.60
   Price: $110.00 -> Profit: $0.60
   Price: $10000.00 -> Profit: $0.60
   --------------------------------
   ```

3. **Performance Metrics**:
   ```
   Search completed in 1525 microseconds.
   ```

## 5. High-Level Design

### 5.1 Common Components

Both implementations share the same conceptual components:

1. **Option Representation**:
   - Properties: type, strike, bid, ask
   - Operations: calculate intrinsic value at a given price

2. **Option Chain**:
   - Collection of options organized by strike price
   - Operations:
      -  add option
      -  get options at strike
      -  get all strikes

3. **Option Leg**:
   - Represents a position in a specific option
   - Properties: option reference, position type (long/short)
   - Operations: calculate debit/credit

4. **Problem Representation**:
   - Current combination of option legs
   - Operations:
     - Add/remove legs
     - Calculate total cost
     - Calculate critical prices
     - Calculate payoff at a given price
     - Generate possible next moves
     - Determine if a combination is an arbitrage opportunity

5. **Input/Output Handling**:
   - File reading and parsing
   - Option chain display
   - Solution display

### 5.2 Imperative Implementation (C++)

The C++ implementation organizes these concepts into classes with the following structure:

1. **OptionChain Class**:
   - Manages a collection of options using a map data structure
   - Methods for adding options and querying the chain

2. **Problem Class**:
   - Implements the depth-first search with backtracking algorithm
   - Maintains the current combination of legs
   - Methods for evaluating combinations and tracking solutions

3. **Main Program**:
   - Handles command-line arguments
   - Coordinates the overall flow from input to output

### 5.3 Functional Implementation (Haskell)

The Haskell implementation organizes these concepts into modules with the following structure:

1. **Option Module**:
   - Defines the Option data type and related functions

2. **OptionChain Module**:
   - Defines the OptionChain data type and functions for manipulating chains

3. **OptionLeg Module**:
   - Defines the OptionLeg data type and related functions

4. **Problem Module**:
   - Implements the solution logic using list comprehensions
   - Functions for calculating payoffs and determining arbitrage opportunities

5. **IO Module**:
   - Functions for reading input files and formatting output

6. **Main Module**:
   - Coordinates the overall program flow

## 6. Algorithm Description

The core algorithm follows these steps:

1. Load options from the input file
2. Initialize an empty combination
3. Recursively explore all combinations of option legs:
   
   - Check if the current combination is an arbitrage opportunity
   - If yes, record it as a solution
   - Generate all possible next legs to add
   - For each possible leg:
      i. Add the leg to the combination
      ii. Recursively continue the search
      iii. Remove the leg from the combination (backtrack)
     
5. Output all found solutions

### 6.1 Arbitrage Opportunity Criteria

A combination is considered an arbitrage opportunity if:
1. It consists of at least 2 legs
2. The payoff is non-negative at all critical prices (all strike prices, 0, 10000)

### 6.2 Search Space Optimization

To make the search efficient:
1. Limit combinations to at most 4 legs
2. Avoid adding the same option multiple times
3. Avoid offsetting positions (same strike and type, but opposite position)
4. Detect and eliminate duplicate solutions (same set of legs in different order)
