# Test Plan

## 1. Introduction

This document outlines the testing approach for the Options Arbitrage Finder project, covering both the C++ (imperative) and Haskell (functional) implementations. The goal of this testing is to ensure both implementations produce correct results, find the same solutions, and maintain acceptable performance.

## 2. Testing Strategy

1. **Unit Testing**: Testing individual components in isolation
2. **Integration Testing**: Testing interactions between components
3. **System Testing**: End-to-end testing of the complete application
4. **Performance Testing**: Measuring execution time
5. **Comparison Testing**: Comparing results between C++ and Haskell implementations

## 3. Test Environments

### 3.1 Development Environment
- **Primary IDE**: Visual Studio Code with WSL 2 (Windows Subsystem for Linux 2)
- **Operating System**: Ubuntu on WSL 2
- **C++ Compiler**: Clang compiler with C++17 support
- **Haskell Stack**: GHC compiler with Cabal package manager

### 3.2 Testing Tools and Extensions
- **VS Code Extensions**:
  - C/C++ extension for IntelliSense and debugging
  - Haskell extension for syntax highlighting and language support

## 4. Unit Tests

### 4.1 OptionChain Module

#### Test Cases:
1. **Empty Chain Creation**
   - Input: None
   - Expected: Empty option chain with no strikes
   
2. **Option Addition**
   - Input: Multiple options with different strikes
   - Expected: Options correctly added to the chain with proper organization
   
3. **Option Retrieval**
   - Input: Strike price
   - Expected: Correct call/put options returned for that strike
   
4. **Invalid Option Handling**
   - Input: Invalid option data (negative prices, etc.)
   - Expected: Appropriate error handling

### 4.2 Problem Module

#### Test Cases:
1. **Empty Problem Creation**
   - Input: Option chain
   - Expected: Problem with empty combination
   
2. **Leg Addition/Removal**
   - Input: Option legs to add/remove
   - Expected: Correct updating of the combination
   
3. **Critical Price Calculation**
   - Input: Problem with various legs
   - Expected: Set of all strike prices plus 0 and 10000
   
4. **Payoff Calculation**
   - Input: Problem and price point
   - Expected: Correct payoff calculation

5. **Solution Detection**
   - Input: Various combinations (including known arbitrage opportunities)
   - Expected: Correct identification of arbitrage opportunities

### 4.3 Input/Output Module

#### Test Cases:
1. **Valid File Reading**
   - Input: Well-formed .txt file
   - Expected: Correctly populated option chain
   
2. **Invalid File Handling**
   - Input: Malformed .txt, missing file
   - Expected: Appropriate error messages
   
3. **Output Formatting**
   - Input: Option chain and solutions
   - Expected: Correctly formatted output

## 5. Integration Tests

### 5.1 File Loading to Problem Solving

#### Test Cases:
1. **End-to-End Flow**
   - Input: .txt file with option data
   - Expected: Loaded option chain and identified solutions
   
2. **Error Propagation**
   - Input: Invalid input that causes errors in different modules
   - Expected: Appropriate error handling and reporting

## 6. System Tests

### 6.1 Complete Application

#### Test Cases:
1. **Simple Arbitrage Identification**
   - Input: .txt file with known simple arbitrage opportunity (e.g. box spread)
   - Expected: Identification of the arbitrage opportunity
   
2. **Complex Arbitrage Identification**
   - Input: .txt file with multi-leg arbitrage opportunities
   - Expected: Identification of all arbitrage opportunities
   
3. **No Arbitrage Handling**
   - Input: .txt file with no arbitrage opportunities
   - Expected: Appropriate message indicating no opportunities

## 7. Performance Tests

### 7.1 Execution Time

#### Test Cases:
1. **Small Dataset**
   - Input: 5 strike prices (10 options)
   - Expected: TBD
   
2. **Medium Dataset**
   - Input: 20 strike prices (40 options)
   - Expected: TBD
   
3. **Large Dataset**
   - Input: 100+ strike prices (200+ options)
   - Expected: TBD

## 8. Comparison Tests

### 8.1 C++ vs. Haskell Implementation

#### Test Cases:
1. **Result Consistency**
   - Input: Same option chain for both implementations
   - Expected: Same arbitrage opportunities identified
   
2. **Performance Comparison**
   - Input: Various datasets
   - Expected: Documentation of performance differences
