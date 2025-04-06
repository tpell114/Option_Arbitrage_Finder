# Performance Testing Results

## Execution Times

| Test Case | Brute Force | C++ v1 | Haskell v1 | C++ v2 | Haskell v2 | Haskell v3 |
|-----------|-------------|--------|------------|--------|------------|------------|
| Data 1    | 167,917 µS  | 2,654,174 µS | 605,633 µS | 1,773,109 µS | 406,424 µS | 401,027 µS |
| Data 2    | 168,740 µS  | 2,727,347 µS | 598,656 µS | 1,774,383 µS | 407,871 µS | 400,135 µS |
| Data 3    | 168,528 µS  | 2,654,376 µS | 592,449 µS | 1,976,657 µS | 450,682 µS | 447,874 µS |
| Data 4    | 156,829 µS  | 2,459,125 µS | 527,826 µS | 744,718 µS   | 153,105 µS | 149,414 µS |
| Data 5    | 71.36 min   | -            | -          | -            | -          | -          |

**All tests averaged over 5 iterations**

## Test Data Descriptions

### Data 1
No arbitrage

### Data 2
Box spread

Net Credit: $-14.70

Legs:
1. Long Call Strike: $85.00 Price: $15.30
2. Short Put Strike: $85.00 Price: $0.60
3. Short Call Strike: $100.00 Price: $2.60
4. Long Put Strike: $100.00 Price: $2.60

Payoff at critical prices:
- Price: $0.00 -> Profit: $0.30
- Price: $85.00 -> Profit: $0.30
- Price: $100.00 -> Profit: $0.30
- Price: $10000.00 -> Profit: $0.30

### Data 3
6 arbitrage solutions

#### Arbitrage Solution #1

Net Credit: $30.30

Legs:
1. Short Put Strike: $100.00 Price: $3.00
2. Long Call Strike: $100.00 Price: $2.60
3. Long Put Strike: $70.00 Price: $0.10
4. Short Call Strike: $70.00 Price: $30.00

Payoff at critical prices:
- Price: $0.00 -> Profit: $0.30
- Price: $70.00 -> Profit: $0.30
- Price: $100.00 -> Profit: $0.30
- Price: $10000.00 -> Profit: $0.30

#### Arbitrage Solution #2

Net Credit: $30.25

Legs:
1. Short Put Strike: $100.00 Price: $3.00
2. Long Call Strike: $100.00 Price: $2.60
3. Long Put Strike: $75.00 Price: $0.15
4. Short Call Strike: $70.00 Price: $30.00

Payoff at critical prices:
- Price: $0.00 -> Profit: $5.25
- Price: $70.00 -> Profit: $5.25
- Price: $75.00 -> Profit: $0.25
- Price: $100.00 -> Profit: $0.25
- Price: $10000.00 -> Profit: $0.25

#### Arbitrage Solution #3

Net Credit: $25.25

Legs:
1. Short Put Strike: $100.00 Price: $3.00
2. Long Call Strike: $100.00 Price: $2.60
3. Long Put Strike: $75.00 Price: $0.15
4. Short Call Strike: $75.00 Price: $25.00

Payoff at critical prices:
- Price: $0.00 -> Profit: $0.25
- Price: $75.00 -> Profit: $0.25
- Price: $100.00 -> Profit: $0.25
- Price: $10000.00 -> Profit: $0.25

#### Arbitrage Solution #4

Net Credit: $20.10

Legs:
1. Short Put Strike: $100.00 Price: $3.00
2. Long Call Strike: $100.00 Price: $2.60
3. Long Put Strike: $80.00 Price: $0.40
4. Short Call Strike: $80.00 Price: $20.10

Payoff at critical prices:
- Price: $0.00 -> Profit: $0.10
- Price: $80.00 -> Profit: $0.10
- Price: $100.00 -> Profit: $0.10
- Price: $10000.00 -> Profit: $0.10

#### Arbitrage Solution #5

Net Credit: $-9.75

Legs:
1. Long Put Strike: $110.00 Price: $10.30
2. Short Call Strike: $110.00 Price: $0.15
3. Short Put Strike: $100.00 Price: $3.00
4. Long Call Strike: $100.00 Price: $2.60

Payoff at critical prices:
- Price: $0.00 -> Profit: $0.25
- Price: $100.00 -> Profit: $0.25
- Price: $110.00 -> Profit: $0.25
- Price: $10000.00 -> Profit: $0.25

#### Arbitrage Solution #6

Net Credit: $-9.90

Legs:
1. Long Put Strike: $110.00 Price: $10.30
2. Short Put Strike: $100.00 Price: $3.00
3. Long Call Strike: $100.00 Price: $2.60

Payoff at critical prices:
- Price: $0.00 -> Profit: $0.10
- Price: $100.00 -> Profit: $0.10
- Price: $110.00 -> Profit: $0.10
- Price: $10000.00 -> Profit: $9890.10

### Data 4
Real world data (VTRS April 4, 2025, end of day), no arbitrage

### Data 5
Real world data (COST April 6, 2025, end of day), no arbitrage

## Optimization Versions

### Version 1
The only optimization in this version is when the program is adding potential next moves, options with the same type (call/put) but opposite position (long/short) are not added, this prevents offsetting positions. The effect on performance of this is negligible, as only (2 * number of legs) moves are not added to the potential moves list. Thus, in a standard arbitrage opportunity with 4 legs, only 8 moves are not considered.

### Version 2
This version eliminates adding short positions for options that have a $0.00 bid, as these cannot practically be used.

### Version 3 (Haskell only)
This version eliminates deduplication on every level of recursion and only does it once at the end.
