# Code and Documentation Standards

## C++ Coding Standards

### Naming Conventions
- **Variables and Functions**: Use camalCase
  - Example: `optionChain`, `calculatePayoff`
- **Classes and Types**: Use PascalCase
  - Example: `OptionChain`, `Problem`
- **Constants**: Use uppercase with underscores
  - Example: `MAX_LEGS`, `DEFAULT_PRICE`

### Formatting
- Indentation: 1 tab
- Braces: Open brace on the same line as the statement, close brace aligned with the start of the statement
- Maximum line length: 100 characters
- Space after keywords (`if`, `for`, etc.)
- Space around operators (`+`, `-`, `=`, etc.)

### Comments
- Function-level documentation: Describe purpose, parameters, return values, and any side effects
- Inline comments: Only when necessary to explain complex logic
  

## Haskell Coding Standards

### Naming Conventions
- **Functions and Variables**: Use camelCase
  - Example: `getTotalCost`, `calculatePayoff`
- **Types and Constructors**: Use PascalCase
  - Example: `OptionChain`, `Problem`
- **Modules**: Use PascalCase
  - Example: `OptionChain`, `Problem`

### Formatting
- Indentation: 1 tab
- Function type signatures: On a separate line before the function definition
- Maximum line length: 100 characters
- Space around operators

### Comments
- Function-level documentation: Describe purpose, parameters, return values, and any side effects
- Inline comments: Only when necessary to explain complex logic

### Functional Style
- Favor pure functions over functions with side effects
- Use higher-order functions where appropriate
- Leverage pattern matching for readable code
- Minimize use of partial functions
