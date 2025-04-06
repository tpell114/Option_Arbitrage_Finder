# Code and Documentation Standards

## General Principles
Our team adheres to the following general principles across both implementations:
- Code should be clear, correct, and readable
- Code should be easy to maintain, modify, extend, and reuse
- Documentation should be comprehensive yet concise
- Consistent style should be maintained throughout the project

## C++ Coding Standards

### Naming Conventions
- **Variables and Functions**: Use lowercase with underscores for multi-word names (snake_case)
  - Example: `option_chain`, `calculate_payoff`
- **Classes and Types**: Use PascalCase (capitalize first letter of each word)
  - Example: `OptionChain`, `Problem`
- **Constants**: Use uppercase with underscores
  - Example: `MAX_LEGS`, `DEFAULT_PRICE`
- **Member Variables**: No special prefix (unlike some conventions that use `m_` prefix)
  - Example: `combination`, `market_data`

### Formatting
- Indentation: 4 spaces (no tabs)
- Braces: Open brace on the same line as the statement, close brace aligned with the start of the statement
- Maximum line length: 100 characters
- Space after keywords (`if`, `for`, etc.)
- Space around operators (`+`, `-`, `=`, etc.)

### Comments
- Class-level documentation: Describe purpose, usage, and any invariants
- Function-level documentation: Describe purpose, parameters, return values, and any side effects
- Inline comments: Only when necessary to explain complex logic
- Avoid unnecessary comments that just restate the code

### Error Handling
- Use exceptions for exceptional conditions
- Use return values for expected failure modes
- Validate input thoroughly
- Provide meaningful error messages

## Haskell Coding Standards

### Naming Conventions
- **Functions and Variables**: Use camelCase (lowercase first letter, capitalize subsequent words)
  - Example: `getTotalCost`, `calculatePayoff`
- **Types and Constructors**: Use PascalCase
  - Example: `OptionChain`, `Problem`
- **Modules**: Use PascalCase
  - Example: `OptionChain`, `Problem`

### Formatting
- Indentation: 4 spaces (no tabs)
- Function type signatures: On a separate line before the function definition
- Maximum line length: 80 characters
- Space around operators
- No trailing whitespace

### Comments
- Module-level documentation: Describe purpose and usage
- Function-level documentation: Describe purpose, parameters, and return values
- Use proper Haddock syntax for documentation

### Functional Style
- Favor pure functions over functions with side effects
- Use higher-order functions where appropriate
- Leverage pattern matching for readable code
- Minimize use of partial functions

## Documentation Standards

### In-Code Documentation
- Every module/file should have a header comment describing its purpose
- Every public function/method should have a documentation comment
- Complex algorithms should have detailed step-by-step explanations
- Critical design decisions should be documented

### External Documentation
- README: Provide overview, build instructions, and usage examples
- Design document: Explain the overall architecture and key abstractions
- Test plan: Document the testing approach and test cases
- User manual: Describe how to use the software from an end-user perspective

## Version Control Practices
- Commit messages should be clear and descriptive
- Each commit should represent a logical unit of work
- Commit early and often
- Pull/merge before pushing to avoid conflicts

## Testing Standards
- All core functionality should be covered by tests
- Test both normal and edge cases
- Document test cases with expected inputs and outputs
- Automate tests where possible
