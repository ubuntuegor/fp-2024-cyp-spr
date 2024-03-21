# HW07
## Deadline: 23:59 02.04.2024

1. (6 points) Implement a parser of expressions in prefix notation. An expression is either: 
   * A positive integer number.
   * A variable (identifier): cannot be a keyword.
   * A square root of an expression: keyword `sqrt`, then space, then expression. 
   * A binary operator followed a space, followed by two expressions separated by a space. Operators: 

      * `+`
      * `-` 
      * `*`
      * `/`
      * `^`

   * Examples of expressions:

     * `123`
     * `xyz` 
     * `sqrt 123`, `sqrt xyz`
     * `+ 123 45`, `* xyz 123`, `\ xyz sqr` 
     * `+ 123 * 45 6` -- corresponds to `123 + (45 * 6)`
     * `+ * 123 45 6` -- corresponds to `(123 * 45) + 6`
     * `\ sqrt 123 xyz` -- corresponds to `(sqrt 123) / xyz`

   * Non-examples of expressions: 
     
     * `sqrt`: keyword (not an identifier) without the expression 
     * `- 123`: minus can only be a binary operator in our language and this string doesn't have 

   * The result of this parsing should be `Expr` from previous homeworks.
2. (2 points) Make your interpreter for expressions work in the `State` monad. 
    * We don't have `let`-expressions in our expression language yet, so the state is never changed, but we'll extend the language in the next homework. 

3. (2 points) Implement a simple console app that 
   * First, asks the user for an expression in prefix notation. 
   * Then asks to input the variable map (you can use any format, make it easy for you to parse this input, but make sure to explain it to the user).
   * Then runs the evaluator of the expression and outputs to the console. 
   * If there are any errors, try to be as descriptive as possible when reporting them. 

Don't forget about tests in the first two tasks. 