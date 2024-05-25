# Report
## Task decription
Write an interpreter for a simple L language.
## Architecture
- **Parser.hs** - parser of the language into AST written using Megaparsec.  
  Megaparsec was chosen as it was recommended during the course, and I saw recommendations for it online.
  The parser is mostly written using techniques described in the megaparsec tutorial, not much else to say here.
- **Interpreter.hs** - interpreter of the AST.  
  Evaluation results use monad transformers from MTL to store both State and Either. I didn't use them at first but I'm glad I did, it simplifies the code a lot and makes working with IO trivial. The L language has expressions and statements, but in addition to the original specification, I made it so an expression can be a statement so that functions could have their last statement as their return value. 
- **Runner.hs** - facade for running the parser + the interpreter.
- **Main.hs** - entry point for the CLI.  
  I was thinking about how to write output to a file, but also to keep an option to write output to console. After a quick look into the IO functions of Haskell I saw that there was a family of functions that accepts an IO handle, which seemed like a good solution for my problem. Due to this, the evaluation state also contains the IO handle to write output to.  
  I also wanted to free myself from the burden of parsing command line options and look at packages that do it, so I found optparse-applicative. It seems to do its job pretty well with simple usage but a lot of features. Although I couldn't get it to simply accept the input filepath without the "-i" key.
- **Spec.hs** - tests written using Hspec.  
  I chose Hspec because it seems to be one of the industry standard testing frameworks, and megaparsec has test helpers for it in the hspec-megaparsec package.

## Investigation of performance and issues
- **Parser performance.**

  Megaparsec accepts multiple types of input that it can parse, most notably - String and Text. From my understanding, Text is based on an array buffer, while String is the usual lazy evaluated linked list (aka [Char]). I chose String because I am more familiar with it and it seems sufficient for working with small keywords and taking prefixes (i.e. what happens most often during parsing).
  
  I saw some tips about performance in Megaparsec's documentation but wasn't eager to try them (e.g. using takeWhile instead of many $ satisfy).
- **Problems with keywords and identifiers.**

  I didn't want to explicitly prohibit stuff like
  ```
  if = 2;
  write if
  ```
  but it seems to cause some problems, for example this fails:
  ```
  function a () {
    if = 2;
    if
  }

  write a()
  ```
- **Can't test write and read instructions.**

  I couldn't find a simple way to abstract IO operations so that I could mock read and write statements.
