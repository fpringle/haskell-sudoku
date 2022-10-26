# Haskell-Sudoku

Sudoku solver implemented in Haskell

For documentation, see [here](https://fpringle.github.io/haskell-sudoku/).

## Instructions

1.  Run server with cabal:
    ```bash
    cabal new-run exe:sudoku
    ```

2.  Run library tests with cabal:
    ```bash
    cabal new-test --test-show-details=streaming
    ```

3.  Run server tests with JS (with server running in separate window/tab):
    ```bash
    cd testsuite/test-server-js/
    npm install
    npm start
    ```
