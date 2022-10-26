# Haskell-Sudoku

A Sudoku library and server-based solver, implemented in Haskell.

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

3.  Run server tests with JS, with server running in separate window/tab (see #1):
    ```bash
    cd testsuite/test-server-js/
    npm install
    npm start
    ```
## Server API

The server runs on localhost at port 3421 (to change this, edit src/Main.hs). There are 2 API paths:

### /board
Get - generate a sudoku puzzle.

Query parameters:

- blanks(integer): the number of blank spaces desired. Use this parameter to control the difficulty of the board.
    
Returns:
```
{
    board: [[...]]      # a 9x9 array containing an unsolved Sudoku grid
}
```

### /solve
Post - solve a sudoku puzzle.

Request body:
```
{
    board: [[...]]      # a 9x9 array containing an unsolved Sudoku grid
}
```
    
Returns:
```
{
    board: [[...]]      # a 9x9 array containing the solved sudoku grid
}
```
