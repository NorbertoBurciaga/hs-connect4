# hs-connect4

## Contents

* [Introduction](#introduction)
* [Project Hierarchy](#project-hierarchy)
* [Getting Started](#getting-started)
* [State Representation](#state-representation)
* [Initial State](#initial-state)
* [Goal States](#goal-states)
* [Actions](#actions)
* [Heuristics](#heuristics)
* [Testing](#testing)
* [Support](#support)
* [FAQ](#faq)
* [Development](#development)
* [License](#license)
* [References](#references)

## Introduction

This is an implementation of connect4 game in Haskell.

If you are gamer and want to play connect4 or if you are a developer that want to test a functional language like Haskell,
this project is done using behavior driven development using hspec that let a better understanding of each function.

This is an example to learn and know what can be done with Haskell.

[Contents](#contents)

## Project Hierarchy

```console
```

[Contents](#contents)

## Getting Started

### Requirements
* Haskell
* matrix package

### Installation

To install the requirements:
1. sudo apt install haskell-platform

To get the matrix library update your cabal package list (if needed) with cabal update and then use cabal install matrix, assuming that you already have Cabal installed.


[Contents](#contents)

## State Representation

Connect4 game has a fixed board of n columns by m rows, usually 7-columns x 6-rows. The pieces fall straight down, occupying the lowest available space within the column.

The sequence of moves is represented on the board by columns that behaves as a stack of checkers, this board is implemented by a matrix.

Example: for a 7-columns x 6-rows board, the following sequence of moves have been done:

- Player 1 column 4
- Player 2 column 5
- Player 1 column 4
- Player 2 column 4

```console
┌               ┐
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 0 2 0 0 0 │
│ 0 0 0 1 0 0 0 │
│ 0 0 0 1 2 0 0 │
└               ┘ 
```

Every move should be validated against the restrictions on the board:
- Cannot drop in a column outside the defined columns
- Cannot drop in a row that is already filled

As the game has a fixed board the following can also be defined:

```haskell
columns = 7
rows = 6
total_turns = columns x rows
players = 2
turns_per_player = total_turns / players

```

[Contents](#contents)

## Initial State

The representation of the sequence of moves starts with an empty board of size columns x rows.

For example 7 columns x 6 rows:

```console
┌               ┐
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
└               ┘
```

If player 1 is going to be first then:

```haskell
next_turn_player = 1
```

If player 2 is going to be first then:

```haskell
next_turn_player = 2
```

[Contents](#contents)

## Goal States

The objective is to connect four of your checkers in a row while preventing your opponent from doing the same.[1]

Examples of winning states for player 1:

- Horizontal

```console
┌               ┐
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 1 1 1 1 0 0 │
└               ┘
```
- Vertical

```console
┌               ┐
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 0 1 0 0 0 │
│ 0 0 0 1 0 0 0 │
│ 0 0 0 1 0 0 0 │
│ 0 0 0 1 0 0 0 │
└               ┘
```

- Diagonal

```console
┌               ┐
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 1 0 0 │
│ 0 0 0 1 x 0 0 │
│ 0 0 1 x x 0 0 │
│ 0 1 x x x 0 0 │
└               ┘
```

- Diagonal

```console
┌               ┐
│ 0 0 0 0 0 0 0 │
│ 0 0 0 0 0 0 0 │
│ 0 0 1 0 0 0 0 │
│ 0 0 x 1 0 0 0 │
│ 0 0 x x 1 0 0 │
│ 0 0 x x x 1 0 │
└               ┘
```

[Contents](#contents)

## Actions

```haskell
initiate_board :: Num a => Int -> Int -> Matrix a
initiate_board rows columns = zero rows columns 

is_valid_position :: Matrix a -> Int -> Bool
is_valid_position board column =

is_user_turn :: Matrix a -> Int -> Bool
is_user_turn next_turn_player player = (== next_turn_player)

update_board :: Matrix a -> Int -> Board_State
update_board board column = 

is_win_position :: Matrix a -> Int -> 
is_win_position board  last_column_played = 

drop_checker :: Board_state -> Int -> Int -> Bool
drop_checker board_state player column = 
```

[Contents](#contents)

## Heuristics

[Contents](#contents)

## Testing

```console
$ stack build --test
```

[Contents](#contents)

## Support
Technical support is available in 

[Contents](#contents)

## FAQ

[Contents](#contents)

## Development
If you want to contribute....

[Contents](#contents)

## License

See LICENSE file

[Contents](#contents)

## References

1. [Wikipedia Connect Four](https://en.wikipedia.org/wiki/Connect_Four)
2. [Solving Connect four: How to build a perfect AI](http://blog.gamesolver.org/)

[Contents](#contents)
