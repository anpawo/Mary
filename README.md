# Mary Programming Language (GLaDOS)  
<div align="center">
  <img src="https://static.wikia.nocookie.net/epicpixelbattles/images/5/5f/400px-GLaDOS_P2.png/revision/latest?cb=20191127181958">

![Static Badge](https://img.shields.io/badge/Mary-programming_language-blue)

</div>

The goal of this project is to implement a programming language of our own design in Haskell.

## Needed Dependencies ⚙️

The best way to install Haskell, is using [GHCup](https://www.haskell.org/ghcup/).

## Build the Project 🛠️

After installing the necessary dependencies, in order to build the project, use the Makefile. Follow these steps:

- Clone the project:

```sh
git clone git@github.com:anpawo/mary.git
```

- Go to the project directory:

```sh
cd mary
```

- Build using Makefile:

```sh
make
```

## How to Use? 🛠️

- First, you need to write a file according to the `Mary` syntax.
- You should consider installing the [vscode-extension](https://github.com/anpawo/Mary/tree/main/vscode-extension) for better readability.
- Once your file is done, you can run it with `./glados <filename>`

For starters, here's a little file with a guess the number game.

```ts
import std

function guess_the_number(n: int) -> void {
    print("guess the number between 0 and 100");
    guess: int = n + 1;
    while guess != n then
    {
        guess = toInt(getline());
        if guess == n then
        {
            print("you won");
        }
        else
        {
            if guess < n then
            {
                print("higher");
            }
            else
            {
                print("lower");
            }
        }
    }
}

function main() -> void {
    guess_the_number(random(0, 100));
}
```

## Quick Guide

### functions

```ts
function main() -> void {
  print("hello world");
}
```

### binary operators with custom precedence

```ts
operator ** precedence 8 (n: int, power: int) -> int {
    if power == 0 then
    {
        return 1;
    }
    else
    {
        return n * n ** (power - 1);
    }
}
```

### union types

```ts
type number = int | float
```

### structures

```rust
struct person {
  name: str,
  age: int
}
```

### lambdas and imports

```rust
import list
// provides the function map
// and the operator `..` (range)
// which are both coded in the mary language.

function main() -> void {
    print(map(\x -> x * 2, 1..10));
    //        ^^^^^^^^^^^
    //          lambda
}

// output [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]


```

You can do great things with this much!

For more information about the language, check out the [Mary User Manual](https://github.com/anpawo/Mary/blob/main/doc/User_Manual.md).

## Our Team ❤️
### Developers
| [<img src="https://avatars.githubusercontent.com/u/99143096?v=4" width=85><br><sub>Zacharie Rodde</sub>](https://github.com/Zach0s) | [<img src="https://avatars.githubusercontent.com/u/114692454?v=4" width=85><br><sub>Jean-Garice Denoncin</sub>](https://github.com/Garois) | [<img src="https://avatars.githubusercontent.com/u/114686618?v=4" width=85><br><sub>Ryan Hercule</sub>](https://github.com/ryanoux42) | [<img src="https://avatars.githubusercontent.com/u/114910961?v=4" width=85><br><sub>Camil Kaddouri</sub>](https://github.com/camilepitech) | [<img src="https://avatars.githubusercontent.com/u/112256146?v=4" width=85><br><sub>Marius Rousset</sub>](https://github.com/anpawo) |
| :-------------------------------------------------------------------------------------------------------------------------------: | :-------------------------------------------------------------------------------------------------------------------------------: | :-------------------------------------------------------------------------------------------------------------------------------: | :-------------------------------------------------------------------------------------------------------------------------------: | :-------------------------------------------------------------------------------------------------------------------------------: |
