# Random Username Generator

This Haskell program generates a random username based on the user's first and last names. The resulting username follows the format: `firstName-lastName-randomNumber`, e.g., `peter-parker-83`. The `parameterize` function from the `inflections` library is used to format the names.

## Usage

### Prerequisites

- `inflections` package
- `text` package
- `random` package

### Running the Program

1. Navigate to the project examples directory:

```bash
cd examples/random-username
```

2. Start GHCi by running the following command in your terminal:

```bash
stack ghci src/Main.hs
```

3. Execute the 'main' function to interact with the program:

```bash
>> main
```

4. Follow the prompts and enter your first and last names when requested.

## Code Overview

- `makeUserNameWith`: Function to concatenate a list of text elements into a single `Text` with hyphens as separators.
- `generateRandomUserName`: Function to create a random username from provided first and last names. It generates a random number and formats the names using `parameterize` function before combining them to from the username.
- `main`: Function to interactively collect the user's first and last names and display the generated username.
