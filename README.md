# EXPERT SYSTEM

## Base Logic

This document explains the process of resolving queries in an expert system based on a set of rules and initial facts. The example provided demonstrates how to evaluate the truth values of specific variables using backward chaining.

### Input Structure

The parsing file includes:

1. **Comments**: Lines beginning with `#` are comments and are ignored by the parser.
2. **Rules**: Each rule describes a logical implication or relationship between variables, formatted as:
   - `A => B` (A implies B)
   - `A + B => C` (A and B imply C)
   - `A | B => C` (A or B imply C)
   - `A + !B => F` (A and not B imply F)
   - `A + B <=> C` (A and B if and only if C)
3. **Initial Facts**: The line starting with `=` indicates the initial truth values of the variables.
4. **Queries**: Lines starting with `?` indicate which variables we want to determine the truth values for.

### Example Input

```sh
# this is a comment$
# all the required rules and symbols, along with the bonus ones, will be
# shown here. spacing is not important

C => E              # C implies E
A + B + C => D      # A and B and C implies D
A | B => C          # A or B implies C
A + !B => F         # A and not B implies F
C | !G => H         # C or not G implies H
V ^ W => X          # V xor W implies X
A + B => Y + Z      # A and B implies Y and Z
C | D => X | V      # C or D implies X or V
E + F => !V         # E and F implies not V
A + B <=> C         # A and B if and only if C
!A + B <=> !C        # A and B if and only if not C

=ABG                # Initial facts: A, B, and G are true. All others are false.
                    # If no facts are initially true, then a simple "=" followed
                    # by a newline is used

?GVX                # Queries: What are G, V, and X?
```

## Resolution Process

The goal is to evaluate the queries `?G`, `?V`, and `?X` using backward chaining based on the provided rules and initial facts.

### Step 1: Evaluate Query for G

Since `G` is included in the initial facts as **True**, we conclude:
- **Result**: `G = True`.

### Step 2: Evaluate Query for V

To determine if `V` is **True**, we need to analyze the relevant rules:

#### Check E and F:

- **Finding E**: From the rule `C => E`, since `C` is **False**, `E` must also be **False**.
- **Finding F**: The rule `A + !B => F`. Here, `A` is **True**, `B` is **True**, thus `!B` is **False**. Therefore, `F` is **True**.

#### Final Evaluation of V:

In the rule `E + F => !V`, we have `False + True`, which implies `!V` is **False**. Thus, `V = True`.

- **Result**: `V = True`.

### Step 3: Evaluate Query for X

To determine `X`, we look for relevant rules:

#### Check C and D:

- **Finding D**: The rule `A + B + C => D`. Since `C` is **False**, `D` must also be **False**.

#### Evaluate C | D:

Both `C` and `D` being **False** means `C | D` is **False**, which does not provide information about `X`.

#### Evaluate V ^ W:

Given `V = True` and `W = False` (from the initial facts), `V ^ W` evaluates to **True**, thus `X` must also be **True**.

- **Result**: `X = True`.

### Final Results of Queries

- `G = True`
- `V = True`
- `X = True`
