# EXPERT SYSTEM

Validated at 125%

The goal of this project is to make a propositional calculus expert system by implementing a backward-chaining inference engine.

## Usage

### Virtualenv
   
   First of all, setup virtualenv using the following command:
   ```sh
   source ./setup.sh
   ```
   The script will automatically install all dependencies listed in the requirements.txt file. You can also deactivate the virtualenv by executing the script as so :
   ```sh
   source ./setup.sh --deactivate
   ```

### Expert system

   To execute the main program, execute the following script
   ```sh
   python3 -B main.py [-v] [-h] file
   ```
   Where:
   - `file` is the path to the input file to read and parse from.
   - `-v` or `--verbose` to enables verbose mode (outputs details information on the logic)
   - `-h` or `--help` do display a help message concerning the usage of the script

### Unit testing

   To perform unit tests on the privously mentioned programe:
   ```sh
   python3 -B test.py
   ```
   Where:
   - `-v` or `--valid` to perfom tests only on valid cases
   - `-i` or `--invalid` to perfom tests only on error cases
   - `-h` or `--help` do display a help message concerning the usage of the script
   By default the program runs both valid and error cases.


