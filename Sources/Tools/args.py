# +------------------------------------------------+ 
# |                   ARGUMENTS                    |
# +------------------------------------------------+

# Description:
# Personalized ArgumentParser class that provides a
# custom error message and usage output.

# +-------------------- IMPORTS -------------------+

from typing import IO
from Sources.Tools import *

# +--------------------  CLASS --------------------+

class CustomArgumentParser(argparse.ArgumentParser):

    """
    Custom ArgumentParser class that provides a custom
    error message and usage output.
    """
    
    def __init__(self):
        super().__init__(prog="Expert System")
        self.description = "Propositional logic expert system that can determine the truth value of a given fact based on a set of rules and facts."
        self.epilog = "Made by: mboy - 42 Nice"
        self.add_argument("file", type=str, help="Path to the input file to read and parse from.")
        self.add_argument("-v", "--verbose", action="store_true", help="Enable verbose mode.")

    def error(self, message):
        # Use the Logger to output the error message and usage
        Logger.error(message.capitalize())
        self.print_usage()
        sys.exit(2)

    def print_usage(self, file=None):
        Logger.usage(self.format_usage()[7:].capitalize())

    def print_help(self, file=None):
        # Custom header
        header = f"\033[96m{'='*60}\033[0m\n"
        header += f"\033[96m{self.description}\033[0m\n\n"
        print(header, file=file or sys.stdout)
        super().print_help(file)

class CustomArgumentParserTest(argparse.ArgumentParser):

    """
    Custom ArgumentParser class for the test unit that 
    provides a custom error message and usage output.
    """
    
    def __init__(self):
        super().__init__(prog="Expert System Unit Test")
        self.description = "Run valid or invalid test cases for the project."
        self.epilog = "Made by: mboy - 42 Nice"
        self.add_argument('-v', '--valid', action='store_true', help="Run only the valid test cases.")
        self.add_argument('-i', '--invalid', action='store_true', help="Run only the invalid test cases.")

    def error(self, message):
        # Use the Logger to output the error message and usage
        Logger.error(message.capitalize())
        self.print_usage()
        sys.exit(2)

    def print_usage(self, file=None):
        Logger.usage(self.format_usage()[7:].capitalize())

    def print_help(self, file=None):
        # Custom header
        header = f"\033[96m{'='*60}\033[0m\n"
        header += f"\033[96m{self.description}\033[0m\n\n"
        print(header, file=file or sys.stdout)
        super().print_help(file)