# +------------------------------------------------+ 
# |                      MAIN                      |
# +------------------------------------------------+

# Description:
# Main file for the Mine project. This file is the
# entry point for the project, and contains the
# main function that is called when the program is
# run.

# +-------------------- IMPORTS -------------------+

from Sources import *
import argparse
import sys

# +------------------- FUNCTIONS ------------------+

def ft_argparse() -> Data:
    
    """
    Parse the command line arguments, initialize the Data class 
    with these arguments, and return the Data instance.

    Returns:
        Data: Initialized Data instance with parsed arguments.
    """
    
    parser = CustomArgumentParser()
    args = parser.parse_args()
    return Data(file=args.file, verbose=args.verbose)

def ft_main() -> None:
    
    """
    Main function that runs the expert system using the provided data.
    
    Args:
        data (Data): The data object containing facts, queries, and rules.
    """
    
    # Logger.header()
    data = ft_argparse()
    if data.verbose:
        Logger.info("Verbose mode enabled!")
    Logger.info("Starting expert system processing...\n")
    ft_expert_system(ft_parse(data))
    
if __name__ == "__main__":
    try:
        ft_main()
    except Exception as e:
        ft_exit(str(e), 1, True)
