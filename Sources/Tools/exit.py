# +------------------------------------------------+ 
# |                      EXIT                      |
# +------------------------------------------------+

# Description:
# This file contains functions to output 
# messages, colored messages, headers and 
# errors.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *

# +------------------- FUNCTIONS ------------------+

def ft_exit(message: str, code: int, usage: bool) -> None:

    """
    Main function to exit the program.
    Depending on the parameters, it will print an error message,
    a usage message or both and exit the program with the given
    code.

    Parameters:
        message (str): Error message to print.
        code (int): Error code to exit with.
        usage (bool): Whether to print the usage message or not.
    Returns: None
    Raises: None
    """

    Logger.error(message)
    if usage:
        CustomArgumentParser().print_usage()
    sys.exit(code)
