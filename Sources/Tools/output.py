# +------------------------------------------------+ 
# |                     OUTPUT                     |
# +------------------------------------------------+

# Description:
# This file contains functions to output 
# messages, colored messages, headers and 
# errors.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *

# +------------------- FUNCTIONS ------------------+

def ft_output_info(message: str, endswith: str = '\n') -> None:

    """
    Function that prints a colored info message.
    
    Parameters:
        message (str): Message to output with color.
        endswith (str): String to end with (default: '')
    Returns:
        None
    Raises:
        None
    """

    print(f'{fg(204, 219, 253)}[INFO] {message}{fg.rs}', end=endswith)


def ft_output_usage() -> None:

    """
    Function that prints the usage message.

    Parameters:
        None
    Returns:
        None
    Raises:
        None
    """

    print(f'{fg(204, 219, 253)}[USAGE] python3 -B main.py [input_file]{fg.rs}', end='\n')
    print(f'{fg(204, 219, 253)} - [input_file]: File to read and parse from.{fg.rs}', end='\n')


def ft_output_sucess(message: str, endswith: str = '\n') -> None:

    """
    Function that prints a colored success message.
    
    Parameters:
        message (str): Message to output.
        endswith (str): String to end with (default: '')
    Returns:
        None
    Raises:
        None
    """

    print(f'{fg(207, 225, 185)}[SUCCESS] {message}{fg.rs}', end=endswith)


def ft_output_error(message: str, endswith: str = '\n') -> None:
    
    """
    Function that prints a colored error message.

    Parameters:
        message (str): Message to output with color.
        endswith (str): String to end with (default: '\n')
    Returns:
        None
    Raises:
        None
    """

    print(f'{fg(255, 173, 194)}[ERROR] {message}{fg.rs}', end=endswith)