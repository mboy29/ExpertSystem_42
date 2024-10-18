# +------------------------------------------------+ 
# |                     PARSE                      |
# +------------------------------------------------+

# Description:
# This file contains functions to parse the input
# arguments and files.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *

# +------------------- FUNCTIONS ------------------+

def ft_parse(file_path: str) -> None:
    
    """
    Function that parses the input arguments and files.

    Parameters:
        file_path (str): Path to the file.
    Returns:
        None
    Raises:
        Exception: If an error occurs when opening,
            reading or closing the file.
    """

    fd = ft_file_open(file_path)
    lines = fd.readlines()
    ft_file_close(fd)
    print(lines)