# +------------------------------------------------+ 
# |                      FILE                      |
# +------------------------------------------------+

# Description:
# This file contains functions to handle files, 
# such as opening, reading and closing.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *

# +------------------- FUNCTIONS ------------------+

def ft_file_open(file_path: str) -> int:

    """
    Function that opens a file.

    Parameters:
        file_path (str): Path to the file.
    Returns:
        fd (int): File descriptor.
    Raises:
        Exception: If the file is not found, permission is 
            denied, is a directory or any other error that 
            can occur when opening a file.
        
    """

    try:
        fd = open(file_path, 'r')
    except FileNotFoundError:
        raise Exception(f"File '{file_path}' not found.")
    except PermissionError:
        raise Exception(f"Permission denied for file '{file_path}'")
    except IsADirectoryError:
        raise Exception(f"file '{file_path}' is a directory.")
    except Exception as e:
        raise Exception(f"Error opening file '{file_path}': {e}")
    return fd

def ft_file_close(fd: int) -> None:

    """
    Function that closes a file.

    Parameters:
        fd (TextIO): File descriptor.
    Returns: 
        None
    Raises:
        Exception: If an error occurs when closing the file.
    """

    try:
        fd.close()
    except Exception as e:
        raise Exception(f"Error closing file: {e}")

def ft_file_read(file_path: str) -> list:

    """
    Function that reads a file.

    Parameters:
        file_path (str): Path to the file.
    Returns:
        lines (List[str]): List of lines read from the file.
    Raises:
        Exception: If an error occurs when reading the file.
    """

    try:
        fd = ft_file_open(file_path)
        lines = fd.readlines()
        if len(lines) == 0:
            raise Exception(f"File '{file_path}' is empty.")
        ft_file_close(fd)
        return lines
    except Exception as e:
        raise Exception(e)