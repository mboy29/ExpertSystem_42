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

# +------------------- FUNCTIONS ------------------+

def ft_main(args: list) -> None:
    if len(args) != 1:
        ft_exit("Invalid argument(s)", 1, True)
    ft_parse(args[0])
    

if __name__ == "__main__":
    try:
        ft_main(sys.argv[1:])
    except Exception as e:
        ft_exit(str(e), 1, True)