# +------------------------------------------------+ 
# |                 EXPERT SYSTEM                  |
# +------------------------------------------------+

# Description:
# This file contains the main expert system class that
# is responsible for managing the expert system. It
# contains the main functions that are called to run
# the expert system, and the functions to manage the
# rules, facts and queries.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *

# +------------------- FUNCTIONS ------------------+

def ft_expert_system(data: Data) -> None:
    
    """
    Main function to run the expert system using backward 
    chaining. It resolves all queries in the data structure 
    and outputs the results.

    Parameters:
        data (Data): The data structure containing facts 
            and rules.
    Returns: None
    Raises: None
    """

    Logger.info("Starting backward chaining...", endswith=f"{'\n\n' if data.verbose else '\n'}")
    print(data)
