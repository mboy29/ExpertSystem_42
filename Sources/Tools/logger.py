# +------------------------------------------------+ 
# |                     LOGGER                     |
# +------------------------------------------------+

# Description:
# This class provides methods to output messages,
# colored messages, headers, and errors.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *

# +-------------------- GLOBALS -------------------+

INFO = f"{fg(204, 219, 253)}"
SUCCESS = f"{fg(207, 225, 185)}"
ERROR = f"{fg(255, 173, 194)}"
RESET = "\033[0m"
BOLD = "\033[1m"
ITALIC = "\033[3m"

# +--------------------  CLASS --------------------+

class Logger:

    """
    Logger class that provides methods to output messages,
    colored messages, headers, and errors.
    """

    def __init__(self):
        pass

    @staticmethod
    def info(message: str, endswith: str = '\n') -> None:
        print(f'{INFO}[INFO] {message}{RESET}', end=endswith)

    @staticmethod
    def usage(message: str, endswith: str = '') -> None:
        print(f'{INFO}[USAGE] {message}{RESET}', end=endswith)

    @staticmethod
    def success(message: str, endswith: str = '\n') -> None:
        print(f'{SUCCESS}[SUCCESS] {message}{RESET}', end=endswith)

    @staticmethod
    def error(message: str, endswith: str = '\n') -> None:
        print(f'{ERROR}[ERROR] {message}{RESET}', end=endswith)

    @staticmethod
    def verbose(logic: str, fact: FactNode, rule: RuleNode = None) -> None:
        fact_name = f"{BOLD}{fact.get_fact()}{RESET}"
        fact_state = f"{ITALIC}{fact.get_state()}{RESET}"
        if rule is not None:
            rule_expression = rule.visualize_expression()
            
        if logic == "established":
            print(f"  - {SUCCESS}We know that {fact_name}{SUCCESS} is {fact_state}{RESET} as it is established as a fact.\n"
                  f"    ∃ facts : {fact_name} = {fact_state} ⇒ {fact_name} ∈ KnownFacts\n")
        
        elif logic == "satisfied":
            print(f"  - {SUCCESS}We know that {fact_name}{SUCCESS} is {fact_state}{RESET} based on the satisfied rule {rule_expression}.\n"
                  f"    ∃ rules : ({rule_expression}) ⇒ {fact_name} = True\n")
        
        elif logic == "failed":
            print(f"  - {ERROR}Failed to satisfy rule for {fact_name}{RESET} based on the rule {rule_expression}.\n"
                  f"    ∃ rules : ({rule_expression}) ⇒ {fact_name} ⊥\n")
        
        elif logic == "unknown":
            print(f"  - {ERROR}Unable to deduce {fact_name} as True{RESET} based on the current knowledge. It remains unknown.\n"
                  f"    ∃ facts : {fact_name} ⊭ known facts\n")
    
    @staticmethod
    def header() -> None:
        header = "\n----------------------------------------------------------------------------\n"
        header += " ____  _  _  ____  ____  ____  ____      ____  _  _  ____  ____  ____  _  _ \n"
        header += "(  __)( \\/ )(  _ \\(  __)(  _ \\(_  _)___ / ___)( \\/ )/ ___)(_  _)(  __)( \\/ )\n"
        header += " ) _)  )  (  ) __/ ) _)  )   /  )( (___)\\___ \\ )  / \\___ \\  )(   ) _) / \\/ \\ \n"
        header += "(____)(_/\\_)(__)  (____)(__\\_) (__)     (____/(__/  (____/ (__) (____)\\_)(_/\n"
        header += "\n---------------------------------------------------------- BY MBOY - 42 NICE\n"
        # header += " _   _   _   _   _   _    ___    _        _       _  _      _             _\n"
        # header += "|_| |_| | | |_| | | |_  |  |  | | | |\\ | |_| |   |  |_| |  |  | | |  | | |_\n"
        # header += "|   | \\ |_| |   |_|  _| |  |  | |_| | \\| | | |_  |_ | | |_ |_ |_| |_ |_|  _|\n"
        print(f'{INFO}{header}{RESET}', end='\n\n')

