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

# +------------------- LOGGER CLASS ------------------+

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
    def usage() -> None:
        print(f'{INFO}[USAGE] python3 -B main.py [input_file]{RESET}', end='\n')
        print(f'{INFO} - [input_file]: File to read and parse from.{RESET}', end='\n')

    @staticmethod
    def success(message: str, endswith: str = '\n') -> None:
        print(f'{SUCCESS}[SUCCESS] {message}{RESET}', end=endswith)

    @staticmethod
    def error(message: str, endswith: str = '\n') -> None:
        print(f'{ERROR}[ERROR] {message}{RESET}', end=endswith)

    @staticmethod
    def logic(logic: str, fact: FactNode, rule: RuleNode = None) -> None:
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

