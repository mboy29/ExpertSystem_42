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
VERBOSE = f"{fg(255, 255, 255)}"
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
    def header() -> None:
        header = "\n----------------------------------------------------------------------------\n"
        header += " ____  _  _  ____  ____  ____  ____      ____  _  _  ____  ____  ____  _  _ \n"
        header += "(  __)( \\/ )(  _ \\(  __)(  _ \\(_  _)___ / ___)( \\/ )/ ___)(_  _)(  __)( \\/ )\n"
        header += " ) _)  )  (  ) __/ ) _)  )   /  )( (___)\\___ \\ )  / \\___ \\  )(   ) _) / \\/ \\ \n"
        header += "(____)(_/\\_)(__)  (____)(__\\_) (__)     (____/(__/  (____/ (__) (____)\\_)(_/\n"
        header += "\n---------------------------------------------------------- BY MBOY - 42 NICE\n"
        print(f'{INFO}{header}{RESET}', end='\n\n')
    
    @staticmethod
    def verbose(logic: str, data) -> None:
        if logic == "RULE":
            return f"{VERBOSE}[VERBOSE] Resolving rule {data['rule']} for fact '{data['fact']}'{RESET}\n"
        elif logic == "FACT":
            return (f"  - We know that '{data['name']}' is {data['value'] if data['value'] is not None else 'Undetermined'}.\n"
                f"    ∃ fact: {data['name']} = {data['value'] if data['value'] is not None else 'unknown'}")
        elif logic == "NOT":
            return ( f"  - Applying NOT: ¬{data} → {not data}.\n"
                f"    ¬{data} = {not data}")
        elif logic == "AND":
            return (f"  - Applying AND: {data['a']} ∧ {data['b']} → {data['a'] and data['b']}.\n"
                f"    ({data['a']} ∧ {data['b']}) = {data['a'] and data['b']}")
        elif logic == "OR":
            return (f"  - Applying OR: {data['a']} ∨ {data['b']} → {data['a'] or data['b']}.\n"
                f"    ({data['a']} ∨ {data['b']}) = {data['a'] or data['b']}")
        elif logic == "XOR":
            return (f"  - Applying XOR: '{data['a']} ⊕ {data['b']} → {data['a'] ^ data['b']}'\n"
                f"    ({data['a']} ⊕ {data['b']}) = {data['a'] ^ data['b']}")
        elif logic == "RES":
            if data['res']:
                return (f"  - {SUCCESS}Rule satisfied '{data['exp']}'. "
                f"Therefore, '{data['con']}' is set to {data['res']}.{RESET}")
            return (f"  - {ERROR}Failed to satisfy rule: '{data['exp']}'. "
                f"Therefore, '{data['con']}' remains {data['res']}.{RESET}")
        elif logic == "PRINT":
            print(f"{data}\n")
        elif logic == "CONDITION":
            return (
                f"  - We know that '{data['a'].name} {data['operator']} {data['b'].name}' is {data['res']}.\n"
                f"    ∃ fact: {data['a'].name} {data['operator']} {data['b'].name} = {data['res']}\n"
                f"  - {SUCCESS}Resolving '{data['a'].name}' as {data['a'].value} and '{data['b'].name}' as {data['b'].value}.{RESET}\n"
            )


    def test(type: str, file: str, results: dict = None, ouput: dict = None):
        if type == "VALID_PASS":
            print(f"{SUCCESS}[PASS] {file} output matched expected results.{RESET}")
        elif type == "VALID_FAIL":
            expected_str = ', '.join(f"'{k}': {v}" for k, v in results.items()) if len(results) > 0 else "None"
            actual_str = ', '.join(f"'{k}': {v}" for k, v in ouput.items()) if len(ouput) > 0 else "None"
            print(f"{ERROR}[FAIL] {file} output did not match expected results.\n"
                f"  - Expected results: {expected_str}\n"
                f"  - Actual results: {actual_str}{RESET}")
        elif type == "INVALID_FAIL":
            print(f"{ERROR}[FAIL] {file} dit not end with an error.{RESET}")
        elif type == "INVALID_PASS":
            print(f"{SUCCESS}[PASS] {file} ended with an error as expected.{RESET}")
