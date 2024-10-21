# +------------------------------------------------+ 
# |                     CHECK                      |
# +------------------------------------------------+

# Description:
# This file contains functions to check the syntax of
# the rules, facts and queries.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *

# +------------------- FUNCTIONS ------------------+

def ft_check_rules_parentheses(rule: str) -> bool:

    """
    Function that checks if a rule has balanced parentheses.
    It uses a stack to keep track of the parentheses and
    returns True if the rule has balanced parentheses.

    Parameters:
        rule (str): Rule.
    Returns:
        bool: True if the rule has balanced parentheses, False otherwise.
    Raises: None
    """

    stack = []
    for char in rule:
        if char == '(':
            stack.append(char)
        elif char == ')':
            if not stack:
                return False
            stack.pop()
    return len(stack) == 0

def ft_check_rules_structure(tokens: list) -> bool:

    """
    Function that checks if a rule has a valid structure.
    It checks if the rule has a valid structure by checking
    if the tokens are in the correct order.

    Parameters:
        tokens (list): List of tokens.
    Returns:
        bool: True if the rule has a valid structure, False otherwise.
    Raises: None
    """

    valid_facts = set('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    operators = {'+', '|', '^'}
    expect_fact = True 
    
    for i, token in enumerate(tokens):
        if token in valid_facts:
            if not expect_fact:
                return False
            expect_fact = False
        elif token in operators:
            if expect_fact:
                return False
            expect_fact = True
        elif token == '!':
            if not expect_fact:
                return False
        elif token == '(': pass
        elif token == ')': pass
        elif token == '=>' or token == '<=>': expect_fact = True
        else: return False
    return not expect_fact


def ft_check_rules_tokens(rule: str) -> bool:

    """
    Function that checks if a rule contains invalid
    characters. It uses a regular expression to match
    the rule with the pattern.

    Parameters:
        rule (str): Rule.
    Returns:
        bool: True if the rule is valid, False otherwise.
    Raises: None
    """

    pattern = r'^[A-Z+|^!()<=]+$'
    return re.match(pattern, rule) is not None

def ft_check_rules_syntax(data: Data, rule: str) -> None:
    
    """
    Main function to check if a rule is valid.
    It checks if the rule has invalid characters,
    unbalanced parentheses and invalid structure
    and raises an exception if any of these cases
    are true.

    Parameters:
        data (Data): Data object.
        rule (str): Rule to validate.
    Returns: None
    Raises:
        Exception: If the rule has invalid syntax or structure.
    """
    if not ft_check_rules_tokens(rule):
        raise Exception(f"Invalid characters in rule: {rule}.")  
    if not ft_check_rules_parentheses(rule):
        raise Exception(f"Unbalanced parentheses in rule: {rule}.")
    tokens = re.findall(r'[A-Z]|[+|^!()=><]', rule)
    if not ft_check_rules_structure(tokens):
        raise Exception(f"Invalid structure in rule: {rule}.")

def ft_check_rules(data: Data, rule: str) -> None:

    """
    Main function to check if a rule is valid.
    It checks the position of the declaration of the rules,
    if the rule has invalid implicit operators, characters,
    unbalanced parentheses and invalid structure
    and raises an exception if any of these cases
    are true.

    Parameters:
        data (Data): Data object.
        rule (str): Rule.
    Returns: None
    Raises:
        Exception if rule is invalid.
    """

    if data.get_facts() is not None:
        raise Exception(f"Rules must be declared before facts and queries: {rule}.")
    splitted = re.split(r'\s*=>\s*|\s*<=>\s*', rule)
    if len(splitted) != 2:
        raise Exception(f"Rules must contain one implication operator: {rule}.")
    ft_check_rules_syntax(data, splitted[0])
    ft_check_rules_syntax(data, splitted[1])