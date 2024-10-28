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
    tokens = re.findall(r'[A-Z]|[+|^!()=><]', rule)
    if not ft_check_rules_structure(tokens):
        raise Exception(f"Invalid structure in rule: {rule}.")
    if not ft_check_rules_tokens(rule):
        raise Exception(f"Invalid characters in rule: {rule}.")  
    if not ft_check_rules_parentheses(rule):
        raise Exception(f"Unbalanced parentheses in rule: {rule}.")
    

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

    # if data.get_facts() is not None:
    #     raise Exception(f"Rules must be declared before facts and queries: {rule}.")
    splitted = re.split(r'\s*=>\s*|\s*<=>\s*', rule)
    if len(splitted) != 2:
        raise Exception(f"Rules must contain one implication operator: {rule}.")
    if len(splitted[0]) == 0 or len(splitted[1]) == 0:
        raise Exception(f"Rules must contain expressions on both sides of the operator: {rule}.")
    ft_check_rules_syntax(data, splitted[0])
    ft_check_rules_syntax(data, splitted[1])


def ft_check_facts(data: Data, facts: str) -> None:

    """
    Main function to check if a fact is valid.
    It checks the position of the declaration of the facts
    and raises an exception if the fact is empty or declared
    before queries.

    Parameters:
        data (Data): Data object.
        facts (str): Facts.
    Returns: None
    Raises:
        Exception if fact is invalid.
    """

    valid_facts = set('ABCDEFGHIJKLMNOPQRSTUVWXYZ')

    # if data.get_queries() is not None:
    #     raise Exception("Facts must be declared before queries.")
    if data.get_initial_facts() is True:
        raise Exception("Multiple declarations of facts.")
        
    for fact in facts:
        if fact not in valid_facts:
            raise Exception(f"Invalid fact statement: {facts}.")

def ft_check_queries(data: Data, query: str) -> None:

    """
    Main function to check if a query is valid.
    It checks the position of the declaration of the queries
    and raises an exception if the query is empty or declared
    before facts.

    Parameters:
        data (Data): Data object.
        query (str): Query.
    Returns: None
    Raises:
        Exception if query is invalid.
    """

    valid_queries = set('ABCDEFGHIJKLMNOPQRSTUVWXYZ')

    # if data.get_facts() is None:
    #     raise Exception("Queries must be declared after facts.")
    if data.get_queries() is not None:
        raise Exception("Multiple declarations of queries.")
    elif len(query) == 0:
        raise Exception("Query is empty.")
    for elem in query:
        if elem not in valid_queries:
            raise Exception(f"Invalid query statement: {query}.")


def ft_check(data: Data) -> None:

    """
    Final check function to verify if the data parsed
    is valid. It checks if the data object contains
    facts, queries and rules.

    Parameters:
        data (Data): Data object.
    Returns: None
    Raises:
        Exception if data is missing facts, queries or rules.
    """

    if data.get_facts() is None or data.get_queries() is None or data.get_rules() is None or data.get_initial_facts() is False: 
        raise Exception("Missing one or multiple of facts, queries or rules.");
    
