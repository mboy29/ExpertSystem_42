# +------------------------------------------------+ 
# |                     PARSE                      |
# +------------------------------------------------+

# Description:
# This file contains functions to parse the input
# arguments and files, while ensuring syntax is 
# validated by check.py.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *
import re  # Ensure re is imported for regex operations

# +------------------- FUNCTIONS ------------------+

def ft_parse_strip(lines: list) -> list:
    
    """
    Function that strips/clears all lines from the input file.
    It removes all whitespaces (spaces, tabs, etc.), empty lines
    and comments, returning a cleared list of lines with only rules,
    facts, and queries.

    Parameters:
        lines (list): List of lines from the input file.
    Returns:
        stripped_lines (list): List of stripped lines.
    Raises: None
    """
    
    stripped_lines = []
    for line in lines:
        stripped = re.sub(r'\s+', '', line).split('#')[0]
        if not stripped or (len(stripped) > 0 and stripped[0] == '#'):
            continue
        stripped_lines.append(stripped)
    return stripped_lines


def ft_parse_rules_tokenize(expression: str) -> list:
    """
    Converts an infix logical expression to Reverse Polish Notation (RPN)
    using the Shunting Yard algorithm.

    Parameters:
        expression (str): Logical expression in infix notation.
    Returns:
        list: Logical expression in RPN.
    """
    precedence = {
        '|': 1,  # OR
        '+': 2,  # AND
        '^': 3,  # XOR
        '!': 4,  # NOT
    }
    output = []
    operators = []

    # Tokenize the expression into valid components (facts, operators, parentheses)
    tokens = re.findall(r'[A-Z]|[+|^!()]', expression)

    for token in tokens:
        if token.isalpha():  # If token is a fact
            output.append(token)
        elif token == '!':  # Unary NOT operator
            operators.append(token)
        elif token in precedence:  # Binary operators
            while (operators and operators[-1] != '(' and
                   precedence.get(operators[-1], 0) >= precedence[token]):
                output.append(operators.pop())
            operators.append(token)
        elif token == '(':  # Left parenthesis
            operators.append(token)
        elif token == ')':  # Right parenthesis
            # Pop operators to output until a left parenthesis is found
            while operators and operators[-1] != '(':
                output.append(operators.pop())
            if operators and operators[-1] == '(':
                operators.pop()  # Remove the '('

    # Pop all remaining operators
    while operators:
        output.append(operators.pop())

    return output


def ft_parse_rules(data: Data, rule: str) -> None:
    """
    Processes a rule by validating and parsing it into RPN. 
    The rule is then added to the Data object and linked to the relevant facts.

    Parameters:
        data (Data): Data object to which the parsed rule is added.
        rule (str): The rule string to be parsed.
    Returns:
        None
    Raises:
        Exception: If the rule is invalid or cannot be parsed.
    """
    ft_check_rules(data, rule)

    # Split rule into condition (LHS) and conclusion (RHS)
    splitted = re.split(r'\s*=>\s*|\s*<=>\s*', rule)
    is_biconditional = '<=>' in rule
    if is_biconditional is True:
        implied = Operator.IMPLIES_BI
    else:
        implied = Operator.IMPLIES
    condition_rpn = ft_parse_rules_tokenize(splitted[0])
    conclusion_rpn = ft_parse_rules_tokenize(splitted[1])

    # Validate RPN syntax
    ft_check_rules_rpn(condition_rpn)
    ft_check_rules_rpn(conclusion_rpn)

    # Create the RuleNode and add it to the Data object
    rule_node = RuleNode(rule, condition_rpn, implied, conclusion_rpn)
    data.add_rule(rule_node)

    # Link the rule to the relevant facts
    for token in set(condition_rpn + conclusion_rpn):
        if token.isalpha():
            data.add_fact(token)
            data.facts[token].add_rule(rule_node)

    # If it's a biconditional rule, add the reverse rule
    if is_biconditional:
        reverse_rule_node = RuleNode(rule, conclusion_rpn, implied, condition_rpn)
        data.add_rule(reverse_rule_node)
        for token in set(condition_rpn + conclusion_rpn):
            if token.isalpha():
                data.facts[token].add_rule(reverse_rule_node)



def ft_parse_facts(data: Data, facts: str) -> None:
    
    """
    Parses the initial facts from the input file, validates them,
    and updates the Data object with the corresponding FactNodes.

    Parameters:
        data (Data): Data object.
        facts (str): String containing facts.
    Returns:
        None
    Raises:
        Exception: If the facts are invalid.
    """
    
    ft_check_facts(data, facts)
    data.init_facts = True
    for fact in facts:
        data.add_fact(fact)
        data.facts[fact].value = True


def ft_parse_queries(data: Data, queries: str) -> None:
    
    """
    Parses the queries from the input file, validates them,
    and adds them to the Data object.

    Parameters:
        data (Data): Data object.
        queries (str): String containing queries.
    Returns:
        None
    Raises:
        Exception: If the queries are invalid.
    """
    
    ft_check_queries(data, queries)
    for query in queries:
        data.add_query(query)


def ft_parse_line(data: Data, line: str) -> None:
    
    """
    Parses a single line from the input file. It determines if the line
    is a query, fact, or rule and processes it accordingly.

    Parameters:
        data (Data): Data object.
        line (str): Line to be parsed.
    Returns:
        None
    Raises:
        Exception: If the line cannot be parsed.
    """
    
    op = line[0]
    if Operator.is_query(op):
        ft_parse_queries(data, line[1:])
    elif Operator.is_fact(op) and (len(line) == 1 or (len(line) > 1 and line[1] != '>')):
        ft_parse_facts(data, line[1:])
    else:
        ft_parse_rules(data, line)


def ft_parse(data: Data) -> None:
    
    """
    Parses the input file and updates the Data object with rules, facts, and queries.

    Parameters:
        data (Data): Data object to be updated.
    Returns:
        None
    Raises:
        Exception: If there is an error in parsing the input file.
    """
    
    try:
        Logger.info(f"Parsing data from file '{data.file}'...")
        lines = ft_file_read(data.file)
        stripped_lines = ft_parse_strip(lines)
        for line in stripped_lines:
            ft_parse_line(data, line)
        ft_check(data)
        Logger.success("Data parsed successfully.\n")
        return data
    except Exception as e:
        raise Exception(f"Invalid input file. {str(e)}")
