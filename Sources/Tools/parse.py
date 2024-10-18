# +------------------------------------------------+ 
# |                     PARSE                      |
# +------------------------------------------------+

# Description:
# This file contains functions to parse the input
# arguments and files.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *

# +------------------- FUNCTIONS ------------------+

def ft_parse_strip(lines: list) -> list:

    """
    Function that strips/clears all lines from the
    input file. It removes all whitespaces (spaces, 
    tabs, etc.), empty lines and comments : returning
    a cleared list of lines with only rules, facts
    and queries.

    Parameters:
        lines (list): List of
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

def ft_split_rules_left(expression: str):

    """
    Function that splits the rules by the operators
    and parentheses and returns a list of expressions.

    Parameters:
        expression (str): Expression.
    Returns:
        elements (list): List of expressions.
    Raises: None
    """

    elements = []
    current = []
    stack = []
    i = 0

    while i < len(expression):
        char = expression[i]

        if char == '(':
            if current:
                elements.append(''.join(current).strip())
                current = []
            stack.append(len(elements))
            current.append(char)
        elif char == ')':
            current.append(char)
            if stack:
                start_index = stack.pop()
                if current:
                    elements.append(''.join(current).strip())
                current = []
                sub_expr = ''.join(elements[start_index:]).strip()
                elements = elements[:start_index]
                elements.append(sub_expr)
        elif char in ' +|^':
            if current:
                elements.append(''.join(current).strip())
                current = []
            if char.strip():
                elements.append(char)
        else:
            current.append(char)

        i += 1

    if current:
        elements.append(''.join(current).strip())

    return [elem for elem in elements if elem]  

def ft_check_rules_left(data: Data, rule: str) -> None:

    """
    Function that checks if the left side of the rules
    is valid. It checks if the expression contains only
    valid characters, if the parentheses are balanced,
    and if the operators are valid (at least one, not,
    at the beginning or end, not two in a row).

    Parameters:
        data (Data): Data object.
        rule (str): Rule.
    Returns:
        expressions (list): List of expressions.
    Raises:
        Exception: If the rules contain invalid characters,
            unbalanced parentheses, no operators, operators
            at the beginning or end, or two operators in a row.
    """

    for elem in rule:
        if not CalcOperators.is_operator(elem) and not elem.isalpha():
            raise Exception(f"rules must only contain valid operators and characters {rule}.")
    expressions = ft_split_rules_left(rule)

    open_count = sum(1 for char in rule if char == '(')
    close_count = sum(1 for char in rule if char == ')')
    if open_count != close_count:
        raise Exception(f"rules must contain balanced parentheses {rule}.")
    
    operators = [(elem, idx) for idx, elem in enumerate(expressions) if CalcOperators.is_operator(elem)]
    if len(operators) == 0:
        raise Exception(f"rules must contain at least one operator {rule}.")
    if operators[0][1] == 0 or operators[-1][1] == len(expressions) - 1:
        raise Exception(f"rules containers an invalid expression {rule}.")
    for operator in operators:
        if operator[1] % 2 == 0:
            raise Exception(f"rules must contain a valid expression {rule}.")
    return expressions

def ft_parse_rules_left(data: Data, rule: str) -> None:

    """
    Function that parses the left side of the rules.
    Checks if the expression is valid and adds, formats
    it and stores it in the Data object.

    Parameters:
        data (Data): Data object.
        rule (str): Rule.
    Returns: None
    Raises: None
    """

    expressions = ft_check_rules_left(data, rule)
    for idx, expr in enumerate(expressions):
        if expr.startswith('(') and expr.endswith(')'):
            inner_expr = expr[1:-1]
            ft_parse_rules_left(data, inner_expr) 


def ft_parse_rules(data: Data, rule: str) -> None:
    
    """
    Function that parses the facts from the input file.
    If the operator is a fact, it adds the values to the
    Data object.

    Parameters:
        data (Data): Data object.
        rule (str): Rule.
    Returns: None
    Raises: None
    """
    
    if data.get_rules() is not None or data.get_facts() is not None:
        raise Exception("rules must be declared before facts and queries.")
    splitted = re.split(r'\s*=>\s*|\s*<=>\s*', rule)
    if len(splitted) != 2:
        raise Exception("rules must contain one implication operator.")
    ft_parse_rules_left(data, splitted[0])
    

def ft_parse_facts(data: Data, facts: str) -> None:

    """
    Function that parses the facts from the input file.
    If the operator is a fact, it adds the values to the
    Data object.

    Parameters:
        data (Data): Data object.
        op (str): Operator.
        values (str): Values.
    Returns: None
    Raises: None
    """

    # if data.get_rules() is None:
    #     raise Exception("facts must be declared after rules.")
    if data.get_queries() is not None:
        raise Exception("facts must be declared before queries.")
    if data.get_facts() is not None:
        raise Exception("multiple declarations of facts.")
    data.set_facts([])
    for fact in facts:
        data.add_fact(fact)

def ft_parse_queries(data: Data, queries: str) -> None:

    """
    Function that parses the queries from the input file.
    If the operator is a query, it adds the values to the
    Data object.

    Parameters:
        data (Data): Data object.
        op (str): Operator.
        values (str): Values.
    Returns: None
    Raises:
        Exception: If the query is empty.
    """

    # if data.get_rules() is None:
    #     raise Exception("queries must be declared after rules.")
    if data.get_facts() is None:
        raise Exception("queries must be declared after facts.")
    if data.get_queries() is not None:
        raise Exception("multiple declarations of queries.")
    elif (len(queries) == 0):
        raise Exception("query is empty.")
    data.set_queries([])
    for query in queries:
        data.add_query(query)

def ft_parse_line(data: Data, line: str) -> None:

    """
    Function that parses a line from the input file.
    It checks if the line is a query or a fact and
    adds the values to the Data object.

    Parameters:
        data (Data): Data object.
        line (str): Line.
    Returns: None
    Raises: None
    """
    
    op = line[0]
    if CalcOperators.is_query(op):
        ft_parse_queries(data, line[1:])
    elif CalcOperators.is_fact(op):
        ft_parse_facts(data, line[1:])
    else:
        ft_parse_rules(data, line)


def ft_parse(file_path: str) -> None:
    
    """
    Function that parses the input arguments and files.

    Parameters:
        file_path (str): Path to the file.
    Returns: None
    Raises:
        Exception: If an error occurs when opening,
            reading or closing the file.
    """
    data = Data()

    try:
        lines = ft_file_read(file_path)
        stripped_lines = ft_parse_strip(lines)
        for line in stripped_lines:
            ft_parse_line(data, line)
        
        print("--> ", data)
    except Exception as e:
        raise Exception(f"Invalid input file, {e}")

