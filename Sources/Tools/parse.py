# +------------------------------------------------+ 
# |                     PARSE                      |
# +------------------------------------------------+

# Description:
# This file contains functions to parse the input
# arguments and files.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *
import re  # Ensure re is imported for regex operations

# +------------------- FUNCTIONS ------------------+

def ft_parse_strip(lines: list) -> list:
    
    """
    Function that strips/clears all lines from the
    input file. It removes all whitespaces (spaces, 
    tabs, etc.), empty lines and comments : returning
    a cleared list of lines with only rules, facts
    and queries.

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


def ft_parse_rules_tokenize(data, rule: str) -> list:
    
    """
    Function that tokenizes a rule into its constituent
    parts. It identifies operators and facts, converting
    them into a structured format for further processing.

    Parameters:
        rule (str): The rule string to tokenize.
    Returns:
        tokens (list): A list of tokens representing
        operators and facts.
    Raises: None
    """
    
    token_list = re.findall(r'[A-Z]|[+|^!()=><]', rule)
    
    tokens = []
    for token in token_list:
        operator = Operator.get_operator(token)
        if operator:
            tokens.append(operator)
        else:
            factRef = data.get_fact(token)
            if factRef is None:
                data.add_fact(token, False)
                factRef = data.get_fact(token) 
            tokens.append(factRef) 
    return tokens

def ft_parse_rules_expression(tokens: list) -> DefaultNode:
    
    """
    Function that constructs an abstract syntax tree (AST)
    from the provided list of tokens. It recursively parses
    the tokens to create nodes representing logical expressions.

    Parameters:
        tokens (list): A list of tokens representing
        the rule to be parsed.
    Returns:
        DefaultNode: The root node of the constructed AST.
    Raises:
        Exception: If the tokens do not represent a valid
        expression.
    """
    
    def parse_primary() -> DefaultNode:
        token = tokens.pop(0)
        if token == Operator.PRIORITY_LEFT:
            node = ft_parse_rules_expression(tokens) 
            tokens.pop(0)
            return node
        elif token == Operator.NOT:
            return NotNode(parse_primary())
        else:
            return token

    def parse_term() -> DefaultNode:
        node = parse_primary()
        while tokens and tokens[0] == Operator.XOR:
            operator = tokens.pop(0)
            node = OperatorNode(operator, node, parse_primary())
        return node

    def parse_full_expression() -> DefaultNode:
        node = parse_term()
        while tokens and tokens[0] in (Operator.OR, Operator.AND):
            operator = tokens.pop(0)
            node = OperatorNode(operator, node, parse_term())
        return node

    return parse_full_expression()  # Call the renamed function

def ft_parse_rules(data: Data, rule: str) -> None:
    
    """
    Function that processes a rule by first validating it,
    then parsing it into an abstract syntax tree (AST).
    It separates the rule into left-hand side (LHS) and
    right-hand side (RHS) based on the implication operator,
    tokenizes both sides, constructs their ASTs, and combines
    them into a final AST representing the rule.

    Parameters:
        data (Data): Data object to which the parsed rule is added.
        rule (str): The rule string to be parsed.
    Returns: None
    Raises:
        Exception: If the rule is invalid or cannot be parsed.
    """
    
    ft_check_rules(data, rule)
    if '<=>' in rule:
        lhs, rhs = rule.split('<=>')
        implication_operator = Operator.IMPLIES_BI
    else:
        lhs, rhs = rule.split('=>')
        implication_operator = Operator.IMPLIES

    lhs_tokens = ft_parse_rules_tokenize(data, lhs.strip())
    rhs_tokens = ft_parse_rules_tokenize(data, rhs.strip())
    lhs_ast = ft_parse_rules_expression(lhs_tokens)
    rhs_ast = ft_parse_rules_expression(rhs_tokens)
    data.add_rule(OperatorNode(implication_operator, lhs_ast, rhs_ast))

def ft_parse_facts(data: Data, facts: str) -> None:
    
    """
    Function that parses the facts from the input file.
    If the operator is a fact, it adds the values to the
    Data object.

    Parameters:
        data (Data): Data object.
        facts (str): String containing facts.
    Returns: None
    Raises: None
    """
    
    ft_check_facts(data, facts)
    for fact in facts:
        data.add_fact(fact, True)

def ft_parse_queries(data: Data, queries: str) -> None:
    
    """
    Function that parses the queries from the input file.
    If the operator is a query, it adds the values to the
    Data object.

    Parameters:
        data (Data): Data object.
        queries (str): String containing queries.
    Returns: None
    Raises:
        Exception: If the query is empty or declared before 
            facts.
    
    """
    ft_check_queries(data, queries)
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
    if Operator.is_query(op):
        ft_parse_queries(data, line[1:])
    elif Operator.is_fact(op) and (len(line) == 1 or (len(line) > 1 and line[1] != '>')):
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

    try:
        data = Data()
        lines = ft_file_read(file_path)
        stripped_lines = ft_parse_strip(lines)
        for line in stripped_lines:
            ft_parse_line(data, line)
        ft_check(data)
        return data
    except Exception as e:
        raise Exception(f"Invalid input file. {str(e)}")
