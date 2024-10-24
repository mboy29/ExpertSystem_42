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

def ft_build_ast(data, expr: str) -> ASTExprNode:
    """
    Converts a string expression into an Abstract Syntax Tree (AST).
    Handles operator precedence and parenthesis correctly.
    
    Parameters:
        expr (str): The string expression to be parsed.
    
    Returns:
        ASTExprNode: The root of the AST representing the parsed expression.
    """
    def precedence(op):
        if op == '!':
            return 3
        elif op in ('+',):
            return 2
        elif op in ('|',):
            return 1
        return 0

    def shunting_yard(dara, tokens):
        output = []
        operators = []
        for token in tokens:
            if token.isalpha():  # Operand
                fact = data.get_graph().get_fact_node(token)
                output.append(ASTFactNode(fact))
            elif token == '!':  # NOT operator
                operators.append(Operator.NOT)
            elif token in ('+', '|'):
                while (operators and precedence(operators[-1].get_symbol()) >= precedence(token)):
                    op = operators.pop()
                    if op == Operator.NOT:
                        output.append(ASTNotNode(output.pop()))
                    else:
                        right = output.pop()
                        left = output.pop()
                        output.append(ASTOperatorNode(op, left, right))
                operators.append(Operator.get_operator(token))
            elif token == '(':
                operators.append(token)
            elif token == ')':
                while operators and operators[-1] != '(':
                    op = operators.pop()
                    if op == Operator.NOT:
                        output.append(ASTNotNode(output.pop()))
                    else:
                        right = output.pop()
                        left = output.pop()
                        output.append(ASTOperatorNode(op, left, right))
                operators.pop()  # Remove the '('
        while operators:
            op = operators.pop()
            if op == Operator.NOT:
                output.append(ASTNotNode(output.pop()))
            else:
                right = output.pop()
                left = output.pop()
                output.append(ASTOperatorNode(op, left, right))
        
        return output[0]  # Return the root of the AST

    # Tokenization
    tokens = re.findall(r'[A-Z]|\!|\+|\||\(|\)', expr)
    return shunting_yard(data, tokens)




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
    if "<=>" in rule:
        parts = rule.split("<=>")
        operator = Operator.IMPLIES_BI
    elif "=>" in rule:
        parts = rule.split("=>")
        operator = Operator.IMPLIES
    else:
        raise Exception(f"Invalid rule format: {rule}")

    lhs, rhs = parts[0], parts[1]

    # Build AST for left-hand side (premises) and right-hand side (conclusions)
    lhs_ast = ft_build_ast(data, lhs)  # Will be responsible for tokenizing the expression and building AST
    rhs_ast = ft_build_ast(data, rhs)

    # Combine into one AST where the root is the implication operator
    ast = AST()
    ast.set_root(ASTOperatorNode(operator, lhs_ast, rhs_ast))

    # Add the rule's AST to the graph
    data.graph.add_rule(ast)

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
    data.set_initial_facts([])
    for fact in facts:
        fact_node = data.graph.get_fact_node(fact)
        data.add_initial_fact(fact)
        fact_node.value = True  # Set the fact as True
        


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
