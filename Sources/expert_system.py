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


def ft_evaluate_rule(rule_node: RuleNode, data: Data) -> bool:
    
    """
    Evaluates a RuleNode to determine if the left side (antecedents) 
    of the rule can be satisfied.
    
    Parameters:
        rule_node (RuleNode): The rule node to evaluate.
        data (Data): The data structure containing facts 
            and rules.
    Returns:
        bool: True if the left side of the rule is satisfied, 
             baelse False.
    Raises: None
    """
    
    left, _ = rule_node.split()
    return all(ft_resolve_rule(node, data) for node in rule_node.extract_left())

def ft_resolve_rule(node: DefaultNode, data: Data) -> bool:
    
    """
    Recursively evaluates a logical expression represented by a 
    node in the AST.
    
    Parameters:
        node (DefaultNode): The root of the expression to evaluate.
        data (Data): The data structure containing facts and rules.
    Returns:
        bool: True if the expression is satisfied, else False.
    Raises: None
    """
    
    if isinstance(node, FactNode):
        fact = data.get_fact(node.get_fact())
        if fact is None:
            return False
        if not fact.get_known():
            ft_resolve_query(data, fact)
        return fact.get_state()
    elif isinstance(node, NotNode):
        return not ft_resolve_rule(node.get_operand(), data)
    elif isinstance(node, OperatorNode):
        left_result = ft_resolve_rule(node.get_left(), data)
        right_result = ft_resolve_rule(node.get_right(), data)
        if node.get_operator() == Operator.AND:
            return left_result and right_result
        elif node.get_operator() == Operator.OR:
            return left_result or right_result
        elif node.get_operator() == Operator.XOR:
            return left_result ^ right_result
    return False

def ft_resolve_query(data: Data, query: FactNode) -> None:

    """
    Resolves a query using backward chaining to determine the
    state of the query fact.
    If the query is already known, it returns the state directly.
    Otherwise, it iterates over the rules associated with the
    query fact on the right side and evaluates the left side
    (antecedent) to see if it can be satisfied.

    Parameters:
        data (Data): The data structure containing facts 
            and rules.
        query (FactNode): The query to resolve.
    Returns: None
    Raises: None
    """

    if query.get_known():
        if data.get_verbose():
            Logger.verbose("established", query)
        return query.get_state()

    for rule in query.get_rules_right():
        if not any(isinstance(node, FactNode) and node.get_fact() == query.get_fact() for node in rule.extract_right()):
            continue
        if ft_evaluate_rule(rule, data):
            query.set_state(True)
            data.set_fact(query, True)
            if data.get_verbose():
                Logger.verbose("satisfied", query, rule)
            return True
        elif data.get_verbose():
            Logger.verbose("failed", query, rule)

    if not query.get_known():
        query.set_state(False)
        data.set_fact(query, False)
        if data.get_verbose():
            Logger.verbose("unknown", query)
    return False

def ft_resolve_queries(data: Data) -> None:
        
    """
    Resolves all queries in the data structure.
    
    Parameters:
        data (Data): The data structure containing facts
            and rules.
    Returns: None
    """
    
    for query in data.get_queries():
        Logger.info(f"Querying {query.get_fact()}...")
        ft_resolve_query(data, query)

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

    Logger.info("Starting backward chaining...\n")
    ft_resolve_queries(data)
    Logger.success("Backward chaining completed.\n")

    results = []
    for query in data.get_queries():
        result = query.get_state()
        if result is not None: results.append(f"{query.get_fact()} = {result}")
        else: results.append(f"{query.get_fact()} = Unknown")
    if len(results) > 1: final_output = ", ".join(results[:-1]) + " and " + results[-1]
    else: final_output = results[0]
    Logger.info(f"Final Results: {final_output}")