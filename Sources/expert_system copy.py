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
def evaluate_query(data: Data, query: str) -> str:
    """
    Evaluate a query using backward chaining.

    Parameters:
        data (Data): The data structure containing facts and rules.
        query (str): The fact to evaluate.

    Returns:
        str: "True", "False", or "Undetermined".
    """
    fact = data.facts.get(query)
    if not fact:
        raise ValueError(f"Fact '{query}' is not defined in the ruleset.")
    result = _evaluate_fact(data, fact)
    return "True" if result is True else "False" if result is False else "Undetermined"


def _evaluate_fact(data: Data, fact: FactNode) -> bool:
    """
    Recursively evaluate a fact using its rules.

    Parameters:
        data (Data): The data structure containing facts and rules.
        fact (FactNode): The fact to evaluate.

    Returns:
        bool: True, False, or None (undetermined).
    """
    if fact.value is not None:  # Already determined
        return fact.value

    results = []
    for rule in fact.rules:
        condition_met = _evaluate_rpn(data, rule.get_condition())
        if condition_met is True:
            for conclusion in rule.get_conclusions():
                if conclusion == fact.name:
                    results.append(True)
                else:
                    dependent_fact = data.facts.get(conclusion)
                    if dependent_fact:
                        results.append(_evaluate_fact(data, dependent_fact))
        else:
            results.append(False)

    if all(res is True for res in results):
        fact.value = True
    elif all(res is False for res in results):
        fact.value = False
    else:
        fact.value = None
    return fact.value


def _evaluate_rpn(data: Data, rpn: list) -> bool:
    """
    Evaluate a condition in reverse Polish notation (RPN).

    Parameters:
        data (Data): The data structure containing facts and rules.
        rpn (list): The RPN expression.

    Returns:
        bool: The evaluation result.
    """
    stack = []
    for token in rpn:
        if token.isalpha():  # It's a fact
            stack.append(data.facts[token].value)
        elif token in Operator.__members__:
            op = Operator.get_operator(token)
            if op == Operator.NOT:
                operand = stack.pop()
                stack.append(not operand)
            elif op in {Operator.AND, Operator.OR, Operator.XOR}:
                b = stack.pop()
                a = stack.pop()
                if op == Operator.AND:
                    stack.append(a and b)
                elif op == Operator.OR:
                    stack.append(a or b)
                elif op == Operator.XOR:
                    stack.append(a ^ b)
            elif op == Operator.IMPLIES:
                b = stack.pop()
                a = stack.pop()
                stack.append(not a or b)
            elif op == Operator.IMPLIES_BI:
                b = stack.pop()
                a = stack.pop()
                stack.append(a == b)
    return stack[0]


def ft_expert_system(data: Data) -> None:
    """
    Main function to run the expert system using backward chaining.
    Resolves all queries in the data structure and outputs the results.

    Parameters:
        data (Data): The data structure containing facts, rules, and queries.
    """
    Logger.info("Starting backward chaining...", endswith=f"{'\n\n' if data.verbose else '\n'}")
    results = {}

    print(data.facts)
    for query in data.queries:  # Ensure all queries are processed
        try:
            results[query] = evaluate_query(data, query)
        except Exception as e:
            results[query] = f"Error: {e}"

    Logger.info("Query results:")
    for query in data.queries:  # Display results for each query
        result = results.get(query, "Undetermined")
        Logger.info(f"{query}: {result}")

    print("\nFinal Query Results:")
    for query, result in results.items():
        print(f"{query}: {result}")

