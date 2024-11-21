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

def ft_evaluate_rpn(data: Data, rpn: list) -> bool:
    """
    Evaluates a Reverse Polish Notation (RPN) logical expression.

    Parameters:
        data (Data): The data structure containing facts and rules.
        rpn (list): The RPN expression to evaluate.
    Returns:
        bool: The result of the evaluation.
    """
    stack = []
    for token in rpn:
        if token.isalpha():  # Fact
            value = data.facts.get(token, FactNode(token)).value
            stack.append(False if value is None else value)
        elif token == '!':  # NOT operator
            stack.append(not stack.pop())
        elif token == '+':  # AND operator
            b, a = stack.pop(), stack.pop()
            stack.append(a and b)
        elif token == '|':  # OR operator
            b, a = stack.pop(), stack.pop()
            stack.append(a or b)
        elif token == '^':  # XOR operator
            b, a = stack.pop(), stack.pop()
            stack.append(a ^ b)
    return stack.pop()

def ft_resolve_fact(data: Data, fact: FactNode, resolving: set = None) -> bool:
    """
    Attempts to resolve a fact using backward chaining with propagation.

    Parameters:
        data (Data): The data structure containing facts and rules.
        fact (FactNode): The fact to resolve.
        resolving (set): A set of facts currently being resolved to detect cycles.
    Returns:
        bool: True if the fact is resolved to True, False otherwise.
    """
    if fact.value:  # If the fact is already True, return True
        return True

    if resolving is None:
        resolving = set()
    if fact.name in resolving:  # Detect circular dependencies
        return False

    resolving.add(fact.name)  # Mark the fact as being resolved

    for rule in fact.rules:  # Iterate over rules that influence the fact
        if ft_evaluate_rpn(data, rule.condition):  # Evaluate the rule's condition
            for conclusion in rule.conclusions:
                if conclusion.isalpha() and conclusion == fact.name:
                    fact.value = True
                    resolving.remove(fact.name)  # Remove from resolving set
                    return True

    resolving.remove(fact.name)  # Remove from resolving set
    return False  # Fact cannot be resolved to True


def ft_expert_system(data: Data) -> None:
    """
    Main function to run the expert system using backward chaining.
    Resolves all queries in the data structure and outputs the results.

    Parameters:
        data (Data): The data structure containing facts and rules.
    Returns:
        None
    """
    Logger.info("Starting backward chaining...", endswith=f"{'\n\n' if data.verbose else '\n'}")

    # Step 1: Propagate initial facts
    facts_to_check = [fact for fact in data.facts.values() if fact.value]
    while facts_to_check:
        current_fact = facts_to_check.pop()
        for rule in current_fact.rules:
            if ft_evaluate_rpn(data, rule.condition):
                for conclusion in rule.conclusions:
                    if conclusion.isalpha() and conclusion in data.facts:
                        fact = data.facts[conclusion]
                        if not fact.value:  # Mark new fact as true
                            fact.value = True
                            facts_to_check.append(fact)

    # Step 2: Resolve each query
    results = {}  # Dictionary to store the results of each query
    for query in data.queries:
        if query not in data.facts:
            results[query] = "Undetermined"
            raise Exception(f"Query {query} is not a valid fact. Marking as Undetermined.")

        # Resolve the fact
        fact = data.facts[query]
        resolved = ft_resolve_fact(data, fact)

        # Check the result
        if resolved or fact.value:
            results[query] = "True"
        else:
            results[query] = "False"

    # Output results
    for query, result in results.items():
        Logger.info(f"{query}: {result}")

    Logger.success("Backward chaining completed.\n")
