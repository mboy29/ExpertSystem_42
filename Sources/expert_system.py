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
    # print(f'ft_evaluate_rpn: {rpn}')
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
    Attempts to resolve a fact using backward chaining with proper handling of negation.

    Parameters:
        data (Data): The data structure containing facts and rules.
        fact (FactNode): The fact to resolve.
        resolving (set): A set of facts currently being resolved to detect cycles.
    Returns:
        bool: True if the fact is resolved to True, False otherwise.
    """
    # print(f'ft_resolve_fact: {fact.name}')
    if fact.value is True:  # If the fact is already True, return True
        return True

    if resolving is None:
        resolving = set()
    if fact.name in resolving:  # Detect actual circular dependencies
        # print(f"Circular dependency detected for fact {fact.name}.")
        return False

    resolving.add(fact.name)  # Mark the fact as being resolved

    for rule in fact.rules:  # Iterate over rules that influence the fact
        # print(f'Checking rule: {rule}')
        condition_met = True
        stack = []

        # Evaluate the RPN condition while resolving dependencies
        for token in rule.condition:
            if token.isalpha():  # Regular fact
                # print(f'Fact found: {token}')
                if not ft_resolve_fact(data, data.facts[token], resolving):
                    stack.append(False)
                else:
                    stack.append(data.facts[token].value)
            elif token == '!':  # Negation
                if not stack:
                    condition_met = False
                    break
                stack.append(not stack.pop())
            elif token == '+':  # AND operator
                if len(stack) < 2:
                    condition_met = False
                    break
                b, a = stack.pop(), stack.pop()
                stack.append(a and b)
            elif token == '|':  # OR operator
                if len(stack) < 2:
                    condition_met = False
                    break
                b, a = stack.pop(), stack.pop()
                stack.append(a or b)
            elif token == '^':  # XOR operator
                if len(stack) < 2:
                    condition_met = False
                    break
                b, a = stack.pop(), stack.pop()
                stack.append(a ^ b)

        # Final condition evaluation
        if not stack:
            condition_met = False
        else:
            condition_met = stack.pop()

        # If the condition is met, resolve all conclusions
        if condition_met:
            for conclusion in rule.conclusions:
                if conclusion.isalpha() and conclusion == fact.name:
                    fact.value = True
                    resolving.remove(fact.name)
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

    # Resolve each query dynamically with backward chaining
    results = {}
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
