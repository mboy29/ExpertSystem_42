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

def ft_resolve_rpn(data: Data, rule: RuleNode, resolving: set = None) -> bool:
    """
    Resolves a rule using reverse polish notation (RPN) and backward chaining.

    Parameters:
        data (Data): The data structure containing facts and rules.
        rule (RuleNode): The rule to resolve.
        resolving (set): A set of facts currently being resolved to detect cycles.
        verbose (bool): Whether to show reasoning steps.
    Returns:
        bool: True if the rule is resolved to True, False otherwise.
    """
    stack = []
    reasoning_steps = []

    reasoning_steps.append(f"Resolving rule: {rule.visualize()}")
    for token in rule.condition:
        if token.isalpha():
            fact = data.facts[token]
            resolved = ft_resolve_fact(data, fact, resolving, data.verbose)
            value = fact.value if fact.value is not None else False  # Treat None as False
            stack.append(value)
            if data.verbose:
                reasoning_steps.append(f"{token} = {value}")
        elif token == '!':
            operand = stack.pop()
            stack.append(not operand)
            if data.verbose:
                reasoning_steps.append(f"!{operand} -> {not operand}")
        elif token == '+':
            b, a = stack.pop(), stack.pop()
            a = a if a is not None else False
            b = b if b is not None else False
            stack.append(a and b)
            if data.verbose:
                reasoning_steps.append(f"{a} + {b} -> {a and b}")
        elif token == '|':
            b, a = stack.pop(), stack.pop()
            a = a if a is not None else False
            b = b if b is not None else False
            stack.append(a or b)
            if data.verbose:
                reasoning_steps.append(f"{a} | {b} -> {a or b}")
        elif token == '^':
            b, a = stack.pop(), stack.pop()
            a = a if a is not None else False
            b = b if b is not None else False
            stack.append(a ^ b)
            if data.verbose:
                reasoning_steps.append(f"{a} ⊕ {b} -> {a ^ b}")

    result = stack.pop() if stack else False

    if data.verbose:
        print("\n  - ".join(reasoning_steps))
    return result



def ft_resolve_fact(data: Data, fact: FactNode, resolving: set = None, verbose: bool = False) -> bool:
    """
    Attempts to resolve a fact using backward chaining with reasoning visualization.

    Parameters:
        data (Data): The data structure containing facts and rules.
        fact (FactNode): The fact to resolve.
        resolving (set): A set of facts currently being resolved to detect cycles.
        verbose (bool): Whether to show reasoning steps.
    Returns:
        bool: True if the fact is resolved to True, False otherwise.
    """
    if fact.value is not None:  # Already determined
        print(f"  - {SUCCESS}We know that {fact.name}{SUCCESS} is {fact.value}{RESET} as it has already been resolved.")
        return fact.value

    if resolving is None:
        resolving = set()
    if fact.name in resolving:
        print(f"  - {ERROR}Cycle detected for {fact.name}. Unable to resolve, marking as False.{RESET}")
        return False

    resolving.add(fact.name)
    for rule in fact.rules:
        condition_met = ft_resolve_rpn(data, rule, resolving)
        print(f"  - {SUCCESS}We know that {fact.name}{SUCCESS} is {condition_met}{RESET} based on the satisfied rule {rule.visualize()}.\n"
                f"    ∃ rules : ({rule.visualize()} = True")
        if condition_met:
            for conclusion in rule.conclusions:
                if conclusion == fact.name:
                    fact.value = True
                    resolving.remove(fact.name)
                    return True

    resolving.remove(fact.name)
    fact.value = False
    print(f"  - {ERROR}Unable to deduce {fact.name} as True{RESET} based on the current knowledge. It remains {fact.value}.")
    return False


def ft_expert_system(data: Data, verbose: bool = False) -> None:
    """
    Main function to run the expert system using backward chaining with reasoning visualization.

    Parameters:
        data (Data): The data structure containing facts and rules.
        verbose (bool): Whether to show reasoning steps.
    Returns:
        None
    """
    Logger.info("Starting backward chaining...", endswith=f"{'\n\n' if verbose else '\n'}")

    results = {}
    reasoning_steps = {}

    for query in data.queries:
        if query not in data.facts:
            results[query] = "Undetermined"
            raise Exception(f"Query {query} is not a valid fact. Marking as Undetermined.")

        fact = data.facts[query]
        resolved = ft_resolve_fact(data, fact, verbose=data.verbose)
        if resolved:
            results[query] = "True"

        elif fact.value is False:
            results[query] = "False"
        else:
            results[query] = "Undetermined"
            raise Exception(f"Query {query} is not a valid fact. Marking as Undetermined.")

    Logger.success("Backward chaining completed.\n")

    if data.verbose:
        Logger.info("Reasoning Visualization:")
        for query, reasoning in reasoning_steps.items():
            Logger.info(reasoning)
    print(results)

