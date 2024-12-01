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
    Returns:
        bool: True if the rule is resolved to True, False otherwise.
    """
    
    stack = []
    rule_expression = rule.visualize()  # Get the string representation of the rule
    rule_condition = re.split(r'\s*=>\s*|\s*<=>\s*', rule.visualize())[-1]
    reasoning_steps = []

    reasoning_steps.append(Logger.verbose("RULE", rule_expression))
    for token in rule.condition:
        if token.isalpha():
            fact = data.facts[token]
            resolved = ft_resolve_fact(data, fact, resolving)
            value = fact.value if fact.value is not None else False
            stack.append(value)
            reasoning_steps.append(Logger.verbose("FACT", fact))
        elif token == '!':
            operand = stack.pop()
            stack.append(not operand)
            reasoning_steps.append(Logger.verbose("NOT", operand))
        elif token == '+':
            b, a = stack.pop(), stack.pop()
            stack.append(a and b)
            reasoning_steps.append(Logger.verbose("AND", {"a": a, "b": b}))
        elif token == '|':
            b, a = stack.pop(), stack.pop()
            stack.append(a or b)
            reasoning_steps.append(Logger.verbose("OR", {"a": a, "b": b}))
        elif token == '^':
            b, a = stack.pop(), stack.pop()
            stack.append(a ^ b)
            reasoning_steps.append(Logger.verbose("XOR", {"a": a, "b": b}))

    result = stack.pop() if stack else False

    reasoning_steps.append(Logger.verbose("RES", {"res": result, "exp": rule_expression, "con": rule_condition}))
    if data.verbose:
        Logger.verbose("PRINT", "\n".join(reasoning_steps))
    return result


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
    
    if fact.value is True: return True
    if resolving is None: resolving = set()
    if fact.name in resolving: return False

    resolving.add(fact.name)

    for rule in fact.rules:
        condition_met = ft_resolve_rpn(data, rule, resolving)
        if condition_met:
            for conclusion in rule.conclusions:
                if conclusion.isalpha() and conclusion == fact.name:
                    fact.value = True
                    resolving.remove(fact.name)
                    return True      
    resolving.remove(fact.name)
    return False


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

    results = {}
    for query in data.queries:
        if query not in data.facts:
            results[query] = "Undetermined"
            raise Exception(f"Query {query} is not a valid fact. Marking as Undetermined.")
        fact = data.facts[query]
        resolved = ft_resolve_fact(data, fact)
        if resolved or fact.value:
            results[query] = "True"
        else:
            results[query] = "False"

    Logger.success("Backward chaining completed.\n")
    
    for query, result in results.items():
        Logger.info(f"{query}: {result}")

   