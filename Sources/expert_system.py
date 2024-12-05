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

def ft_resolve_rpn_condition(data: Data, condition: str, res: bool) -> bool:
    
    """
    Resolves and assigns values to facts based on the compound condition and result.

    Parameters:
        data (Data): The data structure containing facts and rules.
        condition (str): The compound condition (e.g., "A + B", "A ^ B").
        res (bool): The result of the compound condition.
    Returns:
        bool: True if the condition was successfully resolved, False otherwise.
    """
    
    fact_a, operator, fact_b = re.split(r'\s*(\+|\||\^)\s*', condition)
    a = data.facts[fact_a]
    b = data.facts[fact_b]
    reasoning_steps = []

    if a.value is None and b.value is None and operator != '+' and res is not False:
        raise Exception(f"Cannot resolve condition '{condition}' with unknown fact values.")

    reasoning_steps.append(Logger.verbose("FACT", {'name': a.name, 'value': a.value}))
    reasoning_steps.append(Logger.verbose("FACT", {'name': b.name, 'value': b.value}))
    if operator == '+':
        if res:
            a.value = True if a.value is None else a.value
            b.value = True if b.value is None else b.value
        else:
            a.value = False if a.value is None else a.value
            b.value = False if b.value is None else b.value
    elif operator == '|':
        if res:
            a.value = True if a.value is None else a.value
            b.value = False if b.value is None else b.value
        else:
            a.value = False if a.value is None else a.value
            b.value = False if b.value is None else b.value
    elif operator == '^':
        if res:
            a.value = True if a.value is None else a.value
            b.value = False if b.value is None else b.value
        else:
            a.value = True if a.value is None else a.value
            b.value = True if b.value is None else b.value

    reasoning_steps.append(Logger.verbose("CONDITION", {"condition": condition, "res": res, "a": a, "operator": operator, "b": b}))
    return reasoning_steps 

def ft_resolve_rpn(data: Data, fact: FactNode, rule: RuleNode, resolving: set = None) -> dict:
    
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
    rule_expression = rule.visualize()
    rule_condition = re.split(r'\s*=>\s*|\s*<=>\s*', rule_expression)[-1]
    reasoning_steps = []

    reasoning_steps.append(Logger.verbose("RULE", {'fact': fact.name, 'rule': rule_expression}))
    for token in rule.condition:
        if token.isalpha():
            fact = data.facts[token]
            resolved = ft_resolve_fact(data, fact, resolving)
            value = resolved if resolved is not None else False
            stack.append(value)
            reasoning_steps.append(Logger.verbose("FACT", {'name': fact.name, 'value': value}))
        elif token == '!':
            operand = stack.pop()
            stack.append(not operand)
        elif token == '+':
            b, a = stack.pop(), stack.pop()
            stack.append(a and b)
        elif token == '|':
            b, a = stack.pop(), stack.pop()
            stack.append(a or b)
        elif token == '^':
            b, a = stack.pop(), stack.pop()
            stack.append(a ^ b)

    result = stack.pop() if stack else False
    reasoning_steps.append(Logger.verbose("RES", {"res": result, "exp": rule_expression, "con": rule_condition}))
    if len(rule_condition) > 1:
        reasoning_steps.extend(ft_resolve_rpn_condition(data, rule_condition, result))
    if data.verbose:
        Logger.verbose("PRINT", "\n".join(reasoning_steps))
    return {"res": result, "exp": rule_expression, "con": rule_condition}

def ft_resolve_fact(data: Data, fact: FactNode, resolving: set = None, resolved: list = []) -> bool:
    
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
    if resolving is None:  resolving = set()
    if fact.name in resolving: return False 

    resolving.add(fact.name)
    fact.rules.sort(key=lambda rule: len(rule.conclusions))
    for rule in fact.rules:
        results = ft_resolve_rpn(data, fact, rule, resolving)
        if results['res'] is True and results['con'] not in resolved:
            resolved.append(results['con'])
        elif results['res'] is False and results['con'] in resolved:
            raise Exception(f"Ambiguity detected: '{results['con']}' is both True and False.")

        if results['res']:
            conclusion = rule.conclusions[0]
            if conclusion == fact.name:
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
    
    newline = "\n\n" if data.verbose else "\n"
    Logger.info("Starting backward chaining...", endswith=newline)

    results = {}
    for query in data.queries:
        fact = data.facts[query]
        resolved = ft_resolve_fact(data, fact)
        if resolved or fact.value:
            results[query] = "True"
        else:
            results[query] = "False"

    Logger.success("Backward chaining completed.\n")
    
    for query, result in results.items():
        Logger.info(f"{query}: {result}")

   