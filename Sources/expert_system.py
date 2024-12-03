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

def ft_resolve_rpn(data: Data, rule: RuleNode, resolving: set = None) -> dict:
    
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

    reasoning_steps.append(Logger.verbose("RULE", rule_expression))
    for token in rule.condition:
        if token.isalpha():  # Fact
            fact = data.facts[token]
            resolved = ft_resolve_fact(data, fact, resolving)
            value = resolved if resolved is not None else False
            stack.append(value)
            reasoning_steps.append(Logger.verbose("FACT", {'name': fact.name, 'value': value}))
        elif token == '!':  # NOT
            operand = stack.pop()
            stack.append(not operand)
        elif token == '+':  # AND
            b, a = stack.pop(), stack.pop()
            stack.append(a and b)
        elif token == '|':  # OR
            b, a = stack.pop(), stack.pop()
            stack.append(a or b)
        elif token == '^':  # XOR
            b, a = stack.pop(), stack.pop()
            stack.append(a ^ b)

    result = stack.pop() if stack else False
    reasoning_steps.append(Logger.verbose("RES", {"res": result, "exp": rule_expression, "con": rule_condition}))
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
        results = ft_resolve_rpn(data, rule, resolving)
        if results['res'] is True and results['con'] not in resolved:
            resolved.append(results['con'])
        elif results['res'] is False and results['con'] in resolved:
            raise Exception(f"Ambiguity detected: '{results['con']}' is both True and False.")

        if results['res']:
            if len(rule.conclusions) == 1:
                conclusion = rule.conclusions[0]
                if conclusion == fact.name:
                    fact.value = True
                    resolving.remove(fact.name)
                    return True
                
            elif len(rule.conclusions) == 3:
                a, b, operator = rule.conclusions[0], rule.conclusions[1], rule.conclusions[2]
                a_fact = data.facts[a]
                b_fact = data.facts[b]
                if operator == '^' or operator == '|':
                    if a_fact.value is None or b_fact.value is None or (a_fact.value == b_fact.value):
                        raise Exception(f"Ambiguity detected: XOR cannot resolve {a} and {b} uniquely.")
                if operator == '+':
                    a_fact.value = True
                    b_fact.value = True
                if fact.name in [a, b]:
                    fact.value = results['res']
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
        fact = data.facts[query]
        resolved = ft_resolve_fact(data, fact)
        if resolved or fact.value:
            results[query] = "True"
        else:
            results[query] = "False"

    Logger.success("Backward chaining completed.\n")
    
    for query, result in results.items():
        Logger.info(f"{query}: {result}")

   