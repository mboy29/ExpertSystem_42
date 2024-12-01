# +------------------------------------------------+ 
# |                     CLASSES                    |
# +------------------------------------------------+

# +-------------------- IMPORTS -------------------+

import re
from enum import Enum

from Sources.Tools import *  

# +------------------- CLASSES --------------------+


class Operator(Enum):
    
    """
    Enum class representing various logical operators
    such as NOT, AND, OR, XOR, IMPLIES, and IMPLIES_BI.

    Attributes:
        symbol (str): The operator symbol.
        precedence (int): The operator precedence.
        text (str): The operator text.
    
    Methods:
        is_rule(op: str) -> bool: Check if the operator is a rule.
        is_query(op: str) -> bool: Check if the operator is a query.
        is_fact(op: str) -> bool: Check if the operator is a fact.
        is_operator(op: str) -> bool: Check if the operator is an operator.
        get_operator(symbol: str): Get the operator instance from the symbol.
    """

    PRIORITY_LEFT = ('(', 0, 'PRIORITY_LEFT')
    PRIORITY_RIGHT = (')', 0, 'PRIORITY_RIGHT')
    NOT = ('!', 1, 'NOT')
    AND = ('+', 2, 'AND')
    OR = ('|', 3, 'OR')
    XOR = ('^', 4, 'XOR')
    IMPLIES = ('=>', 5, 'IMPLIES')
    IMPLIES_BI = ('<=>', 5, 'IMPLIES_BI')

    QUERY = ('?', None)
    FACT = ('=', None)

    def __init__(self, symbol: str, precedence: int, text: str = None):
        self.symbol = symbol
        self.precedence = precedence
        self.text = text

    def __str__(self): return f"{self.symbol}"

    @staticmethod
    def is_rule(op: str) -> bool:
        return op in [
            Operator.NOT.symbol,
            Operator.AND.symbol,
            Operator.OR.symbol,
            Operator.XOR.symbol,
            Operator.IMPLIES.symbol,
            Operator.IMPLIES_BI.symbol
        ]

    @staticmethod
    def is_query(op: str) -> bool: return op == Operator.QUERY.symbol

    @staticmethod
    def is_fact(op: str) -> bool: return op == Operator.FACT.symbol

    @staticmethod
    def is_operator(op: str) -> bool:
        return op in {
            Operator.PRIORITY_LEFT.symbol,
            Operator.PRIORITY_RIGHT.symbol,
            Operator.NOT.symbol,
            Operator.AND.symbol,
            Operator.OR.symbol,
            Operator.XOR.symbol,
        }


class FactNode:
    
    """
    Represents a fact with a name, value, and associated rules.

    Attributes:
        name (str): The fact name.
        value (bool): The fact value.
        rules (list): The associated rules.
    
    Methods:
        add_rule(rule: RuleNode): Add a rule to the fact.
    """

    def __init__(self, name: str):
        self.name = name
        self.value = None  # Default to False
        self.rules = []  # Associated rules

    def add_rule(self, rule: 'RuleNode') -> None: self.rules.append(rule)

    def __repr__(self):
        return f"FactNode({self.name}, {self.value})"


class RuleNode:
    
    """
    Represents a rule with conditions and conclusions in RPN.

    Attributes:
        condition (list): The condition in RPN.
        conclusion (list): The conclusion in RPN.
    
    Methods:
        get_condition() -> list: Get the condition in RPN.
        get_conclusion() -> list: Get the conclusion in RPN.
    """

    def __init__(self, base: str, condition: list, implied: str, conclusion: list):
        self.base = base
        self.condition = condition
        self.implied = implied
        self.conclusions = conclusion

    def __repr__(self):
        return f"RuleNode(Condition={self.condition}, Conclusions={self.conclusions})"

    def visualize(self):
        splitted = re.split(r'\s*=>\s*|\s*<=>\s*', self.base)
        return f"{' '.join(splitted[0])} {self.implied} {' '.join(splitted[1])}"
        
class Data:
    """
    Class representing the data structure of the expert system.
    Manages facts, rules, and queries.

    Attributes:
        file (str): The input file.
        facts (dict): The facts.
        rules (list): The rules.
        queries (list): The queries.
        init_facts (bool): The initial facts.
    
    Methods:
        add_fact(fact_name: str) -> None: Add a fact to the data structure.
        add_rule(rule: RuleNode) -> None: Add a rule to the data structure.
        add_query(query: str) -> None: Add a query to the data structure.
        evaluate_query(query: str) -> str: Evaluate a query.
        _evaluate_fact(fact: FactNode) -> bool: Evaluate a fact.
        _evaluate_rpn(rpn: list) -> bool: Evaluate RPN.
        to_rpn(expr: str) -> list: Convert an expression to RPN.
    """

    def __init__(self, file: str, verbose: bool = False):
        self.file = file
        self.verbose = verbose

        self.facts = None
        self.rules = None
        self.queries = None
        self.init_facts = False
        

    def __repr__(self):
        return f"Data({self.file}\n, {self.facts}\n, {self.rules}\n, {self.queries}\n)"
    
    def __str__(self):
        return f"Data({self.file}\n, {self.facts}\n, {self.rules}\n, {self.queries}\n)"

    def add_fact(self, fact_name: str) -> None:
        if self.facts is None:
            self.facts = {}
        if fact_name not in self.facts:
            self.facts[fact_name] = FactNode(fact_name)

    def add_rule(self, rule: RuleNode) -> None: 
        if self.rules is None:
            self.rules = []
        self.rules.append(rule)

    def add_query(self, query: str) -> None:
        if self.queries is None:
            self.queries = []
        self.queries.append(query)

    