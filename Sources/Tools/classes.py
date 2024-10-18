# +------------------------------------------------+ 
# |                     CLASSES                    |
# +------------------------------------------------+

# Description:
# This file contains all the classes used in the
# project.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *

# +------------------- CLASSES --------------------+

class CalcOperators(Enum):

    PRIORITY_LEFT = ('(', 0)
    PRIORITY_RIGHT = (')', 0)
    NOT = ('!', 1)
    AND = ('+', 2)
    OR = ('|', 3)
    XOR = ('^', 4)
    IMPLIES = ('=>', 5)
    IMPLIES_BI = ('<=>', 6)

    QUERY = ('?', None)
    FACT = ('=', None)

    def __init__(self, symbol: str, precedence: int):
        self.symbol = symbol
        self.precedence = precedence

    def __str__(self):
        return f"{self.symbol} ({self.precedence})"
    
    def get_symbol(self) -> str: return self.symbol
    def get_precedence(self) -> int: return self.precedence
    
    @staticmethod   
    def is_rule(op: str) -> bool: return op in [CalcOperators.NOT.get_symbol(), CalcOperators.AND.get_symbol(), CalcOperators.OR.get_symbol(), CalcOperators.XOR.get_symbol(), CalcOperators.IMPLIES.get_symbol(), CalcOperators.BIDIRECTIONAL.get_symbol()]
    
    @staticmethod
    def is_query(op: str) -> bool: return op == CalcOperators.QUERY.get_symbol()

    @staticmethod
    def is_fact(op: str) -> bool: return op == CalcOperators.FACT.get_symbol()
    
    @staticmethod
    def is_operator(op: str) -> bool:
        return op in {
            CalcOperators.PRIORITY_LEFT.get_symbol(),
            CalcOperators.PRIORITY_RIGHT.get_symbol(),
            CalcOperators.NOT.get_symbol(),
            CalcOperators.AND.get_symbol(),
            CalcOperators.OR.get_symbol(),
            CalcOperators.XOR.get_symbol(),
        }

    @staticmethod
    def get_operator(symbole: str): 
        for op in CalcOperators:
            if op.get_symbol() == symbole:
                return op
        return None

class CalcElement:

    def __init__(self, value: str, is_not: bool):
        self.set_value(value)
        self.set_is_not(is_not)
    
    def __str__(self):
        return f"{'!' if self.is_not else ''} {self.value}"
    
    def set_value(self, value: str) -> None: self.value = value
    def set_is_not(self, is_not: bool) -> None: self.is_not = is_not

    def get_value(self) -> str: return self.value

    def is_not(self) -> bool: return self.is_not

class Calc:

    def __init__(self, elements: list, type: CalcOperators):
        self.set_elements(elements)
        self.set_type(type)

    def __str__(self):
        return f"{f' {self.type.get_symbol()} '.join([str(elem) for elem in self.elements])}"
    
    def set_elements(self, elements: list) -> None: self.elements = elements
    def set_type(self, type: CalcOperators) -> None: self.type = type
    
    def get_elements(self) -> list: return self.elements
    def get_type(self) -> CalcOperators: return self.type 

class Rule:

    def __init__(self, left: Calc, right: Calc):
        self.set_left(left)
        self.set_right(right)

    def __str__(self):
        return f"{self.left} => {self.right}"
    
    def set_left(self, left: Calc) -> None: self.left = left
    def set_right(self, right: Calc) -> None: self.right = right

    def get_left(self) -> Calc: return self.left
    def get_right(self) -> Calc: return self.right

class Data:

    def __init__(self):
        self.rules = None
        self.set_facts(None)
        self.set_queries(None)

    def __str__(self):
        return f"Rules: {self.rules}\nFacts: {self.facts}\nQueries: {self.queries}"

    def set_facts(self, facts: list) -> None: self.facts = facts
    def set_rules(self, rules: list) -> None: self.rules = rules
    def set_queries(self, queries: list) -> None: self.queries = queries

    def get_rules(self) -> list: return self.rules 
    def get_facts(self) -> list:  return self.facts
    
    def get_queries(self) -> list: return self.queries

    def add_query(self, query: str) -> None:
        if self.queries is None:
            self.set_queries([])
        self.queries.append(query)
    
    def add_fact(self, fact: str) -> None:
        if self.facts is None:
            self.set_facts([])
        self.facts.append(fact)

