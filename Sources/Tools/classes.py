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

    def __str__(self): return f"{self.symbol} ({self.precedence})"

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

    @staticmethod
    def get_operator(symbol: str):
        for op in Operator:
            if op.symbol == symbol:
                return op
        return None


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
        self.value = False  # Default to False
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

    def __init__(self, condition: list, conclusion: list):
        self.condition = condition  # Condition in RPN
        self.conclusion = conclusion  # Conclusion in RPN

    def get_condition(self) -> list: return self.condition
    def get_conclusion(self) -> list: return self.conclusion

    def __repr__(self):
        return f"RuleNode(Condition={self.condition}, Conclusion={self.conclusion})"


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

    def evaluate_query(self, query: str) -> str:
        if query not in self.facts:
            return "Undetermined"
        return "True" if self._evaluate_fact(self.facts[query]) else "False"

    def _evaluate_fact(self, fact: FactNode) -> bool:
        if fact.value:
            return True
        for rule in fact.rules:
            if self._evaluate_rpn(rule.get_condition()):
                fact.value = True
                return True
        return False

    def _evaluate_rpn(self, rpn: list) -> bool:
        stack = []
        for token in rpn:
            if token in self.facts:
                stack.append(self.facts[token].value)
            elif token == '!':
                stack.append(not stack.pop())
            elif token == '+':
                stack.append(stack.pop() and stack.pop())
            elif token == '|':
                stack.append(stack.pop() or stack.pop())
            elif token == '^':
                b, a = stack.pop(), stack.pop()
                stack.append(a ^ b)
        return stack.pop()

    def to_rpn(self, expr: str) -> list:
        precedence = {op.symbol: op.precedence for op in Operator}
        output = []
        operators = []
        tokens = re.findall(r'[A-Z]|[+|^!()=><]', expr)
        for token in tokens:
            if token.isalpha():
                output.append(token)
            elif token in precedence:
                while (operators and operators[-1] != '(' and
                       precedence[operators[-1]] >= precedence[token]):
                    output.append(operators.pop())
                operators.append(token)
            elif token == '(':
                operators.append(token)
            elif token == ')':
                while operators and operators[-1] != '(':
                    output.append(operators.pop())
                operators.pop()
        while operators:
            output.append(operators.pop())
        return output
