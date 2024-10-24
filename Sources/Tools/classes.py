# +------------------------------------------------+ 
# |                     CLASSES                    |
# +------------------------------------------------+

# +-------------------- IMPORTS -------------------+
from Sources.Tools import *  # Assuming this imports necessary external utilities
from enum import Enum

# +------------------- CLASSES --------------------+

class Operator(Enum):
    """
    Enum class representing various logical operators.
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
        self.set_symbol(symbol)
        self.set_precedence(precedence)
        self.set_text(text)

    def set_symbol(self, symbol: str) -> None: self.symbol = symbol
    def set_precedence(self, precedence: int) -> None: self.precedence = precedence
    def set_text(self, text: str) -> None: self.text = text

    def get_symbol(self) -> str: return self.symbol
    def get_precedence(self) -> int: return self.precedence
    def get_text(self) -> str: return self.text
    
    @staticmethod   
    def is_rule(op: str) -> bool: 
        return op in [Operator.NOT.get_symbol(), Operator.AND.get_symbol(), 
                      Operator.OR.get_symbol(), Operator.XOR.get_symbol(), 
                      Operator.IMPLIES.get_symbol(), Operator.IMPLIES_BI.get_symbol()]

    @staticmethod
    def is_query(op: str) -> bool: return op == Operator.QUERY.get_symbol()

    @staticmethod
    def is_fact(op: str) -> bool: return op == Operator.FACT.get_symbol()

    @staticmethod
    def is_operator(op: str) -> bool:
        return op in {
            Operator.PRIORITY_LEFT.get_symbol(),
            Operator.PRIORITY_RIGHT.get_symbol(),
            Operator.NOT.get_symbol(),
            Operator.AND.get_symbol(),
            Operator.OR.get_symbol(),
            Operator.XOR.get_symbol(),
        }

    @staticmethod
    def get_operator(symbol: str): 
        for op in Operator:
            if op.get_symbol() == symbol:
                return op
        return None

class DefaultNode: pass

class FactNode:

    def __init__(self, fact: str, state: bool = None):
        self.set_fact(fact)
        self.set_state(state)

    def __str__ (self): return f"FactNode({self.get_fact()} = {self.get_state()})"
    def __repr__(self): return f"FactNode({self.get_fact()} = {self.get_state()})"

    def get_fact(self) -> str: return self.fact
    def get_state(self) -> bool: return self.state

    def set_fact(self, fact: str) -> None: self.fact = fact
    def set_state(self, state: bool) -> None: self.state = state
    

class OperatorNode(DefaultNode):
    def __init__(self, operator: Operator, left: DefaultNode = None, right: DefaultNode = None):
        self.set_operator(operator)
        self.set_left(left)
        self.set_right(right)

    def __str__(self): return f"OperatorNode({self.get_operator()}, {self.get_left()}, {self.get_right()})"
    def __repr__(self): return f"OperatorNode({self.get_operator()}, {self.get_left()}, {self.get_right()})"

    def get_operator(self) -> Operator: return self.operator
    def get_left(self) -> DefaultNode: return self.left
    def get_right(self) -> DefaultNode: return self.right

    def set_operator(self, operator: Operator) -> None: self.operator = operator
    def set_left(self, left: DefaultNode) -> None: self.left = left
    def set_right(self, right: DefaultNode) -> None: self.right = right


class NotNode(DefaultNode):

    def __init__(self, operand): self.set_operand(operand)
    def __repr__(self): return f"NotNode({self.get_operand()})"

    def get_operand(self) -> DefaultNode: return self.operand
    def set_operand(self, operand: DefaultNode) -> None: self.operand = operand


class RuleNode:
    def __init__(self, ast: DefaultNode = None): 
        self.set_ast(ast)
    
    def __repr__(self): return f"RuleNode(ast: {self.visualize(self.get_ast())})"
    def __str__(self): return f"RuleNode(ast: {self.visualize(self.get_ast())})"

    def set_ast(self, rule: DefaultNode) -> None: self.ast = rule
    def get_ast(self) -> DefaultNode: return self.ast

    def visualize(self, node, prefix="", is_left=True) -> str:
        if node is None:
            return ""
        result = ""
        if isinstance(node, OperatorNode):
            result += prefix + ("|-- " if is_left else "`-- ") + f"{node.get_operator().get_text()} ({node.get_operator().get_precedence()})\n"
            prefix += "    " if is_left else "    "
            result += self.visualize(node.get_left(), prefix, True)
            result += self.visualize(node.get_right(), prefix, False)
        elif isinstance(node, NotNode):
            result += prefix + ("|-- " if is_left else "`-- ") + "NOT\n"
            prefix += "    " if is_left else "    "
            result += self.visualize(node.get_operand(), prefix, True)
        elif isinstance(node, FactNode):
            result += prefix + ("|-- " if is_left else "`-- ") + f"{node}\n"
        return result



# +---------------- Updated Data Class -----------------+

class Data:
    def __init__(self):
        self.set_facts(None)
        self.set_queries(None)
        self.set_rules(None)
        

    def __repr__(self): return f"Initial Facts: {self.get_facts()}\nQueries: {self.get_queries()}\nRules: {self.get_rules()}"

    def set_queries(self, queries: list) -> None: self.queries = queries
    def set_facts(self, facts: dict) -> None: self.facts = facts
    def set_rules(self, rules: list) -> None: self.rules = rules

    def set_fact(self, fact: str, state: bool) -> None:
        if self.get_facts() is not None:
            self.get_facts()[fact] = state


    def get_queries(self) -> list: return self.queries
    def get_facts(self) -> dict: return self.facts
    def get_rules(self) -> list: return self.rules

    def get_fact(self, fact) -> FactNode:
        if self.get_facts() is not None:
            for f in self.get_facts():
                if f.get_fact() == fact:
                    return f
        return None

    def add_query(self, query: str) -> None:
        if self.get_queries() is None:
            self.set_queries([])
        self.get_queries().append(query)

    def add_fact(self, fact: str, state: bool = None) -> None:
        if self.get_facts() is None:
            self.set_facts([])
        if self.get_fact(fact) is not None:
            self.get_fact(fact).set_state(state)
        else:
            self.get_facts().append(FactNode(fact, state))

    def add_rule(self, rule: DefaultNode) -> None:
        if self.rules is None:
            self.set_rules([])
        self.rules.append(RuleNode(rule))