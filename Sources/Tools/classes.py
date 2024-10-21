# +------------------------------------------------+ 
# |                     CLASSES                    |
# +------------------------------------------------+

# Description:
# This file contains all the classes used in the
# project.

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *

# +------------------- CLASSES --------------------+

class Operator(Enum):
    
    """
    Enum class representing various logical operators 
    used in expressions.
    
    Each operator is associated with a symbol, precedence, 
    and an optional text representation.
    
    Attributes:
        PRIORITY_LEFT (str): Symbol for left parenthesis.
        PRIORITY_RIGHT (str): Symbol for right parenthesis.
        NOT (str): Symbol for logical NOT operation.
        AND (str): Symbol for logical AND operation.
        OR (str): Symbol for logical OR operation.
        XOR (str): Symbol for logical XOR operation.
        IMPLIES (str): Symbol for logical implication.
        IMPLIES_BI (str): Symbol for bi-conditional implication.
        QUERY (str): Symbol for query operations.
        FACT (str): Symbol for fact assignments.
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
    
    def get_symbol(self) -> str: return self.symbol
    def get_precedence(self) -> int: return self.precedence
    def get_text(self) -> str: return self.text
    
    @staticmethod   
    def is_rule(op: str) -> bool: 
        return op in [
            Operator.NOT.get_symbol(), 
            Operator.AND.get_symbol(), 
            Operator.OR.get_symbol(), 
            Operator.XOR.get_symbol(), 
            Operator.IMPLIES.get_symbol(), 
            Operator.IMPLIES_BI.get_symbol()
        ]
    
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

class ASTExprNode:
    
    """
    Base class for all nodes in the Abstract Syntax
    Tree (AST).
    
    This class serves as a parent for all expression 
    nodes, including operators and facts. It does not 
    implement any functionality itself, but serves as
    a common type for type hinting and structural 
    organization.
    """
    
    pass

class ASTFactNode(ASTExprNode):
   
    """
    Class representing a fact node in the Abstract Syntax 
    Tree (AST).
    
    A fact node contains a single fact that can be evaluated
    or referenced in logical expressions.

    Attributes:
        fact (str): The fact represented by this node.
    """
    
    def __init__(self, fact):
        self.fact = fact

    def __repr__(self):
        return f"Fact({self.fact})"

class ASTOperatorNode(ASTExprNode):
    
    """
    Class representing an operator node in the Abstract
    Syntax Tree (AST).
    
    An operator node encapsulates an operator and its left 
    and right operands. It allows for the representation of 
    binary operations within the AST.

    Attributes:
        operator (Operator): The operator associated with 
            this node.
        left (ASTExprNode): The left operand, which is another 
            AST node.
        right (ASTExprNode): The right operand, which is another 
            AST node.
    """
    
    def __init__(self, operator: Operator, left: ASTExprNode = None, right: ASTExprNode = None):
        self.operator = operator
        self.left = left
        self.right = right

    def __repr__(self):
        return f"Operator({self.operator}, {self.left}, {self.right})"

class ASTNotNode(ASTExprNode):
    
    """
    Class representing a NOT operation node in the Abstract 
    Syntax Tree (AST).
    
    This node is specifically used for negating a single
    operand. It is useful for handling logical negation within 
    the expression tree.

    Attributes:
        operand (ASTExprNode): The operand to be negated, 
            represented as another AST node.
    """
    
    def __init__(self, operand):
        self.operand = operand

    def __repr__(self):
        return f"Not({self.operand})"

class AST:
    
    """
    Class representing an Abstract Syntax Tree (AST) for 
    logical expressions.
    
    The AST serves as a structured representation of a
    logical expression, allowing for easy traversal and 
    evaluation of the expression components.

    Attributes:
        root (ASTExprNode): The root node of this AST.
    """
    
    def __init__(self): self.root = None
    def __repr__(self): return f"AST(Root: {self.root})"
    def __str__(self): return self.visualize(self.root)

    def set_root(self, rule: ASTExprNode) -> None: self.root = rule

    def visualize(self, node, prefix="", is_left=True) -> str:
        if node is None:
            return ""
        
        result = ""

        if isinstance(node, ASTOperatorNode):
            result += prefix + ("|-- " if is_left else "`-- ") + f"{node.operator.get_text()} ({node.operator.get_precedence()})\n"
            prefix += "    " if is_left else "    "
            result += self.visualize(node.left, prefix, True) 
            result += self.visualize(node.right, prefix, False)
        elif isinstance(node, ASTNotNode):
            result += prefix + ("|-- " if is_left else "`-- ") + "NOT\n"
            prefix += "    " if is_left else "    "
            result += self.visualize(node.operand, prefix, True)
        elif isinstance(node, ASTFactNode):
            result += prefix + ("|-- " if is_left else "`-- ") + f"Fact({node.fact})\n"

        return result

class Data:
    
    """
    Class for managing rules, facts, and queries in the 
    logical system.
    
    This class encapsulates the data structures used to 
    store rules, facts, and queries, allowing for the 
    addition and retrieval of these components.

    Attributes:
        rules (list): A list of rules represented as ASTs.
        facts (dict): A dictionary mapping facts to their 
            boolean values.
        queries (list): A list of queries that can be executed.
    """
    
    def __init__(self):
        self.rules = None
        self.facts = None
        self.queries = None

    def __str__(self): return f"Rules: {self.rules}\nFacts: {self.facts}\nQueries: {self.queries}"

    def set_facts(self, facts: list) -> None: self.facts = facts
    def set_queries(self, queries: list) -> None: self.queries = queries
    def set_rules(self, rules: list) -> None: self.rules = rules

    def get_facts(self) -> list: return self.facts
    def get_queries(self) -> list: return self.queries
    def get_rules(self) -> list: return self.rules

    def add_query(self, query: str) -> None:
        if self.queries is None:
            self.set_queries([])
        self.queries.append(query)

    def add_fact(self, fact: str, value: bool) -> None: 
        if self.facts is None:
            self.set_facts({})
        self.facts[fact] = value

    def add_rule(self, rule: ASTExprNode) -> None:
        if self.rules is None:
            self.set_rules([])
        ast = AST()
        ast.set_root(rule)
        self.rules.append(ast)
