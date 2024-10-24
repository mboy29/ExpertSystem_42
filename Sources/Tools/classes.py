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
        self.symbol = symbol
        self.precedence = precedence
        self.text = text

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


class FactNode:
    """
    Represents a fact in the graph.
    """
    def __init__(self, fact):
        self.fact = fact
        self.value = None  # Start with undetermined
        self.rules = []    # Rules that affect this fact

    def add_rule(self, rule_node):
        self.rules.append(rule_node)
    
    def __repr__(self): return f"FactNode({self.fact}, {self.value})"



class ASTExprNode:
    """Base class for all nodes in the AST."""
    pass

class ASTFactNode(ASTExprNode):
    """Class representing a fact node in the AST."""
    
    def __init__(self, fact: FactNode):
        """
        Initialize an ASTFactNode.

        :param fact: The name of the fact (string).
        :param fact_node: The corresponding FactNode instance.
        """
        self.fact = fact  # The corresponding FactNode

    def __repr__(self):
        return f"ASTFactNode(Fact: {self.fact}, FactNode: {self.fact_node})"

class ASTOperatorNode(ASTExprNode):
    """Class representing an operator node in the AST."""
    
    def __init__(self, operator: Operator, left: ASTExprNode = None, right: ASTExprNode = None):
        self.operator = operator
        self.left = left
        self.right = right

    def __repr__(self): return f"Operator({self.operator}, {self.left}, {self.right})"

class ASTNotNode(ASTExprNode):
    """Class representing a NOT operation node in the AST."""
    
    def __init__(self, operand):
        self.operand = operand

    def __repr__(self): return f"Not({self.operand})"

class AST:
    """Class representing an Abstract Syntax Tree (AST) for logical expressions."""
    
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


# +---------------- Graph Implementation -----------------+


class RuleNode:
    """
    Represents a rule in the graph.
    """
    def __init__(self, ast: AST):
        self.ast = ast
        self.premises = []
        self.conclusions = []

    def add_premise(self, fact_node):
        self.premises.append(fact_node)
    
    def add_conclusion(self, fact_node):
        self.conclusions.append(fact_node)
    
    def __repr__(self): return f"RuleNode(AST: {self.ast})"


class Graph:
    """
    Represents the dependency graph for facts and rules.
    """
    def __init__(self):
        self.facts = None  # Initialized to None until needed
        self.rules = None  # Initialized to None until needed

    def __str__(self):
        """Return a string representation of the graph's facts, rules, and relationships."""
        result = []

        # Display facts
        result.append("Facts:")
        if self.facts:
            for fact_name, fact_node in self.facts.items():
                result.append(f"  - {fact_name}: {fact_node.value}")
        else:
            result.append("  (None)")

        # Display rules
        result.append("\nRules:")
        if self.rules:
            for i, rule in enumerate(self.rules):
                result.append(f"  ------------------------------")
                result.append(f"  Rule {i+1}:")
                result.append(f"    AST: {rule.ast}")
                result.append("    Premises:")
                for premise in rule.premises:
                    result.append(f"      - {premise.fact}")
                result.append("    Conclusions:")
                for conclusion in rule.conclusions:
                    result.append(f"      - {conclusion.fact}")
        else:
            result.append("  (None)")

        return "\n".join(result)

    def get_fact_node(self, fact_name):
        if self.facts is None:
            self.facts = {}  # Initialize when first used

        if fact_name not in self.facts:
            self.facts[fact_name] = FactNode(fact_name)
        return self.facts[fact_name]
    
    def add_rule(self, ast: AST):
        if self.rules is None:
            self.rules = []  # Initialize when first used
        
        rule_node = RuleNode(ast)
        self.extract_premises_and_conclusions(ast.root, rule_node)
        self.rules.append(rule_node)
    
    def extract_premises_and_conclusions(self, node, rule_node):
        if isinstance(node, ASTOperatorNode):
            if node.operator == Operator.IMPLIES or node.operator == Operator.IMPLIES_BI:
                # Process the left side for premises
                self.add_to_rule(node.left, rule_node.premises, rule_node)
                # Process the right side for conclusions
                self.add_to_rule(node.right, rule_node.conclusions, rule_node)
    
    def add_to_rule(self, node, fact_list, rule_node):
        if isinstance(node, ASTFactNode):
            # Ensure we have a FactNode for the fact
            fact_node = self.get_fact_node(node.fact)
            # Create a new ASTFactNode with the corresponding FactNode
            ast_fact_node = ASTFactNode(fact_node)
            fact_list.append(fact_node)  # Append the FactNode to the list
            if fact_list is rule_node.premises:
                fact_node.add_rule(rule_node)
        elif isinstance(node, ASTNotNode):
            # If it's a NOT node, add its operand to the fact list
            self.add_to_rule(node.operand, fact_list, rule_node)
        elif isinstance(node, ASTOperatorNode):
            # For operator nodes, traverse left and right
            self.add_to_rule(node.left, fact_list, rule_node)
            self.add_to_rule(node.right, fact_list, rule_node)

    def evaluate_fact(self, fact_name):
        if self.facts is None:
            return False  # If no facts exist, the fact cannot be true
        
        fact_node = self.get_fact_node(fact_name)
        if fact_node.value is not None:
            return fact_node.value
        
        for rule in fact_node.rules:
            if self.evaluate_rule(rule):
                fact_node.value = True
                return True

        fact_node.value = False
        return False

    def evaluate_rule(self, rule_node):
        premises_valid = all(self.evaluate_fact(p.fact) for p in rule_node.premises)
        if premises_valid:
            for conclusion in rule_node.conclusions:
                conclusion.value = True
            return True
        return False

    def evaluate_query(self, query):
        return self.evaluate_fact(query)

# +---------------- Updated Data Class -----------------+

class Data:
    """
    Manages rules, facts, and queries in the logical system.
    """
    def __init__(self):
        self.initial_facts = None
        self.queries = None
        self.graph = Graph()

    def __repr__(self): return f"Initial Facts: {self.get_initial_facts()}\nQueries: {self.get_queries()}\nGraph: {self.get_graph()}"

    def set_queries(self, queries: list) -> None: self.queries = queries
    def set_initial_facts(self, facts: dict) -> None: self.initial_facts = facts
    def set_graph(self, graph: Graph) -> None: self.graph = graph

    def get_queries(self) -> list: return self.queries
    def get_initial_facts(self) -> dict: return self.initial_facts
    def get_graph(self) -> Graph: return self.graph


    def get_rules(self) -> list: return self.graph.rules
    def get_facts(self) -> dict: return self.graph.facts

    def add_query(self, query: str) -> None:
        if self.queries is None:
            self.set_queries([])
        self.queries.append(query)

    def add_rule(self, rule: AST) -> None:
        self.graph.add_rule(rule)

    def add_initial_fact(self, fact: str) -> None:
        if self.initial_facts is None:
            self.set_initial_facts([])
        self.initial_facts.append(fact)

    def evaluate_query(self, query: str) -> bool:
        return self.graph.evaluate_query(query)
