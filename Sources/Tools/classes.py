# +------------------------------------------------+ 
# |                     CLASSES                    |
# +------------------------------------------------+

# +-------------------- IMPORTS -------------------+

from Sources.Tools import *  

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

class DefaultNode: 
    
    """
    Abstract class representing a node in an abstract syntax 
    tree (AST). It is the base class for all other node types.
    """

    pass

class FactNode:

    """
    Class representing a fact node in an abstract syntax tree (AST).
    It contains a fact value, a state value, and references to
    rules that use the fact node on either the left or right side
    of the implication operator.
    """

    def __init__(self, fact: str, state: bool = None, known: bool = False):
        self.set_fact(fact)
        self.set_state(state)
        self.set_known(known)
        self.set_rules_left([])
        self.set_rules_right([])

    def __str__ (self): return (f"FactNode({'KNOWN ' if self.get_known() else ''}{self.get_fact()} = {self.get_state()})\n")
    def __repr__(self): return (f"FactNode({'KNOWN ' if self.get_known() else ''}{self.get_fact()} = {self.get_state()})\n")

    def get_fact(self) -> str: return self.fact
    def get_state(self) -> bool: return self.state
    def get_known(self) -> bool: return self.known
    def get_rules_left(self) -> list: return self.rules_left
    def get_rules_right(self) -> list: return self.rules_right

    def set_fact(self, fact: str) -> None: self.fact = fact
    def set_state(self, state: bool) -> None: self.state = state
    def set_known(self, known: bool) -> None: self.known = known
    def set_rules_left(self, rules: list) -> None: self.rules_left = rules
    def set_rules_right(self, rules: list) -> None: self.rules_right = rules

    def add_rule_left(self, rule: DefaultNode) -> None: self.get_rules_left().append(rule)
    def add_rule_right(self, rule: DefaultNode) -> None: self.get_rules_right().append(rule)

    def visualize_rules(self) -> str:
        rules_left = [rule.visualize_expression() for rule in self.get_rules_left()]
        rules_right = [rule.visualize_expression() for rule in self.get_rules_right()]
        return f"Rules Left: {rules_left}\nRules Right: {rules_right}"

class OperatorNode(DefaultNode):

    """
    Class representing an operator node in an abstract syntax 
    tree (AST). It contains an operator value, and references
    to left and right child nodes.
    """

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

    def extract_left(self) -> list:
        facts_left = []

        def traverse_left(node):
            if isinstance(node, FactNode):
                facts_left.append(node)
            elif isinstance(node, OperatorNode):
                traverse_left(node.get_left())
                traverse_left(node.get_right())
            elif isinstance(node, NotNode):
                traverse_left(node.get_operand())

        traverse_left(self.get_left())
        return facts_left

    def extract_right(self) -> list:
        facts_right = []

        def traverse_right(node):
            if isinstance(node, FactNode):
                facts_right.append(node)
            elif isinstance(node, OperatorNode):
                traverse_right(node.get_left())
                traverse_right(node.get_right())
            elif isinstance(node, NotNode):
                traverse_right(node.get_operand())

        traverse_right(self.get_right())
        return facts_right
        
class NotNode(DefaultNode):
    
    """
    Class representing a NOT node in an abstract syntax tree (AST).
    It contains a reference to the operand node.
    """

    def __init__(self, operand): self.set_operand(operand)
    def __repr__(self): return f"NotNode({self.get_operand()})"

    def get_operand(self) -> DefaultNode: return self.operand
    def set_operand(self, operand: DefaultNode) -> None: self.operand = operand


class RuleNode:

    """
    Class representing a rule node in an abstract syntax tree (AST).
    It contains a reference to the rule AST.
    """

    def __init__(self, ast: DefaultNode = None): 
        self.set_ast(ast)
    
    def __repr__(self): return f"RuleNode(ast: {self.visualize_tree(self.get_ast())})\n{self.visualize_expression()}\n"
    def __str__(self): return f"RuleNode(ast: {self.visualize_tree(self.get_ast())})\n{self.visualize_expression()}\n"

    def set_ast(self, rule: DefaultNode) -> None: self.ast = rule
    def get_ast(self) -> DefaultNode: return self.ast

    def visualize_tree(self, node = None, prefix="", is_left=True) -> str:

        if node is None:
            node = self.get_ast()
        result = ""
        if isinstance(node, OperatorNode):
            result += prefix + ("|-- " if is_left else "`-- ") + f"{node.get_operator().get_text()} ({node.get_operator().get_precedence()})\n"
            prefix += "    " if is_left else "    "
            result += self.visualize_tree(node.get_left(), prefix, True)
            result += self.visualize_tree(node.get_right(), prefix, False)
        elif isinstance(node, NotNode):
            result += prefix + ("|-- " if is_left else "`-- ") + "NOT\n"
            prefix += "    " if is_left else "    "
            result += self.visualize_tree(node.get_operand(), prefix, True)
        elif isinstance(node, FactNode):
            result += prefix + ("|-- " if is_left else "`-- ") + f"{node}\n"
        return result

    def visualize_expression(self, node=None) -> str:
        if node is None:
            node = self.get_ast()
        if isinstance(node, FactNode):
            return str(node.get_fact())
        if isinstance(node, NotNode):
            return f"!{self.visualize_expression(node.get_operand())}"
        if isinstance(node, OperatorNode):
            left_expr = self.visualize_expression(node.get_left())
            right_expr = self.visualize_expression(node.get_right())
            operator_symbol = node.get_operator().get_symbol()
            if node.get_operator() == Operator.IMPLIES or node.get_operator() == Operator.IMPLIES_BI:
                return f"{left_expr} {operator_symbol} {right_expr}"
            return f"({left_expr} {operator_symbol} {right_expr})"

        return ""
    
    def split(self, node=None) -> tuple:
        if node is None:
            node = self.get_ast()
        if isinstance(node, OperatorNode):
            if node.get_operator() in {Operator.IMPLIES, Operator.IMPLIES_BI}:
                return node.get_left(), node.get_right()
            left_rule = self.split(node.get_left())
            right_rule = self.split(node.get_right())
            return left_rule.get_ast(), right_rule.get_ast()
        return (RuleNode(node), None)
    
    def extract(self, node=None) -> list:
        if node is None:
            node = self.get_ast()
        facts = []

        def traverse(node):
            if isinstance(node, FactNode):
                facts.append(node)
            elif isinstance(node, OperatorNode):
                traverse(node.get_left())
                traverse(node.get_right())
            elif isinstance(node, NotNode):
                traverse(node.get_operand())

        traverse(node)
        return facts
    
    def extract_left(self) -> list:
        left, right = self.split()
        return self.extract(left)
    
    def extract_right(self) -> list:
        left, right = self.split()
        return self.extract(right)

class Data:

    """
    Class representing the data structure of the expert system.
    It contains the known facts, queries, and rules of the system.
    Each fact is represented by a FactNode, each query is a string,
    and each rule is represented by a RuleNode containing an AST.
    """

    def __init__(self, file: str = None, verbose: bool = False):
        self.set_file(file)
        self.set_verbose(verbose)

        self.set_init_facts(False)
        self.set_facts(None)
        self.set_queries(None)
        self.set_rules(None)
        

    def __repr__(self): 
        return f"Facts: {self.get_facts()}\n\nQueries: {self.get_queries()}\n\nRules: {self.get_rules()}"

    def set_file(self, file: str) -> None: self.file = file
    def set_verbose(self, verbose: bool) -> None: self.verbose = verbose
    def set_queries(self, queries: list) -> None: self.queries = queries
    def set_facts(self, facts: dict) -> None: self.facts = facts
    def set_rules(self, rules: list) -> None: self.rules = rules
    def set_init_facts(self, state: bool) -> None: self.init_facts = state

    def set_fact(self, fact: FactNode, value: bool) -> None:
        if self.get_facts() is not None:
            for f in self.get_facts():
                if f.get_fact() == fact.get_fact():
                    f.set_state(value)
                    return f
        return None

    def get_file(self) -> str: return self.file
    def get_verbose(self) -> bool: return self.verbose
    def get_queries(self) -> list: return self.queries
    def get_facts(self) -> dict: return self.facts
    def get_rules(self) -> list: return self.rules
    def get_init_facts(self) -> bool: return self.init_facts

    def get_rule(self, ast: DefaultNode) -> RuleNode:
        if self.get_rules() is not None:
            for rule in self.get_rules():
                if rule.get_ast() == ast:
                    return rule
        return None

    def get_fact(self, fact) -> FactNode:
        if self.get_facts() is not None:
            for f in self.get_facts():
                if f.get_fact() == fact:
                    return f
        return None
    
    def get_known_facts(self) -> bool:
        known_facts = []
        if self.get_facts() is not None:
            for facts in self.get_facts():
                if facts.get_known():
                    known_facts.append(facts)
        return known_facts

    def add_query(self, query: str, state: str = None) -> None:
        if self.get_queries() is None:
            self.set_queries([])
        factNode = self.add_fact(query, state, False, False)
        self.get_queries().append(factNode)
        return query

    def add_fact(self, fact: str, state: bool = None, known: bool = False, override: bool = True) -> None:
        if self.get_facts() is None:
            self.set_facts([])
        if self.get_fact(fact) is not None:
            if override:
                self.get_fact(fact).set_state(state)
                self.get_fact(fact).set_known(known)
            return self.get_fact(fact)
        factNode = FactNode(fact, state, known)
        self.get_facts().append(factNode)
        return factNode

    def add_rule(self, rule: DefaultNode) -> None:
        if self.rules is None:
            self.set_rules([])
        ruleNode = RuleNode(rule)
        self.rules.append(ruleNode)
        return ruleNode