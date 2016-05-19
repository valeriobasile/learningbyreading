from pygraph.classes.digraph import digraph
from pygraph.algorithms.searching import depth_first_search
import sys
import re

RHETORICAL_RELATIONS = [
    "continuation",
    "narration",
    "result",
    "contrast",
    "parallel",
    "precondition",
    "consequence",
    "conditional",
    "alternation",
    "background",
    "elaboration",
    "explanation",
    "source",
    "attribution",
    "presupposition",
    "because",
    "since",
    "until"
]

def is_event(node):
    return node[0] == "e" and node[1:].isdigit()

class DRGTuple:
    def __init__(self):
        self.from_node = ""
        self.edge_type = ""
        self.to_node = ""
        self.structure = ""
        self.token_index = ""
        self.tokens = []

class DRG:
    def __init__(self):
        self.tuples = []
        self.nodes = set()
        self.parent = dict()

    def add_tuple(self, tup):
        self.tuples.append(tup)
        self.nodes.add(tup.from_node)
        self.nodes.add(tup.to_node)
        self.parent[tup.to_node] = tup.from_node

    def root(self):
        for node in self.nodes:
            if not node in self.parent:
                return node

    # returns an ordered list of discourse unit to generate from
    def discourse_units(self):
        discourse_units = digraph()
        for tup in self.tuples:
            if tup.structure == "discourse" or tup.edge_type == "dominates" or "subordinates" in tup.edge_type:
                if not tup.from_node in discourse_units.nodes():
                    discourse_units.add_node(tup.from_node)
                if not tup.to_node in discourse_units.nodes():
                    discourse_units.add_node(tup.to_node)
                discourse_units.add_edge((tup.from_node, tup.to_node))
        st, order_pre, order_post = depth_first_search(discourse_units, root="k0")
        return order_pre

    def in_edges(self, node, edge_type="", structure=""):
        edges = []
        for tup in self.tuples:
            if tup.to_node == node and (edge_type == "" or edge_type == tup.edge_type) and (structure == "" or structure == tup.structure):
                edges.append(tup)
        return edges

    def out_edges(self, node, edge_type="", structure=""):
        edges = []
        for tup in self.tuples:
            if tup.from_node == node and (edge_type == "" or edge_type == tup.edge_type) and (structure == "" or structure == tup.structure):
                edges.append(tup)
        return edges

    def neighbors(self, node):
        neighbors = []
        for tup in self.tuples:
            if tup.from_node == node:
                neighbors.append(tup.to_node)
        return neighbors

    # returns the list of neighbors to visit, ordered by token index
    def visit_neighbors(self, node):
        neighbors = []
        for tup in self.tuples:
            if tup.from_node == node:
                if not (tup.edge_type == "referent" and tup.from_node == "k0"):
                    neighbors.append((tup.token_index, tup.to_node, tup.tokens))
        neighbors = sorted(neighbors, key=lambda token_index: neighbors[0])
        return neighbors

    def make_reification_mapping(self):
        # de-reificate variables (build a mapping)
        self.reificated = dict()
        self.dereificated = dict()
        for t in self.tuples:
            if t.edge_type == "referent":
                dereificated_var = re.sub(".*:", "", t.to_node)
                if not dereificated_var in self.reificated:
                    self.reificated[dereificated_var] = set()
                self.reificated[dereificated_var].add(t.to_node)
                self.dereificated[t.to_node] = dereificated_var

class DRGParser:
    def __init__(self):
        pass

    def parse_tup_file(self, tup_file):
        drg = DRG()
        fd_tup = open(tup_file)
        for line in fd_tup:
            if line[0] != "%" and line != "\n":
                tup = self.parse_tup_line(line)
                drg.add_tuple(tup)
        fd_tup.close()
        drg.make_reification_mapping()
        return drg

    def parse_tup_lines(self, lines):
        drg = DRG()
        for line in lines:
            if line[0] != "%" and line != "\n":
                tup = self.parse_tup_line(line)
                drg.add_tuple(tup)
        drg.make_reification_mapping()
        return drg

    def parse_tup_line(self, line):
        tup = DRGTuple()
        fields = line[:-1].decode("utf-8").split()

        tup.edge_type = fields[1].split("-")[0]
        tup.from_node = fields[0]
        tup.to_node = fields[2]

        if tup.edge_type in RHETORICAL_RELATIONS:
            tup.structure = "discourse"
        elif tup.edge_type in ["referent", "dominates"] or ("subordinates" in tup.edge_type):
            tup.structure = "structure"
        elif tup.edge_type in ["surface", "punctuation"]:
            tup.structure = "surface"
        else:
            tup.structure = "argument"

        try:
            tup.token_index = eval(fields[3])
        except:
           print line
           sys.exit(1)
        tup.tokens = fields[5:-1]
        return tup
