# unboxer.py
from . import drg
import sys

def generate_from_referent(drg, ref, surface, complete=False, generic=False):
    #sys.stderr.write("generate from %s\n" % (ref))

    in_edges_dict = dict()
    for edge in drg.in_edges(ref):
        in_edges_dict[edge.token_index] = edge

    in_edges = []
    for key in sorted(in_edges_dict.iterkeys()):
        in_edges.append(in_edges_dict[key])

    for edge in in_edges:
        for t in edge.tokens:
            if not t in surface:
                surface.append(t)
        if edge.edge_type == "int":
            if complete:
                # recursively generate complete surface forms
                generate_from_referent(drg, drg.out_edges(edge.from_node, edge_type="ext")[0].to_node, surface)
            else:
                # generate incomplete surface forms
                if generic:
                    surface.append("*")
                else:
                    surface.append(drg.out_edges(edge.from_node, edge_type="ext")[0].to_node)
    # search for embedded DRSs
    for edge in in_edges:
        if edge.edge_type == "int":
            potential_embed = drg.out_edges(edge.from_node, edge_type="ext")[0].to_node
            # if it has an event
            #if len(drg.out_edges(potential_embed, edge_type="event")) > 0:
            #      generate_from_referent(drg, drg.out_edges(drg.out_edges(potential_embed, edge_type="event")[0].to_node, edge_type="instance")[0].to_node, surface)

def generate_from_relation(drg, int_ref, ext_ref, generic=False):
    #sys.stderr.write("generate from %s\n" % (ref))

    in_edges_dict = dict()
    for edge in drg.in_edges(int_ref):
        in_edges_dict[edge.token_index] = edge

    in_edges_int = []
    for key in sorted(in_edges_dict.iterkeys()):
        in_edges_int.append(in_edges_dict[key])

    in_edges_ext = []
    for key in sorted(in_edges_dict.iterkeys()):
        in_edges_ext.append(in_edges_dict[key])

    surface_ext = []
    generate_from_referent(drg, ext_ref, surface_ext)
    surface_int = []
    generate_from_referent(drg, int_ref, surface_int)

    # crude composition
    surface = []
    composition = False
    for token in surface_int:
        if token == ext_ref:
            surface.extend(surface_ext)
            composition = True
        else:
            surface.append(token)

    if composition:
        surface_generic = []
        for token in surface:
            if token in drg.nodes:
                if generic:
                    surface_generic.append("*")
                else:
                    surface_generic.append(drg.dereificated[token])
            else:
                surface_generic.append(token)
        return ' '.join(surface_generic)

def unbox(tuples):
    surface = []
    parser = drg.DRGParser()
    drg = parser.parse_tup_lines(tuples)

    du_list = drg.discourse_units()
    sys.stderr.write("discourse unit to visit: %s\n" % ", ".join(du_list))
    for du in du_list:
        sys.stderr.write("discourse unit: %s\n" % du)

        # generate tokens from discourse structure
        for edge in drg.in_edges(du, structure="discourse"):
            for t in edge.tokens:
                if not t in surface:
                    surface.append(t)

        # look for events (assume at most one event per discourse unit)
        for e1 in drg.out_edges(du, edge_type="event"):
            for e2 in drg.out_edges(e1.to_node, edge_type="arg"):
                sys.stderr.write("found event: %s\n" % e2.to_node)
                generate_from_referent(drg, e2.to_node, surface)
    return surface
