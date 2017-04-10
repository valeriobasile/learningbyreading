def get_variables_lexicalizations(tokenized, variables):
    # read DRG
    tuples = get_drg(tokenized)
    drgparser = drg.DRGParser()
    d = drgparser.parse_tup_lines(tuples)
    # de-reificate variables (build a mapping)
    reificated = dict()
    for t in d.tuples:
        if t.edge_type == "referent":
            dereificated = re.sub(".*:", "", t.to_node)
            if not dereificated in reificated:
                reificated[dereificated] = set()
            reificated[dereificated].add(t.to_node)

    # get variables surface forms
    surfaceforms = dict()
    for variable in variables.keys():
        try:
            for reificated_var in reificated[variable]:
                surface = []
                unboxer.generate_from_referent(d, reificated_var, surface, complete=True, generic=True)
                if len(surface) > 0 and ' '.join(surface)!='*':
                    if not variable in surfaceforms:
                        surfaceforms[variable] = []
                        surfaceforms[variable].append(' '.join(surface))
        except:
            log.error('cannot find mapping for variable {0}'.format(variable))
    return surfaceforms
    '''
    with open(options.output_file, "a") as f:
        for variable, content in variables.iteritems():
            if variable in surfaceforms:
                for item in content:
                    for surfaceform in surfaceforms[variable]:
                        triple = ('<{0}>'.format(item),
                                  '<{0}#entity>'.format(config.get('namespace', 'lexicalization')),
                                  '"{0}"'.format(surfaceform))
                        triples.append(triple)
                        f.write("{0} {1} {2} .\n".format(*triple))
    '''

def get_relation_lexicalizations(relations, output_file):
    # get surface forms for relations
    with open(output_file, "a") as f:
        for relation in relations:
            try:
                for arg1, arg2 in product(reificated[relation['arg1']], reificated[relation['arg2']]):
                    surface = unboxer.generate_from_relation(d, arg1, arg2)
                    if surface:
                        # TODO: implement schema from FrameBase

                        triple = ('<{0}>'.format(variables[relation['arg1']]),
                                  '<{0}>'.format(relation['symbol']),
                                  '<{0}>'.format(variables[relation['arg2']]),
                                  '<{0}#relation>'.format(config.get('namespace', 'lexicalization')),
                                  '"{0}"'.format(surface))
                        triples.append(triple)
                        f.write("{0} {1} {2} {3} {4} .\n".format(*triple))
            except:
                log.error('cannot find variable mapping for relation {0} {1} {2}'.format(relation['arg1'], relation['symbol'], relation['arg2']))
