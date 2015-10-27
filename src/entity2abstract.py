#!/usr/bin/env python
from sys import argv
from SPARQLWrapper import SPARQLWrapper, JSON

def get_abstract(entity):
    query = """
    select ?abstract where {{
      {0}   <http://dbpedia.org/ontology/abstract> ?abstract .
      FILTER langMatches(lang(?abstract),'en')
    }} LIMIT 100
    """.format(entity)

    sparql = SPARQLWrapper("http://lod.openlinksw.com/sparql/")
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()
    try:
        abstract = results['results']['bindings'][0]['abstract']['value'].encode('utf-8')
        return abstract
    except:
        return None

abstract = get_abstract(argv[1])
if abstract:
    print abstract
