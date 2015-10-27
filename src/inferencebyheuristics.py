#!/usr/bin/env python
from SPARQLWrapper import SPARQLWrapper, JSON
from sys import stderr

DEPTH = 3

def get_dbpedia_categories(entity):
    query = """
    SELECT ?cat2
    WHERE {{
      {0} <http://purl.org/dc/terms/subject> ?cat1 .
      ?cat1 <http://www.w3.org/2004/02/skos/core#broader>{{,2}} ?cat2 .
    }} limit 100
    """.format(entity)

    sparql = SPARQLWrapper("http://lod.openlinksw.com/sparql/")
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    try:
        results = sparql.query().convert()
        categories = map(lambda x: x['cat2']['value'], results["results"]["bindings"])
        return categories
    except:
        stderr.write('error retrieving the categories for {0}\n'.format(entity))
        return []

def is_room(entity):
    categories = get_dbpedia_categories(entity)
    return 'http://dbpedia.org/resource/Category:Rooms' in categories

def is_tool(entity):
    categories = get_dbpedia_categories(entity)
    return 'http://dbpedia.org/resource/Category:Tools' in categories

with open('build/cooccurrence.ttl') as f:
    lines = f.readlines()

stderr.write('reading cooccurrence graph\n')
triples = map(lambda x: x.strip().split(' '), lines)

# filter only dbpedia entities
triples = [t for t in triples if 'dbpedia' in t[0] and 'dbpedia' in t[2]]

# get the list of entities
stderr.write('extracting entities\n')
entities = set()
for triple in triples:
    subj, pred, obj = triple
    entities.add(subj)
    entities.add(obj)

# get tools and rooms
n = 0
rooms = set()
tools = set()
for entity in entities:
    stderr.write('progress: {0:.2f}%\r'.format((float(n)*100.0)/float(len(entities))))
    if is_tool(entity):
        tools.add(entity)
    if is_room(entity):
        rooms.add(entity)
    n += 1
stderr.write('\n')

# write down lists of entities, rooms and tools:
stderr.write('writing down entities\n')
with open('build/entities.txt', 'w') as f:
    f.write('{0}\n'.format('\n'.join(list(entities))))
with open('build/rooms.txt', 'w') as f:
    f.write('{0}\n'.format('\n'.join(list(rooms))))
with open('build/tools.txt', 'w') as f:
    f.write('{0}\n'.format('\n'.join(list(tools))))

n = 0
stderr.write('inferencing\n')
with open('build/inferences.ttl', 'w') as f:
    for triple in triples:
        subj, pred, obj = triple
        stderr.write('progress: {0:.2f}%\r'.format((float(n)*100.0)/float(len(triples))))
        if subj in rooms and obj in tools:
            f.write("{0} aloof:likelyLocation {1}\n".format(obj,subj))
        n += 1
    stderr.write('\n')
