#!/usr/bin/env python
from requests import post
from lxml import etree, objectify

def boxer(text):
    # HTTP request
    r = post('http://gingerbeard.alwaysdata.net/candcapi/proxy.php/raw/pipeline?resolve=true&instantiate=true&format=xml', data=text)
    xml = r.text.encode("utf-8")
    #with open("boxer.xml") as f:
    #       xml = f.read()
    drs = objectify.fromstring(xml)
    tagtokens = drs['xdrs']['taggedtokens']['tagtoken']
    '''
    for tagtoken in tagtokens:
        print tagtoken.attrib['{http://www.w3.org/XML/1998/namespace}id']
        for tag in tagtokens['tags']['tag']:
            print "\t{0}".format(tag.attrib)
    '''
    predicates = []
    preds = drs.findall('.//pred')
    for pred in preds:
        poslist = map(lambda x: x.attrib['pos'], pred['indexlist']['index'])
        predicate = {'token_start':eval(poslist[0])-1,
                     'token_end':eval(poslist[-1])-1,
                     'variable':pred.attrib['arg']}
        predicates.append(predicate)

    return predicates

'''
<xdrs xml:id="xdrs1">
 <taggedtokens>
  <tagtoken xml:id="i1001">
   <tags>
     <tag type="tok">When</tag>
     <tag type="pos">WRB</tag>
     <tag type="lemma">when</tag>
     <tag type="namex">O</tag>
   </tags>
  </tagtoken>
'''
