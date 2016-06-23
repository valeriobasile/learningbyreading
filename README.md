# KNEWS: Knowledge Extraction With Semantics

A Learning by Reading pipeline of NLP and Entity Linking tools.

KNEWS is a composite tool that bridges semantic parsing (using [C&C
tools and Boxer](http://valeriobasile.github.io/candcapi/)), word
sense disambiguation (using [UKB](http://ixa2.si.ehu.es/ukb/) or
[Babelfy](http://babelfy.org/)) and entity linking (using Babelfy or
[DBpedia
Spotlight](https://github.com/dbpedia-spotlight/dbpedia-spotlight)) to
produce a unified, LOD-compliant abstract representation of meaning.

KNEWS can produce several kinds of output:

1. Frame instances, based on the [FrameBase](http://www.framebase.org/) scheme:

```<http://framebase.org/ns/fi-Operate_vehicle_0059a98c-3870-49ed-87e1-f882e11a49f7> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://framebase.org/ns/frame-Operate_vehicle-drive.v> .
<http://framebase.org/ns/fi-Operate_vehicle_0059a98c-3870-49ed-87e1-f882e11a49f7> <http://framebase.org/ns/fe-Driver> <http://dbpedia.org/resource/Robot> .
<http://framebase.org/ns/fi-Operate_vehicle_0059a98c-3870-49ed-87e1-f882e11a49f7> <http://framebase.org/ns/fe-Vehicle> <http://wordnet-rdf.princeton.edu/wn31/02961779-n> .```

2. Word-aligned semantics, based on [lexicalized Discourse Representation Graphs](http://www.rug.nl/research/portal/files/26252478/Complete_thesis.pdf):

```
<frameinstances>
  <frameinstance id="Operate_vehicle_9a3fa55e-4d97-406a-ab0d-cf681e277296" type="Operate_vehicle-drive.v" internalvariable="e1">
    <framelexicalization>k3:x1 is driving k3:x2</framelexicalization>
    <instancelexicalization>A robot is driving the car</instancelexicalization>
    <frameelements>
      <frameelement role="Driver" internalvariable="x1">
        <concept>http://dbpedia.org/resource/Robot</concept>
        <roleexicalization>A robot is driving x2</roleexicalization>
        <conceptlexicalization/>
      </frameelement>
      <frameelement role="Vehicle" internalvariable="x2">
        <concept>http://wordnet-rdf.princeton.edu/wn31/02961779-n</concept>
        <roleexicalization>x1 is driving the car</roleexicalization>
        <conceptlexicalization/>
      </frameelement>
    </frameelements>
  </frameinstance>
</frameinstances>
```

3. First-order logic formulae with WordNet synsets and DBpedia ids as symbols:

```
fol(1,some(A,and(02961779-n(A),some(B,some(C,and(r1Theme(B,A),and(r1Agent(B,C),and(01934845-v(B),Robot(C))))))))).
```

Online demo
-----------

Coming soon


Test the installation
---------------------

$ src/pipeline.py -i input.txt -o output.txt

or

$ src/pipeline.py -d input/ -o output.txt

Install the C&C tools
---------------------

The C&C tools are needed if an endpoint for an equivalent HTTP API is
not provided. The script **ext/install_candc.sh** downloads and install
the development version of the C&C tools. valid credentials
are required, obtainable by registration:
http://svn.ask.it.usyd.edu.au/trac/candc/wiki/Register

Put the username and password in the first two lines of the script
CANDCUSER=*your username*
CANDCPASSWORD=*your password*
