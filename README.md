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
```
<http://framebase.org/ns/fi-Operate_vehicle_0059a98c-3870-49ed-87e1-f882e11a49f7> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://framebase.org/ns/frame-Operate_vehicle-drive.v> .
<http://framebase.org/ns/fi-Operate_vehicle_0059a98c-3870-49ed-87e1-f882e11a49f7> <http://framebase.org/ns/fe-Driver> <http://dbpedia.org/resource/Robot> .
<http://framebase.org/ns/fi-Operate_vehicle_0059a98c-3870-49ed-87e1-f882e11a49f7> <http://framebase.org/ns/fe-Vehicle> <http://wordnet-rdf.princeton.edu/wn31/02961779-n> .
```
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

A demo of KNEWS is now available at [http://gingerbeard.alwaysdata.net/knews/](http://gingerbeard.alwaysdata.net/knews/).

# Installation and configuration

After cloning the repository or otherwise downloaded the KNEWS source code, you must instals the prerequisite Python packages listed in the file *requirements.txt*.
With *pip*, this is done with:

    $ pip install -r requirements.txt

Next, you must configure how to run the C&C tools. Open the file *config/boxer.conf* and select a value for *mode*:

 * *online* will access the [online API]. This is the easiest solution but is it unpractical if KNEWS is used to parse a large amount of text.
 * *local* will use a local installation of the C&C tools (see below for instructions on how to get this running).
 * *soap* will usa a local installation of the C&C tools with the SOAP-based client/server architecture, convenient for parsing many different files.

## Installation of the C&C tools and Boxer

The C&C source code is included in the KNEWS repository (revision v2614). A shell script is provided to automate the compilation and installation. To install the C&C tools locally run

    $ cd ext/
    $ ./install_candc.sh

By default the script expects to be compiled on unix. In order to compile on other platforms
please modify the `install_candc.sh` accordingly. For example, on macOS you change:
 
`ln -s Makefile.unix Makefile` to `ln -s Makefile.macosx Makefile`

**Please note**: you will need a working installation of [swi-prolog](http://www.swi-prolog.org/Download.html) in order to compile Boxer.

To test that the installation has completed successfully run (from the *candc/* directory):

    $ bin/candc --version
    $ candc v2614 (unix build on 19 April 2016, 11:35:31)
    $ bin/boxer --version
    $ boxer v2614 (unix build on 19 April 2016, 11:35:31)

To use the SOAP client/server version of the C&C tools, run the server first with the following command line (from the *candc/* directory):

    $ bin/soap_server --server localhost:8888 --models models/boxer/ --candc-printer boxer
    $ waiting for connections on localhost:8888

## Configuration of the disambiguation tools

You must configure which module to use for word sense disambiguation and entity linking. Open the file *config/disambiguation.conf* and set a value for wsd->module:

  * *babelfy* uses the [Babelfy](http://babelfy.org/) online API. **Note**: a valid API key is needed. You must [request it](http://babelnet.org/register) and write it in the *config/babelfy.var.properties* file.
  * *ukb* uses the [UKB](http://ixa2.si.ehu.es/ukb/) Word Sense Disambiguation system. A script is provided in the **ext/** directory to download and install it.
  * *lesk* uses the [Enhanced Lesk WSD algorithm](https://github.com/pippokill/lesk-wsd-dsm) proposed by P. Basile et al. A script is provided in the *ext/* directory to download and install it.
  
You can also configure an entity linking module in the **config/disambiguation.conf* file:

  * *babelfy* uses the [Babelfy](http://babelfy.org/) online API. *Note*: a valid API key is needed. You must [request it](http://babelnet.org/register) and write it in the *config/babelfy.var.properties* file.
  * *spotlight* uses the [DBpedia Spotlight](https://github.com/dbpedia-spotlight/dbpedia-spotlight) online API.
  * *none* makes KNEWS skip the entity linking step altogether.

## Test the installation

$ src/pipeline.py -i input.txt -o output.txt

or

$ src/pipeline.py -d input/ -o output.txt
