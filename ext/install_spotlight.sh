#!/bin/sh

mkdir -p spotlight
cd spotligh
wget http://spotlight.sztaki.hu/downloads/dbpedia-spotlight-latest.jar
wget http://spotlight.sztaki.hu/downloads/latest_models/en.tar.gz
tar -xzvf en.tar.gz



# test

curl http://localhost:2222/rest/annotate   --data-urlencode "text=President Obama called Wednesday on Congress to extend a tax break for students included in last year's economic stimulus package, arguing that the policy provides more generous assistance."   --data "confidence=0.35"   -H "Accept: application/json"

