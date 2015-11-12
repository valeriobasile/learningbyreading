#!/bin/sh

javac -d bin -sourcepath src/ -cp `find lib/ -type f | tr "\n" ":"` src/BabelfyAloof.java
jar cvfm babelfy-aloof.jar manifest.txt -C bin/ BabelfyAloof.class
