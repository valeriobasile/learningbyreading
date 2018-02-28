#!/bin/sh

/opt/java/jdk1.8/bin/javac -d bin -sourcepath src/ -cp `find libs/ -type f | tr "\n" ":"` src/BabelfyAloof.java
jar cvfm babelfy-aloof.jar manifest.txt -C bin/ BabelfyAloof.class
