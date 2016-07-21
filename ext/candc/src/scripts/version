#!/bin/bash

PORT=$1
CPP_VERSION=src/lib/version.cc
PROLOG_VERSION=src/prolog/boxer/version.pl

ccpversion()
{
    (
	echo    'namespace NLP {'
	echo -n '  const char *VERSION = "v'
	
	if [[ -a .svn ]]; then
	    svnversion -n . | tr -d '\n';
	else
	    tr -d '\n' < RELEASE.txt;
	fi
	
	echo '";'
	echo
	
	echo -n '  const char *BUILD = "('
	echo -n $PORT 'build on' `date '+%e %B %Y, %T' | sed 's/^ //'`
	echo ')";'
	
	echo '}'
	
	) > $CPP_VERSION
}

swiversion()
{
    (
	echo ':- module(version,[version/1]).'
	echo -n "version('boxer "v
	
	if [[ -a .svn ]]; then
	    svnversion -n . | tr -d '\n';
	else
	    tr -d '\n' < RELEASE.txt;
	fi
	
	echo -n ' ('
	echo -n $PORT 'build on' `date '+%e %B %Y, %T' | sed 's/^ //'`
	echo ")').";
	
	) > $PROLOG_VERSION
}

ccpversion

if [ -f ${PROLOG_VERSION} ]; then
    SVN=`svnversion -n`
    if [ `cat ${PROLOG_VERSION} | grep $SVN | wc -l` = "0" ]; then
	swiversion
    fi
else
    swiversion
fi

exit 0


