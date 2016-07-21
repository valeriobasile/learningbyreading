/* -*- Mode: C++; -*- */
// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#import "stlstring.h"

//gsoap ns1 service name: candc.binding
//gsoap ns1 service type: candc.port_type
//gsoap ns1 service port: http://localhost:9004
//gsoap ns1 service namespace: urn:candc

typedef std::string xsd__string;
typedef double xsd__double;
typedef bool xsd__bool;

int ns1__parse_USCOREstring(xsd__string _text, bool _start, xsd__string &_result);
int ns1__parse_USCOREsentence(xsd__string _text, xsd__string _printer, xsd__string &_result);
int ns1__parse_USCOREoracle(xsd__string _text, xsd__string _constraints,
                            xsd__string _printer, xsd__string &_result);
