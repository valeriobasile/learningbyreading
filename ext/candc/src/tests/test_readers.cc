// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "base.h"

#include "io/reader.h"
#include "io/reader_group.h"
#include "io/reader_horiz.h"
#include "io/reader_multi_horiz.h"

#include "io/writer.h"
#include "io/writer_stream.h"
#include "io/writer_horiz.h"
#include "io/writer_multi_horiz.h"

using namespace std;

using namespace NLP;
using namespace NLP::IO;

int
main(int argc, char **argv){
  Sentence::FieldNames infields = argv[1];
  Sentence::FieldNames outfields = argv[2];

  while(1){
    Reader *reader = 0;
    Writer *writer = 0;
    Sentence sent;

    try {
      GroupReader *greader = new GroupReader("group");
      greader->join(new HReader(cin, IO::STDIN));
      greader->join(new MultiHReader(cin, IO::STDIN, infields));
      reader = greader;
      writer = new MultiHWriter(cout, IO::STDERR, outfields);
      while(reader->next(sent))
	writer->next(sent);
      if(cin.eof())
	return 0;
    }catch(NLP::IOException e){
      cout << "ioerror:" << e.msg << ':' << e.uri << ':' << e.line << endl;
      return 0;
    }catch(NLP::Exception e){
      cout << "error:" << e.msg << endl;
      return 0;
    }

    delete reader;
    delete writer;
  }

  return 0;
}
