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

#include "io/format.h"

#include "io/writer.h"
#include "io/writer_factory.h"
#include "io/writer_stream.h"
#include "io/writer_format.h"
#include "io/writer_horiz.h"
#include "io/writer_multi_horiz.h"
#include "io/writer_vert.h"
#include "io/writer_multi_vert.h"

using namespace std;

namespace NLP { namespace IO {

WriterFactory::WriterFactory(const std::string &uri, const Format &fmt)
  : Writer(uri, "WriterFactory(" + std::string(fmt) + ")"),
    output(uri),
    writer(new FormatWriter(output.stream, uri, fmt)){
}

} }
