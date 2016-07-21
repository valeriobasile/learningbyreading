#!/bin/bash
#
# Running this script assumes that the Make target `py_candc' is already build.
# $ cd ../../ && make py_candc
# 
# Typical usage:
# $ ./run.sh --pos /home/tim/models/pos --super /home/tim/models/super --parser /home/tim/models/parser
#

cd `dirname $0`/../..

if [ -z "${LD_LIBRARY_PATH}" ]; then
  export LD_LIBRARY_PATH="`pwd`/lib:`pwd`/ext/lib"
else
  export LD_LIBRARY_PATH="`pwd`/lib:`pwd`/ext/lib:${LD_LIBRARY_PATH}"
fi

if [ -z "${PYTHONPATH}" ]; then
  export PYTHONPATH="`pwd`:`pwd`/src/api"
else
  export PYTHONPATH="`pwd`:`pwd`/src/api:${PYTHONPATH}"
fi

cd src/web
./interface.py $@
