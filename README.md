# learningbyreading
Learning by Reading pipeline of NLP and Entity Linking tools

test:

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
