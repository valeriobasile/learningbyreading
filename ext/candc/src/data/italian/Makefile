
NEW = newspaper
JRC = jrc_acquis
LAW = civil_law

VERSION = 1.0

LEX = tutccgbank/lex/$(NEW).lex \
      tutccgbank/lex/$(LAW).lex \
      tutccgbank/lex/$(JRC).lex

PRO = tutccgbank/pro/$(NEW).pro \
      tutccgbank/pro/$(LAW).pro \
      tutccgbank/pro/$(JRC).pro

CCG = tutccgbank/ccg/$(NEW).ccg \
      tutccgbank/ccg/$(LAW).ccg \
      tutccgbank/ccg/$(JRC).ccg

ccgbank: $(LEX) $(PRO) $(CCG)

release: ccgbank
	@tar cvf tutccgbank$(VERSION).tar tutccgbank/*
	@gzip tutccgbank$(VERSION).tar

TUT2CCG = prolog/binarise.pl \
          prolog/idioms.pl \
          prolog/printCCG.pl \
          prolog/printTUT.pl \
          prolog/slashes.pl \
          prolog/ccg.pl	\
          prolog/pp.pl \
          prolog/printError.pl \
          prolog/printTree.pl \
          prolog/tut2ccg.pl

# CCG lexical categories

tutccgbank/lex/$(NEW).lex: data/$(NEW).pl $(TUT2CCG)
	mkdir -p tutccgbank/lex
	swipl -c prolog/tut2ccg.pl data/$(NEW).pl --lex | grep ^LEX | cut -d" " -f2- > tutccgbank/lex/$(NEW).lex

tutccgbank/lex/$(LAW).lex: data/$(LAW).pl $(TUT2CCG)
	mkdir -p tutccgbank/lex
	swipl -c prolog/tut2ccg.pl data/$(LAW).pl --lex | grep ^LEX | cut -d" " -f2- > tutccgbank/lex/$(LAW).lex

tutccgbank/lex/$(JRC).lex: data/$(JRC).pl $(TUT2CCG)
	mkdir -p tutccgbank/lex
	swipl -c prolog/tut2ccg.pl data/$(JRC).pl --lex | grep ^LEX | cut -d" " -f2- > tutccgbank/lex/$(JRC).lex

# CCG derivations in Prolog terms

tutccgbank/pro/$(NEW).pro: data/$(NEW).pl $(TUT2CCG)
	mkdir -p tutccgbank/pro
	swipl -c prolog/tut2ccg.pl data/$(NEW).pl tutccgbank/pro/$(NEW).pro --pro

tutccgbank/pro/$(LAW).pro: data/$(LAW).pl $(TUT2CCG)
	mkdir -p tutccgbank/pro
	swipl -c prolog/tut2ccg.pl data/$(LAW).pl tutccgbank/pro/$(LAW).pro --pro

tutccgbank/pro/$(JRC).pro: data/$(JRC).pl $(TUT2CCG)
	mkdir -p tutccgbank/pro
	swipl -c prolog/tut2ccg.pl data/$(JRC).pl tutccgbank/pro/$(JRC).pro --pro

# CCG derivations, sorted on length

tutccgbank/ccg/$(NEW).ccg: data/$(NEW).pl $(TUT2CCG)
	mkdir -p tutccgbank/ccg
	swipl -c prolog/tut2ccg.pl data/$(NEW).pl tutccgbank/ccg/$(NEW).ccg --sort --ccg

tutccgbank/ccg/$(LAW).ccg: data/$(LAW).pl $(TUT2CCG)
	mkdir -p tutccgbank/ccg
	swipl -c prolog/tut2ccg.pl data/$(LAW).pl tutccgbank/ccg/$(LAW).ccg --sort --ccg

tutccgbank/ccg/$(JRC).ccg: data/$(JRC).pl $(TUT2CCG)
	mkdir -p tutccgbank/ccg
	swipl -c prolog/tut2ccg.pl data/$(JRC).pl tutccgbank/ccg/$(JRC).ccg --sort --ccg

# Converting ConsTUT into Prolog format

data/$(NEW).pl: data/$(NEW).ctu
	scripts/_convert data/$(NEW).ctu data/$(NEW).pl

data/$(LAW).pl: data/$(LAW).ctu
	scripts/_convert data/$(LAW).ctu data/$(LAW).pl

data/$(JRC).pl: data/$(JRC).ctu
	scripts/_convert data/$(JRC).ctu data/$(JRC).pl
