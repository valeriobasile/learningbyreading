import sys
import candc

def run():
  parser.parse(sentence, postags, stags)

def test(sentence):
  candc.read(parser.sentence, sentence)
  integration.parse(parser.sentence, decoder, printer, False)
  for label, head, filler in printer.deps:
    print '%s_%s %s %s_%s' % (head[0], head[1], label, filler[0], filler[1])

bare = ['NP[nb]/N']
N = ['N']
NP = ['NP']
ditrans = ['((S[dcl]\\NP)/NP)/NP']
trans = ['(S[dcl]\\NP)/NP']
conj = ['conj']
control = ['((S[dcl]\NP)/(S[to]\NP))/NP']

base = "models/"

integration = candc.load(base + "/super", base + "/parser")
cats = integration.cats
parser = integration.parser
decoder = candc.ccg.DecoderFactory('random')
chart = parser.chart
printer = candc.ccg.PythonPrinter(cats)

#sentence = "likes and slowly drinks".split()
#postags = "VBZ CC RB VBZ".split()
#stags = "(S[dcl]\NP)/NP conj (S\NP)/(S\NP) (S[dcl]\NP)/NP".split()

#sentence = "Today , the pixie-like clarinetist has mostly dropped the missionary work -LRB- though a touch of the old Tashi still survives -RRB- and now goes on the road with piano , bass , a slide show , and a repertoire that ranges from light classical to light jazz to light pop , with a few notable exceptions .".split()
#postags = "NN , DT JJ NN VBZ RB VBN DT JJ NN LRB IN DT NN IN DT JJ NNP RB VBZ RRB CC RB VBZ IN DT NN IN NN , NN , DT NN NN , CC DT NN WDT VBZ IN JJ NN TO JJ NN TO JJ NN , IN DT JJ JJ NNS .".split()
#stags = [[ 'S/S' ], [ ',' ], [ 'NP[nb]/N' ], [ 'N/N' ], [ 'N' ], [ '(S[dcl]\NP)/(S[pt]\NP)' ], [ '(S\NP)\(S\NP)',  '(S\NP)/(S\NP)' ], [ '(S[pt]\NP)/NP' ], [ 'NP[nb]/N' ], [ 'N/N' ], [ 'N' ], [ 'LRB',  '(NP\NP)/NP' ], [ '((S\NP)\(S\NP))/S[dcl]',  '(NP\NP)/NP',  'NP/NP',  '(S/S)/NP' ], [ 'NP[nb]/N' ], [ 'N' ], [ '(NP\NP)/NP' ], [ 'NP[nb]/N' ], [ 'N/N' ], [ 'N' ], [ '(S\NP)/(S\NP)' ], [ 'S[dcl]\NP',  '(S[dcl]\NP)/NP' ], [ 'RRB' ], [ 'conj' ], [ '(S\NP)/(S\NP)' ], [ '(S[dcl]\NP)/PP',  'S[dcl]\NP' ], [ 'PP/NP',  '((S\NP)\(S\NP))/NP' ], [ 'NP[nb]/N' ], [ 'N' ], [ '(NP\NP)/NP',  '((S\NP)\(S\NP))/NP',  'PP/NP' ], [ 'N',  'N/N' ], [ ',' ], [ 'N' ], [ ',' ], [ 'NP[nb]/N' ], [ 'N/N' ], [ 'N' ], [ ',' ], [ 'conj' ], [ 'NP[nb]/N' ], [ 'N' ], [ '(NP\NP)/(S[dcl]\NP)' ], [ 'S[dcl]\NP',  '(S[dcl]\NP)/PP' ], [ 'PP/NP',  '((S\NP)\(S\NP))/NP',  '(((S\NP)\(S\NP))/((S\NP)\(S\NP)))/NP' ], [ 'N/N' ], [ 'N' ], [ '((S\NP)\(S\NP))/NP', 'PP/NP',  '(NP\NP)/NP' ], [ 'N/N' ], [ 'N' ], [ '((S\NP)\(S\NP))/NP',  '(NP\NP)/NP',  'PP/NP' ], [ 'N/N' ], [ 'N' ], [ ',' ], [ '((S\NP)\(S\NP))/NP',  '(NP\NP)/NP',  'PP/NP',  '((S\NP)/(S\NP))/NP',  '(S/S)/NP',  '(((S\NP)\(S\NP))/(S[ng]\NP))/NP' ], [ 'NP[nb]/N' ], [ 'N/N' ], [ 'N/N' ], [ 'N' ], [ '.' ]]

#sentence = "Ms. Haag plays Elianti .".split()
#postags = "NNP NNP VBZ NNP .".split()
#stags = "N/N N (S[dcl]\NP)/NP N .".split()

raw = map(lambda x: x.split('|'), "A|DT|NP[nb]/N SEC|NNP|N/N proposal|NN|N to|TO|(S[to]\NP)/(S[b]\NP) ease|VB|(S[b]\NP)/NP reporting|NN|N/N requirements|NNS|N for|IN|(NP\NP)/NP some|DT|NP[nb]/N company|NN|N/N executives|NNS|N would|MD|(S[dcl]\NP)/(S[b]\NP) undermine|VB|(S[b]\NP)/NP the|DT|NP[nb]/N usefulness|NN|N of|IN|(NP\NP)/NP information|NN|N on|IN|(NP\NP)/NP insider|NN|N/N trades|NNS|N as|IN|(NP\NP)/NP a|DT|NP[nb]/N stock-picking|JJ|N/N tool|NN|N ,|,|, individual|JJ|N/N investors|NNS|N and|CC|conj professional|JJ|N/N money|NN|N/N managers|NNS|N contend|VBP|(S[dcl]\S[dcl])\NP .|.|.".split())
sentence = map(lambda x: x[0], raw)
postags = map(lambda x: x[1], raw)
stags = map(lambda x: x[2], raw)

#sentence = "slept said John ,".split()
#postags = "VBN VBD NNP ,".split()
#stags = "S[pt]\NP (S[dcl]/S[dcl])/NP NP ,".split()

#sentence = "John said yesterday and reiterated today".split()
#postags = "NNP VBD RB CC VBD RB".split()
#stags = "NP (S[dcl]\S[dcl])\NP S\S conj (S[dcl]\S[dcl])\NP S\S".split()

#sentence = "John likes the woman running the pub".split()
#postags = "NNP VBZ DT NN VBG DT NN".split()
#stags = "NP (S[dcl]\NP)/NP NP[nb]/N N (S[ng]\NP)/NP NP[nb]/N N".split()

#sentence = "These combines , some of which are great , are expensive".split()
#postags = "DT NNS , DT IN WDT VBP JJ , VBD JJ".split()
#stags = "NP[nb]/N N , NP (NP\NP)/NP ((NP\NP)/(S[dcl]\NP))\(NP/NP) (S[dcl]\NP)/(S[adj]\NP) S[adj]\NP , (S[dcl]\NP)/(S[adj]\NP) S[adj]\NP".split()

#sentence = "The work which they will pay the man 5 pounds for".split()
#postags = "DT NN WDT PRP VB VBD DT NN CD NNS IN".split()
#stags = "NP[nb]/N N (NP\NP)/(S[dcl]/NP) NP (S[dcl]\NP)/(S[b]\NP) (((S[b]\NP)/PP)/NP)/NP NP[nb]/N N N/N N PP/NP".split()

#sentence = "It is one thing to sterilize and another to pollute".split()
#postags = "PRP VBZ CD NN TO VB CC DT NN TO VB".split()
#stags = "NP[expl] ((S[dcl]\NP[expl])/(S[to]\NP))/NP N/N N (S[to]\NP)/(S[b]\NP) S[b]\NP conj N (S[to]\NP)/(S[b]\NP) S[b]\NP".split()

#sentence = ['jeopardise', 'efforts', 'to', 'stop', 'or', 'slow', 'down', 'the', 'subsidence']  
#postags = ['VB', 'NNS', 'TO', 'VB', 'CC', 'VB', 'RP', 'DT', 'NN']  
#stags = [['(S[dcl]\NP)/NP'], ['N'], ['(S[to]\NP)/(S[b]\NP)'], ['(S[b]\NP)/NP'], ['conj'], ['(S[b]\NP)/NP'], ['(S\NP)\(S\NP)'], ['NP[nb]/N'], ['N']]

#sentence = ['dinner', 'and', 'dancing', '--', 'a', 'block', 'away', '.']
#postags = 'NN CC NN : DT NN RB .'.split()
#stags_corr = [['N'], ['conj'], ['N'], [':'], ['NP[nb]/N'], ['N'], ['(NP\NP)\NP'], ['.']]

#stags = [['N'], ['conj','N/N'], ['N'], [':','(NP\NP)/NP'], ['NP[nb]/N'], ['N'], ['(NP\NP)\NP'], ['.']]

#sentence = ['Work', 'which', 'they', 'paid', 'Barry', 'money', 'for']
#postags = 'NN WDT PRP VBD NNP NNP IN'.split()
#stags = [['NP'], ['(NP\NP)/(S[dcl]/NP)'], ['NP'], ['(((S[dcl]\NP)/PP)/NP)/NP'], ['NP'], ['NP'], ['PP/NP']]

#gen = Generator(cats, 300)
#TBchart = gen.TBchart
#chart = gen.chart

#sentence = ['Taiwan', 'has', 'improved', 'its', 'standing', 'with', 'the', 'U.S.', 'by', 'initialing', 'a', 'bilateral', 'copyright', 'agreement', ',', 'amending', 'its', 'trademark', 'law', 'and', 'introducing', 'legislation', 'to', 'protect', 'foreign', 'movie', 'producers', 'from', 'unauthorized', 'showings', 'of', 'their', 'films', '.']

#stags = [['N'], ['(S[dcl]\NP)/(S[pt]\NP)'], ['(S[pt]\NP)/NP'], ['NP[nb]/N'], ['N'], ['(NP\NP)/NP', '((S\NP)\(S\NP))/NP', 'PP/NP'], ['NP[nb]/N'], ['N'], ['((S\NP)\(S\NP))/(S[ng]\NP)'], ['(S[ng]\NP)/NP', '((S[ng]\NP)/PP)/NP'], ['(NP\NP)/N', 'NP[nb]/N'], ['N/N'], ['N/N'], ['N'], [','], ['(S[ng]\NP)/NP', '((S[ng]\NP)/PP)/NP'], ['NP[nb]/N'], ['N/N'], ['N'], ['conj'], ['N/N', '(S[ng]\NP)/NP', '((S[ng]\NP)/PP)/NP'], ['N'], ['(S[to]\NP)/(S[b]\NP)'], ['((S[b]\NP)/PP)/NP', '(S[b]\NP)/NP'], ['N/N'], ['N/N'], ['N'], ['((S[adj]\NP)\(S[adj]\NP))/(S[adj]\NP)', 'PP/(S[adj]\NP)', '(NP\NP)/NP', '(((S\NP)\(S\NP))/((S\NP)\(S\NP)))/NP', '((S\NP)\(S\NP))/(S[adj]\NP)', '(NP\NP)/(S[adj]\NP)', '(((S\NP)\(S\NP))/NP)/NP', 'PP/NP', '((S\NP)\(S\NP))/NP', '((NP\NP)/(NP\NP))/NP'], ['N/N'], ['N'], ['PP/NP', '(NP\NP)/NP'], ['NP[nb]/N'], ['N'], ['.']]

#stags = [['N'], ['(S[dcl]\NP)/(S[pt]\NP)'], ['(S[dcl]\NP)/NP', 'S[pt]\NP', 'N/N', 'S[dcl]\NP'], ['NP[nb]/N'], ['S[ng]\NP', 'N'], ['(NP\NP)/NP', '(((S\NP)\(S\NP))/(S[ng]\NP))/NP', '((S\NP)\(S\NP))/NP', 'PP/NP', '(S/S)/NP'], ['NP[nb]/N'], ['N', 'N/N'], ['((S\NP)\(S\NP))/NP', '((S\NP)\(S\NP))/(S[ng]\NP)', 'PP/(S[ng]\NP)', '(S/S)/(S[ng]\NP)'], ['((S[ng]\NP)/(S[to]\NP))/NP', '((S[ng]\NP)/(S[b]\NP))/NP', '(S[ng]\NP)/S[dcl]', 'N', '((S[ng]\NP)/(S[adj]\NP))/NP', '(S[ng]\NP)/NP', '((S\NP)\(S\NP))/NP', '((S[ng]\NP)/PP)/NP', '((S[ng]\NP)/NP)/NP', '(S[ng]\NP)/S[em]'], ['((S[adj]\NP)/(S[adj]\NP))/N', '(NP\NP)/N', '((S\NP)\(S\NP))/N', 'NP[nb]/N', '(N/N)/(N/N)', 'NP/NP'], ['(N/N)/(N/N)', 'N/N'], ['N', 'N/N'], ['N'], [','], ['((S[ng]\NP)/(S[to]\NP))/NP', '(S[ng]\NP)/S[dcl]', '(S/S)/NP', 'N', '((S[ng]\NP)/(S[adj]\NP))/NP', 'N/N', '(NP\NP)/NP', '(S[ng]\NP)/NP', '((S\NP)\(S\NP))/NP', '((S[ng]\NP)/PP)/(S[adj]\NP)', '((S[ng]\NP)/PP)/NP', '(S[ng]\NP)/PP', '((S[ng]\NP)/NP)/NP', '((S[ng]\NP)/(S[ng]\NP))/NP', '((S[ng]\NP)/NP)/PP', 'S[ng]\NP'], ['NP[nb]/N'], ['N/N'], ['N', 'N/N'], ['conj', 'N'], ['((S[ng]\NP)/(S[to]\NP))/NP', '(S[ng]\NP)/(S[adj]\NP)', '((S[ng]\NP)/(S[adj]\NP))/NP', 'N/N', '(S[ng]\NP)/NP', '((S[ng]\NP)/PP)/(S[adj]\NP)', '((S[ng]\NP)/PP)/NP', '(S[ng]\NP)/PP', '(S[ng]\NP)/(S[to]\NP)', 'S[ng]\NP'], ['N'], ['(S[to]\NP)/(S[b]\NP)'], ['((S[b]\NP)/PP)/NP', '(S[b]\NP)/NP'], ['N/N'], ['N', 'N/N'], ['N'], ['((S[adj]\NP)\(S[adj]\NP))/(S[adj]\NP)', 'PP/(S[adj]\NP)', '(N/N)/(N/N)', '(NP\NP)/NP', '((S[adj]\NP)\(S[adj]\NP))/NP', '(((S\NP)\(S\NP))/((S\NP)\(S\NP)))/NP', '(PP/PP)/NP', 'PP/(S[ng]\NP)', '((S\NP)\(S\NP))/(S[adj]\NP)', '(S/S)/NP', '(NP\NP)/(S[adj]\NP)', '((NP\NP)\(NP\NP))/NP', '(((S\NP)\(S\NP))\((S\NP)\(S\NP)))/NP', '(((S\NP)\(S\NP))/NP)/NP', 'PP/NP', '((S\NP)\(S\NP))/NP', '((S\NP)\(S\NP))/PP', '((NP\NP)/(NP\NP))/NP'], ['N/N'], ['N'], ['(S\NP)\(S\NP)', 'PP/NP', '((S[adj]\NP)\(S[adj]\NP))/NP', '((S\NP)\(S\NP))/NP', '(NP\NP)/NP', '((N/N)\(N/N))/NP', '(NP\NP)/(S[ng]\NP)'], ['NP\NP', 'NP[nb]/N'], ['N'], ['.']]

#sentence = 'William S. Smith'.split()
#postags = 'NNP NNP NNP'.split()
#stags = [['N/N'], ['N/N'], ['N']]

#sentence = 'Ms. Haag plays Elianti'.split()
#postags = 'NNP NNP VBZ NNP'.split()
#stags = [['N/N', 'N'], ['N', ','], ['S[dcl]\NP', '(S[dcl]\NP)/NP'], ['N', 'S[b]\NP']]

#sentence = 'A record date has n\'t been set'.split()
#postags = 'DT NN NN VBZ RB VBN VBN'.split()

#stags = [['NP[nb]/N'], ['N/N'], ['N'], ['(S[dcl]\NP)/(S[pt]\NP)'], ['(S\NP)\(S\NP)', 'S\S'], ['(S[pt]\NP)/(S[pss]\NP)'], ['S[pss]\NP']]

#sentence = ['The', 'rules', 'will', 'eliminate', 'filings', 'policy-making', 'divisions', ',', 'such', 'as', 'sales', ',', 'marketing', ',', 'finance', 'and', 'research', 'and', 'development', ',', 'Mr.', 'Lane', 'said', '.']

#postags = ['N','N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N']

#stags = [['NP[nb]/N'], ['N'], ['(S[dcl]\NP)/(S[b]\NP)', '(S[dcl]\NP)/(S[dcl]\NP)'], ['(S[b]\NP)/NP', '((S[b]\NP)/PP)/NP'], ['N/N', 'N'], ['((S[ng]\NP)/(S[to]\NP))/NP', '((S[ng]\NP)/(S[b]\NP))/NP', '(S[ng]\NP)/(S[pss]\NP)', '(S[ng]\NP)/(S[adj]\NP)', '((S[ng]\NP)/(S[adj]\NP))/NP', 'N/N', '(NP\NP)/NP', '(S[ng]\NP)/NP', '((S\NP)\(S\NP))/NP', '((S[ng]\NP)/PP)/(S[adj]\NP)', '((S[ng]\NP)/PP)/NP', '(S[ng]\NP)/S[qem]', '((S[ng]\NP)/NP)/NP', '((S[ng]\NP)/(S[ng]\NP))/NP', '((S[ng]\NP)/PP)/PP', '((S[ng]\NP)/NP)/PP', 'S[ng]\NP', '((S[ng]\NP)/(S[pss]\NP))/NP'], ['N'], [','], ['(NP\NP)/(NP\NP)', '((S\NP)\(S\NP))/((S\NP)\(S\NP))'], ['PP/NP', 'N/N', 'PP/(S[adj]\NP)', '(NP\NP)/NP', '((S\NP)\(S\NP))/NP'], ['N/N', 'N'], [','], ['N/N', 'N'], [','], ['(S[b]\NP)/NP', 'N/N', 'N'], ['conj', 'N/N'], ['N', 'N/N'], ['conj', 'N/N'], ['N'], [','], ['N/N'], ['N'], ['(S[dcl]\S[dcl])\NP'], ['.']]

#stags = [['NP[nb]/N'], ['N'], ['(S[dcl]\NP)/(S[b]\NP)', '(S[dcl]\NP)/(S[dcl]\NP)'], ['(S[b]\NP)/NP', '((S[b]\NP)/PP)/NP'], ['N/N', 'N'], ['N/N'], ['N'], [','], ['(NP\NP)/(NP\NP)', '((S\NP)\(S\NP))/((S\NP)\(S\NP))'], ['PP/NP', 'N/N', 'PP/(S[adj]\NP)', '(NP\NP)/NP', '((S\NP)\(S\NP))/NP'], ['N/N', 'N'], [','], ['N/N', 'N'], [','], ['(S[b]\NP)/NP', 'N/N', 'N'], ['conj', 'N/N'], ['N', 'N/N'], ['conj', 'N/N'], ['N'], [','], ['N/N'], ['N'], ['(S[dcl]\S[dcl])\NP'], ['.']]

#sentence = 'Pierre Vinken , 61 years old , will join the board as a nonexecutive director Nov. 29'.split()
#postags = 'NNP NNP , CD NNS JJ , MD VB DT NN IN DT JJ NN NNP CD'.split()

#stags = [['(NP\NP)/(NP\NP)', '(S/S)/NP', '(N/N)/(N/N)', 'N/N'], ['N', '(S[dcl]\NP)/S[dcl]', '(N/N)/(N/N)', '(S[dcl]\NP)/NP', 'NP\NP', 'S\S', 'S/S', 'N\N', 'N/N', 'S[b]\NP', 'S[adj]\NP', '(NP\NP)\(NP\NP)', '(S[dcl]\NP)/S[wq]', '(S\NP)\(S\NP)', 'S[dcl]\NP'], [','], ['N/N'], ['(S[adj]\NP)/(S[adj]\NP)', 'N', '(S\NP)\(S\NP)'], ['N/N', '(S[adj]\NP)\NP', 'N', 'S[adj]\NP'], [','], ['(S[dcl]\NP)/(S[b]\NP)', '(S[b]\NP)/(S[b]\NP)', '(S[q]/(S[b]\NP))/NP'], ['S[b]\NP', '(S[b]\NP)/NP'], ['NP[nb]/N', '((S\NP)\(S\NP))/N'], ['N'], ['PP/NP', '(NP\NP)/NP', '((S\NP)\(S\NP))/NP', '((S\NP)\(S\NP))/S[dcl]'], ['NP[nb]/N'], ['N/N'], ['N', 'N/N'], ['(NP\NP)/N', '((S\NP)\(S\NP))/N', 'N', 'N/N'], ['N', 'N\N']]

#stags = [['N/N'], ['N'], [','], ['N/N'], ['(S[adj]\NP)/(S[adj]\NP)', 'N'], ['(S[adj]\NP)\NP', 'N'], [','], ['(S[dcl]\NP)/(S[b]\NP)'], ['(S[b]\NP)/NP'], ['NP[nb]/N'], ['N'], ['PP/NP', '(NP\NP)/NP', '((S\NP)\(S\NP))/NP'], ['NP[nb]/N'], ['N/N'], ['N'], ['(NP\NP)/N', '((S\NP)\(S\NP))/N', 'N', 'N/N'], ['N', 'N\N']] 

#sentence = ['The', 'proposed', 'rules', 'also', 'would', 'be', 'tougher', 'on', 'the', 'insiders', 'still', 'required', 'to', 'file', 'reports', ',', 'he', 'said']
#stags = [['NP[nb]/N'], ['N/N'], ['N'], ['(S\NP)/(S\NP)'], ['(S[dcl]\NP)/(S[b]\NP)'], ['(S[b]\NP)/(S[adj]\NP)'], ['(S[adj]\NP)/PP'], ['PP/NP'], ['NP[nb]/N'], ['N'], ['(S\NP)/(S\NP)'], ['(S[pt]\NP)/(S[to]\NP)'], ['(S[to]\NP)/(S[b]\NP)'], ['(S[b]\NP)/NP'], ['N'], [','], ['NP'], ['(S[dcl]\S[dcl])\NP']]

#sentence = ['The', 'luxury', 'auto', 'maker', 'last', 'year', 'sold', '1,214', 'cars', 'in', 'the', 'U.S.']

#stags = [['NP[nb]/N', 'N', '((S\NP)/(S\NP))/N'], ['N/N', 'N'], ['N/N'], ['N', 'NP\NP', 'N/N'], ['((S\NP)/(S\NP))/((S\NP)/(S\NP))', 'S[adj]\NP', '(S[adj]\NP)\(S[adj]\NP)'], ['(S\NP)/(S\NP)', 'S/S', 'PP\PP'], ['(S[dcl]\NP)/NP', '((S[dcl]\NP)/PP)/NP', 'S[pss]\NP'], ['N/N'], ['N', 'N/N'], ['((S\NP)\(S\NP))/NP', '(S[adj]\NP)\(S[adj]\NP)', 'S[adj]\NP'], ['NP[nb]/N', '(NP/NP)/N', '(N/N)/N'], ['N', '(N/N)/(N/N)', 'NP/NP']]

#stags = [['NP[nb]/N'], ['N/N'], ['N/N'], ['N'], ['(S/S)/(S/S)', '(NP\NP)/(NP\NP)', '(PP\PP)/(PP\PP)', '(S\S)/(S\S)', '(((S\NP)\(S\NP))\((S\NP)\(S\NP)))/(((S\NP)\(S\NP))\((S\NP)\(S\NP)))', '((S\NP)/(S\NP))/((S\NP)/(S\NP))', 'N/N', '((S\NP)\(S\NP))/((S\NP)\(S\NP))'], ['N', '(S\NP)\(S\NP)', 'NP\NP', '((S\NP)\(S\NP))\((S\NP)\(S\NP))', 'PP\PP', 'S/S', '(S\NP)/(S\NP)'], ['(S[dcl]\NP)/NP', '(((S[dcl]\NP)/PP)/PP)/NP', 'S[dcl]\NP', 'S[pss]\NP', '((S[dcl]\NP)/PP)/NP'], ['N/N', '((S\NP)\(S\NP))/((S\NP)\(S\NP))', '(S\NP)\(S\NP)'], ['N'], ['(PP\PP)/NP', '((S\NP)\(S\NP))/NP', 'PP/NP', '(S/S)/NP', '(NP\NP)/NP'], ['NP[nb]/N', 'N', '(N/N)/N', '(PP\PP)/N', '((S/S)/S[dcl])/(S[adj]\NP)'], ['N', 'N/N']]

#sentence = ['But', 'freshness', 'counts', 'more', 'than', 'it', 'once', 'did', ',', 'and', 'stores', 'are', 'expanding', 'shelf', 'space', 'for', 'unconventional', ',', 'but', 'tastier', ',', 'and', 'often', 'pricier', ',', 'apples']

#stags = [['S/S'], ['N'], ['N', '(S[dcl]\NP)/NP', '(S[dcl]\NP)/PP', '(S[dcl]\NP)/(S[adj]\NP)', 'S[dcl]\NP'], ['S[adj]\NP'], ['(((S\NP)\(S\NP))\((S\NP)\(S\NP)))/NP', '(NP\NP)/S[dcl]', '((S\NP)\(S\NP))/S[dcl]', '(((S\NP)\(S\NP))\((S\NP)\(S\NP)))/(S[dcl]\NP)', '((S[adj]\NP)\(S[adj]\NP))/(S[adj]\NP)', '(NP/NP)\(S[adj]\NP)', '((S[adj]\NP)\(S[adj]\NP))/PP', '((S[adj]\NP)\(S[adj]\NP))/S[dcl]', '(N/N)\(S[adj]\NP)', '((NP/NP)/(NP/NP))\(S[adj]\NP)', '((N/N)/(N/N))\(S[adj]\NP)', '(((S\NP)\(S\NP))\((S\NP)\(S\NP)))/S[dcl]'], ['NP'], ['(S\NP)\(S\NP)', '((S\NP)/(S\NP))/((S\NP)/(S\NP))', '(S\NP)/(S\NP)'], ['S[dcl]\NP', '(S[dcl]\NP)/S[dcl]', '(S[dcl]\NP)/NP'], [','], ['conj', 'N/N'], ['N'], ['(S[dcl]\PP)/NP', '(S[dcl]\NP)/(S[ng]\NP)'], ['(S[ng]\NP)/NP', 'S[ng]\NP'], ['N/N'], ['N'], ['(NP\NP)/(S[adj]\NP)', 'PP/NP', '((NP\NP)/(S[to]\NP))/NP', '((S\NP)\(S\NP))/NP', 'PP/(S[adj]\NP)', '((S\NP)\(S\NP))/(S[adj]\NP)', '(NP\NP)/NP', '(S/S)/(S[adj]\NP)'], ['N', 'S[adj]\NP', 'N/N'], [','], ['(S[adj]\NP)/(S[adj]\NP)', 'conj'], ['N', 'NP\NP', 'N/N', 'S[adj]\NP', '(S\NP)\(S\NP)', '(N/N)/(N/N)'], [','], ['conj', 'N/N'], ['(N/N)/(N/N)', '(NP\NP)/(NP\NP)', 'N/N', 'S/S', 'NP/NP', '(S\NP)/(S\NP)', '(S\NP)\(S\NP)', '(S[adj]\NP)/(S[adj]\NP)', '(N/N)\(N/N)'], ['N', 'S/S', 'N/N', 'S[adj]\NP', '(N/N)\(N/N)'], [','], ['N']]

#sentence = ['They', 'make', 'the', 'argument', 'in', 'letters', 'to', 'the', 'agency', 'about', 'rule', 'changes', 'proposed', 'this', 'past', 'summer', 'that', ',', 'among', 'other', 'things', ',', 'would', 'exempt', 'many', 'middle-management', 'executives', 'from', 'reporting', 'trades', 'in', 'their', 'own', 'companies', '\'', 'shares', '.']

#stags = [['NP'], ['(S[dcl]\NP)/NP'], ['NP[nb]/N'], ['N'], ['((S\NP)\(S\NP))/NP'], ['N'], ['(NP\NP)/NP'], ['NP[nb]/N'], ['N'], ['(NP\NP)/NP'], ['N/N'], ['N'], ['S[pss]\NP'], ['((S\NP)\(S\NP))/N'], ['N/N'], ['N'], ['(NP\NP)/(S[dcl]\NP)'], [','], ['((S\NP)/(S\NP))/NP'], ['N/N'], ['N'], [','], ['(S[dcl]\NP)/(S[b]\NP)'], ['((S[b]\NP)/PP)/NP'], ['N/N'], ['N/N'], ['N'], ['PP/(S[ng]\NP)'], ['(S[ng]\NP)/NP'], ['N'], ['(NP\NP)/NP'], ['NP[nb]/N'], ['N/N'], ['N'], ['(NP[nb]/N)\NP'], ['N'], ['.']]

#sentence = ['New', 'Brunswick', 'Scientific', 'Co.', ',', 'a', 'maker', 'of', 'biotechnology', 'instrumentation', 'and', 'equipment', ',', 'said', 'it', 'adopted', 'an', 'anti-takeover', 'plan', 'giving', 'shareholders', 'the', 'right', 'to', 'purchase', 'shares', 'at', 'half', 'price', 'under', 'certain', 'conditions']

#stags = [['N/N'], ['N/N'], ['N/N'], ['N'], [','], ['NP[nb]/N'], ['N'], ['(NP\NP)/NP'], ['N/N'], ['N'], ['conj'], ['N'], [','], ['(S[dcl]\NP)/S[dcl]'], ['NP'], ['(S[dcl]\NP)/NP'], ['NP[nb]/N'], ['N/N'], ['N'], ['((S[ng]\NP)/NP)/NP'], ['N'], ['NP[nb]/N'], ['N'], ['(S[to]\NP)/(S[b]\NP)'], ['(S[b]\NP)/NP'], ['N'], ['((S\NP)\(S\NP))/NP'], ['NP/NP', 'N/N'], ['N'], ['((S\NP)\(S\NP))/NP'], ['N/N'], ['N']]

#sentence = ['The', 'bond', 'fund', 'will', 'invest', 'in', 'high-grade', 'or', 'medium-grade', 'bonds', ',', 'mortgages', 'or' , 'asset-backed', 'securities', ',', 'including', 'as', 'much', 'as', '15', '%', 'in', 'foreign', 'securities']
#stags = [['NP[nb]/N'], ['N/N'], ['N'], ['(S[dcl]\NP)/(S[b]\NP)'], ['(S[b]\NP)/PP'], ['PP/NP'], ['N/N'], ['conj'], ['N/N'], ['N'], [','], ['N'], ['conj'], ['N/N'], ['N'], [','], ['(NP\NP)/NP'], ['N/N'], ['N/PP'], ['PP/NP'], ['N/N'], ['N'], ['(NP\NP)/NP'], ['N/N'], ['N']]


#sentence = ['the', 'mouse', 'who', 'liked', 'cheese']
#tags = [bare, N, ['(NP\NP)/(S[dcl]\NP)'], trans, N]

# this one to test the TR cat:
#sentence = ['wants', 'cheese']
#tags = [control, N]

#sentence = 'the man gives a dog a bone and a policeman a flower'.split()
#tags = 'NP[nb]/N N ((S[dcl]\\NP)/NP)/NP NP[nb]/N N NP[nb]/N N conj NP[nb]/N N NP[nb]/N N'.split()
#tags = [bare, N, ditrans, bare, N, bare, N, conj, bare, N, bare, N]

#sentence = 'the man gives and gives a dog a bone and a policeman a flower'.split()
#tags = [bare, N, ditrans, conj, ditrans, bare, N, bare, N, conj, bare, N, bare, N]

#sentence = 'mice like cheese and biscuits'.split()
#tags = [NP, trans, NP, conj, NP]

# sentence = "remain steady".split()
# tags = r"(S[b]\NP)/(S[adj]\NP) S[adj]\NP".split()

#print "----------------------"
#cell = chart(3,2)
#print equiv(cell[0], cell[1])

#sentence = ", something".split()
#tags = [[','], ['N', 'NP', 'S[dcl]', 'S/S', 'S[dcl]\\NP']]

#tags = [[','], ['S[dcl]\S[dcl]', 'S[dcl]\NP']]

#tags = r"S[dcl]\S[dcl] S[dcl]\NP".split()

#sentence = "may give".split()
#fwd_comp_tags1 = r"(S[dcl]\NP)/(S[b]\NP) (S[b]\NP)/NP".split()
#fwd_comp_tags2 = r"(S[dcl]\NP)/(S[b]\NP) ((S[b]\NP)/NP)/NP".split()
#fwd_comp_tags3 = r"(S[dcl]\NP)/(S[b]\NP) (((S[b]\NP)/(S[to]\NP))/(S[adj]\NP))/NP".split()

#bwd_cross_tags1 = r"(S[b]\NP)/NP (S\NP)\(S\NP)".split();
#bwd_cross_tags2 = r"((S[b]\NP)/NP)/NP (S\NP)\(S\NP)".split();
#bwd_cross_tags3 = r"(((S[b]\NP)/(S[to]\NP))/(S[adj]\NP))/NP (S\NP)\(S\NP)".split();

#N = Atom('N')
#NP = Atom('NP')
#X = VarID('X')
#Y = VarID('Y')
#Z = VarID('Z')
#dcl = Feature('dcl')
#ng = Feature('ng')
#
#cat = Cat(N, dcl, X, 0, 0)
#
#markedup = Markedup("markedup")
#markedup["blah"] = cat
#markedup["blah2"] = Cat(NP, ng, Y, 0, 0)
#
#print markedup["blah"]
#print markedup["blah2"]
#
#cat2 = Cat(markedup["blah"], '/', markedup["blah2"], Z)
#t
#print cat2.arg
#print cat2.result 
#
#relations = Relations("relations")
#print relations("hello<1><2>", 1)
#print relations("hello<1><2>", 2)
#print relations("hello<1><2>", 1)
#
## print relations[345]

#sentence = "From 1953 After There Canada fell".split()
#postags = "IN CD TO NP NNP VBD".split()
#supertags = "((S/S)/(S/S))/NP N (S/S)/NP N N S[dcl]\NP".split()

def get_var(i, sc):
  x = 0
  for v in sc.vars:
    if x == i:
      return v
    x += 1

def get_sc(s, cell):
  for i in xrange(len(cell)):
    t = str(cell[i])
    if t.find(s) != -1:
      print i
      print t
