import sys
import candc
import doctest

def test(sentence):
  candc.read(parser.sentence, sentence)
  del printer.deps[:]
  integration.parse(parser.sentence, decoder, printer, False)
  for label, head, filler in printer.deps:
    print '%s_%s %s %s_%s' % (head[0], head[1], label, filler[0], filler[1])

base = "models/"

integration = candc.load(base + "/super", base + "/parser")
cats = integration.cats
parser = integration.parser
decoder = candc.ccg.DecoderFactory('derivs')
printer = candc.ccg.PythonPrinter(cats)
chart = parser.chart

def forward_application_accept():
  """simple forward application

  >>> test(r"the|DT|NP[nb]/N dog|NN|N")
  the_0 (NP[nb]{Y}/N{Y}<1>){_} 1 dog_1
  >>>
  """
  pass

def forward_application_conj_reject():
  """forward application conjunction constraint -- parse should fail

  >>> test(r"Fred|NNP|(NP\NP)/(NP\NP) and|CC|conj cars|NNS|NP")
  >>>
  """

def backward_application_accept():
  """simple backward application

  >>> test(r"John|NNP|NP runs|VBZ|S[dcl]\NP")
  runs_1 (S[dcl]{_}\NP{Y}<1>){_} 1 John_0
  >>>
  """

def backward_application_conj_reject():
  """backward application conjunction constraint -- parse should fail

  >>> test(r"and|CC|conj Freedom|NN|NP now|RB|(NP\NP)\(NP\NP)")
  >>>
  """

def backward_application_comma_reject():
  """backward application comma constraint -- parse should fail

  >>> test(r",|,|, Freedom|NN|NP now|RB|(NP\NP)\(NP\NP)")
  >>>
  """

def forward_and_backward_application_accept():
  """forward and backward application

  >>> test(r"John|NNP|NP pays|VBZ|((S[dcl]\NP)/NP)/NP Bill|NNP|NP the|DT|NP[nb]/N money|NN|N")
  pays_1 (((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_}/NP{W}<3>){_} 3 Bill_2
  the_3 (NP[nb]{Y}/N{Y}<1>){_} 1 money_4
  pays_1 (((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_}/NP{W}<3>){_} 2 money_4
  pays_1 (((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_}/NP{W}<3>){_} 1 John_0
  >>>
  """

def simple_coordination_accept():
  """simple coordination

  >>> test(r"John|NNP|NP and|CC|conj Mary|NNP|NP")
  and_1 conj 1 Mary_2
  and_1 conj 1 John_0
  >>>
  """

def subject_coordination_accept():
  """subject coordination

  >>> test(r"I|PRP|NP like|VBP|(S[dcl]\NP)/NP John|NNP|NP and|CC|conj Mary|NNP|NP")
  and_3 conj 1 Mary_4
  and_3 conj 1 John_2
  like_1 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 2 Mary_4
  like_1 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 2 John_2
  like_1 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 1 I_0
  >>>
  """

def subject_and_object_coordination_accept():
  """subject and object coordination

  >>> test(r"Barry|NNP|NP and|CC|conj Neal|NNP|NP like|VBP|(S[dcl]\NP)/NP Sharon|NNP|NP and|CC|conj Jade|NNP|NP")
  and_1 conj 1 Neal_2
  and_1 conj 1 Barry_0
  and_5 conj 1 Jade_6
  and_5 conj 1 Sharon_4
  like_3 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 2 Jade_6
  like_3 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 2 Sharon_4
  like_3 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 1 Neal_2
  like_3 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 1 Barry_0
  >>>
  """

def adjective_coordination_accept():
  """adjective category coordination

  >>> test(r"red|JJ|N/N and|CC|conj green|JJ|N/N apples|NNS|N")
  and_1 conj 1 green_2
  and_1 conj 1 red_0
  green_2 (N{Y}/N{Y}<1>){_} 1 apples_3
  red_0 (N{Y}/N{Y}<1>){_} 1 apples_3
  >>>
  """

def verb_coordination_accept():
  """complex verb category coordination

  >>> test(r"I|PRP|NP like|VBP|(S[dcl]\NP)/NP and|CC|conj hate|VBP|(S[dcl]\NP)/NP Neal|NNP|NP")
  and_2 conj 1 hate_3
  and_2 conj 1 like_1
  hate_3 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 2 Neal_4
  like_1 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 2 Neal_4
  hate_3 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 1 I_0
  like_1 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 1 I_0
  >>>
  """

def SbNP_to_NPbNP_lexical_rule_accept():
  r"""S\NP -> NP\NP lexical rule

  >>> test(r"John|NNP|NP running|VBG|S[ng]\NP ate|VB|S[dcl]\NP")
  running_1 (S[ng]{_}\NP{Y}<1>){_} 1 John_0
  ate_2 (S[dcl]{_}\NP{Y}<1>){_} 1 John_0
  >>>
  """

def lexical_rule_coordination_accept():
  """S\NP -> NP\NP lexical rule applied to coordination of complex categories
  We need to add the parentheses to stop running and organising modifying is
  
  >>> test(r"-LRB-|LRB|LRB Chris|NNP|NP running|VBG|(S[ng]\NP)/NP and|CC|conj organising|VBG|(S[ng]\NP)/NP \
  things|NNS|NP -RRB-|RRB|RRB is|VBZ|(S[dcl]\NP)/NP news|NN|NP")
  and_3 conj 1 organising_4
  and_3 conj 1 running_2
  organising_4 ((S[ng]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 2 things_5
  running_2 ((S[ng]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 2 things_5
  organising_4 ((S[ng]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 1 Chris_1
  running_2 ((S[ng]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 1 Chris_1
  is_7 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 2 news_8
  is_7 ((S[dcl]{_}\NP{Y}<1>){_}/NP{Z}<2>){_} 1 Chris_1
  >>>
  """

def coordination_long_range_accept():
  """long range dependency passing with coordination
  
  >>> test(r"efforts|NNS|N to|TO|(S[to]\NP)/(S[b]\NP) stop|VB|S[b]\NP or|CC|conj slow|VB|S[b]\NP")
  or_3 conj 1 slow_4
  or_3 conj 1 stop_2
  to_1 ((S[to]{_}\NP{Z}<1>){_}/(S[b]{Y}<2>\NP{Z*}){Y}){_} 2 slow_4
  to_1 ((S[to]{_}\NP{Z}<1>){_}/(S[b]{Y}<2>\NP{Z*}){Y}){_} 2 stop_2
  slow_4 (S[b]{_}\NP{Y}<1>){_} 1 efforts_0
  stop_2 (S[b]{_}\NP{Y}<1>){_} 1 efforts_0
  to_1 ((S[to]{_}\NP{Z}<1>){_}/(S[b]{Y}<2>\NP{Z*}){Y}){_} 1 efforts_0
  >>>
  """

if __name__ == "__main__":
  doctest.testmod()
