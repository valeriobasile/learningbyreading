#!/usr/bin/env python

import sys, glob, os

class Model:
  def __init__(self):
    self.info = {}

def attributekey(line):
  attribute = line.split(' ')
  feature = attribute[0]
  instanceinfo = attribute[1:-1]
  count = attribute[-1]
  return feature + ' ' + ' '.join(instanceinfo)
 
def classkey(line):
  klass = line.split(' ')
  supertag = klass[0]
  return supertag

def combinedicts(models, filename):
  lexicon = {}
  for model in models:
    for word in model.info[filename]:
      word = word.split(' ')
      frequency = int(word[-1])
      word = word[0:-1]
      word = ' '.join(word)
      if word in lexicon:
        lexicon[word] += frequency
      else:
        lexicon[word] = frequency
  return lexicon

def printdict(dict, filename, preface):
  output = open(filename, 'w')
  print >> output, preface,
  for key in dict:
    print >> output, key, dict[key]

if len(sys.argv) < 3:
  print "Usage: %s <tmp_storage_location> <model_name>" % sys.argv[0]
  print "e.g. ./combine.py /tmp/ my_model"
  sys.exit(0)

file_location = sys.argv[1]
model_name = sys.argv[2]
os.system('mkdir node_results')
os.system('for host in `cat hosts` ; do scp -r $host:%s/%s node_results/$host ; done' % (file_location, model_name))
files = glob.glob('./node_results/*/*')

preface = ''
models = []
for filename in files:
  temp = glob.glob(filename + '/*')
  if len(temp) != 0:
    model = Model()
    for infofile in temp:
      myfile = open(infofile, 'rU')
      lines = myfile.readlines()
      preface = ''.join(lines[:3])
      lines = lines[3:]
      lines = [line.strip() for line in lines]
      model.info[infofile.split('/')[-1]] = lines
    models.append(model)

#dealing with lexicon, posdict, postags, and tagdict
printdict(combinedicts(models, 'lexicon'), model_name + '/lexicon', preface)
printdict(combinedicts(models, 'posdict'), model_name + '/posdict', preface)
printdict(combinedicts(models, 'postags'), model_name + '/postags', preface)
printdict(combinedicts(models, 'tagdict'), model_name + '/tagdict', preface)
for model in models:
  for filename in ['lexicon', 'posdict', 'postags', 'tagdict']:
    model.info[filename] = None

#dealing with attributes
attributes = {}
for model in models:
  for attribute in model.info['attributes']:
    key = attributekey(attribute)
    attribute = attribute.split(' ')
    count = int(attribute[-1])
    if key in attributes:
      attributes[key] += count
    else:
      attributes[key] = count
  #model.info['attributes'] = None # Needed later (at the moment)
len_attributes = len(attributes)

attributes_out = open(model_name + "/attributes", 'w')
print >> attributes_out, preface,
for attribute in attributes:
  print >> attributes_out, attribute, attributes[attribute]
attributes_out.close()

# Renumbering attributes
num = 0
for key in attributes:
  attributes[key] = (num, attributes[key])
  num += 1

#dealing with classes
classes = {}
for model in models:
  for klass in model.info['classes']:
    key = classkey(klass)
    klass = klass.split(' ')
    count = int(klass[-1])
    if key in classes:
      classes[key] += count
    else:
      classes[key] = count
  #model.info['classes'] = None # Needed later (at the moment)
len_classes = len(classes)

classes_out = open(model_name + "/classes", 'w')
print >> classes_out, preface,
for klass in classes:
  print >> classes_out, klass, classes[klass]
classes_out.close()

# Renumber classes
num = 2
for key in classes:
  classes[key] = (num, classes[key])
  num += 1

#dealing with contexts
contexts = {}
for model in models:
  for context in model.info['contexts']:
    context = context.split(' ')
    klass = int(context[1]) - 2
    tempattributes = []
    for attribute in context[3:]:
      tempattributes.append(attributekey(model.info['attributes'][int(attribute)]))
    frequency = int(context[0])
    klass = classkey(model.info['classes'][klass])
    key = (klass, tuple(tempattributes))
    if key in contexts:
      contexts[key] += frequency
    else:
      contexts[key] = frequency
  model.info['contexts'] = None
len_contexts = len(contexts)

# Output for contexts
contexts_out = open(model_name + "/contexts", "w")
print >> contexts_out, preface,
for context in contexts:
  key = context
  klass = context[0]
  tempattributes = context[1]
  print >> contexts_out, contexts[key], classes[klass][0], len(tempattributes),
  for attribute in tempattributes:
    print >> contexts_out, attributes[attribute][0],
  print >> contexts_out
contexts_out.close()
contexts = None

#dealing with features
features = {}
for model in models:
  for feature in model.info['features']:
    feature = feature.split(' ')
    klass = int(feature[0]) - 2
    attribute = int(feature[1])
    frequency = int(feature[2])
    klass = classkey(model.info['classes'][klass])
    attribute = attributekey(model.info['attributes'][attribute])
    if (klass, attribute) in features:
      features[(klass, attribute)] += frequency
    else:
      features[(klass, attribute)] = frequency
  model.info['features'] = None
len_features = len(features)

features_out = open(model_name + "/features", 'w')
print >> features_out, preface,
output = []
for feature in features:
  print >> features_out, classes[feature[0]][0], attributes[feature[1]][0], features[feature]
features_out.close()
os.system("tail -n +4 %s/features | sort -n -k 2 > tmp_features" % model_name)
os.system("head -3 %s/features | cat - tmp_features > tmp_features2" % model_name)
os.system("mv tmp_features2 %s/features" % model_name)
os.system("rm tmp_features*")

# Output for info file
nevents = 0
for model in models:
  for line in model.info['info']:
    if 'nevents =' in line:
      nevents += int(line.split()[-1])
info_data = """# the number of tags in the tagset
nclasses = %d
# the number of instances in the training data
nevents = %d
# the number of unique instances in the training data
ncontexts = %d
# the number of features in the model
nfeatures = %d
# the number of attributes in the model
nattributes = %d""" % (len(classes) + 2, nevents, len_contexts, len_features, len_attributes)

info_out = open(model_name + "/info", "w")
print >> info_out, preface,
print >> info_out, info_data
info_out.close()

config_out = open(model_name + "/config", "w")
print >> config_out, preface,
for line in models[0].info['config']:
	print >> config_out, line
config_out.close()

unknowns_out = open(model_name + "/unknowns", "w")
print >> unknowns_out, preface,
for line in models[0].info['unknowns']:
	print >> unknowns_out, line
unknowns_out.close()

os.system('for host in `cat hosts` ; do ssh $host "rm -rf %s/%s ; mkdir %s/%s" ; done' % (file_location, model_name, file_location, model_name))
os.system('for host in `cat hosts` ; do scp %s/* $host:%s/%s/. ; done' % (model_name, file_location, model_name))
os.system("for host in `cat hosts` ; do ssh $host 'cd %s/%s ; for i in `seq 0 80` ; do ln -s %s/%s $i ; done' ; done" % (file_location, model_name, file_location, model_name))
current_loc = os.popen('pwd').read().strip()
os.system("for host in `cat hosts` ; do ssh $host 'cd %s ; for i in `seq 0 80` ; do ln -s %s/%s %s/$i ; done' ; done" % (current_loc, file_location, model_name, model_name))

