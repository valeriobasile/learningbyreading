#!/usr/bin/env python

import sys,re, random

ITERS = 10000

class Model:
	def __init__(self, sents, tgold, count):
		self.sents = sents
		self.tgold = tgold
		self.count = count
	def calc_total_test(self):
		total = 0
		for sent in self.sents:
			recall,prec,gold,test = sent
			total += test
		
		return total
		
	def prec(self):
		total_test = self.calc_total_test()		
	
		overall_prec = 0.0
		for sent in self.sents:
			recall,prec,gold,test = sent
			
			weight = float(test) / total_test
			overall_prec += weight * prec
			
		return overall_prec
	def recall(self):
		overall_recall = 0.0
		for sent in self.sents:
			recall,prec,gold,test = sent
				
			weight = float(gold) / self.tgold
			overall_recall += weight * recall
				
		return overall_recall
		

def readevalb(fname):
	reading = False
	
	sents = []
	#can precalculate total gold cause it doesnt change, but total test does
	total_gold = 0
	count = 0
	
	for line in file(fname):
		line = line.strip()
		
		if not reading and line=="============================================================================":
			reading = True
		elif reading and line == "============================================================================":
			break
		elif reading:
			ID, Len, Stat, Recall, Prec, Bracket, gold, test, Bracket, Words, Tags, Accuracy = line.split()
			
			recall = float(Recall)
			prec = float(Prec)
			gold = int(gold)
			test = int(test)
			
			total_gold += gold
			count += 1
				
			sents.append((recall,prec,gold,test))
	
	return Model(sents, total_gold, count)

def readfields(fname):
	sents = []
	#can precalculate total gold cause it doesnt change, but total test does
	total_gold = 0
	count = 0
	
	for line in file(fname):
		line = line.strip()
		
		recall, prec, gold, test = line.split()	
		
		recall = float(recall)
		prec = float(prec)
		gold = int(gold)
		test = int(test)
			
		total_gold += gold
		count += 1
				
		sents.append((recall,prec,gold,test))
	
	return Model(sents, total_gold, count)



def fscore(prec, recall):
	return (2*prec*recall) / (prec+recall)

if len(sys.argv) != 4:
	print "usage:"
	print "\tpython compare.py <model1> <model2> e|f"
	print
	print "e for reading an evalb file"
	print "f for reading just plain fields"
	sys.exit()

if sys.argv[3] == "e":
	readfunc = readevalb
elif sys.argv[3] == "f":
	readfunc = readfields

model1 = readfunc(sys.argv[1])
model2 = readfunc(sys.argv[2])

if model1.count != model2.count:
	print "mismatched counts"
	print "count1 is:",model1.count
	print "count2 is:",model2.count
	sys.exit()

	
original_prec1 = model1.prec()
original_recall1 = model1.recall()
original_fscore1 = fscore(original_prec1, original_recall1)

original_prec2 = model2.prec()
original_recall2 = model2.recall()
original_fscore2 = fscore(original_prec2, original_recall2)


print "Model 1 - precision:",original_prec1, "recall:",original_recall1, "fscore:",original_fscore1
print "Model 2 - precision:",original_prec2, "recall:",original_recall2, "fscore:",original_fscore2

original_prec_diff = original_prec2 - original_prec1
original_recall_diff = original_recall2 - original_recall1
original_fscore_diff = original_fscore2 - original_fscore1

print "Precision diff:", original_prec_diff, " Recall diff:", original_recall_diff, " fscore diff:", original_fscore_diff

print "---"

random_precs = 0
random_recalls = 0
random_fscores = 0

print model1.count


for i in xrange(ITERS):
	if i % 1000 == 0:
		print "done",i,"iterations"
	
	for j in xrange(model1.count):
		num = random.random()
				
		if num >= 0.5:
			temp = model1.sents[j]
			model1.sents[j] = model2.sents[j]
			model2.sents[j] = temp
		
	prec1 = model1.prec()
	recall1 = model1.recall()
	prec2 = model2.prec()
	recall2 = model2.recall()
	
	fscore1 = fscore(prec1,recall1)
	fscore2 = fscore(prec2,recall2)
	
	#print "Model 1 - precision:",prec1, "recall:",recall1
	#print "Model 2 - precision:",prec2, "recall:",recall2


	prec_diff = prec2 - prec1
	recall_diff = recall2 - recall1
	fscore_diff = fscore2 - fscore1
	
	#print "Precision diff:", prec_diff, " Recall diff:", recall_diff

	if original_prec_diff >= 0 and prec_diff >= original_prec_diff:
		random_precs += 1
	elif original_prec_diff < 0 and prec_diff <= original_prec_diff:
		random_precs += 1

	if original_recall_diff >= 0 and recall_diff >= original_recall_diff:
		random_recalls += 1
	elif original_recall_diff < 0 and recall_diff <= original_recall_diff:
		random_recalls += 1
		
	if original_fscore_diff >= 0 and fscore_diff >= original_fscore_diff:
		random_fscores += 1
	elif original_fscore_diff < 0 and fscore_diff <= original_fscore_diff:
		random_fscores += 1

print "done"
print "---"


print "number of random precision diferences equal to or greater than original observed difference:", random_precs
print "number of random recall diferences equal to or greater than original observed difference:", random_recalls
print "number of random fscore diferences equal to or greater than original observed difference:", random_fscores

print "p-value for precision diff:", float(random_precs + 1)/(ITERS + 1)
print "p-value for recall diff:", float(random_recalls + 1)/(ITERS + 1)
print "p-value for fscore diff:", float(random_fscores + 1)/(ITERS + 1)




