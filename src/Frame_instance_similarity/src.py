from nltk.corpus import wordnet as wn
#import nltk
from path import path
import json
import re
import sys, getopt
#from decimal import *

PATH_current = path('src.py').abspath()
PATH_Package = "/".join(PATH_current.split('/')[:-1])
PATH_frames = PATH_Package + "/Frame_files/"

FLAG_FRAME = "WUP"   #select algorithm for similarity between two frame types
FLAG_ELEMENTS = "WUP" #select algorithm for similarity between two elements
ALPHA = 0.5 #constant for main formula
ROLE = "true"
inputfile1 = "F_instance1.nt"
inputfile2 = "F_instance2.nt"
#nltk.download()


def main(argv):
       global inputfile1
       global inputfile2
       global FLAG_FRAME
       global FLAG_ELEMENTS
       global ROLE
       global ALPHA
       try:
          opts, args = getopt.getopt(argv,"hi:I:a:t:e:r:",["ifile1=","ifile2=","alpha=","F_Sim_Algo=","E_Sim_Algo=","role="])
       except getopt.GetoptError:
          print 'src.py -i <inputfile1> -I <inputfile2> -a <alpha value> -t <Frame_similarity_Algo> -e <element_similarity_Algo> -r <true,false>'
          sys.exit(2)
       for opt, arg in opts:
          if opt == '-h':
             print 'src.py -i <inputfile> -I <outputfile> -a <alpha value> -t <Frame_similarity_Algo> -e <element_similarity_Algo> -r <true,false>'
             sys.exit()
          elif opt in ("-i", "--ifile1"):
             inputfile1 = arg
          elif opt in ("-I", "--ifile2"):
             inputfile2 = arg
          elif opt in ("-a", "--alpha"):
              ALPHA = float(arg)
          elif opt in ("-t","--F_Sim_Algo"):
              FLAG_FRAME = arg
          elif opt in ("-e","--E_Sim_Algo"):
              FLAG_ELEMENTS = arg
          elif opt in ("-r", "--role"):
              ROLE = arg

def load_JSON(filename):
    with open(PATH_frames + filename) as fp:
        return json.load(fp)

def read_file(filename):
    fp = open(PATH_frames + filename, "r")
    return fp.readlines()

def extract_F_instance_elements(frame_instances):
    frame_dict = {}
    for frame_instance in frame_instances:
        F_instance = re.findall("(?<=fi-)([a-zA-Z_0-9-]*)", str(frame_instance))
        F_name_offset = re.findall("(?<=frame-)([a-zA-Z_0-9-]*)", str(frame_instance))
        F_offset = "-".join(F_name_offset[0].split("-")[1:])
        F_elements = re.findall("(?<=wn31/)([a-zA-Z_0-9-]*)",str(frame_instance))
        F_roles = re.findall(r"(?<=fe-)([a-zA-Z_ -]*)",str(frame_instance))
        F_elements_roles = [(F_elements[i],F_roles[i]) for i in range(len(F_elements)) ]
        F_instance_offset = F_instance[0] + "_" + F_offset
        frame_dict[F_instance_offset] = (F_elements_roles)
    #print "Frame_instance-elements Dictionary:\n",frame_dict,"\n"
    return frame_dict


def offset2ss(offset_31, wn_31_30):
    offset_30 = wn31_30[offset_31]
    # convert wordnet 3.1 id to 3.0 because NLTK limited to 3.0 for now
    synset = wn._synset_from_pos_and_offset(str(offset_30[-1:]), int(offset_30[:8]))
    #print "offset_31:", offset_31, "\toffset_30:", offset_30, "\tsynset:", synset
    #print type(wn._synset_from_pos_and_offset(str(offset_30[-1:]), int(offset_30[:8])))
    return synset


def WUP_similarity(ss1, ss2):
    return wn.wup_similarity(ss1, ss2)

def find_similarity_frames(frameType1_offset, elements1_offsets_role, frameType2_offset, elements2_offsets_role, wn31_30 ):
    #print "Frame type synsets:"
    synset_F1 = offset2ss(frameType1_offset, wn31_30)
    synset_F2 = offset2ss(frameType2_offset, wn31_30)
    if FLAG_FRAME == "WUP":
        similarity_F_types = WUP_similarity(synset_F1, synset_F2)
                                                        #Add other conditions and respective similarity calculating functions
    #print "Elemnents_synsets:"
    elements1_synsets_role = [(offset2ss(element[0], wn31_30),element[1])  for element in elements1_offsets_role]
    elements2_synsets_role = [(offset2ss(element[0], wn31_30), element[1]) for element in elements2_offsets_role]
    #print "elements1_synsets:\t", elements1_synsets
    #print "elements2_synsets:\t",elements2_synsets
                                                                            #bipartitie, thus do both ways
    #print "Calculating Left2Right_elements_similarities..."
    sum_similarity = 0
    for el_r_F1 in elements1_synsets_role:
        max_similarity = 0
        el_F1 = el_r_F1[0]
        r_F1 = el_r_F1[1]
        for el_r_F2 in elements2_synsets_role:
            el_F2 = el_r_F2[0]
            r_F2 = el_r_F2[1]
            if(ROLE == "false"):
                if(FLAG_ELEMENTS == "WUP"):
                    similarity_element = WUP_similarity(el_F1, el_F2)
                # Add other conditions and respective similarity calculating functions
            elif(ROLE == "true"):
                if(r_F1 == r_F2):
                    if(FLAG_ELEMENTS == "WUP"):
                        similarity_element = WUP_similarity(el_F1, el_F2)
                    # Add other conditions and respective similarity calculating functions
                else:
                    similarity_element = 0

            #print "similarity between",el_F1,el_F2,":\t",similarity_element
            if(similarity_element>max_similarity):
                max_similarity = similarity_element
        #print "max_similarity for",el_F1,":\t", max_similarity
        sum_similarity = max_similarity + sum_similarity
    avg_similarity_1 = sum_similarity / len(elements1_synsets_role)
    #print "avg_similarity_L2R:\t", avg_similarity_1

    #print "Calculating Right2left_elements_similarities..."
    sum_similarity = 0
    for el_F2 in elements2_synsets_role:
        max_similarity = 0
        el_F2 = el_r_F2[0]
        r_F2 = el_r_F2[1]
        for el_F1 in elements1_synsets_role:
            el_F1 = el_r_F1[0]
            r_F1 = el_r_F1[1]
            if (ROLE == "false"):
                if (FLAG_ELEMENTS == "WUP"):
                    similarity_element = WUP_similarity(el_F1, el_F2)
                # Add other conditions and respective similarity calculating functions
            elif (ROLE == "true"):
                if (r_F1 == r_F2):
                    if (FLAG_ELEMENTS == "WUP"):
                        similarity_element = WUP_similarity(el_F1, el_F2)
                    # Add other conditions and respective similarity calculating functions
                else:
                    similarity_element = 0
            #print "similarity between", el_F2, el_F1,":\t", similarity_element
            if (similarity_element > max_similarity):
                max_similarity = similarity_element
        #print "max_similarity for",el_F1,":\t", max_similarity
        sum_similarity = max_similarity + sum_similarity
    avg_similarity_2 = sum_similarity / len(elements2_synsets_role)
    #print "avg_similarity_R2L:\t", avg_similarity_2

    avg_similarity = (avg_similarity_1 + avg_similarity_2)/2
    #print "Avg_both_sides_similarities:\t", avg_similarity

    similarity_Frames = ALPHA*(similarity_F_types) + (1-ALPHA)*(avg_similarity)
    #print "FType_similarity\t", synset_F1, synset_F2, ":\t", similarity_F_types
    #print "Similarity Frame instance:", similarity_Frames
    return similarity_Frames

def build_frame_similarities_dict(F_instance_element_dict):
    similarity_dict = {}
    for key in F_instance_element_dict:
        #frame1_name = str(key.split("_")[0])
        frameType1_offset = str(key.split("_")[-1])
        elements1_offsets_roles = F_instance_element_dict[key]
        for key2 in F_instance_element_dict:
            #frame2_name = str(key2.split("_")[0])
            frameType2_offset = key2.split("_")[-1]
            elements2_offsets_roles = F_instance_element_dict[key2]
            frame_instance1 = "_".join(key.split("_")[:-1])
            frame_instance2 = "_".join(key2.split("_")[:-1])
            Frame_instance_pair = frame_instance1 + "\t" + frame_instance2
            if(frame_instance1!=frame_instance2 and (frame_instance2 + "\t" + frame_instance1 not in similarity_dict)):
                #print "Calculating Frame instance similarity between\n", key,"\t",key2
                similarity_dict[Frame_instance_pair] = find_similarity_frames(frameType1_offset, elements1_offsets_roles, frameType2_offset, elements2_offsets_roles, wn31_30)
    return similarity_dict

def print_frame_similarities(F_instance_similarity_dict):
    print "Printing frame instances similarities..."
    for key in F_instance_similarity_dict:
        print key,":\t",F_instance_similarity_dict[key]

def merge(Instance_1, Instance_2):
    frame_instances = []
    frame_instances.append(Instance_1)
    frame_instances.append(Instance_2)
    return frame_instances

if __name__ == "__main__":
    main(sys.argv[1:])
    wn31_30 = load_JSON("JSON_wn31-30.json")    # load wn31-30 dictionary
    Instance_1 = read_file(inputfile1)           # read file having triples where instances separated by blank line
    Instance_2 = read_file(inputfile2)
    frame_instances = merge(Instance_1, Instance_2)
    F_instance_element_dict = extract_F_instance_elements(frame_instances) #generate dict carrying frame_instance as key and elements as values
    F_instance_similarity_dict = build_frame_similarities_dict(F_instance_element_dict)  #store similarity score between each frame instance
    print_frame_similarities(F_instance_similarity_dict)



