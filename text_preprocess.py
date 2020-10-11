import re
import string
import spacy
import en_core_web_sm
nlp1 = en_core_web_sm.load(disable=["tagger","parser"])
nlp2 = en_core_web_sm.load(disable=["tagger","parser", "ner"])

def pre_process(newString):
    newString = newString.replace("-"," ")
    newString = newString.replace("'s","")
    return newString.lower()

def rep_entity(s):
    doc = s
    newString = str(s)
    for e in reversed(doc.ents): #reversed to not modify the offsets of other entities when substituting
        label = e.label_
        if label in ["ORG","PERSON","EVENT","PRODUCT"]:
            continue
        
        if label == "GPE":
            label = "LOC"

        start = e.start_char
        end = start + len(e.text)
        newString = newString[:start] + label + newString[end:]

    newString = newString.strip()
    newString = newString.translate(str.maketrans(dict.fromkeys(string.punctuation + "\n\t’:")))
    return newString

def postprocess(s):
    doc = s
    tokens = [word.lemma_ for word in doc if not word.is_stop]
    text = " ".join(tokens)
    return text 

def test_sent(s):
    s = pre_process(s)
    s = nlp1(s)
    s = rep_entity(s)
    s = nlp2(s)
    s = postprocess(s)
    return s

#if __name__ == "__main__":
#  print(test_sent("US experiences 5.5 rector earthquake near new york"))
