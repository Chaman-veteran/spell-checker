import os
import json

files = os.path.abspath(".")+"\\"
f1 = open(files+"Harry_Potter_1.txt", 'r')
f2 = open(files+"Harry_Potter_2.txt", 'r')
f3 = open(files+"Harry_Potter_3.txt", 'r')
f4 = open(files+"Harry_Potter_4.txt", 'r')
data = f1.read().lower().split(' ') + f2.read().lower().split(' ') \
        + f3.read().lower().split(' ') + f4.read().lower().split(' ')
f1.close()
f2.close()
f3.close()
f4.close()
dico = {}
freq = {}
# dico :: {String : {String : Int}} pour l'association
# freq :: {String : Int} pour les mots les plus fréquents
for ind in range(len(data)-1):
    nextWord = data[ind+1]
    currentWord = data[ind]
    present = currentWord in dico and nextWord in dico[currentWord]
    if currentWord in dico:
        if nextWord in dico[currentWord]:
            dico[currentWord][nextWord] += 1
        else:
            dico[currentWord][nextWord] = 1
    else:
        dico[currentWord] = {nextWord : 1}
    if currentWord in freq:
        freq[currentWord] += 1
    else:
        freq[currentWord] = 1

for word in dico:
    dico[word] = dict(sorted(dico[word].items(), key=lambda y: y[1], reverse=True)[:7])

f = open(files+"assoc.txt",'w')
f.write(json.dumps(dico, indent=4))
f.close()
