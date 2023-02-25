import os
import json
from typing import Dict

files = os.path.abspath(".")+"\\Py_frequence_2mots\\"

get_fd = lambda s:open(files+s, 'r')

files_descriptors = list(map(get_fd, \
                             ["Harry_Potter_1.txt","Harry_Potter_2.txt",\
                              "Harry_Potter_3.txt","Harry_Potter_4.txt"]))

fold_concat = lambda fn,l: [] if l == [] else fn(l[0]) if len(l) == 1 else fn(l[0])+fold_concat(fn,l[1:])

data = fold_concat(lambda f: f.read().lower().split(' '), files_descriptors)

map(lambda f:f.close(), files_descriptors)

dico : Dict[str, Dict[str, int]] = {}
freq : Dict[str, int] = {}
for ind in range(len(data)-1):
    nextWord = data[ind+1]
    currentWord = data[ind]
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

freq = dict(sorted(freq.items(), key=lambda y: y[1], reverse=True))

freq2 = ""
for word in freq:
    freq2+='{"word":"'+word+'","freq":'+str(freq[word])+'} '

f = open(files+"assoc.txt", 'w')
f.write(json.dumps(dico, indent=4))
f.close()
f = open(files+"freq.txt", 'w')
f.write(json.dumps(freq, indent=0, separators=(',',':')))
f.close()
f = open(files+"freq2.txt", 'w')
f.write(freq2)
f.close()
