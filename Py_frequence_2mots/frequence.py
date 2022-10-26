import os
import json

files = os.path.abspath(".")+"\\"
f1 = open(files+"Harry_Potter_1.txt", 'r')
f2 = open(files+"Harry_Potter_2.txt", 'r')
f3 = open(files+"Harry_Potter_3.txt", 'r')
data = f1.read().lower().split(' ') + f2.read().lower().split(' ') \
        + f3.read().lower().split(' ')
f1.close()
f2.close()
f3.close()
dico = {}
# dico :: {String : {String : Int}} 
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

len(dico)

for word in dico:
    dico[word] = dict(sorted(dico[word].items(), key=lambda y: y[1], reverse=True)[:7])

#for word in dico:
#    dico[word] = json.dumps(dico[word], indent=4)
#print(json.dumps(dico, indent=4)[:100])

f = open("D:/Users/Napp/Documents/Mes_documents/Informatique/Haskell/correcteur/assoc.txt",'w')
f.write(json.dumps(dico, indent=4))
f.close()
