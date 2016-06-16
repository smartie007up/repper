# Read in file from McGill dataset
with open('../dataset/0003/salami_chords.txt', 'r') as f:
    data = f.readlines()

data = data[6:len(data)]

S = []

for i in range(0, len(data)-1):
    d = data[i].split("\t")
    d = d[1].split('|')
    d = d[0:len(d)-1]
    S.append(d)
    i += 1

# data = data[1].split('|')
# data = data[0:len(data)-1]


# for i in range(0, len(data)-1):
#     data[i] = data[i].split("\t")for i in range(0, len(data)-1):
#
# for j in range(0, len(data-1))
#     data[j] = data[1].split('|')

print(d)
