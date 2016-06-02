with open('salami_chords.txt', 'r') as f:
    data = f.readlines()

# an array for all the lines (rows)
L = [data]
# size of the amount of lines (rows)
n = len(L)

for line in data:
    words = line.split(' | ')

# total size of the amount of words on each line
R = []
R.append(words)
k = len(R)

# usable data starts on line 7 (in all salami_chords.txt files)
for x in L[7:n]:
    # from column 2 to length of line
    for y in R[2:k]:
        print x, y
