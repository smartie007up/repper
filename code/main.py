# Read in file from McGill dataset
with open('../dataset/0003/salami_chords.txt', 'r') as f:
    data = f.readlines()

data = data[6:len(data)]
print(data)


data[1] = data[1].split('|')
data[1] = data[1][1:len(data[1])-1]


# # usable data starts on line 7 (in all salami_chords.txt files)
# for x in [7:n]:
#     # from column 2 to length of line
#     for y in R[2:k]:
#         print x, y
