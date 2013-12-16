f = open('names.txt', 'r')
lines = f.readlines()[0] # file has only 1 line
f.close()

names = [name.strip("\"") for name in lines.split(",")]
names.sort()

alpha = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
sc = {}
x = 1

for c in alpha:
	sc[c] = x
	x = x + 1

idx = 1
tot = 0
for name in names:
	v = 0
	for ch in name:
		v += sc[ch]
	tot += v * idx
	idx += 1

print(tot)

