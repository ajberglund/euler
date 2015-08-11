def numRect(j, k):
	# from counting argument
	return j*k*(j+1)*(k+1) / 4

def euler85(target):
	# numRect(j,1) > target
	# then do a search for the nearest value to target

	# strategy:
	# use the fact that numRect(j+x, k+y) > numRect(j,k) for all x,y > 0

	# start at j = 1
	# start at k = j, increment k until
	# check until numRect > target
	# if numRect(j,j) > target, stop
	j = 1
	doneOuter = False
	minErr = target-1 # we know this is acheved at (1,1)
	count = 0

	while not doneOuter:
		doneInner = False
		k = j

		while not doneInner:
			count += 1
			n = numRect(j, k)
			err = abs(target - n)
			if err < minErr:
				candidate = (j,k)
				minErr = err

			if n > target:
				doneInner = True
				if j == k:
					doneOuter = True
			k += 1
		j += 1


	(j,k) = candidate
	print('Done in ' + str(count) + ' iterations.')
	print('Rectangle (' + str(j) + ',' + str(k) + ') has ' + str(numRect(j,k)) + ' rectangles.')

if __name__ == "__main__":
	euler85(2000000)

