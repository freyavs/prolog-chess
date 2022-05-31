
import numpy as np
i = 4 
file_results = f'./output/results{i}.txt'
file_time = f'./output/time{i}.txt'
file_moves = f'./output/moves{i}.txt'
with open(file_time) as f:
	time = f.readlines()
	total = len(time)
	time = [ float(t.strip()) for t in time ]
	avg = np.mean(np.array(time))
	print(f"Time: {avg}") 

with open(file_moves) as f:
	time = f.readlines()
	total = len(time)
	time = [ float(t.strip()) for t in time ]
	avg = np.mean(np.array(time))
	print(f"Moves: {avg}") 

with open(file_results) as f:
	d = dict()
	time = f.readlines()
	total = len(time)
	res = [ t.strip() for t in time ]
	for r in res:
		d[r] = d.get(r,0) +1	
	print(f"Results: {d}") 