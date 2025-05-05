import numpy as np
import time

start_time = time.time()

A = np.array([[1, 2], [3, 4]])
B = np.array([[5, 6], [7, 8]])

for _ in range(10000):
    C = A + B

end_time = time.time()

print("\nPython execetution:", end_time - start_time, "seconds\n")
