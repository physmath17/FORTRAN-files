import numpy as np
import matplotlib.pyplot as plt

fr = open("../data/sample_MC_int_1.txt")
lines = fr.readlines()
fr.close()

x = []

for line in lines:
    p = line.split()
    x.append(float(p[0])

plt.scatter(x)
plt.xticks([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
plt.yticks([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
plt.show()
