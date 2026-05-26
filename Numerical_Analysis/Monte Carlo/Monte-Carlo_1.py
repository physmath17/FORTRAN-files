import numpy as np
import matplotlib.pyplot as plt

fc = open("../data/sample_MC_c.txt")
lines1 = fc.readlines()
fc.close()

fs = open("../data/sample_MC_sq.txt")
lines2 = fs.readlines()
fs.close()

x1 = []
y1 = []
x2 = []
y2 = []

for line in lines1:
    p = line.split()
    x1.append(float(p[0]))
    y1.append(float(p[1]))

for line in lines2:
    q = line.split()
    x2.append(float(q[0]))
    y2.append(float(q[1]))

plt.figure(1)
plt.scatter(x1 ,y1)
plt.figure(2)
plt.scatter(x2 ,y2)
plt.xticks([0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1])
plt.yticks([0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1])
plt.show()
plt.show()
