from matplotlib import pyplot as plt
import numpy as np

data = [(-11,4),(-11,5),(-11,6),(-10,3),(-9,2),(-8,1),(-7,0),(-6,-1),(-5,-2),(-4,-3),(-3,-4),(-2,-5),(-1,-5),(0
,-5),(0,0),(1,-5),(1,1),(1,3),(2,-5),(2,2),(2,4),(3,-5),(3,5),(4,-5),(4,5),(5,-5),(5,5),(6,-4),(6,4),(7,-3),(7,3)
,(8,-2),(8,2),(9,-1),(9,1),(10,0)]

data = np.loadtxt("res.csv", delimiter = ",", dtype = np.int)

data = np.array(data).T

plt.scatter(data[0], data[1])
plt.show()

