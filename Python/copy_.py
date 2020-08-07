#!/usr/bin/env python

# importing copy module
import copy

import numpy

# initializing list 1
li1 = [1, 2, [3, 5], 4]


# using copy for shallow copy
li2 = copy.copy(li1)

# using deepcopy for deepcopy
li3 = copy.deepcopy(li1)

# Numpy deepcopy
y = np.copy(x)
