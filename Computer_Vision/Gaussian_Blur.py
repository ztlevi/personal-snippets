#!/usr/bin/env python

from math import exp


def gaussian(x, mu, sigma):
    return exp(-(((x - mu) / (sigma)) ** 2) / 2.0)


# kernel_height, kernel_width = 7, 7
kernel_radius = 3  # for an 7x7 filter
sigma = kernel_radius / 2.0  # for [-2*sigma, 2*sigma]

# compute the actual kernel elements
hkernel = [gaussian(x, kernel_radius, sigma) for x in range(2 * kernel_radius + 1)]
vkernel = [x for x in hkernel]
kernel2d = [[xh * xv for xh in hkernel] for xv in vkernel]

# normalize the kernel elements
kernelsum = sum([sum(row) for row in kernel2d])
kernel2d = [[x / kernelsum for x in row] for row in kernel2d]

for line in kernel2d:
    print(["%.3f" % x for x in line])
# >>>
# ['0.001', '0.004', '0.008', '0.010', '0.008', '0.004', '0.001']
# ['0.004', '0.012', '0.024', '0.030', '0.024', '0.012', '0.004']
# ['0.008', '0.024', '0.047', '0.059', '0.047', '0.024', '0.008']
# ['0.010', '0.030', '0.059', '0.073', '0.059', '0.030', '0.010']
# ['0.008', '0.024', '0.047', '0.059', '0.047', '0.024', '0.008']
# ['0.004', '0.012', '0.024', '0.030', '0.024', '0.012', '0.004']
# ['0.001', '0.004', '0.008', '0.010', '0.008', '0.004', '0.001']
