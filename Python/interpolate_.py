https://docs.scipy.org/doc/scipy/reference/generated/scipy.interpolate.interp1d.html
---

x = np.arange(0, l)
y = bio_data[i][h]
f = interp1d(x, y)
xnew = np.arange(0, l-1, 0.1)
ynew = f(xnew)
import matplotlib.pyplot as plt
plt.plot(x, y, 'o', xnew, ynew, '-')