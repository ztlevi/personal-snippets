# Error bars

```python
print(k_to_accuracies)
# plot the raw observations
for k in k_choices:
    accuracies = k_to_accuracies[k]
    plt.scatter([k] * len(accuracies), accuracies)

# plot the trend line with error bars that correspond to standard deviation
accuracies_mean = np.array([np.mean(v) for k,v in sorted(k_to_accuracies.items())])
accuracies_std = np.array([np.std(v) for k,v in sorted(k_to_accuracies.items())])
plt.errorbar(k_choices, accuracies_mean, yerr=accuracies_std)
plt.title('Cross-validation on k')
plt.xlabel('k')
plt.ylabel('Cross-validation accuracy')
plt.show()
```

- output:

```
{1: [0.263, 0.257, 0.264, 0.278, 0.266],
3: [0.239, 0.249, 0.24, 0.266, 0.254],
5: [0.248, 0.266, 0.28, 0.292, 0.28],
8: [0.262, 0.282, 0.273, 0.29, 0.273],
10: [0.265, 0.296, 0.276, 0.284, 0.28],
12: [0.26, 0.295, 0.279, 0.283, 0.28],
15: [0.252, 0.289, 0.278, 0.282, 0.274],
20: [0.27, 0.279, 0.279, 0.282, 0.285],
50: [0.271, 0.288, 0.278, 0.269, 0.266],
100: [0.256, 0.27, 0.263, 0.256, 0.263]}
```

![](https://ws4.sinaimg.cn/large/006tNbRwly1fwq8g3l12lj30ny0fidg4.jpg)
