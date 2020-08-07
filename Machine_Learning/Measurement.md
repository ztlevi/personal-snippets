# Measurements

## Confusion matrix

https://en.wikipedia.org/wiki/F1_score

![](https://ws4.sinaimg.cn/large/006tNbRwly1fx8eehvc41j31kw0f57fk.jpg)

## F1 score

In [statistical](https://en.wikipedia.org/wiki/Statistics "Statistics") analysis of
[binary classification](https://en.wikipedia.org/wiki/Binary_classification "Binary classification"), the
**F<sub>1</sub> score** (also **F-score** or **F-measure**) is a measure of a test's accuracy. It considers both the
[precision](<https://en.wikipedia.org/wiki/Precision_(information_retrieval)> "Precision (information retrieval)") _p_
and the [recall](<https://en.wikipedia.org/wiki/Recall_(information_retrieval)> "Recall (information retrieval)") _r_ of
the test to compute the score: _p_ is the number of correct positive results divided by the number of all positive
results returned by the classifier, and _r_ is the number of correct positive results divided by the number of all
relevant samples (all samples that should have been identified as positive). The F<sub>1</sub> score is the
[harmonic average](https://en.wikipedia.org/wiki/Harmonic_mean "Harmonic mean") of the
[precision and recall](https://en.wikipedia.org/wiki/Precision_and_recall "Precision and recall"), where an
F<sub>1</sub> score reaches its best value at 1 (perfect precision and recall) and worst at 0.

- **F score** or [F1 score](https://en.wikipedia.org/wiki/F1_score): that calculates the harmonic mean of the precision
  and recall (harmonic mean because the precision and recall are ratios).

![](https://ws2.sinaimg.cn/large/006tNbRwly1fx8efflx7dj30v204e3z3.jpg)

## ROC an AUC Curve

https://developers.google.com/machine-learning/crash-course/classification/roc-and-auc When using normalized units, the
area under the curve (often referred to as simply the AUC) is equal to the probability that a classifier will rank a
randomly chosen positive instance higher than a randomly chosen negative one (assuming 'positive' ranks higher than
'negative')

## Precision-Recall Curves

There are also composite scores that attempt to summarize the precision and recall; three examples include:

- **Average precision**: that summarizes the weighted increase in precision with each change in recall for the
  thresholds in the precision-recall curve.
- **Area Under Curve**: like the AUC, summarizes the integral or an approximation of the area under the precision-recall
  curve.

In terms of model selection, F1 summarizes model skill for a specific probability threshold, whereas average precision
and area under curve summarize the skill of a model across thresholds, like ROC AUC.
