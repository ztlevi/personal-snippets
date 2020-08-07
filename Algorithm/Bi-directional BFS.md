1.  Single Direction

```python
if endWord not in wordDict:return 0
l=len(beginWord)
steps={beginWord:1}
q=[beginWord]
while q:
    word=q.pop(0)
    step=steps[word]
    for i in xrange(l):
        c=word[i]
        for t in string.ascii_lowercase:
            if c == t:continue
            new_word=word[:i]+t+word[i+1:]
            if new_word == endWord: return step+1
            if new_word not in wordDict: continue
            wordDict.remove(new_word)
            steps[new_word]=step+1
            q.append(new_word)

return 0
```

2.  Bi-direction https://leetcode.com/problems/word-ladder/description/

```python
class Solution(object):
    def ladderLength(self, beginWord, endWord, wordList):
        """
        :type beginWord: str
        :type endWord: str
        :type wordList: List[str]
        :rtype: int
        """
        wordList = set(wordList)
        forward, backward, n, step = {beginWord}, {endWord}, len(beginWord), 0
        dic = set(string.ascii_lowercase)
        if endWord not in wordList:return 0
        while forward and backward:
            if len(forward) > len(backward):
                forward, backward = backward, forward

            step += 1
            next = set()
            for word in forward:
                for i, char in enumerate(word):
                    first, second = word[:i], word[i + 1:]
                    for item in dic:
                        candidate = first + item + second
                        if candidate in backward:
                            return step+1

                        if candidate in wordList:
                            wordList.discard(candidate)
                            next.add(candidate)
            forward = next
        return 0
```
