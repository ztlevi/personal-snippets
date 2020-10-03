# https://leetcode.com/problems/minimum-window-substring/discuss/26808/Here-is-a-10-line-template-that-can-solve-most-'substring'-problems

- Longest Substring with At Most Two Distinct Characters
- Longest Substring Without Repeating Characters

## Python Template

```python
import collections
class Solution(object):
    def findSubstring(self, s):
        """
        :type s: str
        :rtype: int
        """
        i, j = 0,0
        I, J = 0,0
        cnt = collections.defaultdict(int)
        count = 0

        for j, jc in enumerate(s, 1):
            if cnt[jc] ?:
                # modify counter here e.g. count += 1
                cnt[jc] += 1

            while (counter condition):
                if i < j and (counter condition):
                    # modify counter here count -= 1
                    cnt[s[i]] -= 1
                    i += 1
            # update J I here
            # e.g. if not J or J - I < j - i: J, I = j, i
        return J-I
```

## Java Template

```java
public int findSubstring(string s){
  HashMap<Integer> map = new HashMap();
  int counter; // check whether the substring is valid
  int begin=0, end=0; //two pointers, one point to tail and one  head
  int d; //the length of substring

  for() { /* initialize the hash map here */ }

  while(end<s.size()){

    if(map[s[end++]]-- ?){  /**/ }

    while(/* counter condition */){

      /* update d here if finding minimum*/

      //increase begin to make it invalid/valid again

      if(map[s[begin++]]++ ?){ /*modify counter here*/ }
    }

    /* update d here if finding maximum*/
  }
  return d;
}
```
