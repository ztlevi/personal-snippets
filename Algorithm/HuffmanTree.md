```python
from collections import Counter
from heapq import *


class HuffmanTreeNode:
    def __init__(self, _c, _freq):
        self.c = _c
        self.freq = _freq
        self.left = None
        self.right = None

    def __lt__(self, other):
        return self.freq < other.freq


def buildHuffmanTree(s):
    dict = Counter(s)
    pq = []
    for c in dict:
        heappush(pq, HuffmanTreeNode(c, dict[c]))

    while len(pq) > 1:
        node1 = heappop(pq)
        node2 = heappop(pq)
        root = HuffmanTreeNode(0, node1.freq + node2.freq)
        if node1.freq > node2.freq:
            root.left = node1
            root.right = node2
        else:
            root.left = node2
            root.right = node1
        heappush(pq, root)

    return pq[0]


def huffman_dfs(root, s):
    if not root:
        return
    if root.c:
        print(f"{root.c} : {s}\n")
    huffman_dfs(root.left, s + "0")
    huffman_dfs(root.right, s + '1')


def print_huffman_code(s):
    root = buildHuffmanTree(s)
    huffman_dfs(root, "")


if __name__ == '__main__':
    print_huffman_code("this is an example of a huffman tree")
```

```c++
#include "stdc.h"
using namespace std;

class HuffmanTreeNode {
public:
    HuffmanTreeNode(char _c, int _freq) {
        c = _c;
        freq = _freq;
        left = nullptr;
        right = nullptr;
    }
    char c;
    int freq;
    HuffmanTreeNode*left;
    HuffmanTreeNode*right;
};

HuffmanTreeNode* buildHuffmanTree(string str) {
    unordered_map<char, int>dict;
    for (auto c : str) {
        dict[c]++;
    }
    auto cmp = [](HuffmanTreeNode*node1, HuffmanTreeNode*node2) {
        return node1->freq > node2->freq;
    };
    priority_queue<HuffmanTreeNode*, vector<HuffmanTreeNode*>, decltype(cmp)>q(cmp);
    for (auto & p : dict) {
        q.push(new HuffmanTreeNode(p.first, p.second));
    }

    while (q.size() > 1) {
        auto node1 = q.top();
        q.pop();
        auto node2 = q.top();
        q.pop();
        auto root = new HuffmanTreeNode(0, node1->freq + node2->freq);
        root->left = node1->freq > node2->freq ? node1 : node2;
        root->right = node1->freq > node2->freq ? node2 : node1;
        q.push(root);
    }
    return q.top();
}

void huffman_dfs(HuffmanTreeNode*root, string str) {
    if (!root) return;
    if (root->c) {
        cout<<root->c<<":"<<root->freq<<":"<<str<<endl;
    }
    huffman_dfs(root->left, str+"0");
    huffman_dfs(root->right, str+"1");
}
void print_huffman_code(string str) {
    HuffmanTreeNode * root = buildHuffmanTree(str);
    huffman_dfs(root, "");
}

void test_huffman_code() {
    print_huffman_code("this is an example of a huffman tree");
}

int main() {
    test_huffman_code();
    return 0;
}
```
