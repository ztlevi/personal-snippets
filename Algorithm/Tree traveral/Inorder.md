# http://www.lintcode.com/en/problem/inorder-successor-in-binary-search-tree/

```python
def inorder(self, node):
    if node is not None:
        self.inorder(node.left)
        print node.info
        self.inorder(node.right)
```

```java
// Non recursive
List<Integer> inorderTreeTraversal(ListNode root) {
  List<Integer> result = new ArrayList<Integer>();
  if (root == null) {
    return result;
  }
  Stack<ListNode> stack = new Stack<ListNode>();
  ListNode cur = root;

  while (cur != null) {
    stack.push(cur);
    cur = cur.left;
  }

  while (!stack.isEmpty()) {
    cur = stack.pop();
    result.add(cur.val);

    if (cur.right != null) {
      cur = cur.right;
      while (cur != null) {
        stack.push(cur);
        cur = cur.left;
      }
    }
  }
  return result;
}
```
