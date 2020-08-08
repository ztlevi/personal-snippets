def preorder(self,node):
    if node is not None:
        print node.info
        self.preorder(node.left)
        self.preorder(node.right)


# Preorder non recursive version is easiest to write
def preorder_traversal(root):
    """
    :type root: TreeNode
    :rtype: List[int]
    """
    if not root:
        return []
    result = []
    stack = [root]
    while len(stack) > 0:
        curr_node = stack.pop()
        result.append(curr_node.val)
        if curr_node.right:
            stack.append(curr_node.right)
        if curr_node.left:
            stack.append(curr_node.left)
    return result


import re
import sets

a = {}
a[1] = 3
a[4] = 2
a[3] = 1
for k, v in a.items():
    print(k)
b = sets.set([1, 2, 3])
for k in b:
    print(k)
