def preorder(self, node):
    if node is not None:
        print(node.val)
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


class Node:
    left = None
    right = None

    def __init__(self, val):
        self.val = val


node = Node(4)
node.left = Node(2)
node.left.left = Node(1)
node.right = Node(5)

print(preorder_traversal(node))
