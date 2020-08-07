def postorder(self,node):
    if node is not None:
        self.postorder(node.left)
        self.postorder(node.right)
        print node.info

def postorder_traversal(root):
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
        if curr_node.left:
            stack.append(curr_node)
            stack.append(curr_node.left)
            curr_node.left = None
        elif curr_node.right:
            stack.append(curr_node)
            stack.append(curr_node.right)
            curr_node.right = None
        else:
            result.append(curr_node.val)
    return result
