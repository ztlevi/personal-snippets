def dfs(node):
    if not node:
        return None
    if node.val == p.val:
        return node
    elif node.val > p.val:
        return dfs(node.left)
    else:
        return dfs(node.right)

