import collections


# Definition for a binary tree node.
class TreeNode(object):
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None


class Codec:
    def serialize(self, root):
        """Encodes a tree to a single string.

        :type root: TreeNode
        :rtype: str
        """

        # edge case
        if not root:
            return '[]'

        res = []
        q = collections.deque([root])
        while q:
            nq = collections.deque()
            while q:
                node = q.popleft()
                if not node:
                    res.append('null')
                    continue

                res.append(str(node.val))

                if node.left:
                    nq.append(node.left)
                else:
                    nq.append(None)
                if node.right:
                    nq.append(node.right)
                else:
                    nq.append(None)
            q = nq
        return '[' + ','.join(res).rstrip(',null') + ']'

    def deserialize(self, data):
        """Decodes your encoded data to tree.

        :type data: str
        :rtype: TreeNode
        """
        if not data:
            return None
        if data == '[]':
            return []
        s = data[1:-1]
        nodes = s.split(',')
        nodes = collections.deque(nodes)
        root = TreeNode(nodes.popleft())
        q = collections.deque([root])

        while nodes:
            cur = q.popleft()
            if nodes:
                left = nodes.popleft()
                if left != 'null':
                    left = TreeNode(left)
                    q.append(left)
                    cur.left = left
            if nodes:
                right = nodes.popleft()
                if right != 'null':
                    right = TreeNode(right)
                    q.append(right)
                    cur.right = right
        return root



# Your Codec object will be instantiated and called as such:
codec = Codec()
root = TreeNode(1)
root.left = TreeNode(2)
root.right = TreeNode(3)
root.right.left = TreeNode(4)
root.right.right = TreeNode(5)

a = codec.serialize(root)

print(a)

b = codec.deserialize(a)
print(b)
