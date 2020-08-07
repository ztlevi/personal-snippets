    def sortedArrayToBST(self, nums):
        """
        :type nums: List[int]
        :rtype: TreeNode
        """
        if not nums:
            return None
        elif len(nums) == 1:
            return TreeNode(nums[0])
        mid = len(nums) // 2
        node = TreeNode(nums[mid])
        left = self.sortedArrayToBST(nums[:mid])
        right = self.sortedArrayToBST(nums[mid+1:])
        node.left = left
        node.right = right
        return node