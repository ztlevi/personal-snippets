class Solution(object):
    def majorityElement(self, nums):
        """
        :type nums: List[int]
        :rtype: List[int]
        """
        c1, c2, count1, count2 = 0, 1, 0, 0
        for n in nums:
            if n == c1:
                count1 += 1
            elif n == c2:
                count2 += 1
            elif count1 == 0:
                c1 = n
                count1 = 1
            elif count2 == 0:
                c2 = n
                count2 = 1
            else:
                count1 -= 1; count2 -= 1
        return [x for x in (c1, c2) if nums.count(x) > len(nums)/3]
    