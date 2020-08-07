# The main function that sort the given string arr[] in 
# alphabetical order
def countSort(arr):
    n = len(arr)
    output = ['' ]*n
    cnt = [0] * 256

    for i in arr:
        cnt[ord(i)] += 1

    idx = 0
    for i, c in enumerate(cnt):
        for j in range(c):
            output[idx] = chr(i)
            idx += 1
    return output

# Driver program to test above function
arr = "geeksforgeeks"
ans = countSort(arr)
print "Sorted character array is %s"  %("".join(ans))

# This code is contributed by Nikhil Kumar Singh