import org.junit.Test;

import static org.junit.Assert.*;

public class Solution {
    @Test
    public void testAssertions() {
        String str1 = new String("abd");
        String str2 = new String("abd");
        int[] arr1 = new int[2];
        int[] arr2 = new int[2];
        assertEquals(str1, str2);
        assertEquals(arr1, arr2);
    }

}