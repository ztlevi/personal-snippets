int2Integer
---

int[] oldArray = {1,2,3};
Integer[] newArray = new Integer[oldArray.length];
int i = 0;
for (int value : oldArray) {
    newArray[i++] = Integer.valueOf(value);
}
 
// OR
Integer[] IntegerArray = ArrayUtils.toObject(intArray)
keyboard_arrow_right
