Integer2int
---

int[] intArray = new int[IntegerArray.length];
for (int i = 0; i < IntegerArray.length; i++) {
    intArray[i] = IntegerArray[i].intValue();
}
 
// OR
ArraysUtils.toPrimitive(IntegerArray);
