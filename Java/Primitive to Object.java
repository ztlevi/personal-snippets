List<Integer> list = new ArrayList<Integer>(Arrays.asList(1,2,3));

// List to array
int[] array = list.stream().mapToInt(i->i).toArray();

List<Integer> newList = Arrays.stream(array).boxed().collect(Collectors.toList());
