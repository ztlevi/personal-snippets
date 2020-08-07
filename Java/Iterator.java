Two ways to use iterators
---

  public static void main(String... aArguments) {
    List<String> flavours = new ArrayList<>();
    flavours.add("chocolate");
    flavours.add("strawberry");
    flavours.add("vanilla");

    useWhileLoop(flavours);

    useForLoop(flavours);
  }

  private static void useWhileLoop(Collection<String> aFlavours) {
    Iterator<String> flavoursIter = aFlavours.iterator();
    while (flavoursIter.hasNext()){
      System.out.println(flavoursIter.next());
    }
  }

  /**
  * Note that this for-loop does not use an integer index.
  */
  private static void useForLoop(Collection<String> aFlavours) {
    for (Iterator<String> flavoursIter = aFlavours.iterator(); flavoursIter.hasNext();){
      System.out.println(flavoursIter.next());
    }
  }