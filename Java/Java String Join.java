// join(CharSequence delimiter, CharSequence... elements)
String joined = String.join("/", "2014", "10", "28" ); // "2014/10/28"
 
// join(CharSequence delimiter, Iterable<? extends CharSequence> elements)
List<String> list = Arrays.asList("foo", "bar", "baz");
joined = String.join(";", list); // "foo;bar;baz"