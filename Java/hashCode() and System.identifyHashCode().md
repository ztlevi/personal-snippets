- hashCode, as defined in the JavaDocs, says:

> As much as is reasonably practical, the hashCode method defined by class Object does return distinct integers for
> distinct objects. (This is typically implemented by converting the internal address of the object into an integer, but
> this implementation technique is not required by the Java™ programming language.)

So if you are using hashCode() to find out if it is a unique object in memory that isn't a good way to do it.

- System.identityHashCode does the following:

> Returns the same hash code for the given object as would be returned by the default method hashCode(), whether or not
> the given object's class overrides hashCode(). The hash code for the null reference is zero.

```java
      String a = new String("fff");
      String b = new String("fff");
      String c = "fff";
      String d = "fff";
      System.out.println(a==b);
      System.out.println(a==c);
      System.out.println(d==c);
      System.out.println(a.hashCode());
      System.out.println(b.hashCode());
      System.out.println(c.hashCode());
      System.out.println(d.hashCode());
      System.out.println(System.identityHashCode(a));
      System.out.println(System.identityHashCode(b));
      System.out.println(System.identityHashCode(c));
      System.out.println(System.identityHashCode(d));
```

Result:

```
false
false
true

101286
101286
101286
101286

1627674070
1360875712
1625635731
1625635731
```

c and d's references are the same object, while a and b are not.
