It is a marker annotation. It indicates that a declaration is obsolete and has been replaced by a
newer form. The Javadoc @deprecated tag should be used when an element has been deprecated.

@deprecated tag is for documentation and @Deprecated annotation is for runtime reflection.

@deprecated tag has higher priority than @Deprecated annotation when both are together used.

```java
public class DeprecatedTest
{
    @Deprecated
    public void Display()
    {
        System.out.println("Deprecatedtest display()");
    }

    public static void main(String args[])
    {
        DeprecatedTest d1 = new DeprecatedTest();
        d1.Display();
    }
}
```

Output:

```
Deprecatedtest display()
```
