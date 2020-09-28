It is used to inform the compiler to suppress specified compiler warnings. The warnings to suppress
are specified by name, in string form. This type of annotation can be applied to any type of
declaration.

Java groups warnings under two categories. They are : deprecation and unchecked.. Any unchecked
warning is generated when a legacy code interfaces with a code that use generics.

```java
class DeprecatedTest
{
    @Deprecated
    public void Display()
    {
        System.out.println("Deprecatedtest display()");
    }
}

public class SuppressWarningTest
{
    // If we comment below annotation, program generates
    // warning
    @SuppressWarnings({"checked", "deprecation"})
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
