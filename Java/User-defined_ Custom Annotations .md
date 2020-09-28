User-defined annotations can be used to annotate program elements, i.e. variables, constructors,
methods, etc. These annotations can be applied just before declaration of an element (constructor,
method, classes, etc). Syntax of Declaration:-

```
[Access Specifier] @interface<AnnotationName>
{
   DataType <Method Name>() [default value];
}
```

- AnnotationName is an identifier.
- Parameter should not be associated with method declarations and throws clause should not be used
  with method declaration.
- Parameters will not have a null value but can have a default value.
- default value is optional.
- Return type of method should be either primitive, enum, string, class name or array of primitive,
  enum, string or class name type.

```java
package source;
// A Java program to demonstrate user defined annotations
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

// user-defined annotation
@Documented
@Retention(RetentionPolicy.RUNTIME)
@ interface TestAnnotation
{
    String Developer() default "Rahul";
    String Expirydate();
} // will be retained at runtime

// Driver class that uses @TestAnnotation
public class Test
{
    @TestAnnotation(Developer="Rahul", Expirydate="01-10-2020")
    void fun1()
    {
        System.out.println("Test method 1");
    }

    @TestAnnotation(Developer="Anil", Expirydate="01-10-2021")
    void fun2()
    {
        System.out.println("Test method 2");
    }

    public static void main(String args[])
    {
        System.out.println("Hello");
    }
}
```

Output :

```
Hello
```
