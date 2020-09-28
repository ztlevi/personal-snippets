It is a marker annotation that can be used only on methods. A method annotated with @Override must
override a method from a superclass. If it doesn’t, a compile-time error will result (see this for
example). It is used to ensure that a superclass method is actually overridden, and not simply
overloaded.

Example:-

```java
class Base
{
     public void Display()
     {
         System.out.println("Base display()");
     }

     public static void main(String args[])
     {
         Base t1 = new Derived();
         t1.Display();
     }
}
class Derived extends Base
{
     @Override
     public void Display()
     {
         System.out.println("Derived display()");
     }
}
```

Output:

```
Derived display()
```
