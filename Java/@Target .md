It is designed to be used only as an annotation to another annotation. @Target takes one argument,
which must be constant from the ElementType enumeration. This argument specifies the type of
declarations to which the annotation can be applied. The constants are shown below along with the
type of declaration to which they correspond.

| Target Constant | Annotations Can be Applied To    |
| --------------- | -------------------------------- |
| ANNOTATION_TYPE | Another annotation               |
| CONSTRUCTOR     | Constructor                      |
| FIELD           | Field                            |
| LOCAL_VARIABLE  | Local variable                   |
| METHOD          | Method                           |
| PACKAGE         | Package                          |
| PARAMETER       | Parameter                        |
| TYPE            | Class, Interface, or enumeration |

We can specify one or more of these values in a **@Target** annotation. To specify multiple values,
we must specify them within a braces-delimited list. For example, to specify that an annotation
applies only to fields and local variables, you can use this @Target annotation:
@Target({ElementType.FIELD, ElementType.LOCAL_VARIABLE}) **@Retention** Annotation It determines
where and how long the annotation is retent. The 3 values that the @Retention annotation can have:

- SOURCE: Annotations will be retained at the source level and ignored by the compiler.
- CLASS: Annotations will be retained at compile time and ignored by the JVM.
- RUNTIME: These will be retained at runtime.
