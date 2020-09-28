@Inherited is a marker annotation that can be used only on annotation declaration. It affects only
annotations that will be used on class declarations. @Inherited causes the annotation for a
superclass to be inherited by a subclass. Therefore, when a request for a specific annotation is
made to the subclass, if that annotation is not present in the subclass, then its superclass is
checked. If that annotation is present in the superclass, and if it is annotated with @Inherited,
then that annotation will be returned.
