# Definition and Usage

https://www.w3schools.com/cssref/css3_pr_flex.asp

The `flex` property is a shorthand property for:

- [flex-grow](https://www.w3schools.com/cssref/css3_pr_flex-grow.asp)
- [flex-shrink](https://www.w3schools.com/cssref/css3_pr_flex-shrink.asp)
- [flex-basis](https://www.w3schools.com/cssref/css3_pr_flex-basis.asp)

The `flex` property sets the flexible length on flexible items.

> **Note**: If the element is not a flexible item, the flex property has no effect.

- Default value: 0 1 auto
- Inherited: no
- Animatable: yes, see individual properties. Read about animatable
- Version: CSS3
- JavaScript syntax: object.style.flex="1"

# CSS Syntax

```
flex: flex-grow flex-shrink flex-basis|auto|initial|inherit;
```

# Property Values

| Value       | Description                                                                                                               |
| ----------- | ------------------------------------------------------------------------------------------------------------------------- |
| flex-grow   | A number specifying how much the item will grow relative to the rest of the flexible items                                |
| flex-shrink | A number specifying how much the item will shrink relative to the rest of the flexible items                              |
| flex-basis  | The length of the item. Legal values: "auto", "inherit", or a number followed by "%", "px", "em" or any other length unit |
| auto        | Same as 1 1 auto.                                                                                                         |
| initial     | Same as 0 1 auto. Read about initial                                                                                      |
| none        | Same as 0 0 auto.                                                                                                         |
| inherit     | Inherits this property from its parent element. Read about inherit                                                        |

# Flex-wrap

The `flex-wrap` property specifies whether the flexible items should wrap or not.

Note: If the elements are not flexible items, the `flex-wrap` property has no effect.
