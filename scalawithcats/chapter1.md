# Typeclass
Trait with abstract methods can be used to define typeclasses.

## Typeclass Instances
In scala we define typeclass instances by creating concrete implementations of the type class and tagging them with the implicit keyword

## Typeclass Interfaces
Typeclass interface is any functionality we expose to users.
Interfaces are generic methods that accept instances of the type class as implicit parameters.

### Interface Objects
### Interface Syntax

Working with Typeclass means working with Implicits values and implicit parameters.

### Packaging implicits
* Any definition marked as implicit must be placed inside an object or trait.

### Implicit Scope
* Place the typeclass instances in the companion object of the typeclass to create an implicit scope.
* Consists of
  1. Local or inherited definitions
  2. Imported definitions
  3. Definitions in the companion object of the type class
  4. Definitions in the companion object of the parameter type.

### Four ways to package typeclass instances
1. Place them in a separate object. - Bring into scope by importing them.
2. Place them in a trait. - Brought to scope by inheritance.
3. Place them in companion object of the typeclass. - Typeclass instances are always in scope regardless of where we use them.
4. Place them in companion object of the the paramter type - Typeclass instances are always in scope regardless of where we use them.

