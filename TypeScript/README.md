## TypeScript 


**Table of Contents:**
- TypeScript Interfaces
- Comparable Interface for Sorting
- Pattern Matching

### Typescript Interfaces

Please define a TypeScript 'interface' 
In TypeScript, an interface is a way to define a contract for the shape or structure of an object. It describes the properties, methods, and optional/required constraints of an object. Interfaces are used to specify the shape of objects that are expected to conform to a certain structure.

Here's an example of defining an interface in TypeScript:

```typescript
interface Person {
  firstName: string;
  lastName: string;
  age: number;
  gender?: string; // Optional property
}

// Usage of the Person interface
const person: Person = {
  firstName: 'John',
  lastName: 'Doe',
  age: 30,
  gender: 'male'
};

console.log(person.firstName); // Output: John
console.log(person.lastName); // Output: Doe
console.log(person.age); // Output: 30
console.log(person.gender); // Output: male
```

In this example, we define an interface called Person which specifies the shape of a person object. 
It has three properties: firstName, lastName, and age, which are all required properties. 
It also has an optional property gender denoted by the ? symbol, 
which means it may or may not be present in an object that conforms to the Person interface.

We then create an object person that conforms to the Person interface and assign values to its properties. 
We can then access these properties using dot notation, just like with any regular object in JavaScript. 
If an object does not conform to the structure defined by the interface, TypeScript will raise a type error during compile-time, 
helping to catch potential bugs and improve type safety.

https://www.typescriptlang.org/docs/handbook/interfaces.html
https://www.tutorialspoint.com/typescript/typescript_interfaces.htm#
https://www.geeksforgeeks.org/what-is-interfaces-and-explain-it-in-reference-of-typescript/
https://www.w3schools.com/typescript/typescript_aliases_and_interfaces.php

---
**Rectangle Interface:** 
```
// Try creating a new interface using it below
interface Rectangle {
  height: number,
  width: number
};

const rectangle: Rectangle = {
  height: 20,
  width: 10
};

console.log(rectangle);
```

---
**Animal Interface:** 
```
interface Animal {
  name: string;
  age: number;
  speak(): void;
}

class Dog implements Animal {
  name: string;
  age: number;
  
  constructor(name: string, age: number) {
    this.name = name;
    this.age = age;
  }
  
  speak() {
    console.log("Woof!");
  }
}

const myDog = new Dog("Buddy", 3);
console.log(myDog.name); // Output: "Buddy"
console.log(myDog.age); // Output: 3
myDog.speak(); // Output: "Woof!"
```

---
### Comparable Interface for Sorting


> So like Java, Typescript has static typing, so is there something like the comparable interface for typescript ? 

TypeScript is a statically typed superset of JavaScript, so it provides support for static typing and interfaces. 
In TypeScript, you can define interfaces that can be used to specify the shape of objects and their types, similar to Java's Comparable interface.

In TypeScript, you can define an interface with a method that represents a comparison function, and then have objects 
implement that interface to ensure that they implement the required comparison behavior. Here's an example:

```typescript
interface Comparable<T> {
  compareTo(other: T): number;
}

class Person implements Comparable<Person> {
  constructor(public name: string, public age: number) {}

  compareTo(other: Person): number {
    return this.age - other.age;
  }
}

const people: Person[] = [
  new Person("Alice", 30),
  new Person("Bob", 25),
  new Person("Charlie", 35)
];

people.sort((a, b) => a.compareTo(b));

console.log(people);

// Output: [ Person { name: 'Bob', age: 25 },
//           Person { name: 'Alice', age: 30 },
//           Person { name: 'Charlie', age: 35 } ]
```
  
In this example, the Comparable interface is defined with a single method compareTo(), 
which takes another object of the same type and returns a number to indicate the comparison result. 
The Person class implements the Comparable interface, and objects of the Person class can be compared 
using the compareTo() method. The sort() method on the people array takes a comparison function that uses the compareTo() method to sort the array.

Note that TypeScript interfaces are purely for type checking at compile-time and do not generate any runtime code. 
They are used as a way to specify the expected shape of objects and their types, and provide static type checking during development.
  
---
### Pattern Matching

> Functional programming languages have 'pattern matching' that uses static types, can TypeScript be given such a thing ? 

TypeScript does not have built-in support for pattern matching, which is a feature commonly found in functional programming languages. Pattern matching allows you to destructure and match complex data structures based on their shape or values, which can be helpful in handling different cases or scenarios in a concise and expressive way.

However, TypeScript does provide some limited pattern matching capabilities using the instanceof operator and the as keyword for type casting. You can use instanceof to check if an object is an instance of a particular class or constructor function, and use as to perform type casting to a specific type.

Here's an example of using instanceof and as for basic pattern matching in TypeScript:

```typescript
class Circle {
  type: 'circle';
  radius: number;

  constructor(radius: number) {
    this.type = 'circle';
    this.radius = radius;
  }
}

class Rectangle {
  type: 'rectangle';
  width: number;
  height: number;

  constructor(width: number, height: number) {
    this.type = 'rectangle';
    this.width = width;
    this.height = height;
  }
}

function calculateArea(shape: Circle | Rectangle): number {
  if (shape instanceof Circle) {
    // Pattern matching using instanceof
    return Math.PI * shape.radius * shape.radius;
  } else if (shape instanceof Rectangle) {
    // Pattern matching using instanceof
    return shape.width * shape.height;
  } else {
    throw new Error('Unknown shape type');
  }
}

const circle = new Circle(5);
const rectangle = new Rectangle(10, 20);

console.log(calculateArea(circle)); // Output: 78.53981633974483
console.log(calculateArea(rectangle)); // Output: 200
```

In this example, we define two classes Circle and Rectangle, each representing a shape with specific properties. We then define a function calculateArea that takes a parameter shape of type Circle or Rectangle. Inside the function, we use instanceof to check if shape is an instance of Circle or Rectangle, and perform the corresponding area calculation using the properties of the object.

Note that while TypeScript does provide some basic pattern matching capabilities using instanceof and as, it may not be as powerful or expressive as pattern matching in dedicated functional programming languages. If you require more advanced pattern matching features, you may need to consider using a different language or library that provides such functionality.


