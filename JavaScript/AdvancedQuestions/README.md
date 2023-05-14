## Advanced JavaScript Questions

Okay, I will try to create 10 difficult advanced Javascript multiple choice questions with answers that involve evaluating code. Here they are:

1. What is the output of this code?

```javascript
let x = 1;
let y = 2;
let z = 3;
[x, y, z] = [z, x, y];
console.log(x + y + z);
```

A) 6
B) 9
C) 12
D) 15

Answer: A) 6

2. What is the output of this code?

```javascript
function foo() {
  return () => {
    console.log(this);
  };
}

let obj = { name: "bar" };
let baz = foo.call(obj);
baz();
```

A) undefined
B) window
C) { name: "bar" }
D) { name: "foo" }

Answer: C) { name: "bar" }

3. What is the output of this code?

```javascript
let arr = [1, 2, 3];
arr[5] = 6;
console.log(arr.length);
```

A) 3
B) 4
C) 5
D) 6

Answer: D) 6

4. What is the output of this code?

```javascript
function foo(a, b) {
  return a + b;
}

let bar = foo.bind(null, 1);
console.log(bar(2));
```

A) 1
B) 2
C) 3
D) NaN

Answer: C) 3

5. What is the output of this code?

```javascript
let x = "foo";
let y = "bar";
let z = `${x}${y}`;
console.log(z === x + y);
```

A) true
B) false
C) undefined
D) ReferenceError

Answer: A) true

---
Explanations:

Sure, I will explain the answers to the questions I created.

1. The output is 6 because the array destructuring syntax swaps the values of x, y, and z. So x becomes 3, y becomes 1, and z becomes 2. Then the sum of x + y + z is 6.

2. The output is { name: "bar" } because the arrow function inherits the this value from the outer scope, which is set by the call method to obj. So the console.log(this) prints the obj object.

3. The output is 6 because assigning a value to an index that is out of bounds of an array creates a sparse array with empty slots. The length property of an array reflects the highest index plus one, not the number of elements. So arr.length is 5 + 1 = 6.

4. The output is 3 because the bind method returns a new function that has a preset argument of 1. So when bar(2) is called, it is equivalent to foo(1, 2), which returns 1 + 2 = 3.

5. The output is true because the template literal syntax creates a string that is identical to the concatenation of x and y. So z === x + y evaluates to true.

