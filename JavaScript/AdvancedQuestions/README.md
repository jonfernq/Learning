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

Okay, I will create more questions on advanced Javascript. Here they are:

6. What is the output of this code?

```javascript
let x = 1;
let y = 2;
let z = 3;
function foo(a, b, c) {
  a = 4;
  b = 5;
  c = 6;
}
foo(x, y, z);
console.log(x + y + z);
```

A) 6
B) 9
C) 12
D) 15

Answer: A) 6

7. What is the output of this code?

```javascript
let x = { name: "foo" };
let y = { name: "bar" };
let z = x;
z.name = "baz";
console.log(x.name);
```

A) foo
B) bar
C) baz
D) undefined

Answer: C) baz

8. What is the output of this code?

```javascript
let x = [1, 2, 3];
let y = [4, 5, 6];
let z = [...x, ...y];
console.log(z.length);
```

A) 3
B) 4
C) 5
D) 6

Answer: D) 6

9. What is the output of this code?

```javascript
function foo() {
  console.log(this);
}

let obj = { name: "bar" };
let baz = foo.bind(obj);
baz.call(6);
```

A) undefined
B) window
C) { name: "bar" }
D) { name: "foo" }

Answer: C) { name: "bar" }

10. What is the output of this code?

```javascript
function foo() {
  let x = 1;
  function bar() {
    let x = 2;
    function baz() {
      let x = 3;
      console.log(x);
    }
    baz();
    console.log(x);
  }
  bar();
  console.log(x);
}
foo();
```

A) 1, 2, 3
B) 3, 2, 1
C) 1, 1, 1
D) 3, 3, 3

Answer: B) 3, 2, 1

11. What is the output of this code?

```javascript
let x = { name: "foo", age: 20 };
let y = { name: "bar", city: "New York" };
let z = Object.assign({}, x, y);
console.log(z); // 
```

A) { name: "foo", name: "bar", age: 20, city: "New York" }
B) { name: "foo", age: 20, city: "New York" }
C) { name: "bar", age: 20, city: "New York" }
D) undefined

Answer: C) { name: "bar", age: 20, city: "New York" } 

12. What is the output of this code?

```javascript
let x = { name: "foo" };
let y = { name: "bar" };
let z = Object.assign({}, x, y);
z.name = "baz";
console.log(x.name);
```

A) foo
B) bar
C) baz
D) undefined

Answer: A) foo

13. What is the output of this code?

```javascript
let x = [1, 2, 3];
let y = [4, 5, 6];
let z = x.concat(y);
z[0] = 7;
console.log(x[0]);
```

A) 1
B) 4
C) 6
D) 7

Answer: A) 1

14. What is the output of this code?

```javascript
function foo(a, b) {
  return a + b;
}

let bar = foo.bind(null, 1);
let baz = bar.bind(null, 2);
console.log(baz(3));
```

A) 3
B) 4
C) 5
D) 6

Answer: D) 6

15. What is the output of this code?

```javascript
let x = "foo";
let y = "bar";
let z = `${x}${y}`;
console.log(z == x + y);
```

A) true
B) false
C) undefined
D) ReferenceError

Answer: A) true

---
**Explanations:**

1. The output is 6 because the array destructuring syntax swaps the values of x, y, and z. So x becomes 3, y becomes 1, and z becomes 2. Then the sum of x + y + z is 6.

2. The output is { name: "bar" } because the arrow function inherits the this value from the outer scope, which is set by the call method to obj. So the console.log(this) prints the obj object.

3. The output is 6 because assigning a value to an index that is out of bounds of an array creates a sparse array with empty slots. The length property of an array reflects the highest index plus one, not the number of elements. So arr.length is 5 + 1 = 6.

4. The output is 3 because the bind method returns a new function that has a preset argument of 1. So when bar(2) is called, it is equivalent to foo(1, 2), which returns 1 + 2 = 3.

5. The output is true because the template literal syntax creates a string that is identical to the concatenation of x and y. So z === x + y evaluates to true.

6. The output is 9 because the parameters of the foo function are passed by value, not by reference. So changing the values of a, b, and c inside the function does not affect the values of x, y, and z outside the function. The sum of x + y + z is still 1 + 2 + 3 = 9.

7. The output is baz because the variables x and z point to the same object in memory. So changing the name property of z also changes the name property of x. The console.log(x.name) prints baz.

8. The output is 6 because the spread operator (...) creates a new array that contains all the elements of x and y. The length of z is 3 + 3 = 6.

9. The output is { name: "bar" } because the bind method returns a new function that has a fixed this value of obj. So calling baz with any other value of this does not change the this value inside foo. The console.log(this) prints obj.

10. The output is 3, 2, 1 because the variable x is declared with let in each scope, creating a new binding for each x. The console.log(x) prints the value of x in the closest scope. So baz prints 3, bar prints 2, and foo prints 1.

The difference between ‘==’ and ‘===’ in Javascript is that ‘==’ is the equality operator that compares two values after converting them to a common type, while ‘===’ is the strict equality operator that compares two values without any type conversion





12. The output is foo because the Object.assign method creates a new object that copies the properties of x and y. If x and y have the same property, the value of y will overwrite the value of x. So z is a new object with name: "bar". Changing the name property of z does not affect the name property of x. The console.log(x.name) prints foo.

13. The output is 1 because the concat method creates a new array that contains all the elements of x and y. So z is a new array with [1, 2, 3, 4, 5, 6]. Changing the value of z[0] does not affect the value of x[0]. The console.log(x[0]) prints 1.

14. The output is 6 because the bind method returns a new function that has preset arguments. If a function is already bound, calling it with another bind does not change its this value or arguments. So baz is equivalent to foo(1, 2), which returns 1 + 2 = 3. Then baz(3) passes 3 as a third argument to foo, which returns 3 + 3 = 6.

15. The output is true because the template literal syntax creates a string that is identical to the concatenation of x and y. So z == x + y evaluates to true.





