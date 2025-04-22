# P4 - PseudoMat

**PseudoMat** is a psudocode interpreter which enables users to use a standarazied pseudocode language and run their code.
The main advantage of **PseudoMat** is the ability to use matrix operations in a pseudocode language.
Further advantages is the explicity of the language enabeling users to write code in a more readable way.

## Documentation
The following will provide documentation on how to use **PseudoMat** with given code examples.

---
### Print
The syntax for printing:
```javascript
// Simple string
Print("Hello World!")

// Escape characters
Print("Hello \" Joe \" ")

// Concat string
Print("Hello" + " World!")
```

---
### Simple math operations
The syntax for simple math operations:
```javascript
2.00001 + 3 - 3.251 + 5
// -> 6.749010
```

```javascript
3 + 2.000
// -> 5
```


---
### Variable decleration and assignment
The syntax for variable decleration:
```javascript
// No initialization
Let x as number
// x -> none

// With initialization
Let x be 4 as number
// x -> 4
```

Variable assignment:
```javascript
// Assign simple value
Let x as number
x = 4
// x -> 4

// Assign variable
Let x be 4 as number
x = x+2
// x -> 6

x = x*x
// x -> 36
```

---
### Boolean logic
The syntax for boolean logic:
```javascript
// Less than
3 < 3
// -> False
```
```javascript
// Less than or equals to
3 <= 3
// -> True
```
```javascript
// Not equals to
3 != 4
// -> True
```
```javascript
// Logical Or
True Or False 
// -> True
```
```javascript
// Logical And
True And False
// -> False

```
```javascript
// Will print True
If(3 < 4 Or 6 < 7){
    Print("True")
}
```

---
### If, Else If, Else
**NOTE** that nesting (If, Else If, Else) **IS** also supported in **PseudoMat**
The syntax for `If-statements`:
```javascript
// Simple If
If (True){
    Print("If = True")
}
```

```javascript
// Else If
If (False){
    Print("If = True")
} Else If(True){
    Print("Else If = True")
}
```

```javascript
// Else
If (False){
    Print("If = True")
} Else {
    Print("Else")
}
```

```javascript
// If, Else If, Else
If (False){
    Print("If = True")
} Else If(False){
    Print("Else If = True")
} Else {
    Print("Else")
}
```


---
### For loops
**NOTE** that nesting (For, For-range) **IS** also supported in **PseudoMat**
The syntax for `For-loops`:
```javascript
// Default For-loop
// Will print number from 0-9
Let k as number
For(k = 0; k < 10; k = k + 1) {
    Print(k)
}
```

```javascript
// Range For-loop. Will run 3 times: 1,2,3
For(1 to 3){
    Print("Hello!")
}
```

```javascript
// Range For-loop with nested range For-loop. 
// Will run 3 times: 1,2,3
For(1 to 3){
    Print("Hello!")

    // Will run 1 time: 1
    For(1 to 1){
        Print("Inside loop")
    }
}
```

### While loops
The syntax for `While-loops`:
```javascript
// Default While-loop
// Will print number from 0-9
Let k as number
While (k < 10) {
    Print(k)
    k = k + 1
}
```

### Arrays
Arrays in **PseudoMat** are 0 indexed.
The syntax for `Arrays`:
```javascript
// Array decleration and assignment
Let k be [1, 2, 3] as array
Let k as array
k = [1, 2, 3]
```
```javascript
// Accessing element in array
Let k be [1, 2, 3] as array
Let y be k[2] as number
// y -> 3

y = k[0]
// y -> 1
```
```javascript
// Change element in array
Let k be [1, 2, 3] as array

k[0] = 5
// k -> [5, 2, 3]
```

### Matricies
Matrix declaration:
```javascript
// Valid matrix initialization
Let m1 be [1, 2; 1, 3] as matrix

Let m2 be [1, 2;
           1, 3] as matrix

Let m3 as matrix
m3 = [1, 2;
      1, 2]
```
Assign matrix entry:
```javascript
// Assign entry in matrix a value:
m1[0,0] = 4
m1[1,0] = 3
```
Linear algebra:
```javascript
// Adding two matricies:
Let m4 be m1 + m2 as matrix
m4 = m2 + m3
```

### Functions
Declaring a function:
```javascript
// No parameters
Function foo() {
    Print("Hello from function foo!")
}

// Two parameters and return value
Function bar(num1, num2) {
    Return (num1 + num1) + num2
}
```

Calling a function:
```javascript
// Calling with no parameters
foo()

// Calling with two parameters
foo(2, 4)
```

### Comments
The syntax for `Comments`:
```javascript
# This is a single line comment
(* This is a multiline 
comment*)
```
