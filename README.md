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

### Simple string operation:
```javascript
// String comparison
Let a be "Hello" as string
Let b be "Hello" as string

a == b
// -> true
```
```javascript
// String comparison
Let a be "Hello World" as string
Let b be "Hello" as string

a > b
// -> true
```
```javascript
// String comparison
Let a be "Hello World" as string

Print(a[0])
// -> H
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

### Vectors
Important:
- When multiplying vectors, if one is a column and the other is a row, the result is a scalar (dot product).
- When both are columns (or rows), and we transpose one, the result can be a full matrix (outer product).
- We **transpose the right-hand vector** (the second matrix) to ensure compatibility for multiplication.
This behavior depends on the orientation (row or column) of the vectors before multiplication.

Vector declaration:
```javascript
// Valid vector initialization
Let v1 be [1, 1] as matrix

Let v2 be [1; 1] as matrix

```

Vector multiplication: 
```javascript
v1 * v1 
// -> [[2]]

v2 * v2
// -> [[1,1], [1,1]]

// Vector matrix multiplication
Let m1 be [
    1, 1;
    2 ,2
] as matrix

v1 * m1
// -> [[3, 3]]

m1 * v1
// -> [[2], [4]]
```

Vector addition:
```javascript
v1 + v1
// -> [[2,2]]
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
### Test programs:
### arrays.psu
Test program for  `Array Declaration`:
```javascript
Let arr1 be [1, 2, 3] as array
Let arr2 as array
Let arr3 be [2, 3, 4] as array

Print("\nArr1 declared with initizalation: " + arr1)
Print("\nArr2 declared without initizalation: " + arr2)

arr1[0] = 5
Print("\nArr1[0] assignment: " + arr1)

Print("\nArr1 + Arr3: " + (arr1 + arr3))

Print("\n-----------------------------------------------------------------------------\n\n")
```

### bubblesort.psu
Test program for  `Bubblesort`:
```javascript
Let arr be [5, 2, 9, 1, 5, 6] as array
Let n be 6 as number  # Length of the array

Print("\nBefore: " + arr)

Let i as number
Let j as number

For(i = 0; i < n; i = i + 1) {
    For(j = 0; j < n - i - 1; j = j + 1) {
        If (arr[j] > arr[j + 1]) {
            # Swap arr[j] and arr[j + 1]
            Let temp be arr[j] as number
            arr[j] = arr[j + 1]
            arr[j + 1] = temp
        }
    }
}

Print("\nAfter: " + arr)

Print("\n-----------------------------------------------------------------------------\n\n")

```

### functions.psu
Test program for  `Functions`:
```javascript
# Function that adds two numbers
Function Add(a, b) {
    Let result be a + b as number
    Return result
}

Let x be Add(5, 3) as number
Print("\nCalling Add(5, 3): " + x)

Print("\n-----------------------------------------------------------------------------\n\n")

```

### if-statements.psu
Test program for  `If_statements`:
```javascript
# Conditional logic
Let num be 7 as number
Print("\nDeclared num to be 7 as number\n")

If (num < 5) {
    Print("\nnum is less than 5")
} Else If (num == 5) {
    Print("\nnum is equal to 5")
} Else {
    Print("\nnum is greater than 5")
}

Print("\n-----------------------------------------------------------------------------\n\n")

```

### loops.psu
Test program for  `Loops`:
```javascript
# For loop that prints numbers 1 to 5
Print("\nFor loop from 1 to 5: ")

Let i as number
For(i = 1; i <= 5; i = i + 1) {
    Print(i)
}

# While loop that counts up to 5
Print("\nWhile loop counting to 5: ")

Let counter be 1 as number
While (counter <= 5) {
    Print(counter)
    counter = counter + 1
}

# Function that checks if number is even and prints numbers up to it
Function PrintIfEven(n) {
    If (n % 2 == 0) {
        Print("\nEven number. Printing numbers up to " + n)

        Let i as number
        For(i = 1; i <= n; i = i + 1) {
            Print(i)
        }
    } Else {
        Print("\nNot an even number.")
    }
}

Let testNum be 6 as number
PrintIfEven(testNum)

Print("\n-----------------------------------------------------------------------------\n\n")

```

### matrices.psu
Test program for  `Matrix operations`:
```javascript
Let c as 2x2 matrix
c = [ 
    1, 2;
    1, 2
    ]

Let b be [ 
    1, 2;
    1, 2
    ] as matrix

Print("\nMatrix decleration:\nC =\n" + c + ",\n\nb =\n" + b + "\n\n")

Print("\nMatrix addition C + B =\n" + (c + b) + "\n\n")

c[0, 1] = 3

Print("\nC[0, 1] = 3\n\n" + "C =\n" + c + "\n")

Print("\nMatrix multiplication C * B =\n" + (c * b))

Print("\n-----------------------------------------------------------------------------\n\n")

```
