# tipi-lang

![Project Icon](icon.svg)
Tipi/teepee means: 'the place where they live' in Sioux/Dakota. 
see https://sesquiotic.com/2013/02/23/teepee/

Borrowing from that: 'the place where http lives'.

## Why?
1. Existing languages are just fine, but building web services is >always< bolted on, instead of supported within the language/runtime.
2. Whereas every company needs an API these days. 
3. Is it just me? I always have trouble mapping urls the code that handles them.
4. There is no language (AFAIK) that supports layering. (controllers, services, database access, etc). This pattern is ubiquitous (at least where I live). 
5. ORM's are crappy. Mapping from sql rows to objects is a pain. This should be easy.
6. Json is ubiquitous. Convention over configuration: A controller returns json by default.
7. Yes, you can automatically serve json from postgres or whatever, but that is not the point. We want to build services.
   
## Now what?
- An experimental language for web api's.
- Enterprise as a first-class citizen
  - built-in types for dates and uuid for example
  - collection literals
  - ease of use for CRUD operations, like automatic mapping from sql rows to json
- Urls are made up of directories. 
- A controller sourcefile is a file named web.tp
- likewise:
    - service.tp for services
    - db.tp database access code 
    - util.tp utilities
- it is not mandatory to have services. If you want, you can put all your logic in a controller.
- and it can only access functions in its own subtree. Generic code should be put higher up in the tree.
- Therefore, services cannot call other services, because that is the recipe for spaghetti. Refactor your logic, abstract and put lower level code in utilities.
- openapi support

### An compiler/runtime written in Rust.
I cherry picked things I like, mostly from rust and python. 
  - strictly typed
  - [] is a list
  - {} is a map
  - objects, not inheritance
  - everything is an expression
  - nice iterators.
  - First-class functions? Maybe...
  - automatic mapping from database to object to json
  - indenting like python
- It's not written in stone. Things may change. 

**Numeric Types**
  * u32, u64 (also in hex: 0x...)
  * i32, i64 signed 
  * f32, f64 (including scientific notation)

**And also**
  * string: "hello world"
  * uuid , 
  * bool: true, false
  * char '. '
  * object: {field: value}. An object is a map with fixed keys that are strings.
  * enum 
  * date

**Collections**
  * list: \[e1, e2, e3, ...]
  * map: {key: value, key2: value2, ...}


## open questions
- pluggability for middleware?, implement later?
- JWT tokens, I guess

## the example in /source:
- a very simple api that returns "hello world"
  - but it demonstrates the basic concepts
- it starts an axum server
- go to http://localhost:3000/hello
- goal: it listens to GET /api/customers{:id} and returns a customer from the database

## Design
* heavily inspired by Crafting Interpreters. 
* compiler first creates an AST and then compiles to bytecode (no file format yet)
* uses a stack-based virtual machine

## Current status: toddler stage
* compiler and runtime are limited but working
  * next big thing: control flow: branch jumps and loops
* built on a solid foundation: [axum](https://github.com/tokio-rs/axum)
* supports:
  * basic types:
    * 32/64 bit integers, signed and unsigned
    * 32/64 bit floats
    * strings, bools, chars
    * lists and maps (as literals)
    * still todo: dates, uuids, enums, objects
  * type checking and type inference
  * arithmetic expressions (all you'd expect including bitwise ops)
  * function declaration and calling
  * indenting like python (for now just 1 level, but both tabs or double spaces)
  * strict typing like in rust (no implicit numeric conversions)
  * basic set of operators, including logical and/or and bitwise operations
* automatic injection of uri, query parameters and headers
  * if you declare them they will be available in the function body. 
For example:
```html
fn get(path: string, headers: map, query: map) -> string:
    "hello" + path
```
* includes a rudimentary REPL
  * ```cargo run -- --repl```) 
  * list functions and functions that serve endpoints
  * planned: 
    * edit source files
    * test endpoints
* basic http support (GET, POST, PUT, DELETE)
* watch daemon that recompiles on file changes
  * ```cargo run -- --watch``` 
  
## What's next?
* guards: this will be the way to deal with input
```
fn get() -> [Customer] | Customer? | ():
    | / -> service.get_all()
    | /{uuid} -> service.get(uuid)?
    | ?{query.firstname} -> service.get_by_firstname(fname)?
    | ?{query.last_name} -> service.get_by_lastname(lname)?
    | _ -> 404
```
* this may also require ADT's...
* object/struct types: Work in Progress
* control flow
* test support

## What about performance?
* Clueless really! We'll see.
* But it is written in rust
* And it has no GC
* So, maybe it will compete with python?

## A quick taste
**variables**
```
let a = 42
```
* declares a variable of type i64 (signed 64 bit integer)

or explictly as u32 (unsigned 32 bit integer)
```
let a:u32 = 42
```

* All variables are mutable right now. Have not come to a decision yet about mutable vs immutable variables.
* You must declare a variable before using it. Block scoping.
* There is no ```null```.
There is ```void``` though.
* You must initialize a variable when declaring it.

**strings**
```
let b:string = "hello "
```
Strings support concatening with +
```
let c = b + "world"
```

**dates and time**

Create a date with a literal:
```
let d:date = d"1979-12-16 16:12:19.000 +01:00"
```

more date functions will have to follow.

**lists**
```
let list = ["foo", "bar", 1, 1.0]
list[1]
=> "bar"
```
No generic types (yet). A list can hold any type.
* lists support appending with + 
```
let list2 = list + "baz"
=>["foo", "bar", 1, 1.0, "baz"]
```
_note to self: implement adding 2 lists_

**functions**
```
fn add(a:i64, b:i64) -> i64:
    a + b
```
* Everything is an expression. 
* The result of the last expression is returned.
* There are no semicolons. End-of-line chars serve as delimiters.
* Having multiple expressions on one line is not allowed.
* indenting determines a block and therefore the scope.
* The return type declaration is optional. If not specified, it is void.

**function calling**
```
let sum = add(1,2)
```

**for loops**
```
for i in [1,2,3]:
    print(i)
```

**if / else expressions **
```
let a = if true: 
  42
else: 
  0
print(a)
```
=>42

**An actual controller**
```
fn get() -> string:
    add("hello", "world")

fn add(a: string, b: string) -> string:
    a + " " + b
```
* get() is the entry point for http GET method calls, likewise for POST, PUT, DELETE, etc.

ISSUES
* Make everything an expression. If is a statement and so it can not be type checked
* improve indenting