# 🌀 Functional Programming using Racket

## Overview
This repository contains my implementations from the **PL Grad-Level** course at NYU Courant. The assignments involve implementing interpreters, parsers, type checkers, and working with object-oriented constructs in **Racket**. These assignments focus on different aspects of programming language theory and practical implementation. Briefly, they showcase fundamental concepts in **language design, evaluation, type checking, and functional programming**.

## Assignments Breakdown

### 1. **Assignment 1: Basic Functional Programming & Trees**
- 📜 Implemented recursive functions to manipulate strings, lists, and trees.
- 🔹 Key tasks:
  - 🔍 **Palindrome Detection**: Extract palindromic words from a string.
  - 🌳 **Tree Evaluation**: Evaluate arithmetic expressions represented as binary trees.
  - ✅ **Binary Search Tree Verification**: Validate whether a tree maintains the BST invariant.
  - 📏 **Monotone List Splitting**: Segment a list into strictly increasing or decreasing sublists.
  - 📥 **Pile Insertion**: Insert numbers into sorted sublists while maintaining order constraints.

### 2. **Assignment 2: Parsing & Evaluating Arithmetic Expressions**
- 📖 Implemented a **recursive descent parser** and **evaluator** for arithmetic expressions.
- 🔹 Supported operations:
  - ➕ Addition, ✖ Multiplication, ✅ Conditionals.
  - 🔄 Recursive list operations.
  - 📝 Variable bindings with `let` and `let*`.
  - 📂 Structured data using lists and recursion.

### 3. **Assignment 3: Stateful Operations & Transactions**
- 🔄 Extended a functional language with **mutable state**:
  - 📦 **Vectors**: Implemented operations for vector creation, indexing, mutation, and slicing.
  - 🔄 **Transactions**: Implemented transactional memory rollback.
  - 🗂 **Mutable Storage**: Implemented `box`, `unbox`, and `set-box!` operations.
  - 📜 **Multi-Statement Execution**: `begin` expressions.

### 4. **Assignment 4: Object-Oriented Programming in Racket**
- 🎭 Introduced **objects, methods, and delegation**.
- 🔹 Implemented an object system with:
  - 🏗 **Field Definitions**: Objects contain mutable fields.
  - 🔄 **Method Dispatch**: Objects support method calls with `msg`.
  - 🏛 **Inheritance via Delegation**: Objects delegate method calls to a parent.
  - 📌 **Field Access & Mutation**: `get-field` and `set-field!` operations.

### 5. **Assignment 5: Type Checking for a Functional Language**
- ✅ Developed a **type checker** for a statically-typed functional language.
- 🔹 Implemented:
  - 🔢 **Primitive Types**: `numT`, `boolT`, `voidT`.
  - 🏗 **Composite Types**: Lists, pairs, functions, and boxes.
  - 🎯 **Type Inference Rules**: Ensured correctness of `let`, `lambda`, and recursive functions.
  - ⚠ **Error Handling**: Properly raised exceptions for invalid type operations.

## Running the Code

### Prerequisites
- 🖥 Install **Racket** and **PLAIt-Typed** language package.
- 📂 Ensure files are placed in the same directory.

### Execution
Run any file using the following command:
```bash
racket <file>.rkt
```
To run provided test cases:
```bash
racket <file>-test.rkt
```

## Key Concepts Covered
- 📜 **Recursive Descent Parsing**
- 🌲 **Tree-Based Expression Evaluation**
- 🔄 **Mutable State & Transactional Memory**
- 🎭 **Object-Oriented Constructs in Racket**
- ✅ **Static Type Checking & Type Inference**

## Repository Structure
```
├── ps1.rkt            # 📜 Assignment 1: Functional Programming & Trees
├── ps2.rkt            # 📖 Assignment 2: Parsing & Evaluating Expressions
├── ps3.rkt            # 🔄 Assignment 3: Stateful Operations & Transactions
├── ps4.rkt            # 🎭 Assignment 4: Object-Oriented Programming
├── ps5.rkt            # ✅ Assignment 5: Type Checking
├── ps1-test.rkt       # 🧪 Test Cases for Assignment 1
├── ps2-test.rkt       # 🧪 Test Cases for Assignment 2
├── ps3-test.rkt       # 🧪 Test Cases for Assignment 3
├── ps4-test.rkt       # 🧪 Test Cases for Assignment 4
├── ps5-test.rkt       # 🧪 Test Cases for Assignment 5
└── README.md          # 📄 Project Documentation
```
