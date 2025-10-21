
# type-flat

[![npm version](https://img.shields.io/npm/v/type-flat?color=brightgreen)](https://www.npmjs.com/package/type-flat)
[![License: Apache](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)

**TypeScript Type Flattening Tool | Recursively parses complex types and generates nested JSON or type declarations.**

---

## 🧩 Introduction

`type-flat` is a TypeScript type flattening tool that recursively parses complex types, including generics, nested objects, and intersection types, and generates structured nested type definitions. It is suitable for:

- Generating type declaration files
- SDK type interfaces
- Automated documentation
- Build tool type analysis

---

## 🚀 Features

- ✅ Recursively parse types and preserve nested structure  
- 🧠 Automatically substitute generic parameters, supporting instances like `Response<User>`  
- ⚙️ Merge properties from extended types (extends)  
- 🌐 Support cross-file type references  
- 📘 Output as JSON or `.d.ts` type declarations  
- 🪶 Lightweight and dependency-free  

---

## 🛠️ Installation

```bash
npm install type-flat
# or
pnpm add type-flat
````

---

## 🧑‍💻 CLI Usage

```bash
npx type-flat <file> <typeName>
```

- `<file>`: Path to the TypeScript file (`.ts` or `.d.ts`)
- `<typeName>`: Type or interface name to flatten

**Example**

```bash
npx type-flat -f example/types.ts -t ResponseOfUser
```

Output:

```json
{
  "code": "number",
  "message": "string",
  "data": {
    "id": "number",
    "name": "string",
    "profile": {
      "email": "string",
      "address": {
        "city": "string",
        "zip": "number"
      }
    }
  }
}
```

---

## 🧑‍💻 Programming Interface

```ts
import { flatten } from 'type-flat';
import Content from './types.d.ts'

const result = await flatten(Content, 'ResponseOfUser');
console.log(JSON.stringify(result, null, 2));
```

### Cross-file Generic Example

`types.ts`:

```ts
export interface Address { city: string; zip: number; }
export interface Profile { email: string; address: Address; }
export interface User { id: number; name: string; profile: Profile; }
export interface Response<T> { code: number; message: string; data: T; }
export type ResponseOfUser = Response<User>;
```

Usage:

```ts
import { flatten } from 'type-flat';

const res = await flatten(Content, 'ResponseOfUser');
console.log(res);
```

Output:

```json
{
  "code": "number",
  "message": "string",
  "data": {
    "id": "number",
    "name": "string",
    "profile": {
      "email": "string",
      "address": {
        "city": "string",
        "zip": "number"
      }
    }
  }
}
```

---

## 🧱 Use Cases

- Automatically generate type declarations before publishing npm packages
- Generate common type interfaces for SDKs or frontend projects
- Extract nested complex types for documentation or tooling pipelines

---

## 📦 Output Format

- Default JSON (nested objects represent type structure)
- Future support for generating `.d.ts` files

---

## 🪴 License

Apache-2.0 © 2025 [hboot](https://github.com/ngd-b)
