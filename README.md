
# ts-flat

[![npm version](https://img.shields.io/npm/v/ts-flat?color=brightgreen)](https://www.npmjs.com/package/ts-flat)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

**TypeScript Type Flattening Tool | Recursively parses complex types and generates nested JSON or type declarations.**

---

## ⚡ One-line Highlight
Lightweight, fast, and supports generic substitution, inheritance merging, and cross-file type resolution. Make your TypeScript types readable, exportable, and reusable.

---

## 🧩 Introduction
`ts-flat` is a TypeScript type flattening tool that recursively parses complex types, including generics, nested objects, and intersection types, and generates structured nested type definitions. It is suitable for:

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
npm install ts-flat
# or
pnpm add ts-flat
````

---

## 🧑‍💻 CLI Usage

```bash
npx ts-flat <file> <typeName>
```

* `<file>`: Path to the TypeScript file (`.ts` or `.d.ts`)
* `<typeName>`: Type or interface name to flatten

**Example**

```bash
npx ts-flat example/types.ts ResponseOfUser
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
import { flattenTypeFromFile } from 'ts-flat';

const result = await flattenTypeFromFile('./src/types.ts', 'ResponseOfUser');
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
import { flattenTypeFromFile } from 'ts-flat';

const res = await flattenTypeFromFile('types.ts', 'ResponseOfUser');
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

* Automatically generate type declarations before publishing npm packages
* Generate common type interfaces for SDKs or frontend projects
* Extract nested complex types for documentation or tooling pipelines

---

## 📦 Output Format

* Default JSON (nested objects represent type structure)
* Future support for generating `.d.ts` files

---

## 🪴 License

MIT © 2025

```
