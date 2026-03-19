# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

### Rust (Core)
```bash
cargo build          # Build the Rust binary
cargo test           # Run all Rust tests
cargo test test_name # Run a specific test (e.g., cargo test test_union_generic_pick_omit)
cargo run -- -f <file> -t <type>  # Run CLI directly
```

### Node.js (Native Addon)
```bash
pnpm install         # Install dependencies
pnpm build           # Build Node.js package
pnpm build:napi      # Build native addon for current platform
pnpm test            # Run ava tests in test-web/
```

## Architecture Overview

This is a TypeScript type flattening tool written in Rust with Node.js bindings via napi. It takes TypeScript type definitions and "flattens" them by inlining all referenced types, resolving generics, and handling circular references.

### Core Flow
1. **Parse**: Use OXC parser to parse TypeScript into AST
2. **Semantic Analysis**: Build semantic information (scope, symbols, types)
3. **Graph Building**: Build dependency graph of type references (`src/graph/mod.rs`)
4. **Flatten**: Inline all type dependencies and resolve cycles (`src/flatten/mod.rs`)
5. **Codegen**: Generate flattened TypeScript output using OXC codegen

### Key Modules

- `src/flatten/mod.rs` - Main `Flatten` struct orchestrating the flattening process
- `src/flatten/declare.rs` - Handles merging multiple declarations of the same type
- `src/flatten/type_alias.rs` - Type alias flattening logic
- `src/flatten/interface.rs` - Interface flattening and inheritance
- `src/flatten/class.rs` - Class type handling
- `src/flatten/generic.rs` - Generic type parameter resolution
- `src/graph/mod.rs` - Dependency graph with cycle detection for circular type references
- `src/graph/utils.rs` - Type lookup utilities

### Circular Reference Handling
The tool detects circular type references via graph traversal. Self-referencing types (e.g., `type A = { self: A }`) and mutual recursion (e.g., `A` references `B`, `B` references `A`) are detected and handled by keeping the type name reference rather than infinite expansion.

### Dual Output
- **CLI Binary**: `type_flat` crate built from `src/main.rs` using clap
- **Node.js Addon**: Exported via `src/lib.rs` using napi for use as a JavaScript library