---
name: 🐞 Incorrect Flattening Behavior
about: Report a case where type-flat produces unexpected output
title: '[Bug] Flatten should be ..., but got ...'
labels: bug, needs-repro-test
---

## 🧪 Minimal TypeScript Type Definition

```ts
// Example:
type Flatten = T extends Array ? Flatten : T;

declare class Container {
  getValue(): T;
  getFlattenedValue(): Flatten;
}
```

## ✅ Expected Flattened Result

```ts
// What you expect after flattening "Container"
declare class Container { getValue(): T; getFlattenedValue(): T; }
```

## ❌ Actual Output from type-flat

```ts
// What type-flat currently returns (e.g., unchanged or partially flattened)
declare class Container { getValue(): T; getFlattenedValue(): Flatten; }
```

## 💡 Willing to Add a Test?

Even if you can't fix it, you can help by adding a failing test in the correct file under tests/ (e.g., conditional_types.rs, classes.rs, etc.):

```rust
[test]
fn test_conditional_type_in_class_with_method_params_generic() {
    let result = run_flat(
        r#"
        type Flatten = T extends Array ? Flatten : T;

        declare class Container {
            getValue(): T;
            getFlattenedValue(): Flatten;
        }
        "#,
        "Container",
    );

    assert!(result.contains(
        "declare class Container { getValue(): T; getFlattenedValue(): T; }"
    ));
}
```

- [ ] I’d like to submit a PR with just a repro test
