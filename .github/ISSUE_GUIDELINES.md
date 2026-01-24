# 📌 Before Submitting an Issue (Type-Flat Project)

Thank you for using **type-flat**! To help us resolve issues quickly, please follow these guidelines.

## ✅ Required Information

1. **Minimal TypeScript type definition** (keep it simple—avoid complex business logic)
2. **Expected flattened result**
3. **Actual output from `type-flat`**  
   → You can see this by hovering over the type in VS Code or [TS Playground](https://www.typescriptlang.org/play)

## 🎁 Best Contribution: Add a Failing Test Case

You **do NOT need to fix the bug**. Just:

1. Open the appropriate test file in the `tests/` directory (e.g., `test_class.rs`, `test_interface.rs`)
2. Add a new `#[test]` function with your example
3. Submit a PR linking to this issue

✅ This failing test:

- Precisely reproduces the problem
- Prevents regressions
- Speeds up debugging dramatically

> 📂 Example test: see existing files in [`tests/`](../tree/main/tests)

## 🚫 We Will Close Issues That

- Lack a minimal type example (e.g., “It doesn’t work”)
- Use overly complex or nested types without simplification

---
✅ Ready? Go to the [Issues page](https://github.com/ngd-b/type-flat/issues) to create one.
