# Roadmap

This roadmap outlines the development plan for `bel-expr`, focusing on its role as the expression evaluator for `hh200`, its compilation to WASM, and the creation of Python bindings.

## Phase 1: Python Bindings & WASM Integration (High Priority)

The immediate goal is to make the Haskell-based BEL evaluator accessible from Python via WebAssembly.

-   [ ] **WASM Reactor Support**:
    -   Ensure `bel-expr` can be compiled as a WASM Reactor module (library) instead of just a command module.
    -   Implement and export the necessary `hs_init` function and the main `eval` entry point suitable for FFI.
    -   Configure `.cabal` / `package.yaml` to support the reactor model flags (`-no-hs-main`, `-optl-mexec-model=reactor`).

-   [ ] **Python Package (`bel_expr`)**:
    -   Create a Python wrapper module (likely named `bel_expr`).
    -   Use a Python WASM runtime (e.g., `wasmtime` or `wasmer`) to load the generated `.wasm` file.
    -   Implement the `eval(expression, env)` interface.
    -   Handle data serialization between Python (JSON/Dict) and Haskell (JSON/Aeson) across the WASM boundary.

-   [ ] **Verification**:
    -   Create a "Hello World" test case where a Python script loads the module and evaluates `1 + 1`.

## Phase 2: Feature Completeness (BEL Language)

Expand the BEL language capabilities to meet the needs of `hh200`.

-   [ ] **Function Library**:
    -   Review required functions for `hh200`.
    -   Expand string manipulation capabilities.
    -   Add date/time manipulation if needed beyond `today()`, `year()`, `dayOfMonth()`.

-   [ ] **Error Handling**:
    -   Improve `eval` return types to provide structured error messages instead of just returning the input string on failure.
    -   Implement better parse error reporting.

## Phase 3: Testing & CI

Ensure stability and reproducibility.

-   [ ] **Cross-Language Testing**:
    -   Add integration tests where Python tests drive the Haskell WASM module.
    -   Verify consistent behavior between the Haskell `test/Spec.hs` and Python usage.

-   [ ] **WASM Reproducibility**:
    -   Ensure the build process documented in `BUILDING_WASM.md` is robust and repeatable in CI environments.

## Phase 4: Documentation

-   [ ] **Language Reference**: Document the BEL syntax, operators, and built-in functions.
-   [ ] **Integration Guide**: Update `README.md` with instructions for Python developers to install and use the bindings.
