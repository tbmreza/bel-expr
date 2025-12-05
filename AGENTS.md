# AGENTS.md

This file contains instructions and context for AI agents working on the `bel-expr` repository.

## Project Context
- **Purpose**: `bel-expr` is a Haskell library that serves as the expression evaluator for the `hh200` project (https://github.com/tbmreza/hh200).
- **Goals**: Provide a standalone WASM module and Python bindings.
- **Methodology**: The user employs 'Readme Driven Development'. Check `README.md` or other documentation for interface definitions before implementation.

## Environment & Tooling
- **Constraint**: Standard Haskell tools (`hpack`, `stack`, `cabal`, `ghc`) are **not available** in the current environment.
- **Implication**: You must manually edit generated files like `bel-expr.cabal` when dependencies or module structures change. Do not attempt to run `hpack` or `stack` to regenerate it.

## Building WebAssembly
- **Toolchain**: Requires the `ghc-wasm-meta` toolchain.
- **Build Tool**: Use the `wasm32-wasi-cabal` wrapper, utilizing the `.cabal` file directly (not Stack).
- **WASM Library (Reactor)**: When building as a WASM library (Reactor module), ensure the following flags are used:
  - `-no-hs-main`
  - `-optl-mexec-model=reactor`

## File Management
- **`bel-expr.cabal`**: This file is tracked in version control and should not be ignored. Since `hpack` is unavailable, you must update this file manually to reflect any changes in `package.yaml`.

## Python Bindings
- **Module Name**: `bel_expr`
- **Interface**: The binding should expose an `eval(expression, env)` function.

## User Context
- **User Name**: Reza Handzalah.
