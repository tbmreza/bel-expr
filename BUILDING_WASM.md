# Building WebAssembly (WASM) with Haskell Stack

This project is configured as a Haskell Stack project. To compile it to WebAssembly, we use the GHC WASM backend provided by [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta).

## Prerequisites

You need to install the WASM toolchain. `ghc-wasm-meta` provides an easy way to install a pre-built toolchain that includes `wasm32-wasi-ghc` and `wasm32-wasi-cabal`.

**Without Nix:**

```sh
curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | sh
source ~/.ghc-wasm/env
```

This installs the toolchain to `~/.ghc-wasm` and adds the necessary executables to your path.

## Building the Project

While this project is managed by Stack (using `package.yaml`), the recommended way to build for WASM is using the `wasm32-wasi-cabal` wrapper, which consumes the `.cabal` file generated from `package.yaml`.

1.  **Generate the `.cabal` file**:
    If you haven't already, generate `bel-expr.cabal` from `package.yaml`. Stack usually does this automatically, but you can also use `hpack`:
    ```sh
    hpack
    ```

2.  **Build using `wasm32-wasi-cabal`**:
    Run the following command to build the project for the `wasm32-wasi` target:
    ```sh
    wasm32-wasi-cabal build
    ```

    This will compile the project and its dependencies. The output `.wasm` file (typically for the executable) can be found in `dist-newstyle/` (e.g., `dist-newstyle/build/wasm32-wasi/.../bel-expr-exe.wasm`).

### Exporting Functions (Reactor Module)

If you want to create a WASM module that exports specific functions to be called by a host (like Rust or JavaScript) instead of running as a standalone command (Command Module), you need to:

1.  Use `foreign export ccall` in your Haskell code.
2.  Compile with `-no-hs-main` and `-optl-mexec-model=reactor`.

**Example Code (`src/MyExports.hs`):**

```haskell
module MyExports where

foreign export ccall my_function :: Int -> Int

my_function :: Int -> Int
my_function x = x * 2
```

**Build Configuration:**

You can pass the necessary flags via `cabal`:

```sh
wasm32-wasi-ghc src/MyExports.hs -o my_exports.wasm \
    -no-hs-main \
    -optl-mexec-model=reactor \
    -optl-Wl,--export=hs_init,--export=my_function
```

Or configure them in your `package.yaml` for a specific executable/library component.

## Consuming the WASM Module (Cranelift/Wasmtime Example)

To consume the generated `.wasm` module, you typically use a WASM runtime. [Wasmtime](https://wasmtime.dev/) is a popular runtime that uses **Cranelift** for code generation.

Here is an example of how to load and run a Haskell-generated WASM module in a Rust project using the `wasmtime` crate.

**Cargo.toml:**

```toml
[dependencies]
wasmtime = "16.0.0" # Use a compatible version
wasi-common = "16.0.0"
```

**Rust Code (`src/main.rs`):**

```rust
use wasmtime::{Engine, Linker, Module, Store, Config};
use wasi_common::sync::WasiCtxBuilder;

fn main() -> anyhow::Result<()> {
    // 1. Configure the engine
    let mut config = Config::new();
    config.wasm_component_model(false); // Haskell GHC WASM currently targets core WASM/WASI preview1
    let engine = Engine::new(&config)?;

    // 2. Load the module
    let module = Module::from_file(&engine, "path/to/your-output.wasm")?;

    // 3. Set up WASI context (required for GHC-compiled modules)
    let wasi = WasiCtxBuilder::new()
        .inherit_stdio()
        .inherit_args()?
        .build();

    let mut store = Store::new(&engine, wasi);
    let mut linker = Linker::new(&engine);
    wasi_common::sync::add_to_linker(&mut linker, |s| s)?;

    // 4. Instantiate the module
    let instance = linker.instantiate(&mut store, &module)?;

    // 5. Initialize the Haskell Runtime
    // For Reactor modules (libraries), you MUST call hs_init first.
    // hs_init(argc, argv) -> we pass 0, 0
    let hs_init = instance.get_typed_func::<(i32, i32), ()>(&mut store, "hs_init")?;
    hs_init.call(&mut store, (0, 0))?;

    // 6. Call your exported function
    // Assuming 'my_function' was exported
    let my_function = instance.get_typed_func::<i32, i32>(&mut store, "my_function")?;
    let result = my_function.call(&mut store, 42)?;

    println!("Result from Haskell: {}", result);

    Ok(())
}
```

### Notes on Consuming

*   **hs_init**: When using the Reactor model (library), you must export and call `hs_init` before any other function.
*   **WASI**: GHC WASM output depends on WASI (WebAssembly System Interface). Your runtime must provide WASI implementation (as shown with `wasi-common` above).
