# bel-expr

## Python Bindings

The library provides Python bindings to the expression evaluator, allowing you to evaluate expressions within a Python environment.

### Basic Usage

```python
import bel_expr

# Define the environment with variables
env = {
    "foo": 10,
    "bar": 20
}

# Evaluate an expression
result = bel_expr.eval("foo + bar", env)
print(result)  # Output: 30.0

# Using functions
result = bel_expr.eval("today()", {})
print(result)
```

## Distribution & Ecosystem

The primary downstream consumer of `bel-expr` is [hh200](https://github.com/tbmreza/hh200).

In addition to serving `hh200`, we provide:
- **Python Bindings**: As shown above, for easy integration into Python projects.
- **WASM Module**: A standalone WebAssembly release (Reactor module) that can be embedded in any WASM-supporting environment (Rust, Node.js, etc.).

## See also

<details>
<summary>
CEL https://github.com/google/cel-spec
</summary>
An industrial expression language in production systems, the best
I could find on github at the time of writing.
CEL apparently backs evaluation of config programs in the cloud computing ecosystem.
The evaluator is based on Google RPC protobuf messages (hoping to save you a click).
</details>

## Contributing

This project primarily exists to serve and grows hand in hand with [hh200](https://github.com/tbmreza/hh200), nothing too ambitious for now!
