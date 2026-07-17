# Semantics of bel-expr: Incremental Type-Checking & Effects System

This document outlines the semantic model, design philosophy, and an incremental roadmap for implementing static/gradual type-checking and a light-weight effects system for `bel-expr`.

As `bel-expr` serves as the expression evaluation engine for `hh200` (a high-performance, developer-focused HTTP Client/Runner), its primary goals are **developer ergonomics, safety, and fast feedback**. Currently, the engine performs fully dynamic evaluation, falling back to `VNull` when operations fail (e.g., adding a string to a number). While this is resilient, it can lead to silent errors during request template rendering or assertion evaluation.

---

## Part 1: The Type System Design

To balance flexibility with correctness, we propose a **Gradual Type System** utilizing a bidirectional type-checking algorithm.

Gradual typing is ideal because `bel-expr` regularly interacts with dynamic JSON payloads (e.g., via `jsonpath` or dynamic HTTP headers) whose structure is not fully known statically.

### 1. Representation of Types

We define the types in a new Haskell AST representation:

```haskell
data Type
  = TNum                   -- Numeric type (Scientific)
  | TBool                  -- Boolean type
  | TString                -- String type
  | TNull                  -- Null type
  | TArray (Maybe Type)    -- Arrays (optionally parameterized, e.g., Array Num)
  | TObject                -- General JSON object map
  | TAny                   -- Dynamic type to bypass static checking
  deriving (Show, Eq)
```

- **`TAny` (The Escape Hatch)**: When a type cannot be statically resolved (e.g., `EJsonpath` querying a dynamic response), it is inferred as `TAny`.
- **Consistency ($ \sim $)**: Type equality is replaced with a consistency relation:
  - $T \sim T$ (Any type is consistent with itself)
  - $T \sim \text{TAny}$ and $\text{TAny} \sim T$ (Dynamic type is consistent with all types)

### 2. Bidirectional Type-Checking

Bidirectional typing splits type checking into two modes:
1. **Inference (`infer`)**: Given an expression, synthesize its type.
2. **Checking (`check`)**: Given an expression and an expected type, verify that the expression matches the type (by consistency).

```haskell
data TypeError
  = TypeMismatch Type Type Expr -- Expected, Got, Expr
  | UnboundIdentifier Text
  | InvalidOperatorApplication Text [Type]
  deriving (Show, Eq)

type TypeEnv = HM.HashMap Text Type
```

#### Primary Type Inference Rules:
* **Literal values**:
  - `VNum _` $\implies$ `TNum`
  - `VBool _` $\implies$ `TBool`
  - `VString _` $\implies$ `TString`
  - `VNull` $\implies$ `TNull`
  - `VObj _` $\implies$ `TObject`
  - `VArray _` $\implies$ `TArray Nothing`
* **Identifiers**:
  - Look up the identifier in `TypeEnv`. If not present, raise `UnboundIdentifier`.
* **Operators (e.g., `EAdd`, `ESub`, `EMul`, `EDiv`)**:
  - Infer types of both operands. Both must be consistent with `TNum`. Result is `TNum`.
* **Comparisons (`EEq`, `ENeq`)**:
  - Infer types of both operands. They must be consistent with each other. Result is `TBool`.
* **Inequalities (`ELte`, `EGte`)**:
  - Infer types of both operands. Both must be consistent with `TNum`. Result is `TBool`.
* **Unary Negation (`ENeg`)**:
  - Infer type of operand. It must be consistent with either `TBool` or `TNum`. The result matches the consistent type.
* **Special built-ins**:
  - `EJsonpath` $\implies$ `TAny` (dynamically queried, can be anything)
  - `EHeaderExists`, `EHeaderNotExists` $\implies$ `TBool` (header check results in boolean)
  - `EDebug e` $\implies$ Checks `e` under current env, returns `TBool` (or propagates `e`'s type)
  - `ECopy e` $\implies$ Infers type of `e` and returns it.

---

## Part 2: Incremental Implementation Roadmap

We will implement the type checker in three distinct, manageable phases. This ensures `bel-expr` remains functional at every step and allows us to test our assumptions.

### Phase 1: The Local Static Checker (MVP)
The goal is to reject obviously flawed expressions (e.g., `1 + "hello"`) before running them.

1. **Create `BEL.TypeChecker` Module**:
   - Implement `infer :: TypeEnv -> Expr -> Either TypeError Type`.
   - Implement `check :: TypeEnv -> Expr -> Type -> Either TypeError ()`.
2. **Define Base Environment**:
   - Provide types for built-in bindings in `dummy` (e.g., `BASE_URL : TString`, `MAX_RETRIES : TNum`).
3. **Integrate into Parser/Evaluation Pipeline**:
   - In `BEL.run`, type-check the AST before calling `eval`.
   - If type checking fails, return/throw the `TypeError` or log a warning depending on configuration.

### Phase 2: Lightweight Annotations & Scheme Schema
When dynamic values are queried, `TAny` can propagate and weaken the safety net. Phase 2 introduces static safety to dynamic targets.

1. **Inline Type Casts / Assertions (Optional Syntax)**:
   - Allow users to write `jsonpath "$.user.id" : Num` or wrap it with an assertion.
2. **Schema-driven Environment**:
   - Allow `hh200` to feed the expected type of response bodies and headers to `TypeEnv` (e.g., from an OpenAPI spec or a local test config schema).
   - This turns `EJsonpath` from always returning `TAny` to returning concrete types where schema information is available.

### Phase 3: Monadic Context & Warning Framework
Make the type checker non-blocking and deeply integrated into `hh200`.

1. **Soft Type Checking**:
   - In test assertion suites, type-check all expressions statically.
   - For interactive environments, display validation warnings in the UI/terminal inline, without blocking execution unless strict-mode is enabled.

---

## Part 3: The Effects System

Expression languages are most predictable when they are pure. However, `bel-expr` contains features that perform side-effects:
- **`ECopy`**: Modifies system state by writing to the clipboard.
- **`EDebug`**: Performs I/O by outputting to stdout/stderr.
- **`EJsonpath` / `EHeaderExists`**: Reads state from the execution environment (HTTP client responses).

To manage this, we plan a lightweight, static **Effects System** designed to track and constrain these operations.

### 1. Effect Definition

We define a set of primitive effects:

```haskell
data Effect
  = ReadEnv         -- Accesses environment variables or HTTP response body/headers
  | ClipboardWrite  -- Alters system clipboard (ECopy)
  | ConsoleWrite    -- Logs to stdout/terminal (EDebug)
  deriving (Show, Eq, Ord)

type EffectSet = Set.Set Effect
```

### 2. Effect Tracking Rules

We track effects bottom-up through the AST using an effect synthesis pass:

$$\text{synthesizeEffects} :: \text{Expr} \to \text{EffectSet}$$

- Literals, Identifiers, and basic math/comparison operators are **Pure** ($\emptyset$).
- `EJsonpath`, `EHeaderExists`, `EHeaderNotExists` require `ReadEnv` $\implies \{ \text{ReadEnv} \}$.
- `ECopy e` $\implies \text{synthesizeEffects}(e) \cup \{ \text{ClipboardWrite} \}$.
- `EDebug e` $\implies \text{synthesizeEffects}(e) \cup \{ \text{ConsoleWrite} \}$.
- Inductive step: For any binary operator $OP(e_1, e_2)$, its effect set is $\text{synthesizeEffects}(e_1) \cup \text{synthesizeEffects}(e_2)$.

### 3. Practical Use Cases in hh200

By analyzing the `EffectSet` of an expression prior to evaluation, `hh200` can enforce strict sandboxing policies:

1. **Assertion Safety**:
   - Assertions in test runs should be **read-only and non-disruptive**.
   - **Policy**: `ClipboardWrite` is prohibited in test assertion blocks. If an expression contains `ClipboardWrite` inside an assertion block, it is rejected statically.
2. **Hermetic Mode**:
   - For environments requiring high isolation (e.g., CI pipelines or secure runner environments), we can enforce $\text{EffectSet} \subseteq \{ \text{ReadEnv} \}$ or even $\emptyset$.
3. **Execution Optimizations**:
   - Expressions with $\emptyset$ (fully pure) can be safely memoized or evaluated once, optimizing performance in tight test loops.

---

## Summary

By incrementally introducing gradual type-checking and static effect tracking, we ensure that `bel-expr` grows from a simple dynamic interpreter to a robust, self-documenting expression engine. This maintains a low-barrier-to-entry MVP while safeguarding the developer's execution pipeline against bugs before a single network request is dispatched.
