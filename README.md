# Aegle 🦅

A prototype type-based library search tool for Agda.

## What it does

Aegle lets you search for Agda definitions by type.

The search is not limited to syntactic equality:

- Match up to basic type isomorphisms, so we can find definitions whose types are essentially the same as the query but written in a different shape. Aegle supports permutations of arguments and pair components, pair associativity, and currying, specifically.

   <details>
     <summary>Supported type isomorphisms, formally</summary>

  ```math
  \begin{align*}
  \Pi x : A. (\Pi y : B. C) &\cong \Pi y : B. (\Pi x : A. C) & \text{if } x \notin \mathrm{FV}(B) \land y \notin \mathrm{FV}(A) && (\Pi\text{-swap}) \\
  A \times B &\cong B \times A &&& (\Sigma\text{-swap}) \\
  \Sigma x : (\Sigma y : A. B). C &\cong \Sigma x: A. (\Sigma y: B[y\mapsto x]. C[x\mapsto (x, y)]) &&& (\Sigma\text{-assoc}) \\
  \Pi x : (\Sigma y : A. B). C &\cong \Pi x: A. (\Pi y: B[y\mapsto x]. C[x\mapsto (x, y)]) &&& (\text{curry}) \\
  \end{align*}
  ```

   </details>

- Match up to generalisation, so we can find definitions that fit the query type after an appropriate instantiation.

- Expand type aliases, so queries do not have to use the same aliases as the definitions they match.

Moreover, Aegle synthesises code that makes a matched definition fit the query type.

For example, consider the query type `(A B : U) → (A → B) → A → B`. Aegle finds `Function.Base._$′_`, whose type exactly matches the query, and also:

```agda
Function.Base._|>_ : (A : U) (B : A → U) (x : A) → ((x : A) → B x) → B x
```

with the following synthesised code:

```agda
(λ A B x y. _|>_ A (λ z. B) y x) : (A B : U) → (A → B) → A → B
```

## Usage

### Preparation

1. Start PostgreSQL.

   ```sh
   nix run .#service
   ```

   Keep this process running while using Aegle.

2. In another shell, set the connection settings.

   ```sh
   export DATABASE_URL="postgresql://$USER@127.0.0.1:5432/aegle"
   ```

   The web server also needs a port:

   ```sh
   export PORT=8080
   ```

3. Generate Agda HTML. Aegle serves generated Agda HTML files, but it does not generate them by itself.
   For the bundled agda-stdlib, generate `Everything.agda`, then generate HTML for it.

   ```sh
   cd vendor/agda-stdlib
   make Everything.agda
   cd doc
   agda --html Everything.agda
   cd ../../..
   ```

### Build

```sh
stack build
```

### Index a library

Indexing reads an Agda library and stores searchable definitions in PostgreSQL.
The second argument is a JSON file listing definitions that Aegle should treat as type aliases.

```sh
stack exec aegle -- index vendor/agda-stdlib data/transparent_defs.json
```

### Search from the CLI

After indexing, pass a query type to `search`:

```sh
stack exec aegle -- search '(A B : U) → (A → B) → A → B'
```

For repeated queries, use the interactive shell:

```sh
stack exec aegle -- interactive
```

### Serve the web UI

Run the web server with the same directory where Agda placed the generated HTML files.

```sh
stack exec aegle -- serve --html-dir vendor/agda-stdlib/doc/html
```

Then open <http://localhost:8080> in your browser.

## Query Syntax

Queries use an Agda-like syntax with a few restrictions:

- Use `U` instead of `Set` or `Type`.
- Write all arguments explicitly; implicit arguments are not supported.
- Use prefix names instead of operators.
- Give a domain type for every pi binder.

Example: `Commutative Nat (_≡_ Nat) _+_`

## Acknowledgements

- Aegle's core calculus is based in part on [elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo).
- The translation logic from Agda terms to Aegle terms is based on [agda2hs](https://github.com/agda/agda2hs).

## License

BSD-3-Clause. See [LICENSE](LICENSE).
