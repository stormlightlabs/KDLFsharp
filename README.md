# KDL F\#

![Project Banner](./data/assets/banner.png)

KDL F# is a from-scratch, implementation of the [KDL 2.0](https://kdl.dev) document language written in pure F#.

It ships:

- `KDLFSharp.Core` - reusable lexer/parser/AST utilities with strong typing for strings,
  numbers, booleans, nulls, type annotations, and child blocks.
- `KDLFSharp.CLI` - a colored cli app that tokenizes + parses `.kdl` files, then prints
  the AST as a tree (with node/prop counts, typed values, and severity-styled diagnostics on errors).

## CLI

The CLI uses exposes the lexer + parser output from the library and surfaced errors
include token index + friendly messages, inspired by Expecto.

### Example

```sh
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj data/zellij.kdl

By default `kdl-to-json` and `kdl-to-xml` emit a structured representation of the AST (nodes + values with explicit kinds/type annotations).
Pass `--debug` to work with the canonical IR schema defined in `KDLFSharp.Core.JSON` / `XML`, or `--sample --lossy` to mirror the friendly data samples (properties/children surfaced as plain fields with metadata tucked into `_meta` blocks).
The sample XML metadata namespace defaults to the GitHub repo but can be overridden with `--sample-ns`.

<!-- markdownlint-disable MD033 -->
<details>
<summary>
Example Output
</summary>

![Output screenshot](./data/assets/screenshot.png)

</details>

## Local Development

Prereqs: `.NET 9` SDK

```sh
# Format/build everything
dotnet build

# Run Tests
dotnet run --project KDLFSharp.Tests/KDLFSharp.Tests.fsproj

# View AST
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj data/sample-park.kdl
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj data/zellij.kdl
```

## TODO

- [x] JSON <-> KDL
- [x] XML <-> KDL
- [ ] Record serialization to Map records to KDL and back
- [ ] Parse and manipulate KDL documents programmatically (Document Model)
- [ ] Query Language
- [ ] Schema Validation
