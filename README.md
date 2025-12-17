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
```

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
