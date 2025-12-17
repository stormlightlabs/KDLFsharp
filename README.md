<!-- markdownlint-disable MD033 -->
# KDL F\#

![Project Banner](./data/assets/banner.png)

KDL F# is a from-scratch, implementation of the [KDL 2.0](https://kdl.dev) document language written in pure F#.

It ships:

- `KDLFSharp.Core` - reusable lexer/parser/AST utilities with strong typing for strings,
  numbers, booleans, nulls, type annotations, and child blocks.
- `KDLFSharp.CLI` - a colored cli app that tokenizes + parses `.kdl` files, then prints
  the AST as a tree (with node/prop counts, typed values, and severity-styled diagnostics on errors).

## CLI

The CLI exposes the lexer and parser from the library, rendering KDL documents as colored AST trees with node/property counts, typed values, and severity-styled diagnostics for errors.

### Quick Start

View a KDL document as an AST tree

```sh
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj data/zellij.kdl
```

<details>
<summary>
Example Output
</summary>

![Output screenshot](./data/assets/screenshot.png)

</details>

### Commands

<details>
<summary>
Parse KDL files and display the AST structure:
</summary>

```sh
# Parse from file
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj data/sample-park.kdl

# Parse from stdin
cat data/sample-park.kdl | dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- -
```

</details>

<details>
<summary>
Convert KDL documents to JSON
</summary>

```sh
# Structured output (default) - AST with explicit types
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- kdl-to-json data/sample-park.kdl

# Save to file
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- kdl-to-json data/sample-park.kdl --out output.json

# Debug mode - canonical IR for lossless round-trips
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- kdl-to-json data/sample-park.kdl --debug

# Sample mode - human-friendly format (lossy)
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- kdl-to-json data/sample-park.kdl --sample --lossy
```

</details>

<details>
<summary>
Convert JSON back to KDL
</summary>

```sh
# From structured JSON
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- json-to-kdl data/sample-park.json

# From debug IR
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- json-to-kdl data/sample-park.ir.json --debug

# From sample format
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- json-to-kdl data/sample-park.sample.json --sample

# With colored output
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- json-to-kdl data/sample-park.json --pretty
```

</details>

<details>
<summary>
Convert KDL documents to XML
</summary>

```sh
# Structured output (default)
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- kdl-to-xml data/sample-library.kdl

# Debug mode
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- kdl-to-xml data/sample-library.kdl --debug

# Sample mode with custom namespace
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- kdl-to-xml data/sample-library.kdl --sample --lossy --sample-ns https://example.com/kdl-meta
```

</details>

<details>
<summary>
Convert XML back to KDL
</summary>

```sh
# From structured XML
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- xml-to-kdl data/sample-library.xml

# From debug IR
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- xml-to-kdl data/sample-library.ir.xml --debug

# From sample format
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- xml-to-kdl data/sample-library.sample.xml --sample
```

</details>

<details>
<summary>
Conversion Output Modes
</summary>

### Structured (`default`)

AST representation with explicit node types, value kinds, and type annotations.
Suitable for programmatic access and preserves the full KDL structure.

### Debug (`--debug`)

Canonical intermediate representation (IR) defined in `KDLFSharp.Core.JSON` and `KDLFSharp.Core.XML`.
Enables lossless round-trip conversions with complete fidelity.

### Sample (`--sample` `--lossy`)

Human-friendly format where properties and children are surfaced as plain fields with metadata in `_meta` blocks.
Lossy by design and optimized for readability over round-trip accuracy. Requires explicit `--lossy` acknowledgement.

</details>

### Options

```sh
# See help for an exhaustive list of options
dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj -- help
```

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
