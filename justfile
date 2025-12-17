# Run the CLI with a KDL file (defaults to data/austin.kdl)
run kdl_file="data/austin.kdl":
    dotnet run --project KDLFSharp.CLI/KDLFSharp.CLI.fsproj {{kdl_file}}

# Run tests with optional filter (default: all tests)
test filter="":
    #!/usr/bin/env bash
    if [ -z "{{filter}}" ]; then
        dotnet run --project KDLFSharp.Tests/KDLFSharp.Tests.fsproj
    else
        dotnet run --project KDLFSharp.Tests/KDLFSharp.Tests.fsproj -- --filter "{{filter}}"
    fi
