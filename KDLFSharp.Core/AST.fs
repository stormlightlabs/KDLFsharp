namespace KDLFSharp.Core

/// Optional type annotation for a node or value, e.g. (foo)
type TypeAnnotation = string

/// Represents valid identifier/string forms per KDL spec:
/// - Bare identifiers
/// - Quoted strings
/// - Multiline or raw strings
type StringLike =
    | Identifier of string
    | Quoted of string
    | Multiline of string
    | Raw of string

/// Represents a property on a node, e.g. key=value.
type Property = { Key : string; Value : Value }

/// Represents a node in the KDL document.
/// A node may have:
///   - an optional type annotation
///   - a name
///   - ordered arguments
///   - unordered properties
///   - optional children
///
/// TODO: Add optional Span or SourceRange for error reporting
/// TODO: Possibly track original text or formatting for pretty-printing fidelity
and Node =
    { TypeAnn : TypeAnnotation option
      Name : string
      Arguments : Value list
      Properties : Property list
      Children : Node list }

/// Represents possible literal values.
/// Numeric literals are stored as text initially to preserve underscores and precision.
and Value =
    | String of string * TypeAnnotation option
    | Number of string * TypeAnnotation option
    | Boolean of bool
    | Null
    | NodeValue of Node list * TypeAnnotation option // for future expansion (child blocks as values)

    override this.ToString() =
        match this with
        | String(s, _) -> $"\"%s{s}\""
        | Number(n, _) -> n
        | Boolean b -> if b then "#true" else "#false"
        | Null -> "#null"
        | NodeValue _ -> "{…}"



/// Represents a KDL document, consisting of one or more nodes.
/// See §3 of the KDL 2.0 spec for structure details.
type Document = Node list

type Span =
    { StartLine : int
      StartCol : int
      EndLine : int
      EndCol : int }

/// Errors that may occur while parsing or validating AST nodes.
/// 
/// TODO: Add richer metadata: source file, span, context message, etc.
type AstError =
    | UnexpectedValue of string
    | DuplicateProperty of string
    | UnterminatedString of string
    | InvalidTypeAnnotation of string
    | UnexpectedEndOfInput
    | Other of string

/// A helper module for constructing nodes and values ergonomically.
[<RequireQualifiedAccess>]
module Builder =
    let str s = String(s, None)
    let typedStr ty s = String(s, Some ty)
    let num s = Number(s, None)
    let bool b = Boolean b
    let nullv = Null

    let prop key value = { Key = key; Value = value }

    let node name args props children =
        { TypeAnn = None
          Name = name
          Arguments = args
          Properties = props
          Children = children }

    let typedNode ty name args props children =
        { TypeAnn = Some ty
          Name = name
          Arguments = args
          Properties = props
          Children = children }
