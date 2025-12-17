namespace KDLFSharp.Core

open System

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

type SourceSpan =
    { StartLine: int
      StartCol: int
      EndLine: int
      EndCol: int }

/// Represents a property on a node, e.g. key=value.
type Property = { Key: string; Value: Value }

/// Represents a node in the KDL document.
/// A node may have:
///   - an optional type annotation
///   - a name
///   - ordered arguments
///   - unordered properties
///   - optional children
and Node =
    { TypeAnn: TypeAnnotation option
      Name: string
      Arguments: Value list
      Properties: Property list
      Children: Node list
      Span: SourceSpan option
      OriginalText: string option }

and NumberLiteral = { Raw: string; Kind: NumberKind }

and NumberKind =
    | Decimal
    | Hexadecimal
    | Octal
    | Binary
    | Special of SpecialNumberKind

and SpecialNumberKind =
    | Infinity
    | NegativeInfinity
    | NotANumber

/// Represents possible literal values.
/// Numeric literals are stored as text initially to preserve underscores and precision.
and Value =
    | String of string * TypeAnnotation option
    | Number of NumberLiteral * TypeAnnotation option
    | Boolean of bool
    | Null
    | NodeValue of Node list * TypeAnnotation option // for future expansion (child blocks as values)

    override this.ToString() =
        match this with
        | String(s, _) -> $"\"%s{s}\""
        | Number(lit, _) -> lit.Raw
        | Boolean b -> if b then "#true" else "#false"
        | Null -> "#null"
        | NodeValue _ -> "{…}"

module NumberLiteral =
    let private stripSign (raw: string) =
        if raw.StartsWith "-" || raw.StartsWith "+" then
            raw.Substring 1
        else
            raw

    let private classifyUnsigned (raw: string) =
        if raw.StartsWith("0x", StringComparison.OrdinalIgnoreCase) then
            NumberKind.Hexadecimal
        elif raw.StartsWith("0o", StringComparison.OrdinalIgnoreCase) then
            NumberKind.Octal
        elif raw.StartsWith("0b", StringComparison.OrdinalIgnoreCase) then
            NumberKind.Binary
        else
            NumberKind.Decimal

    let ofRaw raw =
        let unsigned = stripSign raw

        { Raw = raw
          Kind = classifyUnsigned unsigned }

    let special raw specialKind =
        { Raw = raw
          Kind = NumberKind.Special specialKind }





/// Represents a KDL document, consisting of one or more nodes.
/// See §3 of the KDL 2.0 spec for structure details.
type Document = Node list

type AstErrorKind =
    | UnexpectedValue
    | DuplicateProperty
    | UnterminatedString
    | InvalidTypeAnnotation
    | UnexpectedEndOfInput
    | Other

/// Errors that may occur while parsing or validating AST nodes.
type AstError =
    { Kind: AstErrorKind
      Message: string
      Span: SourceSpan option
      SourceName: string option
      Context: string option }

[<RequireQualifiedAccess>]
module AstError =
    let private mk kind message =
        { Kind = kind
          Message = message
          Span = None
          SourceName = None
          Context = None }

    let unexpectedValue msg = mk AstErrorKind.UnexpectedValue msg
    let invalidTypeAnnotation msg = mk AstErrorKind.InvalidTypeAnnotation msg
    let duplicateProperty msg = mk AstErrorKind.DuplicateProperty msg
    let unterminatedString msg = mk AstErrorKind.UnterminatedString msg
    let unexpectedEof msg = mk AstErrorKind.UnexpectedEndOfInput msg
    let other msg = mk AstErrorKind.Other msg

    let withSpan (span: SourceSpan) (err: AstError) = { err with Span = Some span }
    let withSource (source: string) (err: AstError) = { err with SourceName = Some source }
    let withContext (context: string) (err: AstError) = { err with Context = Some context }

/// A helper module for constructing nodes and values ergonomically.
[<RequireQualifiedAccess>]
module Builder =
    let str s = String(s, None)
    let typedStr ty s = String(s, Some ty)
    let num s = Number(NumberLiteral.ofRaw s, None)
    let bool b = Boolean b
    let nullv = Null

    let prop key value = { Key = key; Value = value }

    let node name args props children =
        { TypeAnn = None
          Name = name
          Arguments = args
          Properties = props
          Children = children
          Span = None
          OriginalText = None }

    let typedNode ty name args props children =
        { TypeAnn = Some ty
          Name = name
          Arguments = args
          Properties = props
          Children = children
          Span = None
          OriginalText = None }
