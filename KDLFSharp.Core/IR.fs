namespace KDLFSharp.Core

/// Intermediate Representation (IR) for format conversion and testing.
/// This IR provides a canonical format for round-trip conversion testing between JSON/YAML/XML.
/// It is simpler than the full KDL AST and focuses on semantic content rather than parsing details.
module IR =

    /// Value kind for the IR
    type ValueKind =
        | String
        | Number
        | Bool
        | Null
        /// For #inf, #-inf, #nan
        | Keyword

    /// IR Value representation
    type IRValue =
        {
            /// KDL type annotation (t)
            TypeAnnotation: string option
            /// k
            Kind: ValueKind
            /// v (actual value; for Keyword use string: "#inf", "#-inf", "#nan")
            Value: obj
        }

    /// IR Node representation
    type IRNode =
        { Name: string
          Type: string option
          Arguments: IRValue list
          Properties: Map<string, IRValue>
          Children: IRNode list }

    /// IR Document = array of nodes
    type IRDocument = IRNode list

    /// Helper functions for building IR values
    let stringValue v =
        { TypeAnnotation = None
          Kind = String
          Value = v }

    let typedStringValue t v =
        { TypeAnnotation = Some t
          Kind = String
          Value = v }

    let numberValue (v: float) =
        { TypeAnnotation = None
          Kind = Number
          Value = v }

    let typedNumberValue t (v: float) =
        { TypeAnnotation = Some t
          Kind = Number
          Value = v }

    let boolValue v =
        { TypeAnnotation = None
          Kind = Bool
          Value = v }

    let nullValue =
        { TypeAnnotation = None
          Kind = Null
          Value = null }

    let keywordValue v =
        { TypeAnnotation = None
          Kind = Keyword
          Value = v }

    /// Helper for building nodes
    let node name typeAnn args props children =
        { Name = name
          Type = typeAnn
          Arguments = args
          Properties = props
          Children = children }
