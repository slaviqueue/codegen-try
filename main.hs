import Data.List

data Node
  = Program [Node]
  | Sum Node Node
  | Sub Node Node
  | FnCall Node Node
  | Args [Node]
  | VarDecl String Node
  | FnDecl String Node Node
  | Params [String]
  | Body [Node]
  | Return Node
  | Number Integer
  | Id String

toString :: Node -> String
toString (Program body) = intercalate "\n" $ map toString body
toString (Sum left right) = unwords [toString left, "+" , toString right]
toString (Number integer) = show integer
toString (Id identifier) = identifier
toString (FnCall ident args) = unwords [toString ident, "(", toString args, ")"]
toString (Args args) = intercalate ", " $ map toString args
toString (Params params) = intercalate ", " params
toString (Body body) = intercalate "\n " $ map toString body
toString (VarDecl identifier value) = unwords ["var", identifier, "=", toString value]
toString (FnDecl identifier params body)
  = unwords [
    "function", identifier, "(", toString params, ")", "{\n", toString body, "\n}"]
toString (Return value) = "return " ++ toString value

ast =
  Program [
    VarDecl "PI" (Number 3),
    FnDecl
      ("main")
      (Params ["test", "hello"])
      (Body [
        VarDecl "test_value" $ Sum (Number 2) (Number 2),
        VarDecl "result_value" $ Sum (Number 3) (Number 4),
        Return $ Id "result_value"
      ])
  ]

main = putStrLn $ toString $ ast
