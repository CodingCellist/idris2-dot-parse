module Graphics.DOT.Interfaces

import Graphics.DOT.AST

import Data.Vect
import Data.List1
import Data.String

%default total


--------------------------------------------------------------------------------
-- INTERFACES FOR CONVERTING TO DOT
--------------------------------------------------------------------------------

||| The type `t` can be converted to a `CompassPoint`.
public export
interface DOTCompassPoint t where
  toCompassPoint : t -> CompassPoint

||| The type `t` can be converted to a `Keyword`.
public export
interface DOTKeyword t where
  toKeyword : t -> Keyword

||| The type `t` can be converted to a `DOTID`.
public export
interface DOTDOTID t where
  toDOTID : t -> DOTID

||| The type `t` can be converted to a `Port`.
public export
interface DOTPort t where
  toPort : t -> Port

||| The type `t` can be converted to a `NodeID`.
public export
interface DOTNodeID t where
  toNodeID : t -> NodeID

||| The type `t` can be converted to an `Assign`.
public export
interface DOTAssign t where
  toAssign : t -> Assign

||| The type `t` can be converted to an `EdgeOp`.
public export
interface DOTEdgeOp t where
  toEdgeOp : t -> EdgeOp

||| The type `t` can be converted to an `EdgeRHS`.
public export
interface DOTEdgeRHS t where
  toEdgeRHS : t -> EdgeRHS

||| The type `t` can be converted to a `Subgraph`.
public export
interface DOTSubgraph t where
  toSubgraph : t -> Subgraph

||| The type `t` can be converted to a `Stmt`.
public export
interface DOTStmt t where
  toStmt : t -> Stmt

||| The type `t` can be converted to a `Graph`.
public export
interface DOTGraph t where
  toGraph : t -> Graph


--------------------------------------------------------------------------------
-- GENERAL IMPLEMENTATIONS
--------------------------------------------------------------------------------

----------
-- SHOW --
----------

public export
Show CompassPoint where
  show N = "n"
  show NE = "ne"
  show E = "e"
  show SE = "se"
  show S = "s"
  show SW = "sw"
  show W = "w"
  show NW = "nw"
  show Center = "c"
  show Underscore = "_"

public export
Show Keyword where
  show StrictKW = "strict"
  show GraphKW = "graph"
  show DigraphKW = "digraph"
  show NodeKW = "node"
  show EdgeKW = "edge"
  show SubgraphKW = "subgraph"

public export
Show DOTID where
  show (StringID id_) = show id_    -- `show` puts " around id_
  show (NameID name) = name
  show (Numeral num) = num
  show (HTML htmlStr) = htmlStr

public export
Show Port where
  show (IDPort id_ Nothing) = ":" ++ show id_
  show (IDPort id_ (Just cpt)) = ":" ++ show id_ ++ ":" ++ show cpt
  show (PlainPort cpt) = ":" ++ show cpt

public export
Show NodeID where
  show (MkNodeID id_ Nothing) = show id_
  show (MkNodeID id_ (Just port)) = show id_ ++ show port

public export
Show Assign where
  show (MkAssign lhs rhs) = show lhs ++ "=" ++ show rhs

public export
Show EdgeOp where
  show Arrow = "->"
  show Dash  = "--"

mutual
  -- stmtLists should be shown as
  -- elem1; elem2; ...; elemN
  covering
  showStmtList : List Stmt -> String
  showStmtList stmtList = joinBy "; " (map show stmtList)

  -- attrLists should be shown as
  -- "[ elems1 ]"
  -- "[ elems1 ][ elems2 ]"
  -- etc...
  covering
  showAttrList : List (List Assign) -> String
  showAttrList = concatMap show

  public export
  covering
  Show EdgeRHS where
    show (MkEdgeRHS op (Left id_)) = show op ++ " " ++ show id_
    show (MkEdgeRHS op (Right subGr)) = show op ++ " " ++ show subGr

  public export
  covering
  Show Subgraph where
    show (MkSubgraph Nothing stmtList) =
      "{ " ++ showStmtList stmtList ++ " }"

    show (MkSubgraph (Just (kw, Nothing)) stmtList) =
      show kw ++ " { " ++ showStmtList stmtList ++ " }"

    show (MkSubgraph (Just (kw, (Just id_))) stmtList) =
      show kw ++ " " ++ show id_ ++ " { " ++ showStmtList stmtList ++ " }"

  public export
  covering
  Show Stmt where
    show (NodeStmt nodeID attrList) =
      show nodeID ++ " " ++ concatMap show attrList

    show (EdgeStmt (Left nodeID) rhs attrList) =
      show nodeID ++ " " ++ concatMap show rhs ++ " " ++ showAttrList attrList
    show (EdgeStmt (Right subGr) rhs attrList) =
      show subGr ++ " " ++ concatMap show rhs ++ " " ++ showAttrList attrList

    show (AttrStmt kw attrList) =
      show kw ++ " " ++ showAttrList attrList

    show (AssignStmt a) =
      show a

    show (SubgraphStmt subGr) =
      show subGr

public export
covering
Show Graph where
  show (MkGraph Nothing graphTy Nothing stmtList) =
    show graphTy ++ " {\n" ++ showStmtList stmtList ++ "\n}"

  show (MkGraph Nothing graphTy (Just id_) stmtList) =
    show graphTy ++ " " ++ show id_ ++ " {\n" ++ showStmtList stmtList ++ "\n}"

  show (MkGraph (Just strict) graphTy Nothing stmtList) =
    show strict ++ " " ++ show graphTy
    ++ " {\n"
    ++ showStmtList stmtList
    ++ "\n}"

  show (MkGraph (Just strict) graphTy (Just id_) stmtList) =
    show strict ++ " " ++ show graphTy ++ " " ++ show id_
    ++ " {\n"
    ++ showStmtList stmtList
    ++ "\n}"

