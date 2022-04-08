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
export
interface DOTCompassPoint t where
  toCompassPoint : t -> CompassPoint

||| The type `t` can be converted to a `Keyword`.
export
interface DOTKeyword t where
  toKeyword : t -> Keyword

||| The type `t` can be converted to a `DOTID`.
export
interface DOTDOTID t where
  toDOTID : t -> DOTID

||| The type `t` can be converted to a `Port`.
export
interface DOTPort t where
  toPort : t -> Port

||| The type `t` can be converted to a `NodeID`.
export
interface DOTNodeID t where
  toNodeID : t -> NodeID

||| The type `t` can be converted to an `Assign`.
export
interface DOTAssign t where
  toAssign : t -> Assign

||| The type `t` can be converted to an `EdgeOp`.
export
interface DOTEdgeOp t where
  toEdgeOp : t -> EdgeOp

||| The type `t` can be converted to an `EdgeRHS`.
export
interface DOTEdgeRHS t where
  toEdgeRHS : t -> EdgeRHS

||| The type `t` can be converted to a `Subgraph`.
export
interface DOTSubgraph t where
  toSubgraph : t -> Subgraph

||| The type `t` can be converted to a `Stmt`.
export
interface DOTStmt t where
  toStmt : t -> Stmt

||| The type `t` can be converted to a `Graph`.
export
interface DOTGraph t where
  toGraph : t -> Graph


--------------------------------------------------------------------------------
-- GENERAL IMPLEMENTATIONS
--------------------------------------------------------------------------------

----------
-- SHOW --
----------

export
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

export
Show Keyword where
  show StrictKW = "strict"
  show GraphKW = "graph"
  show DigraphKW = "digraph"
  show NodeKW = "node"
  show EdgeKW = "edge"
  show SubgraphKW = "subgraph"

export
Show DOTID where
  show (StringID id_) = show id_    -- `show` puts " around id_
  show (NameID name) = name
  show (Numeral num) = num
  show (HTML htmlStr) = htmlStr

export
Show Port where
  show (IDPort id_ Nothing) = ":" ++ show id_
  show (IDPort id_ (Just cpt)) = ":" ++ show id_ ++ ":" ++ show cpt
  show (PlainPort cpt) = ":" ++ show cpt

export
Show NodeID where
  show (MkNodeID id_ Nothing) = show id_
  show (MkNodeID id_ (Just port)) = show id_ ++ show port

export
Show Assign where
  show (MkAssign lhs rhs) = show lhs ++ "=" ++ show rhs

export
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

  export
  covering
  Show EdgeRHS where
    show (MkEdgeRHS op (Left id_)) = show op ++ " " ++ show id_
    show (MkEdgeRHS op (Right subGr)) = show op ++ " " ++ show subGr

  export
  covering
  Show Subgraph where
    show (MkSubgraph Nothing stmtList) =
      "{ " ++ showStmtList stmtList ++ " }"

    show (MkSubgraph (Just (kw, Nothing)) stmtList) =
      show kw ++ " { " ++ showStmtList stmtList ++ " }"

    show (MkSubgraph (Just (kw, (Just id_))) stmtList) =
      show kw ++ " " ++ show id_ ++ " { " ++ showStmtList stmtList ++ " }"

  export
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

export
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

--------
-- EQ --
--------

export
Eq CompassPoint where
  (==) N  N  = True
  (==) NE NE = True
  (==) E  E  = True
  (==) SE SE = True
  (==) S  S  = True
  (==) SW SW = True
  (==) W  W  = True
  (==) NW NW = True
  (==) Center Center = True
  (==) Underscore Underscore = True
  (==) _  _  = False

export
Eq Keyword where
  (==) StrictKW   StrictKW   = True
  (==) GraphKW    GraphKW    = True
  (==) DigraphKW  DigraphKW  = True
  (==) NodeKW     NodeKW     = True
  (==) EdgeKW     EdgeKW     = True
  (==) SubgraphKW SubgraphKW = True
  (==) _          _          = False

export
Eq DOTID where
  (==) (StringID id1) (StringID id2) = id1 == id2
  (==) (NameID n1)    (NameID n2)    = n1 == n2
  (==) (Numeral num1) (Numeral num2) = num1 == num2
  (==) (HTML html1)   (HTML html2)   = html1 == html2
  (==) _              _              = False

export
Eq Port where
  (==) (IDPort id1 mCPT1) (IDPort id2 mCPT2) = mCPT1 == mCPT2 && id1 == id2 
  (==) (PlainPort cpt1)   (PlainPort cpt2)   = cpt1 == cpt2
  (==) _                  _                  = False

export
Eq NodeID where
  (==) (MkNodeID id1 mPort1) (MkNodeID id2 mPort2) = mPort1 == mPort2 && id1 == id2 

export
Eq Assign where
  (==) (MkAssign lhs1 rhs1) (MkAssign lhs2 rhs2) = lhs1 == lhs2 && rhs1 == rhs2

export
Eq EdgeOp where
  (==) Arrow Arrow = True
  (==) Dash  Dash  = True
  (==) _     _     = False

mutual
  export
  covering
  Eq EdgeRHS where
    (==) (MkEdgeRHS op1 s1) (MkEdgeRHS op2 s2) = op1 == op2 && s1 == s2

  export
  covering
  Eq Subgraph where
    (==) (MkSubgraph mSubGrID1 stmtList1) (MkSubgraph mSubGrID2 stmtList2) =
      mSubGrID1 == mSubGrID2 && stmtList1 == stmtList2

  export
  covering
  Eq Stmt where
    (==) (NodeStmt nodeID1 attrList1) (NodeStmt nodeID2 attrList2) =
      nodeID1 == nodeID2 && attrList1 == attrList2

    (==) (EdgeStmt e1 rhs1 attrList1) (EdgeStmt e2 rhs2 attrList2) =
      e1 == e2 && rhs1 == rhs2 && attrList1 == attrList2

    (==) (AttrStmt kw1 attrList1) (AttrStmt kw2 attrList2) =
      kw1 == kw2 && attrList1 == attrList2

    (==) (AssignStmt a1) (AssignStmt a2) =
      a1 == a2

    (==) (SubgraphStmt subGr1) (SubgraphStmt subGr2) =
      subGr1 == subGr2

    (==) _ _ =
      False

export
covering
Eq Graph where
  (==) (MkGraph s1 grTy1 mID1 stmtList1) (MkGraph s2 grTy2 mID2 stmtList2) =
    s1 == s2 && grTy1 == grTy2 && mID1 == mID2 && stmtList1 == stmtList2

