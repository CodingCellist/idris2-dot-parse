module Graphics.DOT.Interfaces

import Graphics.DOT.AST
import Graphics.DOT.ASTv2

import Data.Vect
import Data.List1
import Data.String

%default total


--------------------------------------------------------------------------------
-- INTERFACES FOR CONVERTING TO DOT
--------------------------------------------------------------------------------

namespace DCPT
  ||| The type `t` can be converted to a `CompassPoint`.
  public export
  interface DOTCompassPoint t where
    convert : t -> CompassPoint

namespace DKW
  ||| The type `t` can be converted to a `Keyword`.
  public export
  interface DOTKeyword t where
    convert : t -> Keyword

namespace DDID
  ||| The type `t` can be converted to a `DOTID`.
  public export
  interface DOTDOTID t where
    convert : t -> DOTID

namespace DPORT
  ||| The type `t` can be converted to a `Port`.
  public export
  interface DOTPort t where
    convert : t -> Port

namespace DNID
  ||| The type `t` can be converted to a `NodeID`.
  public export
  interface DOTNodeID t where
    convert : t -> NodeID

namespace DAssign
  ||| The type `t` can be converted to an `Assign`.
  public export
  interface DOTAssign t where
    convert : t -> Assign

namespace DEOP
  ||| The type `t` can be converted to an `EdgeOp`.
  public export
  interface DOTEdgeOp t where
    convert : t -> EdgeOp

namespace DERHS
  ||| The type `t` can be converted to an `EdgeRHS`.
  public export
  interface DOTEdgeRHS t where
    convert : t -> EdgeRHS

namespace DSUBGR
  ||| The type `t` can be converted to a `Subgraph`.
  public export
  interface DOTSubgraph t where
    convert : t -> Subgraph

namespace DSTMT
  ||| The type `t` can be converted to a `Stmt`.
  public export
  interface DOTStmt t where
    convert : t -> Stmt

namespace DGR
  ||| The type `t` can be converted to a `Graph`.
  public export
  interface DOTGraph t where
    convert : t -> Graph


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


-------------
-- DOTAble --
-------------

public export
DOTAble CompassPoint where
  toDOT N = North
  toDOT NE = NorthEast
  toDOT E = East
  toDOT SE = SouthEast
  toDOT S = South
  toDOT SW = SouthWest
  toDOT W = West
  toDOT NW = NorthWest
  toDOT Center = CenterCPt
  toDOT Underscore = UnderCPt

public export
DOTAble Keyword where
  toDOT StrictKW = Strict
  toDOT GraphKW = GraphKW
  toDOT DigraphKW = DiGraph
  toDOT NodeKW = Node
  toDOT EdgeKW = Edge
  toDOT SubgraphKW = SubGraph

public export
DOTAble DOTID where
  toDOT (StringID id_) = StringID id_
  toDOT (NameID name) = NameID name
  toDOT (Numeral num) = NumeralID num
  toDOT (HTML htmlStr) = HTML_ID htmlStr

public export
DOTAble Port where
  toDOT (IDPort id_ mCPt) = IDPort (toDOT id_) (toDOT <$> mCPt)
  toDOT (PlainPort cpt) = CPTPort (toDOT cpt)

public export
DOTAble NodeID where
  toDOT (MkNodeID id_ mPort) = NodeID (toDOT id_) (toDOT <$> mPort)

public export
DOTAble Assign where
  toDOT (MkAssign lhs rhs) = Assign (map toDOT [lhs, rhs])

public export
DOTAble EdgeOp where
  toDOT Arrow = DiGrEdgeOp
  toDOT Dash = GrEdgeOp

mutual
  covering
  toAList : List Assign -> DOT
  toAList as = AList $ map toDOT as

  public export
  covering
  DOTAble EdgeRHS where
    toDOT (MkEdgeRHS op (Left id_)) = EdgeRHS [toDOT op, toDOT id_]
    toDOT (MkEdgeRHS op (Right subgr)) = EdgeRHS [toDOT op, toDOT subgr]

  public export
  covering
  DOTAble Subgraph where
    toDOT (MkSubgraph Nothing stmtList) =
      Subgraph Nothing (StmtList $ map toDOT stmtList)

    -- `DOT.AST` doesn't store the keyword, but uses `SubgraphID` to denote
    -- whether the 'subgraph' keyword was present in the .gv file...
    toDOT (MkSubgraph (Just (kw, Nothing)) stmtList) =
      Subgraph (Just $ SubgraphID Nothing) (StmtList $ map toDOT stmtList)
    toDOT (MkSubgraph (Just (kw, (Just id_))) stmtList) =
      let sID = SubgraphID $ Just (toDOT id_)
      in Subgraph (Just sID) (StmtList $ map toDOT stmtList)

  public export
  covering
  DOTAble Stmt where
    toDOT (NodeStmt nodeID []) =
      Stmt $ NodeStmt (toDOT nodeID) Nothing
    toDOT (NodeStmt nodeID attrList) =
      Stmt $ NodeStmt (toDOT nodeID) (Just $ AttrList $ map toAList attrList)

    -- IR stores rhs as a `List1`
    toDOT (EdgeStmt (Left id_) rhs []) =
      let dotRHS = toList $ map toDOT rhs
      in Stmt $ EdgeStmt (toDOT id_) (EdgeRHS dotRHS) Nothing
    toDOT (EdgeStmt (Left id_) rhs attrList) =
      let dotRHS = toList $ map toDOT rhs
          dotAList = map toAList attrList
      in Stmt $ EdgeStmt (toDOT id_) (EdgeRHS dotRHS) (Just $ AttrList dotAList)
    toDOT (EdgeStmt (Right subgr) rhs []) =
      let dotRHS = toList $ map toDOT rhs
      in Stmt $ EdgeStmt (toDOT subgr) (EdgeRHS dotRHS) Nothing
    toDOT (EdgeStmt (Right subgr) rhs attrList) =
      let dotRHS = toList $ map toDOT rhs
          dotAList = map toAList attrList
      in Stmt $ EdgeStmt (toDOT subgr) (EdgeRHS dotRHS) (Just $ AttrList dotAList)

    toDOT (AttrStmt kw attrList) =
      Stmt $ AttrStmt (toDOT kw) (AttrList $ map toAList attrList)

    toDOT (AssignStmt a) =
      Stmt $ toDOT a

    toDOT (SubgraphStmt subGr) =
      Stmt $ toDOT subGr

public export
covering
DOTAble Graph where
  toDOT (MkGraph Nothing graphTy mID_ stmtList) =
    Graph False (toDOT graphTy) (toDOT <$> mID_) (StmtList $ map toDOT stmtList)
  toDOT (MkGraph (Just kw) graphTy mID_ stmtList) =
    Graph True (toDOT graphTy) (toDOT <$> mID_) (StmtList $ map toDOT stmtList)

