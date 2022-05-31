module Graphics.DOT.Utils.Filters

import Graphics.DOT.AST

import Data.String
import Data.List1

%default total

----------------
-- Visibility --
----------------

||| Returns `True` iff the `DOTID` was a `NameID` or a `StringID` containing the
||| value 'style.
export
dotidIsStyle : DOTID -> Bool
dotidIsStyle (NameID "style") = True
dotidIsStyle (StringID "\"style\"") = True
dotidIsStyle _ = False

||| Returns `True` iff the `DOTID` was a `NameID` or a `StringID` containing the
||| value 'invis'.
export
dotidIsInvis : DOTID -> Bool
dotidIsInvis (NameID "invis") = True
dotidIsInvis (StringID "\"invis\"") = True
dotidIsInvis _ = False

||| Returns `True` iff the given assignment sets 'style' to 'invis'.
assignIsInvis : Assign -> Bool
assignIsInvis (MkAssign lhs rhs) = dotidIsStyle lhs && dotidIsInvis rhs

||| Remove any assignment which assigns to the value "invis".
export
%inline
removeInvisAssign : List Assign -> List Assign
removeInvisAssign = filter (not . assignIsInvis)

||| Returns `True` iff the given Edge is explicitly invisible.
export
isInvisEdge : Stmt -> Bool
isInvisEdge (EdgeStmt x rhs (assigns :: attrs)) =
   any assignIsInvis assigns || any (any assignIsInvis) attrs
isInvisEdge _ = False

||| Remove all invisible edges from the given `Graph`.
export
removeInvisEdges : Graph -> Graph
removeInvisEdges g@(MkGraph strict graphTy mID_ []) = g
removeInvisEdges (MkGraph strict graphTy mID_ stmts) =
   MkGraph strict graphTy mID_ $ filter (not . isInvisEdge) stmts

----------------
-- Clustering --
----------------

||| Returns `True` iff the `DOTID` is either a `StringID` or a `NameID`, whose
||| value starts with the word "cluster".
export
dotidIsCluster : DOTID -> Bool
dotidIsCluster (StringID id_) = isPrefixOf "\"cluster" $ toLower id_
dotidIsCluster (NameID name) = isPrefixOf "cluster" $ toLower name
dotidIsCluster _ = False

||| Returns `True` iff the `Graph` is a named subgraph, whose name starts with
||| the word "cluster".
export
graphIsCluster : Graph -> Bool
graphIsCluster (MkGraph strict SubgraphKW (Just graphID) stmtList) =
   dotidIsCluster graphID
graphIsCluster _ = False

||| Returns `True` iff the `Subgraph` is a named subgraph, whose name starts
||| with the word "cluster".
export
subgraphIsCluster : Subgraph -> Bool
subgraphIsCluster (MkSubgraph (Just (kw, (Just subgraphID))) stmtList) =
   dotidIsCluster subgraphID
subgraphIsCluster _ = False

||| Returns `True` iff the `EdgeRHS` connects to a cluster subgraph.
export
edgeRHSHasCluster : EdgeRHS -> Bool
edgeRHSHasCluster (MkEdgeRHS op (Right subgr)) = subgraphIsCluster subgr
edgeRHSHasCluster _ = False

||| Returns `True` iff the given `Stmt` contains a cluster subgraph.
export
stmtHasCluster : Stmt -> Bool
stmtHasCluster (EdgeStmt (Right subgr) erhss _) =
   subgraphIsCluster subgr || any edgeRHSHasCluster erhss
stmtHasCluster (EdgeStmt _ erhss _) = any edgeRHSHasCluster erhss
stmtHasCluster (SubgraphStmt subgr) = subgraphIsCluster subgr
stmtHasCluster _ = False

||| Remove any cluster subgraphs from the given `Graph`.
export
removeClusters : Graph -> Graph
removeClusters g@(MkGraph strict graphTy mID_ []) = g
removeClusters (MkGraph strict graphTy mID_ stmts) =
   MkGraph strict graphTy mID_ $ filter (not . stmtHasCluster) stmts

