module Graphics.DOT.Parser

import Text.Parser
import Data.String
import Data.Vect

import Graphics.DOT.Lexer
import Graphics.DOT.Representation

%default total

-- terminals --

lBrace : Grammar DOTToken True ()
lBrace = terminal "Expected LBrace"
            (\case LBrace => Just ()
                   _ => Nothing)

rBrace : Grammar DOTToken True ()
rBrace = terminal "Expected RBrace (might not be properly closed?)"
            (\case RBrace => Just ()
                   _ => Nothing)

lBracket : Grammar DOTToken True ()
lBracket = terminal "Expected LBracket"
              (\case LBracket => Just ()
                     _ => Nothing)

rBracket : Grammar DOTToken True ()
rBracket = terminal "Expected RBracket (might not be properly closed?)"
              (\case RBracket => Just ()
                     _ => Nothing)

semicolon : Grammar DOTToken True ()
semicolon = terminal "Expected semicolon (shouldn't get this message)"
              (\case Semicolon => Just ()
                     _ => Nothing)

comma : Grammar DOTToken True ()
comma = terminal "Expected comma"
          (\case Comma => Just ()
                 _ => Nothing)

equals : Grammar DOTToken True ()
equals = terminal "Expected equals"
          (\case Equal => Just ()
                 _ => Nothing)

identifier : Grammar DOTToken True DOT
identifier = ?identifier_TODO

assign_ : Grammar DOTToken True DOT
assign_ = do idLHS <- identifier
             equals
             idRHS <- identifier
             pure (Assign [idLHS, idRHS])    -- returns assign node in AST

sepChoice : Grammar DOTToken False ()
sepChoice = ignore $ optional (choose semicolon comma)

a_list : Grammar DOTToken True DOT
a_list = (do head <- assign_
             sepChoice
             ?a_list_ast1)    -- TODO: a_list AST node, takes list of stuff
      <|> (do head <- assign_
              sepChoice
              rest <- a_list
              ?a_list_ast2)

keyword : Grammar DOTToken True DOT
keyword = terminal "Expected a keyword"
            (\case Keyword kw =>
                      -- TODO: refine `Keyword` token?
                      case toLower kw of
                           "node" => Just Node
                           "edge" => Just Edge
                           "graph" => Just Graph
                           "digraph" => Just DiGraph
                           "subgraph" => Just SubGraph
                           "strict" => Just Strict
                           _ => Nothing
                   _ => Nothing)

compassPt : Grammar DOTToken True DOT
compassPt = terminal "Expected a compass point"
              (\case CompassPt pt =>
                        -- TODO: refine `CompassPt` token?
                        case pt of
                             "n"  => Just North
                             "ne" => Just NorthEast
                             "e"  => Just East
                             "se" => Just SouthEast
                             "s"  => Just South
                             "sw" => Just SouthWest
                             "w"  => Just West
                             "nw" => Just NorthWest
                             "c"  => Just CenterCPt
                             "_"  => Just UnderCPt
                             _    => Nothing
                     _ => Nothing)


