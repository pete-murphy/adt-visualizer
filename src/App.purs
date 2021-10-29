module App where

import Prelude

import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Tuple.Nested ((/\))
import Debug as Debug
import Expr.Parser (Expr(..))
import Expr.Parser as Parser
import Math as Math
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.DOM.Events as DOM.Events
import React.Basic.DOM.SVG as SVG
import React.Basic.Events as Events
import React.Basic.Hooks (Component)
import React.Basic.Hooks as Hooks

renderSVG :: Expr -> Array JSX
renderSVG = go 0 0
  where
  go x y = case _ of
    Add a b -> go x y a <> go (x + 14) y b
    Mul a b -> go x y a <> go x (y + 20) b
    Pow a b -> go x y a <> go x y b
    Lit a -> pure
      ( SVG.text
          { children: [ R.text (show a) ]
          , x: show (x + 10)
          , y: show (y + 20)
          }
      )

data Validated a
  = Unvalidated
  | Invalid a
  | Valid a

-- instance Functor Validated where
--   map _ Unvalidated = Unvalidated
--   map f (Valid a) = Valid (f a)
--   map f (Invalid a) = Invalid (f a)

-- instance Alt Validated where
--   alt Unvalidated x = x
--   alt (Valid _) x = x
--   alt _ (Valid y) = Valid y
--   alt x _ = x

mkApp :: Component Unit
mkApp = do
  Hooks.component "App" \_ -> Hooks.do
    input /\ setInput <- Hooks.useState ""
    lastValidated /\ setLastValidated <- Hooks.useState Unvalidated

    Hooks.useEffect input do
      let
        f (Valid x) = Invalid x
        f x = x
      case Parser.run input of
        Left _ -> setLastValidated f
        Right x -> setLastValidated (const (Valid x))
      pure mempty

    pure do
      R.div_
        [ R.form
            { onSubmit: Events.handler DOM.Events.preventDefault mempty
            , children:
                [ R.input
                    { value: input
                    , onChange: Events.handler
                        DOM.Events.targetValue
                        (Foldable.traverse_ (setInput <<< const))
                    }
                ]
            }
        , SVG.svg
            { width: "400"
            , height: "400"
            , children: case lastValidated of
                Unvalidated -> mempty
                Invalid a -> renderSVG a
                Valid a -> renderSVG a
            , opacity: case lastValidated of
                Unvalidated -> "1"
                Invalid _ -> "0.2"
                Valid _ -> "1"
            }
        ]