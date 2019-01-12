module HLE.Layout where

import Protolude hiding (Const, evaluate, Sum, Left, Right, zero)
import Lens.Micro.Platform
import Linear hiding (zero)

data Box a = Box {
    dims :: V2 a
  , center :: V2 a -- relative to dims, not global
  } deriving (Functor, Foldable, Traversable)

-- * Terminology
--
-- The distance between the left side of a box and its senter is called the left gap,
-- similarly for right, top, and bottom

data Direction = Left
               | Right
               | Top
               | Bottom

class Maxable n where
  gmax :: [n] -> n

instance Maxable Float where
  gmax = maximum

class Summable n where
  gsum :: [n] -> n

instance Summable Float where
  gsum = sum

zero :: (Summable n) => n
zero = gsum []

class Negatable n where
  gneg :: n -> n

instance Negatable Float where
  gneg = negate

gap :: (Summable a, Negatable a) => Direction -> Box a -> a
gap d b = case d of
  Left -> cx
  Right -> gsum [width, gneg cx]
  Top -> cy
  Bottom -> gsum [height, gneg cy]
  where
    V2 width height = dims b
    V2 cx cy = center b

data Element c a = Element {
    box :: Box a
  , badness :: a
  , content :: c
  } deriving (Functor, Foldable, Traversable)

data Frame c a =
    VFrame [Frame c a]
    -- ^ A VFrame aligns other frames vertically, so that their centers are on a vertical line
    -- The x dimension of a VFrame is the max of the left gaps of the children added to the max
    -- of the right gaps of the children. The y dimension is the sum of the y dimensions of the
    -- children.
    -- 
    -- The center of a VFrame has x coordinate equal to the max of the left gaps of the children,
    -- and y coordinate equal to 0. NOTE, in the future we may want to have this y coordinate be
    -- changed.
  | HFrame [Frame c a]
    -- ^ Same thing as VFrame, just horizontal instead of vertical (switch x and y in description,
    -- and Top\/Bottom with Left\/Right
  | Elt (Element c a)
  deriving (Functor, Foldable, Traversable)

-- * TODO: figure out what global badness measure makes sense (sum? min?)
-- Also, should we be doing things locally?
frameBadness :: Frame c a -> a
frameBadness = undefined

-- * TODO: remove duplication of code for VFrame and HFrame
computeBox :: (Summable a, Maxable a, Negatable a) => Frame c a -> Box a
computeBox (VFrame children) = Box (V2 x_dim y_dim) (V2 cx zero)
  where
    children_boxes = map computeBox children
    children_dims = map dims children_boxes
    [left_gaps, right_gaps] = [map (gap dir) children_boxes | dir <- [Left, Right]]
    x_dim = gsum $ map gmax [left_gaps, right_gaps]
    y_dim = gsum $ map (^._y) children_dims
    cx = gmax left_gaps
computeBox (HFrame children) = Box (V2 x_dim y_dim) (V2 zero cy)
  where
    children_boxes = map computeBox children
    children_dims = map dims children_boxes
    [top_gaps, bottom_gaps] = [map (gap dir) children_boxes | dir <- [Top, Bottom]]
    x_dim = gsum $ map gmax [top_gaps, bottom_gaps]
    y_dim = gsum $ map (^._y) children_dims
    cy = gmax top_gaps
computeBox (Elt elt) = box elt

newtype VarId = VarId Int

data Expr a = Const a
            | Var VarId
            | Sum [Expr a]
            | ScalarMult a (Expr a)
            | Max [Expr a]
  deriving (Functor, Foldable, Traversable)

instance Summable (Expr a) where
  gsum = Sum

instance Maxable (Expr a) where
  gmax = Max

instance (Num a) => Negatable (Expr a) where
  gneg = ScalarMult (-1)

evaluate :: (Num a, Ord a) => (VarId -> Maybe a) -> Expr a -> Maybe a
evaluate _ (Const x) = Just x
evaluate f (Var i) = f i
evaluate f (Sum exprs) = sum <$> mapM (evaluate f) exprs
evaluate f (ScalarMult x expr) = (x *) <$> (evaluate f expr)
evaluate f (Max exprs) = maximum <$> mapM (evaluate f) exprs

class Minimizable a where
  -- ^ take in a list of constraints, an expression, an initial value for the variables
  -- and then minimizes the expression subject to the constraints
  minimize :: [(Expr a, Expr a)] -> Expr a -> (VarId -> Maybe a) -> (VarId -> Maybe a)

-- ^ takes in a frame parameterized by expressions and a box to put it in,
--   minimizes global badness, and then
--   apply the results from minimization
layout :: (Minimizable a, Num a, Ord a) => Frame c (Expr a) -> Box a -> (VarId -> Maybe a) -> Maybe (Frame c a)
layout frame output_box init_vals = mapM (evaluate final_values) frame
  where
    Box (V2 output_x_dim output_y_dim) _ = output_box
    Box (V2 expr_x_dim expr_y_dim) _ = computeBox frame
    constraints = [(expr_x_dim, Const output_x_dim),(expr_y_dim, Const output_y_dim)]
    final_values = minimize constraints (frameBadness frame) init_vals
