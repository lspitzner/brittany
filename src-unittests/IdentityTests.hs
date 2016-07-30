{-# LANGUAGE QuasiQuotes #-}

module IdentityTests
  ( identityTests
  )
where



#include "prelude.inc"

import Test.Hspec

import NeatInterpolation

import Language.Haskell.Brittany

import TestUtils



identityTests :: Spec
identityTests = do
  describe "type signatures" $ typeSignatureTests
  describe "equation" $ do
    describe "basic"    $ basicEquationTests
    describe "patterns" $ patternTests
    describe "guards"   $ guardTests
  describe "expression" $ do
    describe "basic"         $ basicExpressionTests
    describe "do statements" $ doStatementTests
  describe "alignment" $ alignmentTests
  describe "regression" $ regressionTests

typeSignatureTests :: Spec
typeSignatureTests = do
  it "simple001" $ roundTripEqual $
    [text|
    func :: a -> a
    |]
  it "long typeVar" $ roundTripEqual $
    [text|
    func
      :: lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
      -> lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
    |]
  it "keep linebreak mode" $ roundTripEqual $
    [text|
    func
      :: lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
      -> lakjsdlkjasldkj
      -> lakjsdlkjasldkj
    |]
  it "simple parens 1" $ roundTripEqual $
    [text|
    func :: ((a))
    |]
  it "simple parens 2" $ roundTripEqual $
    [text|
    func :: (a -> a) -> a
    |]
  it "simple parens 3" $ roundTripEqual $
    [text|
    func :: a -> (a -> a)
    |]
  it "did anyone say parentheses?" $ roundTripEqual $
    [text|
    func :: (((((((((())))))))))
    |]
  before_ pending $ it "give me more!" $ roundTripEqual $
    -- current output is.. funny. wonder if that can/needs to be improved..
    [text|
    func :: ((((((((((((((((((((((((((((((((((((((((((()))))))))))))))))))))))))))))))))))))))))))
    |]    
  it "unit" $ roundTripEqual $
    [text|
    func :: ()
    |]
  -- ################################################################## --
  -- ################################################################## --
  -- ################################################################## --
  it "paren'd func 1" $ roundTripEqual $
    [text|
    func
      :: (  lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         -> lakjsdlkjasldkj
         -> lakjsdlkjasldkj
         )
    |]
  it "paren'd func 2" $ roundTripEqual $
    [text|
    func
      :: lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
      -> (lakjsdlkjasldkj -> lakjsdlkjasldkj)
    |]
  it "paren'd func 3" $ roundTripEqual $
    [text|
    func
      :: (lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd -> lakjsdlkjasldkj)
      -> lakjsdlkjasldkj
    |]
  it "paren'd func 4" $ roundTripEqual $
    [text|
    func
      :: (  lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         -> lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         )
      -> lakjsdlkjasldkj
    |]
  it "paren'd func 5" $ roundTripEqual $
    [text|
    func
      :: ( (  lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           -> lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           )
         )
    |]
  -- ################################################################## --
  -- ################################################################## --
  -- ################################################################## --
  it "type application 1" $ roundTripEqual $
    [text|
    func :: asd -> Either a b
    |]
  it "type application 2" $ roundTripEqual $
    [text|
    func
      :: asd
      -> Either
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
    |]
  it "type application 3" $ roundTripEqual $
    [text|
    func
      :: asd
      -> Trither
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
    |]
  it "type application 4" $ roundTripEqual $
    [text|
    func
      :: Trither
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
      -> asd
    |]
  it "type application 5" $ roundTripEqual $
    [text|
    func
      :: Trither
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           (lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd -> asd)
    |]
  it "type application 6" $ roundTripEqual $
    [text|
    func
      :: Trither
           asd
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           (  lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           -> lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           )
    |]
  it "type application paren 1" $ roundTripEqual $
    [text|
    func
      :: asd
      -> ( Trither
             lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
             lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
             lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         )
    |]
  it "type application paren 2" $ roundTripEqual $
    [text|
    func
      :: asd
      -> ( Trither
             lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
             lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         )
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
    |]
  it "type application paren 3" $ roundTripEqual $
    [text|
    func
      :: ( Trither
             lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
             lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         )
           lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
      -> asd
    |]
  -- ################################################################## --
  -- ################################################################## --
  -- ################################################################## --
  it "list simple" $ roundTripEqual $
    [text|
    func :: [a -> b]
    |]
  it "list func" $ roundTripEqual $
    [text|
    func
      :: [  lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         -> lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         ]
    |]
  it "list paren" $ roundTripEqual $
    [text|
    func
      :: [ (  lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           -> lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           )
         ]
    |]
  -- ################################################################## --
  -- ################################################################## --
  -- ################################################################## --    
  it "tuple type 1" $ roundTripEqual $
    [text|
    func :: (a, b, c)
    |]
  it "tuple type 2" $ roundTripEqual $
    [text|
    func :: ((a, b, c), (a, b, c), (a, b, c))
    |]
  it "tuple type long" $ roundTripEqual $
    [text|
    func
      :: ( lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         , lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         , lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         )
    |]
  it "tuple type nested" $ roundTripEqual $
    [text|
    func
      :: ( ( lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           , (lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd)
           , lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           )
         )
    |]
  it "tuple type function" $ roundTripEqual $
    [text|
    func
      :: [ ( lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           , lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           , lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
           )
         ]
    |]
  -- ################################################################## --
  -- ################################################################## --
  -- ################################################################## --
  before_ pending $ it "type operator stuff" $ roundTripEqual $
    [text|
    test050 :: a :+: b
    test051 ::  lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
            :+: lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
    test052 ::  lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
            ->  lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
            :+: lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
            ->  lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
    |]
  -- ################################################################## --
  -- ################################################################## --
  -- ################################################################## --
  it "forall oneliner" $ roundTripEqual $
    [text|
    {-# LANGUAGE ScopedTypeVariables #-}
    --this comment is necessary for whatever reason..
    func :: forall (a :: *) b . a -> b
    |]
  it "language pragma issue" $ roundTripEqual $
    [text|
    {-# LANGUAGE ScopedTypeVariables #-}
    func :: forall (a :: *) b . a -> b
    |]
  it "comments 1" $ roundTripEqual $
    [text|
    func :: a -> b -- comment
    |]
  it "comments 2" $ roundTripEqual $
    [text|
    funcA :: a -> b -- comment A
    funcB :: a -> b -- comment B
    |]
  before_ pending $ it "comments all" $ roundTripEqual $
    [text|
    -- a
    func -- b
      :: -- c
      a -- d
      -> -- e
      ( -- f
      c -- g
      , -- h
      d -- i
      ) -- j
    -- k
    |]
  -- ################################################################## --
  -- ################################################################## --
  -- ################################################################## --
  it "ImplicitParams 1" $ roundTripEqual $
    [text|
    {-# LANGUAGE ImplicitParams #-}
    func :: (?asd::Int) -> ()
    |]
  it "ImplicitParams 2" $ roundTripEqual $
    [text|
    {-# LANGUAGE ImplicitParams #-}
    func
      :: (  ?asd
         :: lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         -> lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
         )
      -> ()
    |]



-- some basic testing of different kinds of equations.
-- some focus on column layouting for multiple-equation definitions.
-- (that part probably is not implemented in any way yet.)
basicEquationTests :: Spec
basicEquationTests = do
  it "basic 1" $ roundTripEqual $
    [text|
    func x = x
    |]
  it "infix 1" $ roundTripEqual $
    [text|
    x *** y = x
    |]
  it "symbol prefix" $ roundTripEqual $
    [text|
    (***) x y = x
    |]



patternTests :: Spec
patternTests = do
  it "wildcard" $ roundTripEqual $
    [text|
    func _ = x
    |]
  before_ pending $ it "simple long pattern" $ roundTripEqual $
    [text|
    func reallyreallyreallyreallyreallyreallyreallyreallyreallyreallylongvariable
      = x
    |]
  before_ pending $ it "simple multiline pattern" $ roundTripEqual $
    [text|
    func reallyreallyreallyreallyreallyreallyreallyreallyreallyreallylongvariable
         reallyreallyreallyreallyreallyreallyreallyreallyreallyreallylongvariable
      = x
    |]
  before_ pending $ it "another multiline pattern" $ roundTripEqual $
    [text|
    func reallyreallyreallyreallyreallyreallyreallyreallyreallyreallylongvariable
         a
         b
      = x
    |]
  before_ pending $ it "simple constructor" $ roundTripEqual $
    [text|
    func (A a) = a
    |]
  before_ pending $ it "list constructor" $ roundTripEqual $
    [text|
    func (x:xr) = x
    |]
  before_ pending $ it "some other constructor symbol" $ roundTripEqual $
    [text|
    func (x:+:xr) = x
    |]

guardTests :: Spec
guardTests = do
  it "simple guard" $ roundTripEqual $
    [text|
    func | True = x
    |]

basicExpressionTests :: Spec
basicExpressionTests = do
  it "var" $ roundTripEqual $
    [text|
    func = x
    |]
  describe "infix op" $ do
    it "1" $ roundTripEqual $
      [text|
      func = x + x
      |]
    before_ pending $ it "long" $ roundTripEqual $
      [text|
      func = mweroiuxlskdfjlksjdflkjsdfljksldkjflkjsdflkj
           + mweroiuxlskdfjlksjdflkjsdfljksldkjflkjsdflkj
      |]
    before_ pending $ it "long keep linemode 1" $ roundTripEqual $
      [text|
      func = mweroiuxlskdfjlksjdflkjsdfljksldkjflkjsdflkj
           + mweroiuxlskdfjlksj
           + mweroiuxlskdfjlksj
      |]
    before_ pending $ it "long keep linemode 2" $ roundTripEqual $
      [text|
      func = mweroiuxlskdfjlksj
           + mweroiuxlskdfjlksj
           + mweroiuxlskdfjlksjdflkjsdfljksldkjflkjsdflkj
      |]
  it "literals" $ roundTripEqual $
    [text|
    func = 1
    func = "abc"
    func = 1.1e5
    func = 'x'
    func = 981409823458910394810928414192837123987123987123
    |]
  it "lambdacase" $ roundTripEqual $
    [text|
    {-# LANGUAGE LambdaCase #-}
    func = \case
      FooBar -> x
      Baz    -> y
    |]


doStatementTests :: Spec
doStatementTests = do
  it "simple" $ roundTripEqual $
    [text|
    func = do
      stmt
      stmt
    |]
  it "bind" $ roundTripEqual $
    [text|
    func = do
      x <- stmt
      stmt x
    |]
  it "let" $ roundTripEqual $
    [text|
    func = do
      let x = 13
      stmt x
    |]
  return ()

alignmentTests :: Spec
alignmentTests = do
  return ()

regressionTests :: Spec
regressionTests = do
  it "newlines-comment" $ do
    roundTripEqual $
      [text|
      func = do
        abc <- foo

        --abc
        return ()
      |]
  it "parenthesis-around-unit" $ do
    roundTripEqual $
      [text|
      func = (())
      |]
  it "let-defs indentation" $ do
    roundTripEqual $
      [text|
      func = do
        let foo True = True
            foo _    = False
        return ()
      |]
  it "record update indentation 1" $ do
    roundTripEqual $
      [text|
      func = do
        s <- mGet
        mSet $ s { _lstate_indent = _lstate_indent state }
      |]
  it "record update indentation 2" $ do
    roundTripEqual $
      [text|
      func = do
        s <- mGet
        mSet $ s { _lstate_indent = _lstate_indent state
                 , _lstate_indent = _lstate_indent state
                 }
      |]
  it "record update indentation 3" $ do
    roundTripEqual $
      [text|
      func = do
        s <- mGet
        mSet $ s
          { _lstate_indent = _lstate_indent lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
          , _lstate_indent = _lstate_indent lkasdlkjalsdjlakjsdlkjasldkjalskdjlkajsd
          }
      |]
  it "post-indent comment" $ do
    roundTripEqual $
      [text|
      func = do
      -- abc
        -- def
        return ()
      |]
  it "post-unindent comment" $ do
    roundTripEqual $
      [text|
      func = do
        do
          return ()
          -- abc
        -- def
        return ()
      |]
  it "CPP empty comment case" $ do
    pendingWith "CPP parsing needs fixing for roundTripEqual"
    roundTripEqual $
      [text|
      {-# LANGUAGE CPP #-}
      module Test where
      func = do
      #if FOO
        let x = 13
      #endif
        stmt x
      |]
  -- really, the following should be handled by forcing the Alt to multiline
  -- because there are comments. as long as this is not implemented though,
  -- we should ensure the trivial solution works.
  it "comment inline placement (temporary)" $ do
    roundTripEqual $
      [text|
      func :: Int -> -- basic indentation amount
                     Int -> -- currently used width in current line (after indent)
                            -- used to accurately calc placing of the current-line
                            LayoutDesc -> Int
      |]
