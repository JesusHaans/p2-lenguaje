{-# OPTIONS_GHC -w #-}
module EAB.Sintax where
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,55) ([49552,262,32768,0,960,0,0,36864,1729,27673,0,0,0,316,768,49552,6406,36972,1729,27673,512,6400,108,0,512,12288,0,3,0,962,49552,50182,3,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","EAB","let","in","end","var","int","'='","'||'","'&&'","'+'","'*'","not","'('","')'","true","false","%eof"]
        bit_start = st * 20
        bit_end = (st + 1) * 20
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..19]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (5) = happyShift action_2
action_0 (8) = happyShift action_4
action_0 (9) = happyShift action_5
action_0 (15) = happyShift action_6
action_0 (16) = happyShift action_7
action_0 (18) = happyShift action_8
action_0 (19) = happyShift action_9
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (5) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (8) = happyShift action_16
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (11) = happyShift action_12
action_3 (12) = happyShift action_13
action_3 (13) = happyShift action_14
action_3 (14) = happyShift action_15
action_3 (20) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_8

action_5 _ = happyReduce_9

action_6 (5) = happyShift action_2
action_6 (8) = happyShift action_4
action_6 (9) = happyShift action_5
action_6 (15) = happyShift action_6
action_6 (16) = happyShift action_7
action_6 (18) = happyShift action_8
action_6 (19) = happyShift action_9
action_6 (4) = happyGoto action_11
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (5) = happyShift action_2
action_7 (8) = happyShift action_4
action_7 (9) = happyShift action_5
action_7 (15) = happyShift action_6
action_7 (16) = happyShift action_7
action_7 (18) = happyShift action_8
action_7 (19) = happyShift action_9
action_7 (4) = happyGoto action_10
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_10

action_9 _ = happyReduce_11

action_10 (11) = happyShift action_12
action_10 (12) = happyShift action_13
action_10 (13) = happyShift action_14
action_10 (14) = happyShift action_15
action_10 (17) = happyShift action_22
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (11) = happyFail []
action_11 (12) = happyFail []
action_11 (13) = happyShift action_14
action_11 (14) = happyShift action_15
action_11 _ = happyReduce_6

action_12 (5) = happyShift action_2
action_12 (8) = happyShift action_4
action_12 (9) = happyShift action_5
action_12 (15) = happyShift action_6
action_12 (16) = happyShift action_7
action_12 (18) = happyShift action_8
action_12 (19) = happyShift action_9
action_12 (4) = happyGoto action_21
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (5) = happyShift action_2
action_13 (8) = happyShift action_4
action_13 (9) = happyShift action_5
action_13 (15) = happyShift action_6
action_13 (16) = happyShift action_7
action_13 (18) = happyShift action_8
action_13 (19) = happyShift action_9
action_13 (4) = happyGoto action_20
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (5) = happyShift action_2
action_14 (8) = happyShift action_4
action_14 (9) = happyShift action_5
action_14 (15) = happyShift action_6
action_14 (16) = happyShift action_7
action_14 (18) = happyShift action_8
action_14 (19) = happyShift action_9
action_14 (4) = happyGoto action_19
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (5) = happyShift action_2
action_15 (8) = happyShift action_4
action_15 (9) = happyShift action_5
action_15 (15) = happyShift action_6
action_15 (16) = happyShift action_7
action_15 (18) = happyShift action_8
action_15 (19) = happyShift action_9
action_15 (4) = happyGoto action_18
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (10) = happyShift action_17
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (5) = happyShift action_2
action_17 (8) = happyShift action_4
action_17 (9) = happyShift action_5
action_17 (15) = happyShift action_6
action_17 (16) = happyShift action_7
action_17 (18) = happyShift action_8
action_17 (19) = happyShift action_9
action_17 (4) = happyGoto action_23
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_5

action_19 (14) = happyShift action_15
action_19 _ = happyReduce_4

action_20 (11) = happyFail []
action_20 (12) = happyFail []
action_20 (13) = happyShift action_14
action_20 (14) = happyShift action_15
action_20 _ = happyReduce_3

action_21 (11) = happyFail []
action_21 (12) = happyFail []
action_21 (13) = happyShift action_14
action_21 (14) = happyShift action_15
action_21 _ = happyReduce_2

action_22 _ = happyReduce_7

action_23 (6) = happyShift action_24
action_23 (11) = happyShift action_12
action_23 (12) = happyShift action_13
action_23 (13) = happyShift action_14
action_23 (14) = happyShift action_15
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (5) = happyShift action_2
action_24 (8) = happyShift action_4
action_24 (9) = happyShift action_5
action_24 (15) = happyShift action_6
action_24 (16) = happyShift action_7
action_24 (18) = happyShift action_8
action_24 (19) = happyShift action_9
action_24 (4) = happyGoto action_25
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (7) = happyShift action_26
action_25 (11) = happyShift action_12
action_25 (12) = happyShift action_13
action_25 (13) = happyShift action_14
action_25 (14) = happyShift action_15
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_1

happyReduce_1 = happyReduce 7 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (or happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (and happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (plus happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (times happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (not happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  4 happyReduction_8
happyReduction_8 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (var happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  4 happyReduction_9
happyReduction_9 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn4
		 (int happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  4 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn4
		 (true
	)

happyReduce_11 = happySpecReduce_1  4 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn4
		 (false
	)

happyNewToken action sts stk [] =
	action 20 20 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLet -> cont 5;
	TokenIn -> cont 6;
	TokenEnd -> cont 7;
	TokenVar happy_dollar_dollar -> cont 8;
	TokenInt happy_dollar_dollar -> cont 9;
	TokenAssign -> cont 10;
	TokenOr -> cont 11;
	TokenAnd -> cont 12;
	TokenPlus -> cont 13;
	TokenTimes -> cont 14;
	TokenNot -> cont 15;
	TokenLParen -> cont 16;
	TokenRParen -> cont 17;
	TokenTrue -> cont 18;
	TokenFalse -> cont 19;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 20 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data EAB = Let String EAB EAB
         | Or EAB EAB
         | And EAB EAB
         | Plus EAB EAB
         | Times EAB EAB
         | Not EAB
         | Var String
         | Int Int
         | True
         | False
         deriving (Show)

data Token = TokenLet
           | TokenIn
           | TokenEnd
           | TokenVar String
           | TokenInt Int
           | TokenAssign
           | TokenOr
           | TokenAnd
           | TokenPlus
           | TokenTimes
           | TokenNot
           | TokenLParen
           | TokenRParen
           | TokenTrue
           | TokenFalse
           deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
        | isSpace c = lexer cs
        | isAlpha c = lexVar (c:cs)
        | isDigit c = lexInt (c:cs)
lexer ('=':cs) = TokenAssign : lexer cs
lexer ('|':'|':cs) = TokenOr : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('n':'o':'t':cs) = TokenNot : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer ('t':'r':'u':'e':cs) = TokenTrue : lexer cs
lexer ('f':'a':'l':'s':'e':cs) = TokenFalse : lexer cs

lexInt :: String -> [Token]
lexInt cs = TokenInt (read n) : lexer rest
    where (n, rest) = span isDigit cs

lexVar :: String -> [Token]
lexVar cs = case span isAlpha cs of
                ("let", cs') -> TokenLet : lexer cs'
                ("in", cs') -> TokenIn : lexer cs'
                ("end", cs') -> TokenEnd : lexer cs'
                (var, cs') -> TokenVar var : lexer cs'
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8336_0/ghc_2.h" #-}
































































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
