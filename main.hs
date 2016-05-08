infixr 8 :>:
infixr 9 :|:

data Regex
  = Lit Char         -- individual characters are regexes
  | Regex :>: Regex  -- if E and F are regexes, so is EF
  | Regex :|: Regex  -- if E and F are regexes, so is E|F
  | Opt Regex        -- if E is a regex, so is E?
  | Rep Regex        -- if E is a regex, so is E+



data LocalRegexContext
  = InSeqL Regex  -- a @Seq@ with the left arg removed
  | InSeqR Regex  -- a @Seq@ with the right arg removed
  | InAltL Regex  -- an @Alt@ with the left arg removed
  | InAltR Regex  -- an @Alt@ with the right arg removed
  | InOpt         -- an @Opt@ with its only arg removed
  | InRep         -- a @Rep@ with its only arg removed



type RegexContext = [LocalRegexContext]



plug :: Regex -> LocalRegexContext -> Regex
plug e (InSeqL f) = e :>: f  -- e in _f is ef
plug e (InSeqR f) = f :>: e  -- e in f_ is fe
plug e (InAltL f) = e :|: f  -- e in _|f is e|f
plug e (InAltR f) = f :|: e  -- e in f|_ is f|e
plug e InOpt      = Opt e    -- e in _? is e?
plug e InRep      = Rep e    -- e in _+ is e+



plugContext :: Regex -> RegexContext -> Regex
plugContext e c = foldl plug e c



{-
demoCtx :: RegexContext
demoCtx =
  [ InRep
  , InAltR (Lit 'b')
  , InSeqL (Lit 'd')
  , InSeqR (Lit 'c')
  ]

plugContext e demoContext
== Lit 'a' :>: (Lit 'b' :|: Rep e) :>: Lit 'd'
-}



data Direction = Starting | Finishing



type RegexState = (Direction, Regex, RegexContext)



next :: RegexState -> [RegexState]

-- this group corresponds to the edges out of starting states
next (Starting, Lit x, c)   = [ (Finishing, Lit x, c) ]
next (Starting, e :>: f, c) = [ (Starting, e, InSeqL f : c) ]
next (Starting, e :|: f, c) = [ (Starting, e, InAltL f : c)
                              , (Starting, f, InAltR e : c)
                              ]
next (Starting, Opt e, c)   = [ (Starting, e, InOpt : c)
                              , (Finishing, Opt e, c)
                              ]
next (Starting, Rep e, c)   = [ (Starting, e, InRep : c) ]

-- this group corresponds to the edges out of finishing states

next (Finishing, e, InSeqL f : c) = [ (Starting, f, InSeqR e : c) ]
next (Finishing, f, InSeqR e : c) = [ (Finishing, e :>: f, c) ]
next (Finishing, e, InAltL f : c) = [ (Finishing, e :|: f, c) ]
next (Finishing, f, InAltR e : c) = [ (Finishing, e :|: f, c) ]
next (Finishing, e, InOpt : c)    = [ (Finishing, Opt e, c) ]
next (Finishing, e, InRep : c)    = [ (Finishing, Rep e, c)
                                    , (Starting, e, InRep : c)
                                    ]
next _ = []



generateFirst :: Regex -> String
generateFirst e = go (Starting, e, [])
  where
    go :: RegexState -> String
    go s = case next s of
      [] -> ""
      [ s'@(Finishing, Lit x, c) ] -> x : go s'
      s':_ -> go s'



{-
generateFirst (Lit 'a' :>: (Lit 'b' :>: Rep (Lit 'c')) :>: Lit 'd')
== "abd"
-}



emit :: RegexState -> [(Char,RegexState)]
emit s =
  do s' <- next s
     case s' of
       (Finishing, Lit x, _) -> [ (x,s') ]
       _ -> emit s'



finishesTrivially :: RegexState -> Bool
finishesTrivially (Starting, Lit _, _) = False
finishesTrivially (Starting, e :>: f, c) =
  finishesTrivially (Starting, e, InSeqL f : c)
finishesTrivially (Starting, e :|: f, c) =
  finishesTrivially (Starting, e, InAltL f : c)
finishesTrivially (Starting, Opt e, c) =
  finishesTrivially (Finishing, Opt e, c)
finishesTrivially (Starting, Rep e, c) =
  finishesTrivially (Starting, e, InRep : c)
finishesTrivially (Finishing, _, []) = True
finishesTrivially (Finishing, e, InSeqL f : c) =
  finishesTrivially (Starting, f, InSeqR e : c)
finishesTrivially (Finishing, f, InSeqR e : c) =
  finishesTrivially (Finishing, e :>: f, c)
finishesTrivially (Finishing, e, InAltL f : c) =
  finishesTrivially (Finishing, e :>: f, c)
finishesTrivially (Finishing, f, InAltR e : c) =
  finishesTrivially (Finishing, e :>: f, c)
finishesTrivially (Finishing, e, InOpt : c) =
  finishesTrivially (Finishing, Opt e, c)
finishesTrivially (Finishing, e, InRep : c) =
  finishesTrivially (Finishing, Rep e, c)



generateN :: Int -> Regex -> [String]
generateN n e = go n [("", (Starting, e, []))]
  where
    go :: Int -> [(String,RegexState)] -> [String]
    go 0 ss = [ reverse rcs | (rcs,s) <- ss, finishesTrivially s ]
    go n ss = let ss' = do
                    (rcs,s) <- ss
                    (c,s') <- emit s
                    return (c:rcs, s')
              in go (n-1) ss'



consume :: Char -> RegexState -> [RegexState]
consume c s = [ s' | (x,s') <- emit s, c == x ]



recognize :: String -> Regex -> Bool
recognize cs e = go cs [(Starting, e, [])]
  where
    go :: String -> [RegexState] -> Bool
    go "" ss = any finishesTrivially ss
    go (c:cs) ss = go cs [ s' | s <- ss, s' <- consume c s ]
