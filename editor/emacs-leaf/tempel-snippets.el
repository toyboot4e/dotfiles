;; Template file used by `tempel'
;; https://github.com/minad/tempel#Template-syntax

;; Press `C-j' to insert snippet (with my current configuration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fundamental-mode ;; Available everywhere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(today (format-time-string "%Y-%m-%d"))
;;(m469762049 "469762049")
;;(m998244353 "998244353")
;;(m1000000007 "1000000007")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
haskell-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(verify "-- verification-helper: PROBLEM " (p "url"))
(tr "import Debug.Trace")

;; stack script
(script "#!/usr/bin/env stack" n "{- stack script --resolver lts-21.6 -}")

;; header
(langauge "{-# LANGUAGE " (p "ViewPatterns") " #-}")
(import "import " (p "Data.List"))
;; (import-qualified "import qualified" (p "Data.IntSet") " as " (p "IS"))
(module "module " (p "Main") " (" (p "main") ") where")

(ormolu "{- ORMULU_DISABLE -}" n "{- ORMOLU_ENABLE -}")

;; debug
(dbg "!_ = dbg (" (p "") ")")
(d "!_ = dbg (" (p "") ")")

(assert "!_ = dbgAssert (" (p "") ")")
(a "!_ = dbgAssert (" (p "") ")")

(default "default (Integer, Int)")

(f (s name) " :: " (p "") p n> (s name))
(w "where" n> r> n)

(bnd "!bnd = ((0, 0), (h - 1, w - 1))")

;; (foldM "(\\f -> U.foldM'_ f s0 xs) $ \\acc x -> do")
(foldM "U.foldM'" n> r>
       "  (\\acc x -> do" n> r>
       "    pure acc" n> r>
       "  )" n> r>
       "  (0 :: Int)" n> r>
       "  qs")

(undef
 "undef :: Int
undef = -1")

(twos
  "twos :: U.Vector MyModInt
twos = U.iterateN (2000 * 2) (\\x -> 2 * x) (modInt 1)")

(inline "{-# INLINE " (p "f") " #-}")
(noinline "{-# NOINLINE " (p "f") " #-}")

(pattern
 "pattern INSERT, DELETE :: Int
pattern INSERT = 0
pattern DELETE = 1")

(create
  "U.create $ do
     !vec <- UM.replicate (h * w) (0 :: Int)
     return vec")

(cross4 "cross4 :: U.Vector (Int, Int)
cross4 = U.fromList [(1, 1), (1, -1), (-1, 1), (-1, -1)]")

(dir8 "dir8 :: U.Vector (Int, Int)" n> r>
"dir8 = U.fromList [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]")

(error "error \"unreachable\"")

;; examples
(mapAccumL "mapAccumL (\\!acc !x -> (acc + x, x)) (0 :: Int) [1, 2, 3]")
(stateMap "(`evalState` x0) $ U.mapM (state . step) xs
      where
        -- step :: x -> state -> (x', state')
        step !x !state = (state, x)")

;; let !xs' = (`evalState` IM.empty) $ U.forM xs $ \\x -> state $ \\im -> do
;;       let !cnt = fromMaybe 0 $ IM.lookup x im
;;       -- (IM.insertWith (+) x 1 im, section * x + cnt)
;;       (section * x + cnt, IM.insertWith (+) x 1 im)

(sort "U.modify VAI.sort")
;; (sortD "U.modify (VAI.sortBy (comparing Down))")

(factMods "factMods :: U.Vector MyModInt
factMods = U.scanl' (*) (modInt 1) (U.generate (2 * 10 ^ 5) (modInt . succ))

invFactMods :: U.Vector MyModInt
invFactMods = U.map recip factMods

combMods :: Int -> Int -> MyModInt
combMods n k = factMods U.! n * invFactMods U.! k * invFactMods U.! (n - k)")

(monoidAction
 "-- | Add
type OpRepr = Int

instance Semigroup Op where
  -- @new <> old@ on segment tree
  {-# INLINE (<>) #-}
  (Op !x1) <> (Op !x2) = Op (x1 + x2)

instance Monoid Op where
  {-# INLINE mempty #-}
  mempty = Op 0

-- instance SemigroupAction Op Acc where
--   {-# INLINE sact #-}
--   sact (Op !dx) (Acc !x) = Acc (x + dx)

instance SegmentAction Op Acc where
  {-# INLINE segActWithLength #-}
  segActWithLength len (Op !dx) (Acc !x) = Acc (x + dx)

-- | Max
type AccRepr = Int

instance Semigroup Acc where
  {-# INLINE (<>) #-}
  (Acc !x1) <> (Acc !x2) = Acc (x1 `max` x2)

instance Monoid Acc where
  {-# INLINE mempty #-}
  mempty = Acc minBound

{- ORMOLU_DISABLE -}
newtype Acc = Acc AccRepr deriving newtype (Eq, Ord, Show) ; unAcc :: Acc -> AccRepr ; unAcc (Acc x) = x ; newtype instance U.MVector s Acc = MV_Acc (U.MVector s AccRepr) ; newtype instance U.Vector Acc = V_Acc (U.Vector AccRepr) ; deriving instance GM.MVector UM.MVector Acc ; deriving instance G.Vector U.Vector Acc ; instance U.Unbox Acc ;
newtype Op = Op OpRepr deriving newtype (Eq, Ord, Show) ; unOp :: Op -> OpRepr ; unOp (Op x) = x; newtype instance U.MVector s Op = MV_Op (U.MVector s OpRepr) ; newtype instance U.Vector Op = V_Op (U.Vector OpRepr) ; deriving instance GM.MVector UM.MVector Op ; deriving instance G.Vector U.Vector Op ; instance U.Unbox Op ;
{- ORMOLU_ENABLE -}")

(unbox_
"newtype instance U.MVector s " (p "Type" type) " = MV_" (s type) " (P.MVector s " (s type) ")
newtype instance U.Vector " (s type) " = V_" (s type) " (P.Vector " (s type) ")
deriving via (U.UnboxViaPrim " (s type) ") instance GM.MVector UM.MVector " (s type) "
deriving via (U.UnboxViaPrim " (s type) ") instance G.Vector U.Vector " (s type) "
instance U.Unbox " (s type))

(isounbox
"instance U.IsoUnbox Acc AccRepr where
  {-# INLINE toURepr #-}
  toURepr (Acc x) = x
  {-# INLINE fromURepr #-}
  fromURepr = Acc

newtype instance U.MVector s Acc = MV_Acc (UM.MVector s AccRepr)

newtype instance U.Vector Acc = V_Acc (U.Vector AccRepr)

deriving via (Acc `U.As` AccRepr) instance GM.MVector UM.MVector Acc

deriving via (Acc `U.As` AccRepr) instance G.Vector U.Vector Acc

instance U.Unbox Acc")

(modInt
"{- ORMOLU_DISABLE -}
type MyModulo = (998244353 :: Nat) -- (1_000_000_007 :: Nat)
type MyModInt = ModInt MyModulo ; myMod :: Int ; myMod = fromInteger $ natVal' @MyModulo proxy# ; {-# INLINE modInt #-} ; modInt :: Int -> MyModInt ; modInt = ModInt . (`rem` myMod) ;
{- ORMOLU_ENABLE -}")

(rh "type RH' = RH HashInt MyModulo ;")

(quickcheck
"propQC :: QC.Gen QC.Property
propQC = do
  n <- QC.choose (1, maxN)
  xs <- U.fromList <$> QC.vectorOf n (QC.choose (1, 5000)
  solveAC n xs QC.=== solveWA n xs
  where
    maxN = 1000

runQC :: IO ()
runQC = QC.quickCheck (QC.withMaxSuccess 100 propQC)")

(try-catch-unsafe-perform-io
"  let !_ = unsafePerformIO $ do
        result <- try (evaluate (mainImpl n q xs qs)) :: IO (Either SomeException (VU.Vector Int))
        case result of
          Left ex -> do
            putStrLn $ "Caught an exception: " ++ show ex
            exitSuccess
          Right _ -> pure ()")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(img "#+CAPTION: " p n
"[[./img/" p q "]]")
(attr "#+ATTR_HTML: :width " p "px")
(width "#+ATTR_HTML: :width " p "px")

(ai "#+BEGIN_AI markdown" n> "[ME]: " r> n "#+END_AI")
(yaruo "#+BEGIN_YARUO" n> r> n "#+END_YARUO")

(caption "#+CAPTION: ")
(drawer ":" p ":" n r ":end:")
(begin "#+BEGIN_" (s name) n> r> n "#+END_" name)
(quote "#+BEGIN_QUOTE" n> r> n "#+END_QUOTE")
(sidenote "#+BEGIN_SIDENOTE" n> r> n "#+END_SIDENOTE")
(marginnote "#+BEGIN_MARGINNOTE" n> r> n "#+END_MARGINNOTE")
(example "#+BEGIN_EXAMPLE" n> r> n "#+END_EXAMPLE")
(center "#+BEGIN_CENTER" n> r> n "#+END_CENTER")
(ascii "#+BEGIN_EXPORT ascii" n> r> n "#+END_EXPORT")
(html "#+BEGIN_EXPORT html" n> r> n "#+END_EXPORT")
(latex "#+BEGIN_EXPORT latex" n> r> n "#+END_EXPORT")
(comment "#+BEGIN_COMMENT" n> r> n "#+END_COMMENT")
(verse "#+BEGIN_VERSE" n> r> n "#+end_verse")
(src "#+BEGIN_SRC " (p "haskell") n> >r n "#+END_SRC")
(details "#+BEGIN_DETAILS " q n> "#+END_DETAILS")
(export "#+BEGIN_EXPORT " (p "html") n> "#+END_EXPORT")
(gnuplot "#+BEGIN_SRC gnuplot :var data=" (p "table") " :file " (p "plot.png") n> r> n "#+END_SRC" :post (org-edit-src-code))
(elisp "#+BEGIN_SRC emacs-lisp" n> r> n "#+END_SRC" :post (org-edit-src-code))
;; (inlsrc "src_" p "{" q "}")
(title "#+TITLE: " p n "#+AUTHOR: Daniel Mendler" n "#+LANGUAGE: en")

;; for diary
(abc "* ABC " (p "300" no) "

[[https://atcoder.jp/contests/abc"  (s no) "][ABC " (s no) "]] に参加しました。

#+CAPTION: Diff 予想
| 問題       | A 問題 | B 問題 | C 問題 | D 問題 | E 問題 | F 問題 |
|------------+--------+--------+--------+--------+--------+--------|
| 提出       |        |        |        |        |        |        |
| 予想 diff |        |        |        |        |        |        |
| 実際 diff |        |        |        |        |        |        |

** [[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_a][A 問題]]

** [[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_b][B 問題]]

** [[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_c][C 問題]]

** [[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_d][D 問題]]

** [[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_e][E 問題]]

** [[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_f][F 問題]]
")

(arc "* ARC " (p "100" no) "

[[https://atcoder.jp/contests/arc"  (s no) "][ARC " (s no) "]] に参加しました。

** [[https://atcoder.jp/contests/arc" (s no) "/tasks/arc" (s no) "_a][A 問題]]

** [[https://atcoder.jp/contests/arc" (s no) "/tasks/arc" (s no) "_b][B 問題]]

** [[https://atcoder.jp/contests/arc" (s no) "/tasks/arc" (s no) "_c][C 問題]]
")

(agc "* AGC" (p "100" no) "

[[https://atcoder.jp/contests/agc" (s no) "][AGC " (s no) "]] に参加しました。

* [[https://atcoder.jp/contests/agc" (s no) "/tasks/agc" (s no) "_a][A 問題]]

* [[https://atcoder.jp/contests/agc" (s no) "/tasks/agc" (s no) "_b][B 問題]]

* [[https://atcoder.jp/contests/agc" (s no) "/tasks/agc" (s no) "_c][C 問題]]
")

(ahc "[[https://atcoder.jp/contests/ahc"  (p "001" no) "][AHC " (s no) "]] に参加しました。")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
