;; Template file used by `tempel'
;; https://github.com/minad/tempel#Template-syntax

;; Press `C-j' to insert snippet (with my current configuration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fundamental-mode ;; Available everywhere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(today (format-time-string "%Y-%m-%d"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
haskell-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(verification-helper "-- verification-helper: PROBLEM ")

;; stack script
(script "#!/usr/bin/env stack" n "{- stack script --resolver lts-21.6 -}")

;; header
(langauge "{-# LANGUAGE " (p "BangPatterns") " #-}")
(import "import " (p "Data.List"))
;; (import-qualified "import qualified" (p "Data.IntSet") " as " (p "IS"))
(module "module " (p "Main") " (" (p "main") ") where")

(ormolu "{- ORMULU_DISABLE -}" n "{- ORMOLU_ENABLE -}")

;; debug
(dbg "!_ = dbg (" (p "") ")")
(assert "!_ = dbgAssert (" (p "") ")")

(default "default (Integer, Int)")

(runSTUArray
 "runSTUAray $ do
    !arr <- UM.replicate ((0, 0), (pred n, pred n)) (0 :: Int)
    return arr")

(g1 "!" (p "n") " <- ints1")
(g2 "(!" (p "h") ", !" (p "w") ") <- ints2")
(g3 "(!" (p "z") ", !"(p "y") ", !" (p "x") ") <- ints3")

(bnd "!bnd = ((0, 0), (h - 1, w - 1))")

(undef
 "undef :: Int
undef = -1")

(twos
  "twos :: U.Vector MyModInt
twos = U.iterateN (2000 * 2) (\x -> x * x) (modInt 2)")

(pattern
 "pattern INSERT, DELETE :: Int
pattern INSERT = 0
pattern DELETE = 1")

(create
  "U.create $ do
     !vec <- UM.replicate (h * w) (0 :: Int)
     return vec")

(ortho4 "ortho4 :: U.Vector (Int, Int)
ortho4 = U.fromList [(0, 1), (0, -1), (1, 0), (-1, 0)]

{-# INLINE ortho4' #-}
ortho4' :: ((Int, Int), (Int, Int)) -> (Int, Int) -> U.Vector (Int, Int)
ortho4' bnd base = U.filter (inRange bnd) $ U.map (add2 base) ortho4")

(cross4 "cross4 :: U.Vector (Int, Int)
cross4 = U.fromList [(1, 1), (1, -1), (-1, 1), (-1, -1)]")

(dir8 "dir8 :: U.Vector (Int, Int)" n> r>
"dir8 = U.fromList [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]")

(error "error \"unreachable\"")

;; examples
(mapAccumL "mapAccumL (\\!acc !x -> (acc + x, x)) (0 :: Int) [1, 2, 3]")
(stateMap "evalState (U.mapM (\\x -> state $ \\acc -> (x, x + acc)) (U.fromList [1, 2, 3])) (0 :: Int)")

(monoidAction
 "-- | Add
type OpRepr = Int

instance Semigroup Op where
  (Op !x1) <> (Op !x2) = Op (x1 + x2)

instance Monoid Op where
  mempty = Op 0

instance SemigroupAction Op Acc where
  sact (Op !dx) (Acc !x) = Acc (x + dx)

-- | Max
type AccRepr = Int

instance Semigroup Acc where
  (Acc !x1) <> (Acc !x2) = Acc (x1 `max` x2)

instance Monoid Acc where
  mempty = Acc minBound

{- ORMOLU_DISABLE -}
newtype Op = Op OpRepr deriving newtype (Eq, Ord, Show) ; newtype Acc = Acc AccRepr deriving newtype (Eq, Ord, Show) ; unAcc :: Acc -> AccRepr ; unAcc (Acc x) = x ; newtype instance U.MVector s Op = MV_Op (U.MVector s OpRepr) ; newtype instance U.Vector Op = V_Op (U.Vector OpRepr) ; deriving instance GM.MVector UM.MVector Op ; deriving instance G.Vector U.Vector Op ; instance U.Unbox Op ; newtype instance U.MVector s Acc = MV_Acc (U.MVector s AccRepr) ; newtype instance U.Vector Acc = V_Acc (U.Vector AccRepr) ; deriving instance GM.MVector UM.MVector Acc ; deriving instance G.Vector U.Vector Acc ; instance U.Unbox Acc ;
instance MonoidAction Op Acc
{- ORMOLU_ENABLE -}")

;; (monoidAction
;; "-- | Add
;; type OpRepr = Int
;; newtype Op = Op OpRepr
;;   deriving newtype (Eq, Ord, Show)
;; 
;; newtype instance U.MVector s Op = MV_Op (U.MVector s OpRepr)
;; newtype instance U.Vector Op = V_Op (U.Vector OpRepr)
;; deriving instance GM.MVector UM.MVector Op
;; deriving instance G.Vector U.Vector Op
;; instance U.Unbox Op
;; 
;; instance Semigroup Op where
;;   (Op !x1) <> (Op !x2) = Op (x1 + x2)
;; 
;; instance Monoid Op where
;;   mempty = Op 0
;; 
;; instance SemigroupAction Op Acc where
;;   sact (Op !o) (Acc !a) = Acc (o + a)
;; 
;; instance MonoidAction Op Acc
;; 
;; -- | Max
;; type AccRepr = Int
;; newtype Acc = Acc AccRepr
;;   deriving newtype (Eq, Ord, Show)
;; 
;; unAcc :: Acc -> AccRepr
;; unAcc (Acc x) = x
;; 
;; newtype instance U.MVector s Acc = MV_Acc (U.MVector s AccRepr)
;; newtype instance U.Vector Acc = V_Acc (U.Vector AccRepr)
;; deriving instance GM.MVector UM.MVector Acc
;; deriving instance G.Vector U.Vector Acc
;; instance U.Unbox Acc
;; 
;; instance Semigroup Acc where
;;   (Acc !x1) <> (Acc !x2) = Acc (x1 `max` x2)
;; 
;; instance Monoid Acc where
;;   mempty = Acc (minBound @Int)")

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
 "type MyModulo = (998244353 :: Nat)
-- type MyModulo = (1_000_000_007 :: Nat)

type MyModInt = ModInt MyModulo

myMod :: Int
myMod = fromInteger $ natVal' @MyModulo proxy#

{-# INLINE modInt #-}
modInt :: Int -> MyModInt
modInt = ModInt . (`rem` myMod)

type RH' = RH HashInt MyModulo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(img "#+CAPTION: " p n
"[[./img/" p q "]]")
(width "#+ATTR_HTML: :width " p "px")

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
(src "#+BEGIN_SRC " (p "hs") n> "#+END_SRC")
(details "#+BEGIN_DETAILS " q n> "#+END_DETAILS")
(export "#+BEGIN_EXPORT " (p "html") n> "#+END_EXPORT")
(gnuplot "#+BEGIN_SRC gnuplot :var data=" (p "table") " :file " (p "plot.png") n> r> n "#+END_SRC" :post (org-edit-src-code))
(elisp "#+BEGIN_SRC emacs-lisp" n> r> n "#+END_SRC" :post (org-edit-src-code))
;; (inlsrc "src_" p "{" q "}")
(title "#+TITLE: " p n "#+AUTHOR: Daniel Mendler" n "#+LANGUAGE: en")

;; for diary
(abc "[[https://atcoder.jp/contests/abc"  (p "300" no) "][ABC " (s no) "]] に参加した。

[[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_a][A 問題]] では

[[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_b][B 問題]] では

[[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_c][C 問題]] では

[[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_d][D 問題]] では

[[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_e][E 問題]] では

[[https://atcoder.jp/contests/abc" (s no) "/tasks/abc" (s no) "_f][F 問題]] では
")

(arc "[[https://atcoder.jp/contests/arc"  (p "100" no) "][ARC " (s no) "]] に参加した。

[[https://atcoder.jp/contests/arc" (s no) "/tasks/arc" (s no) "_a][A 問題]] では

[[https://atcoder.jp/contests/arc" (s no) "/tasks/arc" (s no) "_b][B 問題]] では

[[https://atcoder.jp/contests/arc" (s no) "/tasks/arc" (s no) "_c][C 問題]] では
")

(agc "[[https://atcoder.jp/contests/agc"  (p "100" no) "][AGC " (s no) "]] に参加した。

[[https://atcoder.jp/contests/agc" (s no) "/tasks/agc" (s no) "_a][A 問題]] では

[[https://atcoder.jp/contests/agc" (s no) "/tasks/agc" (s no) "_b][B 問題]] では

[[https://atcoder.jp/contests/agc" (s no) "/tasks/agc" (s no) "_c][C 問題]] では
")

(ahc "[[https://atcoder.jp/contests/ahc"  (p "001" no) "][AHC " (s no) "]] に参加した。")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
