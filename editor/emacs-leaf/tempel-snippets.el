;; Template file used by `tempel'
;; https://github.com/minad/tempel#Template-syntax

;; Press `C-j' to insert snippet (with my current configuration)

fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

haskell-mode

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

(runSTUArray
 "runSTUAray $ do
    !arr <- VUM.replicate ((0, 0), (pred n, pred n)) (0 :: Int)
    return arr")

(bounds "!bounds_ = ((0, 0), (h - 1, w - 1))")

(undef
 "undef :: Int
undef = -1")

(twos
  "twos :: VU.Vector MyModInt
twos = VU.iterateN (2000 * 2) (\x -> x * x) (modInt 2)")

(pattern
 "pattern INSERT, DELETE :: Int
pattern INSERT = 0
pattern DELETE = 1")

(create
  "VU.create $ do
     !vec <- VUM.replicate (h * w) (0 :: Int)
     return vec")

(dir4 "!dir4 = VU.fromList [(0, 1), (0, -1), (1, 0), (-1, 0)]")
(crossDir4 "!dir4 = VU.fromList [(1, 1), (1, -1), (-1, 1), (-1, -1)]")
(dir8 "!dir8 = VU.fromList [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]")

(error "error \"unreachable\"")

;; examples
(mapAccumL "mapAccumL (\\!acc !x -> (acc + x, x)) (0 :: Int) [1, 2, 3]")
(stateMap "evalState (VU.mapM (\\x -> state $ \\acc -> (x, x + acc)) (VU.fromList [1, 2, 3])) (0 :: Int)")

(monoidAction
 "-- | Add
type OpRepr = Int
newtype Op = Op OpRepr
  deriving (Eq, Ord, Show)

newtype instance VU.MVector s Op = MV_Op (VUM.MVector s OpRepr)
newtype instance VU.Vector Op = V_Op (VU.Vector OpRepr)
deriving via (Op `VU.As` OpRepr) instance VGM.MVector VUM.MVector Op
deriving via (Op `VU.As` OpRepr) instance VG.Vector VU.Vector Op
instance VU.Unbox Op

instance Semigroup Op where
  (Op !x1) <> (Op !x2) = Op (x1 + x2)

instance Monoid Op where
  mempty = Op 0

instance SemigroupAction Op Acc where
  sact (Op !o) (Acc !a) = Acc (o + a)

instance MonoidAction Op Acc

-- | Max
type AccRepr = Int
newtype Acc = Acc AccRepr
  deriving (Eq, Ord, Show, VP.Prim)

newtype instance VU.MVector s Acc = MV_Acc (VP.MVector s Acc)
newtype instance VU.Vector Acc = V_Acc (VP.Vector Acc)
deriving via (VU.UnboxViaPrim Acc) instance VGM.MVector VUM.MVector Acc
deriving via (VU.UnboxViaPrim Acc) instance VG.Vector VU.Vector Acc
instance VU.Unbox Acc

instance Semigroup Acc where
  (Acc !x1) <> (Acc !x2) = Acc (x1 `max` x2)

instance Monoid Acc where
  mempty = Acc (minBound @Int)
")

(unbox
"newtype instance VU.MVector s " (p "Type" type) " = MV_" (s type) " (VP.MVector s " (s type) ")
newtype instance VU.Vector " (s type) " = V_" (s type) " (VP.Vector " (s type) ")
deriving via (VU.UnboxViaPrim " (s type) ") instance VGM.MVector VUM.MVector " (s type) "
deriving via (VU.UnboxViaPrim " (s type) ") instance VG.Vector VU.Vector " (s type) "
instance VU.Unbox " (s type))

(isounbox
"instance VU.IsoUnbox Acc AccRepr where
  {-# INLINE toURepr #-}
  toURepr (Acc x) = x
  {-# INLINE fromURepr #-}
  fromURepr = Acc

newtype instance VU.MVector s Acc = MV_Acc (VUM.MVector s AccRepr)

newtype instance VU.Vector Acc = V_Acc (VU.Vector AccRepr)

deriving via (Acc `VU.As` AccRepr) instance VGM.MVector VUM.MVector Acc

deriving via (Acc `VU.As` AccRepr) instance VG.Vector VU.Vector Acc

instance VU.Unbox Acc")

(modInt
"data MyModulo = MyModulo

instance TypeInt MyModulo where
  -- typeInt _ = 1_000_000_007
  typeInt _ = 998244353

type MyModInt = ModInt MyModulo

myMod :: Int
myMod = typeInt (Proxy @MyModulo)

modInt :: Int -> MyModInt
modInt = ModInt . (`rem` myMod)")

org-mode

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

(ahc "[[https://atcoder.jp/contests/ahc"  (p "001" no) "][AHC " (s no) "]] に参加した。")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
