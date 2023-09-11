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
(stateMap "evalState (mapM (\\ !x -> state $ \\ !acc -> (x, x + acc)) [1, 2, 3]) (0 :: Int)")

(monoidAction
"-- | Add
newtype Op = Op Int
  deriving (Eq, Ord, Show)

derivingUnbox "Op" [t|Op -> Int|] [|\(Op !x) -> x|] [|\ !x -> Op x|]

instance Semigroup Op where
  (Op !x1) <> (Op !x2) = Op (x1 + x2)

instance Monoid Op where
  mempty = Op 0

instance SemigroupAction Op Acc where
  sact (Op !o) (Acc !a) = Acc (o + a)

instance MonoidAction Op Acc

-- | Max
newtype Acc = Acc Int
  deriving (Eq, Ord, Show)

derivingUnbox "Acc" [t|Acc -> Int|] [|\(Acc !x) -> x|] [|\ !x -> Acc x|]

instance Semigroup Acc where
  (Acc !x1) <> (Acc !x2) = Acc (x1 `max` x2)

instance Monoid Acc where
  mempty = Acc (minBound @Int) -- as `Max`")

(modInt
"data MyModulo = MyModulo

instance TypeInt MyModulo where
  -- typeInt _ = 1_000_000_007
  typeInt _ = 998244353

type MyModInt = ModInt MyModulo

myModulo :: Int
myModulo = typeInt (Proxy @MyModulo)

modInt :: Int -> MyModInt
modInt = ModInt . (`rem` myModulo)")

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
(src "#+BEGIN_SRC " q n> "#+END_SRC")
(gnuplot "#+BEGIN_SRC gnuplot :var data=" (p "table") " :file " (p "plot.png") n> r> n "#+END_SRC" :post (org-edit-src-code))
(elisp "#+BEGIN_SRC emacs-lisp" n> r> n "#+END_SRC" :post (org-edit-src-code))
;; (inlsrc "src_" p "{" q "}")
(title "#+TITLE: " p n "#+AUTHOR: Daniel Mendler" n "#+LANGUAGE: en")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
