;; Template file used by `tempel'
;; https://github.com/minad/tempel#Template-syntax

;; Press `C-j' to insert snippet (with my current configuration)

fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

haskell-mode

;; stack script
(script "#!/usr/bin/env stack" n "{- stack script --resolver lts-16.11 -}")

;; header
(langauge "{-# LANGUAGE " (p "BangPatterns") " #-}")
(import "import " (p "Data.List"))
;; (import-qualified "import qualified" (p "Data.IntSet") " as " (p "IS"))
(module "module " (p "Main") " (" (p "main") ") where")

(ormolu "{- ORMULU_DISABLE -}" n "{- ORMOLU_ENABLE -}")

;; debug
(traceShow "!_ = traceShow (" (p "") ") ()")
(dbg "!_ = dbg (" (p "") ")")
(assert "!_ = dbgAssert (" (p "") ")")

;; competitive programming
(yn "if " (p "result") " then \"Yes\" else \"No\"")

(getL "[!n] <- getLineIntList")
(getV "!xs <- getLineIntList")

(graphGen "!input <- concatMap (\\[a, b] -> [(a, b), (b, a)]) <$> replicateM nEdges getLineIntList" n>
          "let !graph = accumArray @Array (flip (:)) [] (1, nVerts) input" n>)

(fold "let !result = foldl' step s0 " (p "input") n>
      "    s0 = " (p "_") n>
      "    step = !" (p "_"))

(runSTUArray
 "runSTUAray $ do
    !arr <- VUM.replicate ((0, 0), (pred n, pred n)) (0 :: Int)
    return arr")

(create
  "VU.create $ do
     !vec <- VUM.replicate (h * w) (0 :: Int)
     return vec")

(for2 "forM_ [0 .. h - 1] $ \\y -> do" n>
        "forM_ [0 .. w - 1] $ \\x -> do" n>)

(error "error \"unreachable\"")

;; examples
(mapAccumL "mapAccumL (\\!acc !x -> (acc + x, x)) (0 :: Int) [1, 2, 3]")
(stateMap "evalState (mapM (\\ !x -> state $ \\ !acc -> (x, x + acc)) [1, 2, 3]) (0 :: Int)")

(fixDP
  "let !result = VU.create $ do
        !dp <- VUM.replicate (succ n) undef
        VUM.write dp 0 (0 :: Int)

        !_ <- flip fix n $ \loop_ i -> do
          if i < 0
            then return 0
            else do
              !x0 <- VUM.read dp i
              if x0 /= undef
                then return x0
                else do
                  x1 <- succ <$> loop_ (i - 1)
                  x2 <- succ <$> loop_ (i - 2)

                  let x = divModF (x1 * p1 `rem` 998244353 + x2 * p2 `rem` 998244353) 100 998244353
                  -- let !_ = traceShow (i, x1, x2, x) ()

                  VUM.write dp i x
                  return x

        return dp")

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


org-mode

(caption "#+caption: ")
(drawer ":" p ":" n r ":end:")
(begin "#+begin_" (s name) n> r> n "#+end_" name)
(quote "#+begin_quote" n> r> n "#+end_quote")
(sidenote "#+begin_sidenote" n> r> n "#+end_sidenote")
(marginnote "#+begin_marginnote" n> r> n "#+end_marginnote")
(example "#+begin_example" n> r> n "#+end_example")
(center "#+begin_center" n> r> n "#+end_center")
(ascii "#+begin_export ascii" n> r> n "#+end_export")
(html "#+begin_export html" n> r> n "#+end_export")
(latex "#+begin_export latex" n> r> n "#+end_export")
(comment "#+begin_comment" n> r> n "#+end_comment")
(verse "#+begin_verse" n> r> n "#+end_verse")
(src "#+begin_src " q n> "#+end_src")
(gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n> r> n "#+end_src" :post (org-edit-src-code))
(elisp "#+begin_src emacs-lisp" n> r> n "#+end_src" :post (org-edit-src-code))
;; (inlsrc "src_" p "{" q "}")
(title "#+title: " p n "#+author: Daniel Mendler" n "#+language: en")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
