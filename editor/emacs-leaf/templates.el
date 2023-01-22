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
(traceShow "let !_ = traceShow (" (p "") ") ()")

;; competitive programming
(yn "if " (p "result") " then \"Yes\" else \"No\"")

(list "[n] <- getLineIntList")
(vec "xs <- getLineIntList")
(graphGen "input <- concatMap (\\[a, b] -> [(a, b), (b, a)]) <$> replicateM nEdges getLineIntList" n>
          "let graph = accumArray (flip (:)) [] (1, nVerts) input" n>)

(fold "let result = foldl' step s0 " (p "input") n>
      "    s0 = " (p "_") n>
      "    step = " (p "_"))

(runSTUArray "let dp = runSTArray $ do" n>
        "arr <- newArray ((0, 0), (h - 1, w - 1)) (0 :: Int)" n>
        "      return arr")

(VU.create "let dp = VU.create $ do" n>
        "vec <- VUM.replicate (h * w) (0 :: Int)" n>
        "      return vec")

(for2 "forM_ [0 .. h - 1] $ \\y -> do" n>
        "forM_ [0 .. w - 1] $ \\x -> do" n>)

;; examples
(mapAccumL "mapAccumL (\\acc x -> (acc + x, x)) (0 :: Int) [1, 2, 3]")
(stateMap "runState (mapM (\\x -> state $ \\acc -> (x, x + acc)) [1, 2, 3]) (0 :: Int)")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
