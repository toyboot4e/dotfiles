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
