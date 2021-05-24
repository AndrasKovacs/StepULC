import ULC
import Examples
import Criterion.Main

import qualified BigStepNBE as BSNBE

main = defaultMain
    [
      bgroup "sem1"
        [ bench "fac 4" $ nf semantic1 (facN 4)
        ]
    , bgroup "sem1-1"
        [ bench "fac 4" $ nf semantic1_1 (facN 4)
        ]
    , bgroup "sem1-2"
        [ bench "fac 4" $ nf semantic1_2 (facN 4)
        ]
    , bgroup "sem2"
        [ bench "fac 4" $ nf semantic2 (facN 4)
        ]
    , bgroup "sem2-1"
        [ bench "fac 4" $ nf semantic2_1 (facN 4)
        ]
    , bgroup "sem2-2"
        [ bench "fac 4" $ nf semantic2_2 (facN 4)
        ]
    , bgroup "sem3"
        [
          bench "fac 4" $ nf semantic3 (facN 4)
        ]
    , bgroup "bigstep nbe"
        [
          bench "fac 4" $ nf BSNBE.nf (BSNBE.facN 4)
        ]
    ]
