import Control.Monad.State
import DynamicCounterfactuals

-- kangaroo example

-- players
a = ["Q", "R", "S"]

-- worlds
w = ["w000", "w100", "w010", "w110",
     "w001", "w101", "w011", "w111",
     "w00x", "w10x", "w01x", "w11x"]

-- labeling function
l "w000" = []
l "w100" = ["q"]
l "w010" = ["r"]
l "w110" = ["q", "r"]
l "w001" = ["s"]
l "w101" = ["q", "s"]
l "w011" = ["r", "s"]
l "w111" = ["q", "r", "s"]
l "w00x" = ["s"]
l "w10x" = ["q"]
l "w01x" = ["r"]
l "w11x" = ["q", "r"]

-- transition function
t _ vs = "w" ++ (concat $ map snd vs)

-- strategy function
s "Q" w = [[w !! 1]]
s "R" w = [[w !! 2]]
s "S" _ = ["x"]

model = Model { players = a, worlds = w, label_f = l, transition_f = t,
                strategy_f = s }

-- Lewis-Sobel sequence
-- 1. If kangaroos had no tails, they would topple over.
-- 2. If kangaroos had no tails but used crutches, they would not topple over.
sobel :: State Model ([World], [World])
sobel = do
    -- evaluate first sentence
    first <- check (Str "Q" (\w -> ["0"]) (Box (Cond (Ng (Pr "q")) (Pr "s"))))
    -- evaluate second sentence
    second <- check (Str "Q" (\w -> ["0"]) (Str "R" (\w -> ["1"])
                     (Box (Cond (Cnj (Ng (Pr "q")) (Pr "r")) (Ng (Pr "s"))))))
    -- return the two sets of worlds
    return (first, second)

-- evalState sobel model
-- worlds where 1. is true
-- (["w000","w100","w001","w101","w00x","w10x"],
-- worlds where 2. is true
--  ["w000","w100","w010","w110","w001","w101",
--   "w011","w111","w00x","w10x","w01x","w11x"])

-- reverse Sobel sequence
-- 1. If kangaroos had no tails but used crutches, they would not topple over.
-- 2. If kangaroos had no tails, they would topple over.
reverse_sobel :: State Model ([World], [World])
reverse_sobel = do
    first <- check (Str "Q" (\w -> ["0"]) (Str "R" (\w -> ["1"])
                    (Box (Cond (Cnj (Ng (Pr "q")) (Pr "r")) (Ng (Pr "s"))))))
    second <- check (Str "Q" (\w -> ["0"]) (Box (Cond (Ng (Pr "q")) (Pr "s"))))
    return (first, second)

-- evalState reverse_sobel model
-- worlds where 1. is true (same as above)
-- (["w000","w100","w010","w110","w001","w101",
--   "w011","w111","w00x","w10x","w01x","w11x"],
-- worlds where 2. is true (none)
--  [])
