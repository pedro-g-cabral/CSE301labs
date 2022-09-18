asInt :: Either Bool Int -> Int
asInt (Right n) = n
asInt (Left b) = if b
                    then 1
                    else 0