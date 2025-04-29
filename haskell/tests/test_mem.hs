
fib :: Int -> Int
fib = (map fib' [0..] !!)
  where fib' 0 = 0
        fib' 1 = 1
        fib' n = fib (n-2) + fib (n-1)




fibslow :: Int -> Int
fibslow 0 = 0
fibslow 1 = 1
fibslow n = fibslow(n-2) + fibslow(n-1)