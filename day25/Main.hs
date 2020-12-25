{-# LANGUAGE Strict #-}

module Main where

main :: IO ()
main = do
  let cardPublicKey = 10604480
  let doorPublicKey = 4126658

  let cardLoopSize = findLoopSize 7 cardPublicKey
  let doorLoopSize = findLoopSize 7 doorPublicKey

  let cardEncryptionKey = runLoop doorPublicKey cardLoopSize
  let doorEncryptionKey = runLoop cardPublicKey doorLoopSize

  print (cardEncryptionKey, doorEncryptionKey)

findLoopSize :: Int -> Int -> Int
findLoopSize subjectNumber final =
  go subjectNumber final 1
  where
    go current final count =
      if current == final
        then count
        else
          go
            (rem (current * subjectNumber) 20201227)
            final
            (count + 1)

runLoop :: Int -> Int -> Int
runLoop subjectNumber loopSize =
  go subjectNumber loopSize
  where
    go current 1 = current
    go current n =
      go (rem (current * subjectNumber) 20201227) (n - 1)
