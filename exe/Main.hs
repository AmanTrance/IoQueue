{-# LANGUAGE OverloadedStrings #-}

module Main where

import IoQueue

main :: IO ()
main = do
  Just queue <- newIoQueue "queue1"
  pushIoQueue queue "test1"
  pushIoQueue queue "test2"
  value <- popIoQueue queue
  closeIoQueue queue
  print value