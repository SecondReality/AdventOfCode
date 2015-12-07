module Common where

fromInput x fileName= do
  content <- readFile fileName
  print (x content)

count elem list = length $ filter id list
