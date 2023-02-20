module Memo where

lookupMemo :: Eq k => k -> [(k,v)] -> Maybe v
lookupMemo _  [] = Nothing
lookupMemo key ((k,v):kvs)
  | k == key = Just v
  | otherwise = lookupMemo key kvs 
  
updateMemo ::  Eq k => [(k,v)] -> k  -> v -> [(k,v)]
updateMemo [] key value = [(key,value)]
updateMemo ((k,v):kvs) key newValue
   | k == key = (k,newValue):kvs
   | otherwise = (k,v):updateMemo kvs key newValue

