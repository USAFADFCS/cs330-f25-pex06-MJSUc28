-- pex6.hs 
-- unKnot Haskell

-- name: Michael Ulery

{- DOCUMENTATION: I used C2C Smith, Gavin's testing code that was published in the teams chat to rest my implentation. 
I did not recieve any other help or use any other resources.
-}
unKnot :: [(Char, Char)] -> String
unKnot [] = "unknot"
unKnot tripCode = if (listNumElem (unKnot2HelperInclusive (tripCode)) < listNumElem tripCode) ||
   (listNumElem (unKnot1HelperInclusive (tripCode)) < listNumElem tripCode)
   then unKnot (unKnotManager tripCode) 
   else "tangle - resulting trip code: " ++ (show tripCode)
   



unKnotManager :: [(Char, Char)] -> [(Char, Char)]
unKnotManager tripCode = if (listNumElem (unKnot2HelperInclusive (tripCode))) < (listNumElem (unKnot1HelperInclusive (tripCode)))
   then unKnot2HelperInclusive (tripCode)
   else unKnot1HelperInclusive (tripCode)



unKnot1HelperInclusive :: [(Char, Char)] -> [(Char, Char)]
unKnot1HelperInclusive tripCode = if (listNumElem (unKnot1Helper (tail (tripCode) ++ [head (tripCode)]) [])) < (listNumElem (unKnot1Helper (tripCode) []))
   then unKnot1Helper (tail (tripCode) ++ [head (tripCode)]) []
   else unKnot1Helper (tripCode) []

unKnot2HelperInclusive :: [(Char, Char)] -> [(Char, Char)]
unKnot2HelperInclusive tripCode = if (listNumElem (unKnot2Looper (tail (tripCode) ++ [head (tripCode)]) [])) < (listNumElem (unKnot2Looper (tripCode) []))
   then unKnot2Looper (tail (tripCode) ++ [head (tripCode)]) []
   else unKnot2Looper (tripCode) []

--This function uses unKnot2Helper in order to check every reasonable pair combination in a list.
--This function takes in a todolist that is works through,
--a done list that it keeps results in,
--and returns that done list as a base case.
unKnot2Looper :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
unKnot2Looper [] donelist = donelist
unKnot2Looper todolist donelist = if (listNumElem(todolist) >= 4) &&
   (listNumElem (unKnot2Helper (head(todolist), head (tail(todolist))) (tail(tail(todolist))) []) < (-3 + listNumElem (todolist)))
   then (donelist ++ unKnot2Helper (head(todolist), head (tail(todolist))) (tail(tail(todolist))) [])
   else unKnot2Looper (tail (todolist)) (donelist ++ [head(todolist)])

--This function uses type 2 knot solving by checking a pair of 2 tuples against a list.
--This function takes in a tuple of two tuples as a "checking pair",
--a current list that it has to sort through,
--a saved list that it keeps results in,
--and returns the saved list as a base result.
--Furthermore, it checks to make sure that enough elements are left in the list to check
unKnot2Helper :: ((Char, Char), (Char, Char)) -> [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
unKnot2Helper pairOf2 [] [] = []
unKnot2Helper pairOf2 [] saved = saved
unKnot2Helper pairOf2 current saved =if (listNumElem (current)) >= 2 &&
   ((fst(fst(pairOf2)) == fst(head(current)) && snd(fst(pairOf2)) /= snd(head(current)) &&
   fst(snd(pairOf2)) == fst(head(tail(current))) && snd(snd(pairOf2)) /= snd(head(tail(current)))
   ) ||
   (fst(snd(pairOf2)) == fst(head(current)) && snd(snd(pairOf2)) /= snd(head(current)) &&
   fst(fst(pairOf2)) == fst(head(tail(current))) && snd(fst(pairOf2)) /= snd(head(tail(current)))
   )) && (snd(fst(pairOf2)) == snd(snd(pairOf2)))
   then unKnot2Helper pairOf2 [] (saved ++ (drop 2 current))
   else unKnot2Helper pairOf2 (tail current) (saved ++ [head current])


--using type 1 knot solving, take in a list called current to sort through,
--a list called saved that it keeps track of what it has gone through,
--and returns that saved list as a base case
unKnot1Helper :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
unKnot1Helper [] [] = []
unKnot1Helper [] saved = saved
unKnot1Helper current saved = if (listNumElem (current)) >= 2 && 
   fst (head (current)) == fst (head (tail (current))) && 
   snd (head (current)) /= snd(head(tail (current))) 
   then unKnot1Helper (drop 2 current) (saved)
   else unKnot1Helper (tail current) (saved ++ [head current])

--counts how many items are in a list
listNumElem :: Num a => [(Char, Char)] -> a
listNumElem [] = 0
listNumElem list = 1 + listNumElem (tail(list))

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

--save this for potential use
-- typeIknot :: [(Char, Char)] -> String
-- typeIknot tripCode