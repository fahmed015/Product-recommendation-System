import DataFile


append [] =[]
append (x:xs) = x ++ append xs

getItem :: Int-> String
getItem x = items !! x

isMember :: Eq a => a -> [a] -> Bool
isMember n [] = False 
isMember n (x:xs) = if n==x
					then True
					else isMember n xs 
					
count :: Eq a => a ->[a]-> Int
count _ []=0
count a (x:xs)= if a==x
				then 1+  count a xs 
				else count a xs
					
addPairtoList a [] = [a]
addPairtoList (i, c) ((item, count):xs) = if item == i then (item, count + c):xs
														else (item,count):addPairtoList (i, c) xs
														
addLists [] list = list
addLists (x:xs) list = addLists xs (addPairtoList x list)
--findAndCount :: [a] -> c-> [(a, [b])]

findAndCount _ [] = []
findAndCount e (x:xs)  = if isMember e x 
				then  addLists (write uniqueList deleted) (findAndCount e xs)
				else findAndCount e xs
				where
					deleted = delete e x
					uniqueList = unique deleted

unique [] = []
unique (x:xs) = if elem x xs then unique xs else x:unique xs


		
--write ::[[Char]] -> [([Char],Int)]
write [] _ = []
write (x:xs) list =		[(x,(count x list))]++ write xs list


delete a [] =[]
delete a (x:xs) = if a==x
				  then delete a xs
				  else [x] ++ delete a xs 
				  
--getUsersStatsH :: (a, [[a]]) -> (a, [(a,[(a,x)])]
getUsersStatsH  purchasesH item=  if isMember (item)  (append purchasesH)
								 then( item,  findAndCount item purchasesH)
								 else (item,[])

getUsersStats2 (user, l) n = if n==(length items) then []
							else(getUsersStatsH (l) (getItem n):getUsersStats2 (user,l) (n+1))

getUsersStats (user, l) = (user, getUsersStats2 (user, l) 0)

--get all users stats
	
getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats []=[]
getAllUsersStats(user:users) = 	[getUsersStats(user)] ++ getAllUsersStats(users)

--createEmptyFreqList																	
createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList (x:xs) = [(x,[ ])] ++ createEmptyFreqList xs
createEmptyFreqList []=[]


-- remove a user from purchases
deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (x:xs)
   | i == 0    = xs
   | otherwise = x : deleteN (i-1) xs

findIndex _ [] _= error "user not found"	
findIndex userf ((userh,l):xs) c= if userf==userh then c else findIndex userf (xs) (c+1)

deleteUser userf = deleteN (findIndex userf purchasesHistory 0) purchasesHistory


--get a user from purchases
getList userf [] = error "user not found"										
getList userf  ((f,x):px) =      if  userf == f         then  getUsersStats (f,x)
                                                         else getList userf px



--purchases Intersection
purchasesIntersection :: Eq a => [(a, [(a, Int) ]) ] -> [(a, [(a, [(a, Int) ]) ]) ] -> [[(a, [(a, Int) ]) ]]



purchasesIntersection	_ []=[]												
purchasesIntersection firstuser ((a,b):xs) = [purchasesIntersection1 firstuser  b] ++ purchasesIntersection firstuser xs
										
purchasesIntersection1 [][]=[]											      
purchasesIntersection1 ((element, listofelements):xs) ((i,listofi):xs3) = if (length listofelements ==0) || (length listofi ==0)
											  then purchasesIntersection1 xs xs3
											  else [(element,addLists (listofelements) (listofi))]++ purchasesIntersection1 xs xs3

--freq List Users

freqListUsers firstuser =   intersectionlists (purchasesIntersection  (snd (getList firstuser purchasesHistory))  (getAllUsersStats (deleteUser firstuser)))
							
							
intersectionlists [] =[]								  
intersectionlists (x:xs) = addLists  (submerge x) (intersectionlists xs)

submerge [] =[]
submerge ((element,listofelements):xs) = addLists listofelements (submerge xs)


--Recommend Based On Users
expandFreqListUsers []=[]
expandFreqListUsers ((item1,n):xs) = repeatItems item1 n ++ expandFreqListUsers xs

repeatItems _ 0=[]
repeatItems item1 n = [item1]++repeatItems item1 (n-1)													  
													  
recommendBasedOnUsers :: String -> String

recommendBasedOnUsers user =if freqListUsers user ==[] then "" 
							else (expandFreqListUsers(freqListUsers user)) !! (randomZeroToX ( (length (expandFreqListUsers( freqListUsers user)))-1) )


--freqListItems
freqListItems:: String -> [(String, Int)]
freqListItems user =freqListItems2 (snd (getList user purchasesHistory))
freqListItems2 []=[]
freqListItems2 (x:xs) = addLists (snd x) (freqListItems2 xs)


--freqListCart
freqListCart:: String ->[String] -> [(String, Int)]
freqListCart _ []=[]
freqListCart user (x:xs) = addLists (getItemList x (snd(getList user purchasesHistory)) ) (freqListCart user xs)

getItemList _ []=error "item in cart not found"
getItemList item (x:xs) = if item == fst x then snd x else getItemList item xs


--freqListCartAndItems 
freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems user newCart = addLists (freqListItems user) (freqListCart user (newCart))


--recommendBasedOnItemsInCart
recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart user newCart = if expandFreqListUsers(freqListItems user)==[] then ""
											else expandFreqListUsers( freqListCartAndItems user newCart) !!(randomZeroToX ( (length (expandFreqListUsers(freqListCartAndItems user newCart)))-1) )

--recommendEmptyCart
recommendEmptyCart :: String -> String
recommendEmptyCart user = if expandFreqListUsers(freqListItems user)==[] 
						  then ""
						  else (expandFreqListUsers(freqListItems user)) !! (randomZeroToX ( (length (expandFreqListUsers( freqListItems user)))-1) )

--recommend						  
recommend :: String -> [String] -> String
recommend user cart = if (item1=="" && item2=="") then items !! randomZeroToX ((length items) -1)
					else if item2 == "" then item1
					else if item1 == "" then item2
					else [(item1),(item2)]	!! randomZeroToX 1
					where 
						item2 = recommendBasedOnItemsInCart user cart
						item1 = recommendBasedOnUsers user
