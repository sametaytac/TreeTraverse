data Tree = Empty | Node Integer [Tree] deriving (Show)
samet pre post (x:xs) a bos=
			if bos==[] then samet pre post ((xs)++[99]) a (bos++[3])
			else if length xs/=0 then a++preord x pre post []  ++ samet pre post xs a bos				
			else a
			
bul:: [[Integer]]->[Integer]->[[Integer]]->[Integer]->[[Integer]]
bul [] b bos bos2 =
				[b]
bul (x:xs) b bos bos2=
			
			if null xs && length bos2/=0 then  bos++[x]
			else if null xs && length bos2==0 then
					if null b then bos++[x] 
					else if head x==head b then bos++[x++tail b] 
					else bos++[x]++[b]
			else if head x==head b then bul xs b (bos++[x++tail b]) [2] 
		    else bul xs b (bos++[x]) bos2 
		
birles:: [Integer]->[Integer]->[[Integer]]->[Integer]->[[Integer]]->[[Integer]]
birles pre post bos bos3 bos2=
				if null bos && null bos3 then birles pre post (samet pre post pre bos []) bos3 bos2
				else if null bos then bos2
				else if length bos ==1 then birles pre post [] [3] (bul bos2 (head bos) [] [])
				else birles pre post (tail bos) bos3 (bul bos2 (head bos) [] [])
constructTree::[Integer]->[Integer]->Tree
constructTree pre post =
						fonk1 (birles pre post [] [] []) (birles pre post [] [] []) (head(head (birles pre post [] [] [])))
fonk1 (x:xs) b a =
				if null xs then
							if (head x)==a then Node a [fonk1 b b k| k<- (tail x)]
							else Node a [Empty]
				else if (head x)==a then Node a [fonk1 b b k | k<- (tail x)]
				else fonk1 xs b a

preord cocuk (x:xs) post boss=
								
								if cocuk == x then [[bababul cocuk boss post,x]]
								
								else	preord cocuk xs post (boss++[x]) 


		
bababul cocuk list post =
						if isChild cocuk (last list) post == True then last list
						else bababul cocuk (init list) post


isChild a b (x:xs)=
				if x==a then True
				else if x==b then False
				else isChild a b xs
						
isEmpty []= 1
isEmpty _=0 

								
				