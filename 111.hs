type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] [Char] MyState deriving (Show,Eq)
 
up :: MyState -> MyState 
up Null = Null 
up (S (x,y) cells lastAction state) | x==0 = Null 
									| otherwise = (S (x-1,y) cells ['u','p'] previous) 
									where previous = S (x,y) cells lastAction state
							
down:: MyState -> MyState
down Null = Null 
down (S (x,y) cells lastAction state) | x==3 = Null 
									  | otherwise = (S (x+1,y) cells ['d','o','w','n'] previous) 
									  where previous = S (x,y) cells lastAction state	

left :: MyState -> MyState
left Null = Null 
left (S (x,y) cells lastAction state) | y==0 = Null 
									  | otherwise = (S (x,y-1) cells ['l','e','f','t'] previous) 
									  where previous = S (x,y) cells lastAction state										  
								
right :: MyState -> MyState
right Null = Null 
right (S (x,y) cells lastAction state) | y==3 = Null 
									   | otherwise = (S (x,y+1) cells ['r','i','g','h','t'] previous) 
									   where previous = S (x,y) cells lastAction state			

collect :: MyState -> MyState 
collect Null = Null 
collect (S (x,y) ((a,b):t) lastAction state)
		| not (elem (x,y) ((a,b):t)) = Null 
		| a==x && b==y = (S (x,y) t ['c','o','l','l','e','c','t'] previous)  							  
		| otherwise = collect (S (x,y) (t++[(a,b)]) lastAction state)	
		where previous = (S (x,y) ((a,b):t) lastAction state)	
				
		
clear ::[MyState] -> [MyState]	
clear [] = []
clear (a:t)= if a==Null then clear t 
						else a:clear t

						
nextMyStates:: MyState ->[MyState]
nextMyStates a = clear (up a:down a:left a:right a:[collect a])
											   
											   
isGoal:: MyState -> Bool 
isGoal	(S (x,y) cells lastAction state) = if cells==[] then True 
														else False 
														
search :: [MyState] -> MyState 
search (a:t)= if isGoal a then a 
						  else search (t++ nextMyStates a)
						 					  					  
constructSolution:: MyState ->[String]
constructSolution (S (x,y) cells lastAction Null) = []
constructSolution (S (x,y) cells lastAction state)	= constructSolution state ++[lastAction] 	

solve :: Cell->[Cell]->[String]
solve cell mines = constructSolution (search [(S cell mines "" Null)] )
			  

											   
