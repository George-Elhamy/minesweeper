type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] [Char] MyState deriving (Show,Eq)
 
up :: MyState -> MyState 
up Null = Null 
up (S (x,y) cells lastAction state) = (S (x-1,y) cells ['u','p'] previous) 
									where previous = S (x,y) cells lastAction state
							
down:: MyState -> MyState
down Null = Null 
down (S (x,y) cells lastAction state) = (S (x+1,y) cells ['d','o','w','n'] previous) 
									  where previous = S (x,y) cells lastAction state	

left :: MyState -> MyState
left Null = Null 
left (S (x,y) cells lastAction state) = (S (x,y-1) cells ['l','e','f','t'] previous) 
									  where previous = S (x,y) cells lastAction state										  
								
right :: MyState -> MyState
right Null = Null 
right (S (x,y) cells lastAction state)= (S (x,y+1) cells ['r','i','g','h','t'] previous) 
									   where previous = S (x,y) cells lastAction state			

collect :: MyState -> MyState 
collect Null = Null 
collect (S (x,y) [(a,b)] lastAction state) = (S (x,y) [] ['c','o','l','l','e','c','t'] previous)  							  	
										where previous = (S (x,y) [(a,b)] lastAction state)	
				
		
clear ::[MyState] -> [MyState]	
clear [] = []
clear (a:t)= if a==Null then clear t 
						else a:clear t

						
nextMyStates:: MyState ->[MyState]
nextMyStates a = clear (up a:down a:left a:right a:[collect a])
											   
											   
isGoal:: MyState -> Bool 
isGoal	(S (x,y) cells lastAction state) = if cells==[] then True 
														else False 
														
onebyone :: MyState -> MyState 
onebyone (S (x,y) cells lastAction state) = 
	if isGoal (S (x,y) cells lastAction state)
		then route (S (x,y) cells  lastAction state)
			else onebyone (S (xr,yr) remaining lastActionr  stater)
			where {(S (xr,yr) cellsr lastActionr stater) = (route (S (x,y) [now] lastAction state));
					now = nearest (x,y) cells (-1,-1);
					remaining = removeItem now cells}

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys


					
distance :: Num a => (a,a) -> (a,a) -> a 
distance (x,y) (a,b) = abs (x-a) + abs (y-b)



nearest :: (Num a, Ord a) => (a,a) -> [(a,a)] -> (a,a) -> (a,a) 
nearest position [] acc = acc 
nearest position (f:t) acc | distance position f <= distance position acc = nearest position t f  
						   | otherwise =  nearest position t acc 	

						   
						 	
route :: MyState -> MyState
route (S (x,y) [] lastAction state) = (S (x,y) [] lastAction state)  
route  (S (x,y) [(a,b)] lastAction state) | x<a = route (down me)
										  | x>a = route (up me)
										  | x==a = if y<b then route (right me)
															    else if y>b then route (left me)
																				 else collect me
										   where me = (S (x,y) [(a,b)] lastAction state)										 
												
										
constructSolution:: MyState ->[String]
constructSolution (S (x,y) cells lastAction Null) = []
constructSolution (S (x,y) cells lastAction state)	= constructSolution state ++[lastAction] 

clearSolution [] = []
clearSolution (a:t)= if a=="" then clearSolution t 
						else a:clearSolution t
	


solve :: Cell->[Cell]->[String]
solve cell mines = constructSolution (onebyone (S cell mines "" Null) )
			  

											   
