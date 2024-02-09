let c = (constructor $ parent t) in 
case (isUse c, isDecl c, isScope c, isOpen c, c) of 
   (True, _, _, _, _) -> 
   (_, True, _, _, _) -> 
   (_, _, True, _, _) ->
   (_, _, _, True, _) -> 
   (_, _, _, _, CRoot) -> 
   (_, _, _, _, CObama) -> 
  (...) 


  