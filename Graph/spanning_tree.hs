unionFind :: (Eq n, Ord n) => (n,n) -> Table n n -> (Bool,Table n n)
unionFind (x,y) t =
  --let xv = findTable t x
  --    yv = findTable t y
  let xv = findRoot t x 
      yv = findRoot t y
  in
    if (xv == yv)
    then (False,t)
    else (True,updTable (if yv<xv then (x,yv) else (y,xv)) t)

findRoot:: (Eq n) => Table n n -> n -> n
findRoot t x = let v = findTable t x
               in if v == x then v
               else findRoot t v

fillPQ :: (Ord n, Ord w, Ord c) => [(n,w,c)] -> PQueue (c,n,w) -> PQueue (c,n,w)
fillPQ [] pq           = pq
fillPQ ((x,y,w):es) pq = fillPQ es (enPQ (w,x,y) pq)

kruskal :: (Num w, Ix n, Ord w) => Graph n w -> [(w,n,n)]
kruskal g = kruskal' (fillPQ (edgesU g) emptyPQ)
                     (newTable [(x,x) | x<- nodes g])
                     [] 1
   where n             = length (nodes g)
         kruskal' pq t mst i 
             | i==n    = mst
             | otherwise = let e@(_,x,y)    = frontPQ pq
                               pq'          = dePQ pq
                               (updated,t') = unionFind (x,y) t
                           in if updated
                              then kruskal' pq' t'(e:mst) (i+1)
                              else kruskal' pq' t  mst     i
