 data Tree a = Leaf | Branch a (Tree a) (Tree a)  
 -- | Produces a list of pairs, where the first element of each pair is a
 --   value in the tree, and the second element is the depth at which that
 --   value appears.
 --   
 --   Examples:
 --   
 --   >>> depths Leaf
 --   []
 --   
 --   >>> depths (Node True Leaf Leaf)
 --   [(True,0)]
 --   
 --   >>> depths (Node 'a' (Node 'b' Leaf (Node 'c' Leaf Leaf)) Leaf)
 --   [('a',0),('b',1),('c',2)]
 --   
 --   >>> depths (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))
 --   [(1,0),(2,1),(3,1)]
 --  
 depths :: (a -> b) -> Tree a -> Tree b
 depths f = g 
	where
	g (Leaf a) = Leaf (f a)
	g (Branch left right) = Branch (g left) (g right)

