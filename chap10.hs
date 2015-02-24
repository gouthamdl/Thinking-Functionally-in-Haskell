newtype Parser a = Parser (String -> [(a,String)])

instance Monad Parser where
    return x = Parser (\s -> [(x,s)])
    p >>= q = Parser (\s -> [(y,s'') | (x,s') <- apply p s, (y,s'') <- apply (q x) s'])

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) s = p s

getc :: Parser Char
getc = Parser f
       where f [] = []
             f (c:cs) = [(c,cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- getc
  if p c then return c
  else fail'

fail' = Parser (\s -> [])                


type State s a = s -> (a, s)
returnState :: a -> State s a
returnState a = \s -> (a,s)
                
bindState :: State s a -> (a -> State s b) -> State s b
bindState state f = \s -> let (a,s') = state s in f s'
