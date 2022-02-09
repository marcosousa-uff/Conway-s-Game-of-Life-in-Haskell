avalia v m z "v" =
 if(z >=2)
  then ["z"]
 else
  if(z <2 && v <3)
   then ["m"]
  else
   if(v > 4 && z ==0)
    then ["m"]
   else ["v"]
avalia v m z "m" =
 if(v == 3)
  then ["v"]
 else ["m"]
avalia v m z "z" =
 if(v == 0)
  then ["m"]
 else ["z"]

countElems n (x:xs) = fromEnum (n == x) + countElems n xs
countElems _ []     = 0

forma2(x:xs) = take 2 x ++ forma2 xs
forma2 [] = []
forma3(x:xs) = take 3 x ++ forma3 xs
forma3 [] = []

muda22:: [[String]] -> [String]
muda22(x:xs) =  a where
    f = forma2(x:take 1 xs)
    celula = head x
    v = countElems "v" f
    m = countElems "m" f
    z = countElems "z" f
    a = avalia v m z celula
muda22 [] = []

tails (x:xs) = tail x :tails xs
tails [] = []

revTail:: [[String]] -> [[String]]
revTail (x:xs) = reverse(x) : revTail(xs)
revTail [] = []

muda23:: Int -> [[String]] -> [String]
muda23 0 x = []
muda23 n (x:xs) = a ++ muda23 (n-1) (tail x: tails xs)  where
    f = forma3(x:take 1 xs)
    celula = head (tail x)
    v = countElems "v" f
    m = countElems "m" f
    z = countElems "z" f
    a = avalia v m z celula
muda23 _ [] = []

muda2:: [[String]] -> [String]
muda2 (x:xs) = a where
    n = length x
    primeira = muda22(x:xs)
    meio = muda23 (n-2) (x:xs)
    r = revTail(x:xs)
    ultima = muda22(r)
    a = primeira ++ meio ++ ultima
muda2 [] =[]

muda32 :: [[String]] -> [String]
muda32 (x:xs) = a where
    f = forma2(x:take 2 xs)
    celula = head (head xs)
    v = countElems "v" f
    m = countElems "m" f
    z = countElems "z" f
    a = avalia v m z celula
muda32 [] = []

muda33 :: Int -> [[String]] -> [String]
muda33 0 x = []
muda33 n (x:xs) = a ++ muda33 (n-1) (tail x: tails xs)  where
    f = forma3(x:take 2 xs)
    celula = head (tail (head xs))
    v = countElems "v" f
    m = countElems "m" f
    z = countElems "z" f
    a = avalia v m z celula
muda33 _ [] = []

muda3:: Int -> [[String]] -> [[String]]
muda3 0 x = []
muda3 linha (x:xs) = a : muda3 (linha -1) xs where
    n = length x
    nZero = if n < 2 then 2
      else n
    primeira = muda32(x:xs)
    meio = muda33 (nZero-2) (x:xs)
    r = revTail(x:xs)
    ultima = muda32(r)
    a = primeira ++ meio ++ ultima
muda3 _ [] =[]

mudaTabuleiro:: [[String]] -> [[String]]
mudaTabuleiro (x:xs)= a where
    n = length (x:xs)
    nZero = if n < 2 then 0
      else n -2
    primeira = muda2(x:xs)
    meio = muda3 (nZero) (x:xs)
    r = reverse(x:xs)
    ultima = muda2(r)
    a =if n < 3 then (primeira:[]) ++ (ultima:[])
      else (primeira:[]) ++ meio ++ (ultima:[])
mudaTabuleiro [] =[]

resolve::Int -> [[String]] -> ([[String]],Int)
resolve 0 x = (x,0)
resolve n (x:xs) = a  where
    l = mudaTabuleiro(x:xs)
    a = if l == (x:xs)
            then (l,n)
        else resolve (n-1) l
resolve _ [] = ([],0)

mudacoluna2:: [[String]] -> [String]
mudacoluna2(x:xs) =  a where
    f = forma2(x:take 1 xs)
    celula = head x
    v = countElems "v" f
    m = countElems "m" f
    z = countElems "z" f
    a = avalia v m z celula
mudacoluna2 [] = []

mudacoluna3:: Int -> [[String]] -> [[String]]
mudacoluna3 0 x = []
mudacoluna3 n (x:xs) = a : mudacoluna3 (n-1) xs  where
    f = forma3(x:take 2 xs)
    celula = head(head xs)
    v = countElems "v" f
    m = countElems "m" f
    z = countElems "z" f
    a = avalia v m z celula
mudacoluna3 _ [] = []

mudacoluna::[[String]] -> [[String]]
mudacoluna (x:xs) = a where
    n = length (x:xs)
    nZero = if n < 2 then 0
      else n -2
    primeira = mudacoluna2(x:xs)
    meio = mudacoluna3 (nZero) (x:xs)
    r = reverse(x:xs)
    ultima = mudacoluna2(r)
    a =if n < 3 then (primeira:[]) ++ (ultima:[])
      else (primeira:[]) ++ meio ++ (ultima:[])
mudacoluna [] =[]

mudalinha2:: [String] -> [String]
mudalinha2(x:xs) =  a where
    f = take 2 x:xs
    celula = x
    v = countElems "v" f
    m = countElems "m" f
    z = countElems "z" f
    a = avalia v m z celula
mudalinha2 [] = []

mudalinha3:: Int -> [String] -> [String]
mudalinha3 0 x = []
mudalinha3 n (x:xs) = a ++ mudalinha3 (n-1) xs  where
    f = take 3 x:xs
    celula = head(xs)
    v = countElems "v" f
    m = countElems "m" f
    z = countElems "z" f
    a = avalia v m z celula
mudalinha3 _ [] = []

mudalinha::[[String]] -> [[String]]
mudalinha (x:xs) = a where
    n = length (x)
    nZero = if n < 2 then 0
      else n -2
    primeira = mudalinha2(x)
    meio = mudalinha3 (nZero) (x)
    r = reverse(x)
    ultima = mudalinha2(r)
    b =if n < 3 then (primeira) ++ (ultima)
      else (primeira) ++ meio ++ (ultima)
    a = b:[]
mudalinha [] =[]

resolve2::Int -> [[String]] -> ([[String]],Int)
resolve2 0 x = (x,0)
resolve2 n (x:xs) = a where
  l = if length x == 1 then
        mudacoluna(x:xs)
      else
        mudalinha(x:xs)
  a = if l == (x:xs)
        then (l,n)
      else resolve2 (n-1) l
resolve2 _[] = ([],0)

resolve3::Int -> [[String]] -> ([[String]],Int)
resolve3 0 x = (x,0)
resolve3 n (x:xs) = a where
  l = [["m"]]
  a = if l == (x:xs)
        then (l,n)
      else resolve3 (n-1) l
resolve3 _[] = ([],0)

mostraLista:: [[String]] -> IO()
mostraLista(x:xs) = do
  print x
  mostraLista xs
mostraLista [] = putStrLn "--------------------------------------------" 

main = do
 putStrLn "insira a quantidade de linhas da tabela: "
 input1 <- getLine
 putStrLn "insira a quantidade de colunas da tabela: " 
 input2 <- getLine 
 putStrLn "insira uma lista de listas de string contendo as letras v,m e z representando os estados vivo,morto e zumbi respectivamente: " 
 input3 <- getLine
 putStrLn "insira o numero maximo de iterações: " 
 input4 <- getLine
 let linha = (read input1 :: Int)
 let coluna = (read input2 :: Int)
 let tabela = (read input3 :: [[String]])
 let tLinha = length tabela
 let tColuna = length (head tabela)
 let numero = (read input4 :: Int)
 let tupla = if(linha ==1 && coluna ==1) then resolve3 numero tabela
              else
                if(linha ==1 || coluna ==1) then resolve2 numero tabela
                else resolve numero tabela
 let a = fst (tupla)
 let b = snd (tupla)
 let c = (numero - b)
 if( linha /= tLinha || coluna /= tColuna)
    then putStrLn"a tabela foi inserida com tamanho errado"
 else do
    putStrLn " lista final :"
    mostraLista a
    if b == 0 then putStrLn "a tabela não ficou estavel após: "
    else putStrLn "a tabela ficou estavel após:"
    print c
    putStrLn "iterações"
    putStrLn""
    putStrLn "final do programa"


 