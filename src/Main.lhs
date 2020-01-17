  > {-# LANGUAGE ExtendedDefaultRules #-}

> module Main where
> import Data.Complex
> import Data.Time
> import Data.Char
> import Graphics.Image
> import Control.Concurrent

> type C = Complex Double


> maxiter :: Int
> maxiter = 50
> width   = 1600
> widtho4 = fi width /2
> height = 1600
> heighto4 = fi height /2

Let's build a quadratic function from a parameter $c$:

> quad :: C -> (C -> C)
> quad c = \x -> x^2 + c


> modulus = Data.Complex.magnitude

> iterat :: (C -> C) -> C -> Int -> Either () Int
> iterat f z n = if n == maxiter
>                then Left ()
>                else let z' = f z 
>                     in if   modulus z' >= 2.0
>                        then Right n
>                        else iterat f z' (n+1)     

> eval c = iterat (quad c) (0.0 :+ 0.0) 0 

> pixToC :: (Int, Int) -> C
> pixToC (i, j) = let iv = fi i / widtho4 -- ojo que van en terminos de width y heigth
>                     jv = fi j / heighto4 
>                 in ((-1.3) :+ 1.3) + (jv :+ (-iv))


> fi = fromIntegral

> toColor :: Either () Int -> Pixel RGB Double
> toColor (Left ()) = PixelRGB 0.0 0.0 0.0
> toColor (Right n) = PixelRGB (pon1 n / 3) (pon1 n) (pon2 n)
>   where pon1 n = (sqrt $ (fi n / fi maxiter))  :: Double
>         pon2 n = ((fi (maxiter - n) / fi maxiter)) :: Double



> main = forkIO(mkImage) >> 
>        getCurrentTime >>= \t ->
>        return ((filter (not . isSpace) . show ) t) >>= \s ->
>        putStrLn s >>
>        let img = makeImage (width,height) ( toColor .eval . pixToC ) :: Image VU RGB Double
>
>        in writeImage ("./output/image" ++ s ++ ".png") img
>        >>
>        getCurrentTime >>= \t ->
>        return ((filter (not . isSpace) . show ) t) >>= \s ->
>        putStrLn s >>
>        let img = makeImage (width,height) ( toColor . eval .(\x -> 1/x) . pixToC ) :: Image VU RGB Double
>
>        in writeImage ("./output/image" ++ s ++ ".png") img


> mkImage =
>        getCurrentTime >>= \t ->
>        return ((filter (not . isSpace) . show ) t) >>= \s ->
>        putStrLn s >>
>        let img = makeImage (width,height) ( toColor . eval .(\x -> x^2) . pixToC ) :: Image VU RGB Double
>
>        in writeImage ("./output/image" ++ s ++ ".png") img
>        >>
>        getCurrentTime >>= \t ->
>        return ((filter (not . isSpace) . show ) t) >>= \s ->
>        putStrLn s >>
>        let img = makeImage (width,height) ( toColor . eval .(\x -> sqrt x) . pixToC ) :: Image VU RGB Double
>
>        in writeImage ("./output/image" ++ s ++ ".png") img
