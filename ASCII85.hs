module ASCII85 where
import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

dropFromEnd n = reverse . drop n . reverse

byteStrToIntList :: ByteString -> [Integer]
byteStrToIntList s = if B.length s == 0 then
                         []
                     else
                         (fromIntegral (B.head s)):(byteStrToIntList (B.tail s))

intListToByteString :: [Integer] -> ByteString
intListToByteString = C.pack . (map (chr . fromIntegral))


pad :: Integer -> Integer -> [Integer] -> ([Integer], Int)
pad n p = padRec 0
    where padRec c []     = (take c $ repeat p, c)
          padRec c (x:xs) = let (padded, padLen) = padRec (if c == 0 then (fromIntegral n) - 1 else c - 1) xs
                            in (x:padded, padLen)

pad4 = pad 4 0
pad5 = pad 5 117

asc256ToAsc85 :: [Integer] -> [Integer]
asc256ToAsc85 []               = []
asc256ToAsc85 (0:0:0:0:bs)     = [122] ++ (asc256ToAsc85 bs)
asc256ToAsc85 (0:0:0:[])       = [33,33,33,33]
asc256ToAsc85 (0:0:[])         = [33,33,33]
asc256ToAsc85 (0:[])           = [33,33]
asc256ToAsc85 (b0:b1:b2:[])    = dropFromEnd 1 $ asc256ToAsc85 (b0:b1:b2:0:[])
asc256ToAsc85 (b0:b1:[])       = dropFromEnd 2 $ asc256ToAsc85 (b0:b1:0:0:[])
asc256ToAsc85 (b0:[])          = dropFromEnd 3 $ asc256ToAsc85 (b0:0:0:0:[])
asc256ToAsc85 (b0:b1:b2:b3:bs) = (cnvt (b0:b1:b2:b3:[])) ++ (asc256ToAsc85 bs)
    where cnvt bs = let w32 = foldl (\z x -> z * 256 + x) 0 bs
                    in (reverse . w32ToA85 5) w32
                        where w32ToA85 c 0 = take c $ repeat 33
                              w32ToA85 c n = let (q, r) = quotRem n 85
                                             in (r + 33):(w32ToA85 (c - 1) q)

asc85ToAsc256 :: [Integer] -> [Integer]
asc85ToAsc256 []                  = []
asc85ToAsc256 (122:bs)            = [0,0,0,0] ++ (asc85ToAsc256 bs)
asc85ToAsc256 (b0:b1:b2:b3:[])    = dropFromEnd 1 $ asc85ToAsc256 (b0:b1:b2:b3:117:[])
asc85ToAsc256 (b0:b1:b2:[])       = dropFromEnd 2 $ asc85ToAsc256 (b0:b1:b2:117:117:[])
asc85ToAsc256 (b0:b1:[])          = dropFromEnd 3 $ asc85ToAsc256 (b0:b1:117:117:117:[])
asc85ToAsc256 (b0:[])             = dropFromEnd 4 $ asc85ToAsc256 (b0:117:117:117:117:[])
asc85ToAsc256 (b0:b1:b2:b3:b4:bs) = (cnvt (b0:b1:b2:b3:b4:[])) ++ (asc85ToAsc256 bs)
    where cnvt bs = let w32 = foldl (\z x -> z * 85 + (x - 33)) 0 bs
                    in (reverse . w32ToA256 4) w32
                        where w32ToA256 c 0 = take c $ repeat 0
                              w32ToA256 c n = let (q, r) = quotRem n 256
                                              in r:(w32ToA256 (c - 1) q)

prefix = map (fromIntegral . ord) "<~" :: [Integer]
suffix = map (fromIntegral . ord) "~>" :: [Integer]

toAscii85 :: ByteString -> ByteString
toAscii85 = intListToByteString . (flip (++)) suffix . (++) prefix . asc256ToAsc85 . byteStrToIntList

fromAscii85 :: ByteString -> ByteString
fromAscii85 = intListToByteString . asc85ToAsc256 . dropFromEnd 2 . drop 2 . byteStrToIntList . B.filter (not . isSpace . chr . fromIntegral)

in1 = "<~!!~>"
inter1 = fromAscii85 $ C.pack "<~!!~>"
out1 = toAscii85 inter1

in2 = "whitespace test"
