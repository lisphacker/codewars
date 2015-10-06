module PointInPolygon where

type Point = (Double, Double)
x p = fst p
y p = snd p

tol = 0.0001
diff p1 p2 = ((x p1) - (x p2), (y p1) - (y p2))

vmod p = sqrt ((x p) * (x p) + (y p) * (y p))
vdot p1 p2 = (x p1) * (x p2) + (y p1) * (y p2)

angle p1 p2 = acos ((vdot p1 p2) / ((vmod p1) * (vmod p2)))

feq x y = abs (x - y) < tol

pointWithinAngle p1 root p2 t = let sp1 = diff p1 root
                                    sp2 = diff p2 root
                                    st  = diff t root
                                in 
                                  feq ((angle sp1 st) + (angle st sp2)) (angle sp1 sp2)

pointInTriangle a b c t = (pointWithinAngle a b c t) && (pointWithinAngle b c a t)

pointInPoly :: [Point] -> Point -> Bool
pointInPoly poly point = let root = head poly
                             l1 = init $ drop 1 poly
                             l2 = drop 2 poly
                          in any (\(p1, p2) -> pointInTriangle p1 root p2 point) $ zip l1 l2
