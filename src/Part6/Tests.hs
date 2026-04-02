module Part6.Tests where

import qualified Data.Map

import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Part6.Tasks

unit_eye = do
    eye 1 @?= one
    eye 1 @?= [[one]]
    eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
    eye 2 @?= [[one, 0], [0, one]]
    eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])

    where one :: Int; one = 1

unit_zero = do
    zero 1 1 @?= zz
    zero 2 1 @?= [[zz, zz]]
    zero 2 2 @?= [[zz, zz], [zz, zz]]
    zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([]::[((Int, Int), Int)]))
    where zz :: Int; zz = 0

unit_getRow = do
    getRow 0 v @?= [1]
    getRow 0 lst @?= [1, 2, 3]
    getRow 1 lst @?= [4, 5, 6]
    getRow 0 mx @?= [1, 0, 0]
    getRow 1 mx @?= [0, 2, 0]
    getRow 2 mx @?= [0, 0, 0]
    where v = 1 :: Int
          lst = [[1, 2, 3], [4, 5, 6]] :: [[Int]]
          mx = SparseMatrix 3 3 (Data.Map.fromList [((0, 0), 1 :: Int), ((1, 1), 2 :: Int)])

unit_getCol = do
    getCol 0 v @?= [1]
    getCol 0 lst @?= [1, 4]
    getCol 1 lst @?= [2, 5]
    getCol 2 lst @?= [3, 6]
    getCol 0 mx @?= [1, 0, 0]
    getCol 1 mx @?= [0, 2, 0]
    getCol 2 mx @?= [0, 0, 0]
    where v = 1 :: Int
          lst = [[1, 2, 3], [4, 5, 6]] :: [[Int]]
          mx = SparseMatrix 3 3 (Data.Map.fromList [((0, 0), 1 :: Int), ((1, 1), 2 :: Int)])

unit_multiplyMatrix = do
    multiplyMatrix v1 v2 @?= 8
    multiplyMatrix lst1 lst2 @?= lstRes
    multiplyMatrix mx1 mx2 @?= mxRes
    where
        v1 = 2 :: Int
        v2 = 4 :: Int
        lst1 = [[1, 2, 3], [4, 5, 6]] :: [[Int]]
        lst2 = [[1, 2], [3, 4], [5, 6]] :: [[Int]]
        lstRes = [[22, 28], [49, 64]] :: [[Int]]
        mx1 = SparseMatrix 3 2 (Data.Map.fromList [((0, 1), 2), ((1, 1), 4), ((1, 2), 8)])
        mx2 = SparseMatrix 4 3 (Data.Map.fromList [((0, 0), 16), ((1, 2), 32), ((2, 2), 34), ((2, 3), 63)])
        mxRes = SparseMatrix 4 2 (Data.Map.fromList [((0, 2), 64 :: Int), ((1, 2), 400 :: Int),((1, 3), 504 :: Int)])

unit_getAt = do
    getAt 0 0 v @?= 1
    getAt 0 0 lst @?= 4
    getAt 1 1 lst @?= 8
    getAt 0 0 mx  @?= 4
    getAt 2 1 mx  @?= 0
    where v = 1 :: Int
          lst = [[4, 0], [0, 8]] :: [[Int]]
          mx = SparseMatrix 3 2 (Data.Map.fromList [((0,0), 4 :: Int)])

unit_minor = do
    minor 1 1 lst @?= lstMinor
    minor 1 1 mx @?= mxMinor
    where lst = [[1, 2, 0], [4, 0, 6], [0, 8, 9]]
          lstMinor = [[1, 0], [0, 9]] :: [[Int]]
          mx = SparseMatrix 3 3 (Data.Map.fromList [
              ((0,0), 1 :: Int), ((0,1), 2 :: Int),
              ((1,0), 4 :: Int), ((1,2), 6 :: Int),
              ((2,1), 8 :: Int), ((2,2), 9 :: Int)])
          mxMinor = SparseMatrix 2 2 (Data.Map.fromList [((0,0), 1 :: Int), ((1,1), 9 :: Int)])

unit_determinant = do
    determinant v @?= v
    determinant lst1 @?= lst1Det
    determinant lst2 @?= lst2Det
    determinant lst3 @?= lst3Det
    determinant mx @?= mxDet
    where
        v = 4
        lst1 = [[4]] :: [[Int]]
        lst1Det = 4
        lst2 = [[1, 2], [3, 4]] :: [[Int]]
        lst2Det = (-2) :: Int
        lst3 = [[1, 2, 3], [5, 5, 5], [7, -2, 9]] :: [[Int]]
        lst3Det = -100 :: Int
        mx = SparseMatrix 3 3 (Data.Map.fromList [
            ((0, 0), 11 :: Int), ((0, 2), 2 :: Int),
            ((1, 1), 2 :: Int), ((1, 2), 15 :: Int),
            ((2, 0), 7 :: Int)])
        mxDet = -28
