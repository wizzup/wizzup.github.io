---
title: คิดเล่น ๆ กับ Haskell | Recursive data type และ Morphism, ตอนที่ 3
date: 2019-07-14
keywords: [filter, map, sum, fold, list]
---
|[ตอนที่ 1 ](/posts/recursive_data_function-th_1)
|[ตอนที่ 2 ](/posts/recursive_data_function-th_2)
|[ตอนที่ 3 ](/posts/recursive_data_function-th_3)
|

## ตอนที่ 3 : Recursive algorithm เพิ่มเติม

*ข้อสังเกต* เนื้อหาในตอนที่ 3 เป็นต้นไปเน้นไปที่ recursive data type ซึ่งบางเรื่องอาจไม่เกี่ยวข้องกับโจทย์ปัญหาจากตอนที่ 1 อีกต่อไป

### List

นอกจาก `filter`, `map` และ `sum` แล้ว ยังมี function บน List ที่นิยามแบบ recursive อีกหลายตัว จะยกตัวอย่างบาง function มาเพื่อแสดงความเขื่อมโยงของ algorithm กับ structure ดังนี้

```haskell
-- product []     = 1
-- product (x:xs) = x * product xs

product :: [Int] -> Int
product = foldr (*) 1
```

```bash
λ> product [1,2,3,4]
24
```

```haskell
-- length []     = 0
-- length (_:xs) = 1 + length xs

-- length = foldr (\_ b -> 1 + b) 0
-- length = foldr (\_ -> (1 +)) 0

length :: [a] -> Int
length = foldr (const (1 +)) 0
```

```bash
λ> length [1,2,3,4]
4
```

จะเห็นได้ว่า function บน List สามารถนิยามตามโครงสร้างของ List ตามที่เขียนไว้ในตอนที่ 2

อย่างไรก็ตาม ไม่ใช่ทุก function บน List จะนิยามได้ด้วย foldr ซึ่งถือเป็นกรณีเฉพาะที่เรียกว่า foldable

หากสนใจ function บน List ทั้งหมดที่มีให้ใช้ใน base library สามารถเข้าไปอ่านเพิ่มเติมได้ [ที่นี่](https://hackage.haskell.org/package/base/docs/Data-List.html)

### Binary Tree

Binary tree นิยามแบบ recursive ได้เช่นเดียวกันกับ List

```haskell
data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
  deriving Show -- บรรทัดนี้เพื่อให้สามารถแสดงผลออกทาง REPL ได้

a :: Tree Int
a = Leaf 0

b :: Tree Int
b = Branch (Leaf 1) a

c :: Tree Int
c = Branch b b

d :: Tree Int
d = Branch b c

```

```bash
λ> a
Leaf 0

λ> b
Branch (Leaf 1) (Leaf 0)

λ> c
Branch (Branch (Leaf 1) (Leaf 0)) (Branch (Leaf 1) (Leaf 0))

λ> d
Branch (Branch (Leaf 1) (Leaf 0)) (Branch (Branch (Leaf 1) (Leaf 0)) (Branch (Leaf 1) (Leaf 0)))
```

function บน Tree ก็มีการเขียนตามโครงสร้างของ Tree คือเป็น 2 กรณี

- Leaf
- Branch

ตัวอย่าง: `nodeCount` นับจำนวน leaf ทั้งหมดใน Tree

```haskell
nodeCount :: Tree a -> Int
nodeCount (Leaf _)     = 1
nodeCount (Branch l r) = nodeCount l + nodeCount r
```

```bash
λ> map nodeCount [a,b,c,d]
[1,2,4,6]
```

ตัวอย่าง: `branchCount` นับจำนวน Branch ทั้งหมดใน Tree

```haskell
branchCount :: Tree a -> Int
branchCount (Leaf _)     = 0
branchCount (Branch l r) = 1 + branchCount l + branchCount r
```

```bash
λ> map branch [a,b,c,d]
[0,1,3,5]
```

ตอนหน้าจะเขียนเรื่องของการ algorithm และ การ fold Tree เพิ่มเติม
