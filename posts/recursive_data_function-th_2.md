---
title: คิดเล่น ๆ กับ Haskell | Recursive data type และ Morphism, ตอนที่ 2
date: 2019-07-13
keywords: [haskell, filter, map, sum, fold, list]
language: th
---
| [ตอนที่ 1 ](/posts/recursive_data_function-th_1)
| [ตอนที่ 2 ](/posts/recursive_data_function-th_2)
| [ตอนที่ 3 ](/posts/recursive_data_function-th_3)
|

*TLDR:* Recursive algorithm ทำงานกับ recursive data type ได้อย่างเป็นธรรมชาติ เพราะเดินไปตามโครงสร้างของข้อมูล

## ตอนที่ 2 : Recursive data type

### List
Concept ของ List ใน Haskell นิยามแบบ recursive ได้ประมาณนี้

```Haskell
data List a = Empty
            | Cons a (List a)

xs :: List Int
xs = Cons 1 (Cons 2 (Cons 3 Empty))
```

อ่านเป็นภาษามนุษย์ได้ประมาณว่า
> List ของ a เป็นได้สองแบบคือ Empty หรือ การต่อกัน (Cons) ของ a กับ List ของ a

เพื่อความสะดวกในการเขียนและอ่าน Haskell มีการใช้ syntactic sugar ด้วยการใช้ `[]` สำหรับ `Empty` และ `:` สำหรับ `Cons`

```haskell
λ> :i []
data [] a = [] | a : [a]  -- Defined in ‘GHC.Types’

xs :: [Int]
xs = [1,2,3]              -- 1 : 2 : 3 : []
```

### Function ของ List

function ที่ทำงานกับ List นิยามตามโครงสร้างของ List เองแยกเป็น 2 กรณี ได้แก่ 
- กรณี Empty ([])
- กรณี Cons  (x:xs)

ตัวอย่าง: `map`

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map xs
```

```haskell
λ> map (+1) [1,2,3]
[2,3,4]
```

ตัวอย่าง: `filter`

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) | p x       = x : filter p xs
                | otherwise = filter p xs
```

```haskell
λ> filter (>1) [1,2,3]
[2,3]
```
ตัวอย่าง: `sum` (Int specialiaztion เพื่อความเข้าใจง่าย)

```haskell
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs
```

```haskell
λ> sum [1,2,3]
6
```

จะเห็นได้ว่า `map` และ `filter` มีโครงสร้างคล้ายคลึงกันมาก (`sum` ยกไว้ก่อน ในภายหลังจะแสดงให้เห็นว่ามีโครงสร้างไม่ต่างกัน)

ด้วยเหตุนี้จึงมีการแยกหัวใจ (essense) ของ function บน List ออกมาเป็น `foldr`

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = x `f` foldr f z xs
```
และเขียน `map`, `filter` และ `sum` ในรูปของ `foldr`

```haskell
-- map f = foldr (\x xs -> f x : xs) []
--       = foldr (\x xs -> (:) (f x) xs) []
--       = foldr (\x -> (:) (f x)) []
--       = foldr (\x -> ((:) . f) x) []

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []
```

```haskell
-- filter p = foldr (\x xs -> if p x then x : xs else xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr g []
  where g x xs | p x       = x : xs
               | otherwise = xs
```

```haskell
sum :: [Int] -> Int
sum = foldr (+) 0
```

### List comprehension

ใน Python มีการยุบรวม `map` และ `filter` เป็น expression เดียวกันโดยใข้ List comprehension

```python
>> xs = [1,2,3,4]
>> [i * i for i in xs if i > 2]
[9, 16]
```

ใน Haskell ก็มี List comprehension เช่นเดียวกัน ซึ่งมีไวยากรณ์ไกล้เคียงกับ [set builder notation](https://en.wikipedia.org/wiki/Set-builder_notation)

```haskell
λ> xs =  [1,2,3,4]
λ> [ i * i | i <- xs, i > 2]
[9,16]
```
[ตอนหน้า](/posts/recursive_data_function-th_3) เขียนเรื่องการเปลี่ยนรูปของข้อมูล
