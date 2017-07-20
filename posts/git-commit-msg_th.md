---
title: วิธีแก้ข้อความ commit ของ git
date: 2017-07-20
---

เมื่อต้องการแก้ไข commit message

**ระดับ 1:** แก้ข้อความล่าสุด

    git commit --amend -m "ข้อความใหม่"

**ระดับ 2:** แก้ข้อความจำนวนน้อยทีละข้อความ

    git rebase -i <rev_before>

จะมี rebase todo-list ขึ้นมา เลือก `reword` กับ commit ที่ต้องการแก้ไขแล้วบันทึก จากนั้น git จะเรียก editor ขึ้นมาให้แก้ข้อความเมื่อถึงคิวของ commit นั้น (แก้ใน rebase-todo ไม่ได้ จะถูกโยนทิ้งไป)

```git
pick db96dee move newpost.sh out from posts
reword 4cd9e33 2017-06-02
pick f19d2af add socials features

# Rebase e01466e..f19d2af onto e01466e (3 commands)
#
# Commands:
# p, pick = use commit
# r, reword = use commit, but edit the commit message
# e, edit = use commit, but stop for amending
# s, squash = use commit, but meld into previous commit
# f, fixup = like "squash", but discard this commit's log message
# x, exec = run command (the rest of the line) using shell
# d, drop = remove commit
```

**ระดับ 3:** แก้หลายข้อความด้วยคำสั่งแทนค่าอย่างง่าย

    git filter-branch --msg-filter "<คำสั่งแทนค่า>" <rev_begin>..<rev_end>

*ตัวอย่าง* : เพิ่มข้อความ `Fix Bug::` ข้างหน้าข้อความ จาก commit ของ master branch ถึง commit ล่าสุด

    git filter-branch --msg-filter 'sed "s/^/Fix Bug::/" master..HEAD

**ระดับ 3.5:** แก้หลายข้อความด้วยคำสั่งแทนค่าแบบซับซ้อน

    git filter-branch --msg-filter "<โปรแกรม>" <rev_begin>..<rev_end>

ใช้โปรแกรมหรือ script รับข้อความเก่า จาก stdin แล้วส่งขัอความใหม่ ออก stdout

***

```text
rev_before: revison ก่อนหน้า revison แรกที่ต้องการแก้ไขข้อความ
rev_begin:  revison แรกของช่วงที่ต้องการแก้ข้อความ
rev_end:    revison สุดท้ายของช่วงที่ต้องการแก้ข้อความ
```
