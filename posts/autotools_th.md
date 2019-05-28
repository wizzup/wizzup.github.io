---
title: Autotools ตามที่เข้าใจ - Hello world

date: 2019-04-04

---

บทความนี้นำเสนอตัวอย่างการสร้าง `Hello world` application ที่เขียนด้วยภาษา `C` โดยใช้ `GNU autotools` เป็นเครื่องมือจัดการโครงการในระดับเบื้องต้น <!--more--> ตัวอย่างโครงการขนาดเล็กมีไฟล์ source เพียงไฟล์เดียว ไม่รวมการใช้งาน library ภายนอก, เครื่องมือเสริมของ `autotools` (เช่น `aclocal`, `autoheader`, `libtool`) และโครงการขนาดใหญ่ ที่มีไฟล์ header และ source จำนวนมาก

ไฟล์ที่สร้างขึ้นทั้งหมด อยู่ใน [gist](https://gist.github.com/wizzup/1298a13da32ab80bff7ca2856e5e2b67)

![](https://devmanual.gentoo.org/general-concepts/autotools/diagram.png)

รูปประกอบจาก [gentoo.org](https://devmanual.gentoo.org/general-concepts/autotools/index.html) (CC BY-SA 3.0)


* `autoconf` ช่วยในการค้นหาและตั้งค่าเครื่องมือที่จำเป็นต้องใช้เช่น compiler, library และสร้าง `Makefile`

* `automake` สร้าง `Makefile.in` จากโครงแบบ `Makefile.am`

ในการใช้งานปกติ สามารถใช้ `autoreconf` ซึ่งเป็น script ในระดับที่[สูงกว่า](https://stackoverflow.com/questions/27285052/difference-between-autoconf-and-autoreconf) ไม่จำเป็นต้องเรียกใช้ `autoconf` และ `automake` เอง


## 0. สิ่งที่ต้องติดตั้งเรียบร้อยแล้วได้แก่ `automake`, `autoconf` และ C compiler (`gcc`)

```
$ automake --version
automake (GNU automake) 1.16.1
...

$ autoconf --version
autoconf (GNU Autoconf) 2.69
...

$ cc --version
gcc (GCC) 7.4.0
...
```

## 1. เริ่มต้นด้วยไฟล์ที่จำเป็น `configure.ac` และ `Makefile.am`

### 1.1 `configure.ac` สร้างใหม่หรือแก้จากต้นแบบ `configure.scan` ที่สร้างด้วย `autoscan`

ไฟล์ `configure.ac` อาจพบอยู่ในชื่อ `configure.in` ในบางโครงการที่เป็นโครงการเก่า อย่างไรก็ตาม [แนะนำ](https://stackoverflow.com/questions/3782994/any-difference-between-configure-ac-and-configure-in-and-makefile-am-and-makefi)ให้ใช้ `.ac` สำหรับโครงการใหม่ เพื่อบ่งชี้ว่าเป็น input ของ `autoconf` และไม่ให้สับสนกับ `.in` ทีสร้างระหว่างการทำงานของเครื่องมืออื่น

<script src="https://gist.github.com/wizzup/1298a13da32ab80bff7ca2856e5e2b67.js?file=configure.ac"></script>

<!-- ``` -->
<!-- # กำหนด version ต่ำสุด ของ autoconf -->
<!-- AC_PREREQ([2.69]) -->
<!--  -->
<!-- # ตั้งชื่อ package, version และ ช่องทางสำหรับรายงาน bug -->
<!-- AC_INIT([hello], [0.0.1], [hello-bug@example.com]) -->
<!--  -->
<!-- # ตั้งค่าเริ่มต้น automake (foreign หมายถึงไม่ใช่ GNU project คือไม่ต้องมี README, INSTALL, ..) -->
<!-- AM_INIT_AUTOMAKE([foreign]) -->
<!--  -->
<!-- # บอก autoconf ให้ใช้สร้าง Makefile (จาก Makefile.in) -->
<!-- AC_CONFIG_FILES([Makefile]) -->
<!--  -->
<!-- # ตรวจสอบ compiler (C)่่ -->
<!-- AC_PROG_CC -->
<!--  -->
<!-- # สร้าง configure script และ Makefile -->
<!-- AC_OUTPUT -->
<!-- ``` -->

### 1.2 `Makefile.am` สร้างใหม่

<script src="https://gist.github.com/wizzup/1298a13da32ab80bff7ca2856e5e2b67.js?file=Makefile.am"></script>

<!-- ``` -->
<!-- # สร้าง binary ชื่อ hello -->
<!-- bin_PROGRAMS = hello -->
<!--  -->
<!-- # สร้าง hello จาก main.c -->
<!-- hello_SOURCES = main.c -->
<!-- ``` -->

### 1.3 `main.c` สร้างใหม่

<script src="https://gist.github.com/wizzup/1298a13da32ab80bff7ca2856e5e2b67.js?file=main.c"></script>

<!-- ``` -->
<!-- #include<stdio.h> -->
<!--  -->
<!-- int main(void){ -->
<!--   printf("Hello World\n"); -->
<!-- } -->
<!-- ``` -->

## 2. script และ application

### 2.1 `configure` script

เรียก `autoreconf` เพื่อสร้าง `configure` (ถ้าพบปัญหาอาจต้องใช้ `autoreconf -f` ร่วมกับ `automake --add-missing`)

```
$ autoreconf --install
```

GNU package ระดับมืออาชีพ อาจมีแถม `autogen.sh` script เพื่อสร้าง `configure` script ในลักษณะเดียวกัน

```
#! /bin/sh

aclocal \
&& automake --add-missing \
&& autoconf
```

อย่างไรก็ตามข้อมูลจาก[บางแหล่ง](https://stackoverflow.com/questions/1970926/whats-the-point-of-aclocal)ชึ้ว่า `aclocal` อาจไม่จำเป็นต้องใช้

### 2.2 `Makefile`
เรียก `configure` เพื่อสร้าง `Makefile`

```
$ ./configure
checking for a BSD-compatible install... /nix/store/d9s1kq1bnwqgxwcvv4zrc36ysnxg8gv7-coreutils-8.30/bin/install -c
checking whether build environment is sane... yes
checking for a thread-safe mkdir -p... /nix/store/d9s1kq1bnwqgxwcvv4zrc36ysnxg8gv7-coreutils-8.30/bin/mkdir -p
checking for gawk... gawk
checking whether make sets $(MAKE)... yes
checking whether make supports nested variables... yes
checking for gcc... gcc
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables...
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether gcc accepts -g... yes
checking for gcc option to accept ISO C89... none needed
checking whether gcc understands -c and -o together... yes
checking whether make supports the include directive... yes (GNU style)
checking dependency style of gcc... none
checking that generated files are newer than configure... done
configure: creating ./config.status
    config.status: creating Makefile
config.status: executing depfiles commands
```
### 2.3 `hello` application
เรียก `make` เพื่อสร้าง `hello`

```
$ make
gcc -DPACKAGE_NAME=\"hello\" -DPACKAGE_TARNAME=\"hello\" -DPACKAGE_VERSION=\"0.0.1\" -DPACKAGE_STRING=\"hello\ 0.0.1\" -DPACKAGE_BUGREPORT=\"hello-bug@example.com\" -DPACKAGE_URL=\"\" -DPACKAGE=\"hello\" -DVERSION=\"0.0.1\" -I.     -g -O2 -MT main.o -MD -MP -MF .deps/main.Tpo -c -o main.o main.c
mv -f .deps/main.Tpo .deps/main.Po
gcc  -g -O2   -o hello main.o
```

สุดท้ายได้ `hello` binary

```
$ ./hello
Hello World
```

---

แหล่งข้อมูลเพิ่มเติม:

- https://devmanual.gentoo.org/general-concepts/autotools/
- https://autotools.io/index.html
- https://www.gnu.org/software/automake/faq/autotools-faq.html
