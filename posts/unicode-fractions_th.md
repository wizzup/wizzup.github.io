---
title: เขียนเศษส่วนด้วย Unicode subscript/superscript
date: 2019-07-08
---

*คำเตือน:* บทความนี้แสดงผลด้วย Unicode อาจมีตัวอักษรที่แสดงผลไม่ถูกต้องถ้าเข้าถึงด้วย web browser รุ่นที่ไม่รองรับ

Unicode [fractions](http://unicodefractions.com) มีตัวอักษรสำหรับเลขเศษส่วนมาให้จำนวนหนึ่งเช่น ½, ⅔, และ ⅒

ปัญาหาที่พบคือต้องใช้เศษส่วนที่ไม่มีใน fractions ซึ่งสามารถใช้ตัวยก `⁰¹²³⁴⁵⁶⁷⁸⁹` และตัวห้อย `₀₁₂₃₄₅₆₇₈₉` กับตัวขั้นเศษส่วน ` ⁄` เขียนได้

ตัวอย่าง:

¹⁹⁄₈₉, ²³⁴⁄₇₈₉

ถ้าต้องการเขีนเศษส่วนใด ๆ ได้ง่ายขึ้นโดยไม่ต้อง copy/paste ทีละตัวอักษร สามารถใช้ python script นี้ช่วย

```python

"""
Usage:
  unicode-fract.py (-h | --help)
  unicode-fract.py -i
  unicode-fract.py -t

Convert a fraction to unicode superscript/subscript pairs.
Example: 123/456 to ¹²³⁄₄₅₆

Options:
  -h --help
  -i       interactive mode, use stdin as input
  -t       running doctest for this module (for internal use)

"""

from sys import exit, stderr
from docopt import docopt
from doctest import testmod


sups = "⁰¹²³⁴⁵⁶⁷⁸⁹"
subs = "₀₁₂₃₄₅₆₇₈₉"
sep  = "⁄"

def eprint(s):
    print(s, file=stderr)

def match(c,s):
    """
    >>> match('0',sups)
    '⁰'
    >>> match('9',sups)
    '⁹'
    """

    try:
        i = int(c)
        return s[i] if i <= len(s) else None
    except:
        return None

def convert(num,den):
    """
    >>> convert('123','456')
    '¹²³⁄₄₅₆'
    """

    if num == None or den == None:
        return None
    else:
        ns = ''.join([match(i,sups) for i in num])
        ds = ''.join([match(i,subs) for i in den])
        return (f'{ns}{sep}{ds}')

def is_number(n):
    """
    >>> False in [is_number(i) for i in "0123456789"]
    False
    >>> True in [is_number(i) for i in "abcdefghijk"]
    False
    """

    try:
        int(n)
        return True
    except:
        return False

def read(s):
    """
    >>> read("123/456")
    ('123', '456')
    >>> read("abc/def") == None
    True
    """

    try:
        num,den = s.split('/')
        return((num,den) if is_number(num) and is_number(den) else None)
    except:
        return None

def get_input():
    try:
        print('> ', file=stderr, end='')
        i = input()
        r = read(i)
        (num,den) = r if r != None else (None,None)

        out = convert(num,den)
        if out != None:
            print(out)
        else:
            eprint('Error')

        return i
    except EOFError:
        pass
    except:
        return None

def TODO():
    eprint('Unimplement features!')
    eprint('Congraturation! You have reach the end of the world.')
    exit(1)

if __name__ == '__main__':
    args = docopt(__doc__)

    if args['-t']:
        eprint("Running doctest ...")
        testmod(verbose=True)

        eprint(50*'=')
        eprint(f'{sups}{sep}{subs}')
    elif args['-i']:
        eprint('Running in interactive mode')
        eprint('enter a fraction, <Ctrl-C> to quit, invalid number will be ignored')

        while get_input() != None:
            pass
    else:
        TODO()
```




