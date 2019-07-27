def valid(i,j,k):
    """
    >>> valid(3,4,5)
    True
    >>> valid(5,4,3)
    False
    """
    return (k**2 == i**2 + j**2)

n = 20
r = range(1,n)

ans = [(x,y,z) for x in r for y in r for z in r if valid(x,y,z)]
for i in ans:
    print(i)

# for x in r:
#     for y in r:
#         for z in r:
#             if valid(x,y,z):
#                 print (x,y,z)

