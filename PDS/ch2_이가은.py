# -*- coding: utf-8 -*-

import numpy as np

result = 0
for i in range(100):
    result += i
x=4
x="four"
L=list(range(10))
L
type(L[0])
L2=[str(c) for c in L]
L2
type(L2[0])
L3=[True, "2", 3.0, 4]
[type(item) for item in L3]

import array
L=list(range(10))
A=array.array('i',L)
A

np.array([1,4,2,5,3])
np.array([3.14,4,2,3])
np.array([1,2,3,4], dtype='float32')
np.array([range(i, i+3) for i in [2,4,6]])

np.zeros(10, dtype=int)
np.ones((3,5), dtype=float)
np.full((3,5), 3.14)
np.arange(0,20,2)
np.linspace(0,1,5)
np.random.random((3,3))
np.random.normal(0,1,(3,3))
np.random.randint(0,10,(3,3))
np.eye(3)
np.empty(3)

np.zeros(10, dtype='int16')
np.zeros(10, dtype=np.int16)

np.random.seed(0)
x1=np.random.randint(10, size=6)
x2=np.random.randint(10, size=(3,4))
x3=np.random.randint(10, size=(3,4,5))

print("x3 ndim:", x3.ndim)
print("x3 shape:", x3.shape)
print("x3 size:", x1.size)
print("x3 dtype:", x3.dtype)
print("item size:", x3.itemsize, "bytes")
print("nbytes:", x3.nbytes, "bytes")

x1
x1[0]
x1[4]
x1[-1]
x1[-2]

x2
x2[0,0]
x2[2,0]
x2[2,-1]
x2[0,0]=12
x2
x1[0]=3.14159
x1

x=np.arange(10)
x
x[:5]
x[5:]
x[4:7]
x[::2]
x[1::2]
x[::-1]
x[5::-2]

x2
x2[:2,:3]
x2[:3,::2]
x2[::-1,::-1]
print(x2[:,0])
print(x2[0,:])
print(x2[0])

print(x2)
x2_sub = x2[:2,:2]
print(x2_sub)
x2_sub[0,0]=99
print(x2_sub)
print(x2)

x2_sub_copy = x2[:2,:2].copy()
x2_sub_copy[0,0]=42
print(x2_sub_copy)
print(x2)

grid=np.arange(1,10).reshape(3,3)
print(grid)
x=np.array([1,2,3])
x.reshape(1,3)
x[np.newaxis, :]
x.reshape((3,1))
x[:, np.newaxis]

x=np.array([1,2,3])
y=np.array([3,2,1])
np.concatenate([x,y])
z=[99,99,99]
print(np.concatenate([x,y,z]))

grid=np.array([[1,2,3], [4,5,6]])
np.concatenate([grid,grid])
np.concatenate([grid,grid], axis=1)

x=np.array([1,2,3])
grid=np.array([[9,8,7], [6,5,4]])
np.vstack([x,grid])
y=np.array([[99],[99]])
np.hstack([grid,y])

x=[1,2,3,99,99,3,2,1]
x1, x2, x3 = np.split(x,[3,5])
print(x1, x2, x3)

grid=np.arange(16).reshape((4,4))
grid
upper, lower = np.vsplit(grid, [2])
print(upper)
print(lower)
left, right = np.hsplit(grid, [2])
print(left)
print(right)

# 유니버설 함수
import numpy as np
np.random.seed(0)
def compute_reciprocals(values):
    output = np.empty(len(values))
    for i in range(len(values)):
        output[i] = 1.0/values[i]
    return output
values = np.random.randint(1,10,size=5)
compute_reciprocals(values)

big_array=np.random.randint(1,100,size=1000000)
%timeit compute_reciprocals(big_array)

print(compute_reciprocals(values))
print(1.0/values)
np.arange(5)/np.arange(1,6)
x=np.arange(9).reshape((3,3))
2**x

import math
import numpy as np
x=np.arange(4)
print(x, x+5, x-5, x*2, x/2, x//2)
np.add(x,2)

x=np.array([-2,-1,0,1,2])
abs(x)
np.absolute(x)

theta=np.linspace(0, np.pi, 3)
print(theta, np.sin(theta), np.cos(theta), np.tan(theta))
x=[-1,0,1]
print(x, np.arcsin(x), np.arccos(x), np.arctan(x))
x=[0,0.001,0.01,0.1]
np.expm1(x)
np.log1p(x)

from scipy import special
x=[1,5,10]
special.gamma(x)
special.gammaln(x)
special.beta(x,2)

x=np.arange(5)
y=np.empty(5)
np.multiply(x,10,out=y)
y
y=np.zeros(10)
np.power(2,x,out=y[::2])
y

x=np.arange(1,6)
np.add.reduce(x)
np.multiply.reduce(x)
np.add.accumulate(x)
np.multiply.accumulate(x)
np.multiply.outer(x,x)

import numpy as np
L=np.random.random(100)
sum(L)
np.sum(L)
big_array=np.random.rand(1000000)
%timeit sum(big_array)
%timeit np.sum(big_array)
min(big_array), max(big_array)
np.min(big_array), np.max(big_array)
%timeit min(big_array)
%timeit np.min(big_array)
big_array.min(), big_array.max(), big_array.sum()

M=np.random.random((3,4))
M.sum()
M.min(axis=0)
M.max(axis=1)

#broadcasting
import numpy as np
a=np.array([0,1,2])
b=np.array([5,5,5])
a+b
a+5
M=np.ones((3,3))
M+a
a=np.arange(3)
b=np.arange(3)[:, np.newaxis]
a+b
X=np.random.random((10,3))
Xmean=X.mean(0)
Xmean
X_centered=X-Xmean
X_centered.mean(0)
x=np.linspace(0,5,50)
y=np.linspace(0,5,50)[:,np.newaxis]
z=np.sin(x)**10+np.cos(10+y+x)*np.cos(x)
%matplotlib inline
import matplotlib.pyplot as plt
plt.imshow(z, origin='lower', extent=[0,5,0,5], cmap='viridis')
plt.colorbar()

x=np.array([1,2,3,4,5])
x<3
x>3
x<=3
x>=3
x!=3

(2*x)==(x**2)

rng=np.random.RandomState(0)
x=rng.randint(10,size=(3,4))
x
x<6
print(x)
np.count_nonzero(x<6)
np.sum(x<6)
np.sum(x<6,axis=1)
np.any(x>8)
np.all(x<10)
np.all(x==6)
np.all(x<8, axis=1)
x
x[x<5]

#fancy indexing
import numpy as np
rand=np.random.RandomState(42)
x=rand.randint(100,size=10)
print(x)
[x[3],x[7],x[2]]
ind=[3,7,4]
x[ind]
ind=np.array([[3,7],[4,5]])
x[ind]
x=np.arange(10)
i=np.array([2,1,8,4])
x[i]=99
x
x[i]-=10
x
x=np.zeros(10)
x[[0,0]]=[4,6]
x
x=np.zeros(10)
np.add.at(x,i,1)
x






