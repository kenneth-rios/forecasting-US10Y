# -*- coding: utf-8 -*-
"""
Created on Sun Dec 30 18:15:47 2018

@author: kenri
"""

import tensorflow as tf

x1 = tf.constant(5)
x2 = tf.constant(6)

result = tf.multiply(x1, x2)

print(result)

with tf.Session() as sess:
    output = sess.run(result)
    print(output)  # Save computation graph as a Python variable!
    
print(output)

