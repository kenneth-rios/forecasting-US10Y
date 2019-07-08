# -*- coding: utf-8 -*-
"""
Created on Sun Dec 30 19:07:27 2018

@author: kenri
"""

import tensorflow as tf
from tensorflow.examples.tutorials.mnist import input_data


"""
Feedforward deep neural network:

Input layer > weights > Hidden layer 1 > weights > 
Hidden layer 2 > weights > Output layer.

---
Train the model using the training data

Use loss function (MSE, cross-entropy, etc.) to measure the model's prediction error

Minimize loss function w.r.t. deep learning parameters (weights/biases) using stochastic gradient descent
via backpropagation.
---

Feedforward + backpropagation = an epoch

Each additional epoch will lower the cost function up to a point of diminishing returns

"""

mnist = input_data.read_data_sets("/tmp/data/", one_hot=True)


# 10 classes, from 0 to 9
"""
One-hotting:
    
0 = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
1 = [0, 1, 0, 0, 0, 0, 0, 0, 0, 0]
2 = [0, 0, 1, 0, 0, 0, 0, 0, 0, 0]
3 = [0, 0, 0, 1, 0, 0, 0, 0, 0, 0]

"""

n_nodes_hl1 = 500
n_nodes_hl2 = 500
n_nodes_hl3 = 500

n_classes = 10
batch_size = 100

x = tf.placeholder('float', [None, 784])
y = tf.placeholder('float')


def neural_network(data):
    """
    (input_data * weights) + biases
    
    """
    
    hidden_1_layer = {'weights':tf.Variable(tf.truncated_normal([784, n_nodes_hl1], stddev=0.1)),
                      'biases':tf.Variable(tf.constant(0.1, shape=[n_nodes_hl1]))}

    hidden_2_layer = {'weights':tf.Variable(tf.random_normal([n_nodes_hl1, n_nodes_hl2])),
                      'biases':tf.Variable(tf.random_normal([n_nodes_hl2]))}

    hidden_3_layer = {'weights':tf.Variable(tf.random_normal([n_nodes_hl2, n_nodes_hl3])),
                      'biases':tf.Variable(tf.random_normal([n_nodes_hl3]))}

    output_layer = {'weights':tf.Variable(tf.random_normal([n_nodes_hl3, n_classes])),
                    'biases':tf.Variable(tf.random_normal([n_classes]))}
    
    l1 = tf.add(tf.matmul(data, hidden_1_layer['weights']), hidden_1_layer['biases'])
    l1 = tf.nn.relu(l1)

    l2 = tf.add(tf.matmul(l1, hidden_2_layer['weights']), hidden_2_layer['biases'])
    l2 = tf.nn.relu(l2)

    l3 = tf.add(tf.matmul(l2, hidden_3_layer['weights']), hidden_3_layer['biases'])
    l3 = tf.nn.relu(l3)

    output = tf.matmul(l3, output_layer['weights']) + output_layer['biases']

    return output



def train_neural_network(x, y, n_epochs=15):
    
    prediction = neural_network(x)
    loss = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=prediction, labels=y))
    optimizer = tf.train.AdamOptimizer().minimize(loss)
    
    with tf.Session() as sess:
        
        sess.run(tf.global_variables_initializer())
        
        for epoch in range(n_epochs):
            
            epoch_loss = 0
            
            for _ in range(int(mnist.train.num_examples / batch_size)):
                
                epoch_x, epoch_y = mnist.train.next_batch(batch_size)
                _, c = sess.run([optimizer, loss], feed_dict={x: epoch_x, y: epoch_y})
                
                epoch_loss += c
            
            print("Epoch", epoch, "completed out of", n_epochs, "loss:", epoch_loss)


        correct = tf.equal(tf.argmax(prediction, 1), tf.argmax(y, 1))
        accuracy = tf.reduce_mean(tf.cast(correct, 'float'))        
        print('Accuracy:', accuracy.eval({x:mnist.test.images, y:mnist.test.labels}))



train_neural_network(x, y)

 