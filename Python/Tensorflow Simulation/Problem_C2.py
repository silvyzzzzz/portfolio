# -*- coding: utf-8 -*-
"""PROBLEM C2.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1TOaMqP5_4RuR9GxJKSobaWy1lAPgn90F
"""

# =============================================================================
# PROBLEM C2
#
# Create a classifier for the MNIST Handwritten digit dataset.
# The test will expect it to classify 10 classes.
#
# Don't use lambda layers in your model.
#
# Desired accuracy AND validation_accuracy > 91%
# =============================================================================

import tensorflow as tf

class myCallback(tf.keras.callbacks.Callback):
      def on_epoch_end(self, epoch, logs={}):
        if(logs.get('val_accuracy')>0.95):
          print("\nReached 95% validation accuracy so cancelling training!")
          self.model.stop_training = True

def solution_C2():
    mnist = tf.keras.datasets.mnist
    (train_images, train_labels), (test_images, test_labels) = mnist.load_data()

    # NORMALIZE YOUR IMAGE HERE
    train_images = tf.cast(train_images, dtype=tf.float32)/255.0
    test_images = tf.cast(test_images, dtype=tf.float32)/255.0

    # DEFINE YOUR MODEL HERE
    # End with 10 Neuron Dense, activated by softmax
    model = tf.keras.Sequential([
        tf.keras.layers.Conv2D(64,(3,3),activation='relu',input_shape=(28,28,1)),
        tf.keras.layers.MaxPooling2D(2,2),
        tf.keras.layers.Conv2D(64,(3,3),activation='relu'),
        tf.keras.layers.MaxPooling2D(2,2),
        tf.keras.layers.Flatten(),
        tf.keras.layers.Dense(128,activation='relu'),
        tf.keras.layers.Dense(64,activation='relu'),
        tf.keras.layers.Dense(10,activation='softmax')
    ])

    # COMPILE MODEL HERE
    model.compile(optimizer='adam',
             loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True),
             metrics=['accuracy'])

    # TRAIN YOUR MODEL HERE
    model.fit(train_images, train_labels, epochs=20, validation_data=(test_images, test_labels), callbacks=myCallback())

    return model


# The code below is to save your model as a .h5 file.
# It will be saved automatically in your Submission folder.
if __name__ == '__main__':
    # DO NOT CHANGE THIS CODE
    model = solution_C2()
    model.save("model_C2.h5")